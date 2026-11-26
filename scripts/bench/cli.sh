#!/usr/bin/env bash
# Measure the oxabl command line on large inputs and print one table.
#
# Usage: scripts/bench/cli.sh [--preset ci|medium|erp] [--tools time,heaptrack,callgrind]
#                             [--only NAME[,NAME...]] [--out DIR] [--no-build]
#
# Surfaces (NAME):
#   check-lsp      oxabl check resources/lsp_bench.p -I resources
#   check-big      oxabl check big.p               (1 MB: the bench_parser_* fixtures, 40 times)
#   check-ws       oxabl check src                  (in the generated workspace: no finding, no drift)
#   check-ws-findings  oxabl check --include-findings src  (the same, with findings in the hot includes)
#   format-big     oxabl format --check big.p
#   format-ws      oxabl format --check src         (in the generated workspace)
#   conform-big    oxabl conformance -j 1 big.p
#   explain-lsp    oxabl explain resources/lsp_bench.p -I resources
#
# Tools:
#   time       GNU time -v: wall time and peak RSS of the shipped binary (jemalloc).
#   heaptrack  peak heap and allocation calls. heaptrack sees only the C library's
#              allocator, so this runs a second binary built with
#              RUSTFLAGS=--cfg oxabl_system_allocator, in its own target directory.
#   callgrind  instructions of the shipped binary, the inclusive share and calls of
#              `tokenize` and `parse_program` ("full" calls lex or parse a whole file,
#              "small" ones a short piece of it), and the self share of the allocator,
#              memcpy and unnamed C library code. The walks (check-ws*, format-ws)
#              run with -j 1 here, so the counts are the same on each run and two
#              commits compare on a loaded machine. About 50 times slower than the run.
#
# Everything is written below --out (default target/bench-cli). The workspaces
# are generated with scripts/bench/gen-workspace, and made again when the
# generator changes. A missing tool is taken from nixpkgs through `nix shell`
# when nix is on PATH, else the script stops. Exit 0 only when every command
# exited with the status its input gives and wrote no "error:" line.

set -euo pipefail

repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
preset=ci
tools=time,heaptrack,callgrind
only=
out=$repo/target/bench-cli
build=1

while [ $# -gt 0 ]; do
    case $1 in
        --preset) preset=$2; shift 2 ;;
        --tools) tools=$2; shift 2 ;;
        --only) only=$2; shift 2 ;;
        --out) out=$2; shift 2 ;;
        --no-build) build=0; shift ;;
        -h | --help) sed -n '2,33p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
        *) echo "cli.sh: unknown argument $1" >&2; exit 2 ;;
    esac
done

wants() { case ",$tools," in *",$1,"*) return 0 ;; *) return 1 ;; esac; }

# The tools this run needs, from nixpkgs when one is missing.
missing=()
gnu_time=$(type -P time || true)
if wants time && [ -z "$gnu_time" ]; then missing+=(nixpkgs#time); fi
if wants heaptrack && ! command -v heaptrack_print >/dev/null; then missing+=(nixpkgs#heaptrack); fi
if wants callgrind && ! command -v callgrind_annotate >/dev/null; then missing+=(nixpkgs#valgrind); fi
if [ ${#missing[@]} -gt 0 ]; then
    if [ -n "${OXABL_CLI_BENCH_NIX:-}" ] || ! command -v nix >/dev/null; then
        echo "cli.sh: missing tools: ${missing[*]} (install them or put nix on PATH)" >&2
        exit 2
    fi
    export OXABL_CLI_BENCH_NIX=1
    again=(--preset "$preset" --tools "$tools" --only "$only" --out "$out")
    [ $build = 1 ] || again+=(--no-build)
    exec nix shell "${missing[@]}" --command "$0" "${again[@]}"
fi

# Every run parses the schema, so each tool and each commit measures the same
# work. Set OXABL_SCHEMA_CACHE=on to measure runs that read the cache.
export OXABL_SCHEMA_CACHE=${OXABL_SCHEMA_CACHE:-off}

mkdir -p "$out"
out=$(cd "$out" && pwd)
shipped=$out/oxabl
system=$out/oxabl-system-allocator

# The binary that `cargo build` wrote, from cargo itself, so CARGO_TARGET_DIR
# and a target directory in .cargo/config are followed.
built_binary() {
    cargo build --release --manifest-path "$repo/Cargo.toml" -p oxabl --bin oxabl \
        --message-format=json-render-diagnostics |
        python3 -I -c '
import json, sys
found = [m["executable"] for m in map(json.loads, sys.stdin)
         if m.get("reason") == "compiler-artifact" and m.get("executable")
         and m["target"]["name"] == "oxabl"]
if len(found) != 1:
    sys.exit("cli.sh: cargo named no oxabl binary")
print(found[0])'
}

if [ $build = 1 ]; then
    cp "$(built_binary)" "$shipped"
    if wants heaptrack; then
        # Its own target directory: the flag changes every crate, and a shared
        # directory would rebuild the workspace on each switch.
        target=$(cargo metadata --format-version 1 --no-deps --manifest-path "$repo/Cargo.toml" |
            python3 -I -c 'import json, sys; print(json.load(sys.stdin)["target_directory"])')
        cp "$(CARGO_TARGET_DIR=$target/system-allocator RUSTFLAGS="${RUSTFLAGS:-} --cfg oxabl_system_allocator" built_binary)" "$system"
    fi
fi
for binary in "$shipped" $(wants heaptrack && echo "$system"); do
    [ -x "$binary" ] || { echo "cli.sh: no $binary (run without --no-build)" >&2; exit 2; }
done
# Through a file: under pipefail, `nm | grep -q` gives SIGPIPE on a match and
# so reads as no match.
if wants heaptrack; then
    nm "$system" > "$out/system.nm" 2>/dev/null || true
    [ -s "$out/system.nm" ] || { echo "cli.sh: nm read no symbol of $system" >&2; exit 2; }
fi
if wants heaptrack && grep -q _rjem_ "$out/system.nm"; then
    echo "cli.sh: $system links jemalloc, which heaptrack does not see" >&2
    exit 2
fi

# The inputs, each made again when what makes it changed: a stamp beside it
# holds a hash of its sources. Each is written to a temporary name and moved
# into place, so an interrupted run leaves nothing that looks complete.
stamp_of() { cat "$@" | sha256sum | cut -d' ' -f1; }
parts="expressions declarations control_flow oo_abl procs_funcs datasets compiler_forms type_spellings name_forms skipped_forms"
part_files=()
for part in $parts; do part_files+=("$repo/resources/bench_parser_$part.abl"); done
stamp=$(stamp_of "${part_files[@]}")
if [ "$(cat "$out/big.p.stamp" 2>/dev/null)" != "$stamp" ]; then
    rm -f "$out/big.p" "$out/big.p.stamp"
    for _ in $(seq 1 40); do cat "${part_files[@]}"; done > "$out/big.p.tmp"
    mv "$out/big.p.tmp" "$out/big.p"
    echo "$stamp" > "$out/big.p.stamp"
fi
# workspace <directory> <gen-workspace arguments>
workspace() {
    local dir=$1 stamp
    shift
    stamp=$( (stamp_of "$repo/crates/oxabl_bench/src/generate.rs" "$repo/crates/oxabl_bench/src/bin/gen_workspace.rs"; echo "$*") | sha256sum | cut -d' ' -f1)
    if [ "$(cat "$dir.stamp" 2>/dev/null)" != "$stamp" ]; then
        rm -rf "$dir" "$dir.tmp" "$dir.stamp"
        "$repo/scripts/bench/gen-workspace" "$@" "$dir.tmp" >/dev/null
        mv "$dir.tmp" "$dir"
        echo "$stamp" > "$dir.stamp"
    fi
}
ws=$out/ws-$preset
workspace "$ws" --preset "$preset"
wsf=$out/ws-$preset-include-findings
workspace "$wsf" --preset "$preset" --include-findings 100

# name | directory | expected exit status | arguments
#
# The exit status of `check` is 1 for findings or drift and also for a file it
# could not read or analyse, so a status alone cannot tell a finding from a
# failure. Each surface has the one status its input gives, and a line of
# stderr that starts with "error:" (an analysis or formatting failure, an
# unreadable file) fails the run whatever the status.
surfaces=(
    "check-lsp|$repo|1|check resources/lsp_bench.p -I resources"
    "check-big|$out|1|check big.p"
    "check-ws|$ws|0|check src"
    "check-ws-findings|$wsf|1|check --include-findings src"
    "format-big|$out|1|format --check big.p"
    "format-ws|$ws|0|format --check src"
    "conform-big|$out|0|conformance -j 1 big.p"
    "explain-lsp|$repo|0|explain resources/lsp_bench.p -I resources"
)

failed=0
# run_in <dir> <expected status> <command...>: a failure when the command
# exits with another status or writes an "error:" line to stderr. The status
# of `time -v` and `heaptrack` is the status of the command they run.
run_in() {
    local dir=$1 expected=$2 status=0
    shift 2
    (cd "$dir" && "$@") >/dev/null 2>"$out/last.err" || status=$?
    if [ "$status" != "$expected" ] || grep -q '^error:' "$out/last.err"; then
        echo "cli.sh: '$*' in $dir exited $status (expected $expected) or wrote an error:" >&2
        grep '^error:' "$out/last.err" | head -5 >&2 || tail -5 "$out/last.err" >&2
        failed=1
        return 1
    fi
}

mb() { awk -v k="$1" 'BEGIN { printf "%.1f", k / 1024 }'; }

header="surface"
wants time && header+="|wall s|peak RSS MB"
wants heaptrack && header+="|heap peak|alloc calls"
wants callgrind && header+="|instructions|tokenize|parse_program|allocator self|memcpy self|unnamed libc self"
rows=("$header")

for entry in "${surfaces[@]}"; do
    IFS='|' read -r name dir expected args <<< "$entry"
    if [ -n "$only" ]; then case ",$only," in *",$name,"*) ;; *) continue ;; esac; fi
    read -r -a argv <<< "$args"
    row=$name
    if wants time; then
        if run_in "$dir" "$expected" "$gnu_time" -v -o "$out/$name.time" "$shipped" "${argv[@]}"; then
            wall=$(awk -F': ' '/Elapsed \(wall clock\)/ { n = split($2, p, ":"); s = 0; for (i = 1; i <= n; i++) s = s * 60 + p[i]; print s }' "$out/$name.time")
            rss=$(awk -F': ' '/Maximum resident set size/ { print $2 }' "$out/$name.time")
            row+="|$wall|$(mb "$rss")"
        else
            row+="|failed|failed"
        fi
    fi
    if wants heaptrack; then
        rm -f "$out/$name".heaptrack*
        if run_in "$dir" "$expected" heaptrack -o "$out/$name.heaptrack" "$system" "${argv[@]}"; then
            file=
            for f in "$out/$name".heaptrack*; do
                case $f in *.zst | *.gz) file=$f; break ;; esac
            done
            heaptrack_print "$file" > "$out/$name.heaptrack.txt" 2>/dev/null
            peak=$(awk -F': ' '/^peak heap memory consumption/ { print $2 }' "$out/$name.heaptrack.txt")
            calls=$(awk -F': ' '/^calls to allocation functions/ { split($2, p, " "); print p[1] }' "$out/$name.heaptrack.txt")
            if [ -z "$peak" ] || [ -z "$calls" ]; then
                echo "cli.sh: no summary in $out/$name.heaptrack.txt" >&2
                failed=1
                row+="|failed|failed"
            else
                row+="|$peak|$calls"
            fi
        else
            row+="|failed|failed"
        fi
    fi
    if wants callgrind; then
        # A walk on several threads indexes some files again in each worker,
        # as the threads happen to run, so its instructions change between
        # runs. Under callgrind every walk runs on one thread.
        cg_argv=("${argv[@]}")
        case $name in check-ws* | format-ws*) cg_argv=("${argv[0]}" -j 1 "${argv[@]:1}") ;; esac
        if run_in "$dir" "$expected" valgrind --tool=callgrind --callgrind-out-file="$out/$name.callgrind" "$shipped" "${cg_argv[@]}"; then
            row+=$(python3 -I "$repo/scripts/bench/callgrind_summary.py" --row "$out/$name.callgrind")
        else
            row+="|failed|failed|failed|failed|failed|failed"
        fi
    fi
    rows+=("$row")
done

printf '%s\n' "${rows[@]}" | column -t -s '|'
exit $failed
