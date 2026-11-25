#!/usr/bin/env bash
# Measure the oxabl command line on large inputs and print one table.
#
# Usage: scripts/bench/cli.sh [--preset ci|medium|erp] [--tools time,heaptrack,callgrind]
#                             [--only NAME[,NAME...]] [--out DIR] [--no-build]
#
# Surfaces (NAME):
#   check-lsp      oxabl check resources/lsp_bench.p -I resources
#   check-big      oxabl check big.p               (1 MB: the bench_parser_* fixtures, 40 times)
#   check-ws       oxabl check src                  (in the generated workspace)
#   format-big     oxabl format --check big.p
#   format-ws      oxabl format --check src         (in the generated workspace)
#   conform-big    oxabl conformance -j 1 big.p
#   explain-lsp    oxabl explain resources/lsp_bench.p -I resources
#
# Tools:
#   time       GNU time -v: wall time and peak RSS of the shipped binary (jemalloc).
#   heaptrack  peak heap and allocation calls. heaptrack sees only the C library's
#              allocator, so this runs a second binary built with the cargo feature
#              `system-allocator`.
#   callgrind  instructions of the shipped binary, the inclusive share and calls of
#              `tokenize` and `parse_program`, and the self share of the allocator,
#              memcpy and unnamed C library code. Instruction counts are the same on
#              each run, so two commits compare on a loaded machine. About 50 times
#              slower than the run.
#
# Everything is written below --out (default target/bench-cli). The workspace is
# generated with scripts/bench/gen-workspace. A missing tool is taken from
# nixpkgs through `nix shell` when nix is on PATH, else the script stops.
# Exit 0 only when every command ran and exited 0 or 1 (findings or drift).

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
        -h | --help) sed -n '2,32p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
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

mkdir -p "$out"
out=$(cd "$out" && pwd)
shipped=$out/oxabl
system=$out/oxabl-system-allocator

if [ $build = 1 ]; then
    cargo build --quiet --release --manifest-path "$repo/Cargo.toml" -p oxabl
    cp "$repo/target/release/oxabl" "$shipped"
    if wants heaptrack; then
        cargo build --quiet --release --manifest-path "$repo/Cargo.toml" -p oxabl --features system-allocator
        cp "$repo/target/release/oxabl" "$system"
        # Leave the shipped binary in target/release.
        cargo build --quiet --release --manifest-path "$repo/Cargo.toml" -p oxabl
    fi
fi
for binary in "$shipped" $(wants heaptrack && echo "$system"); do
    [ -x "$binary" ] || { echo "cli.sh: no $binary (run without --no-build)" >&2; exit 2; }
done

# Inputs.
if [ ! -f "$out/big.p" ]; then
    parts="expressions declarations control_flow oo_abl procs_funcs datasets compiler_forms type_spellings name_forms skipped_forms"
    for _ in $(seq 1 40); do
        for part in $parts; do cat "$repo/resources/bench_parser_$part.abl"; done
    done > "$out/big.p"
fi
ws=$out/ws-$preset
if [ ! -d "$ws" ]; then
    "$repo/scripts/bench/gen-workspace" --preset "$preset" "$ws" >/dev/null
fi

# name | directory | arguments
surfaces=(
    "check-lsp|$repo|check resources/lsp_bench.p -I resources"
    "check-big|$out|check big.p"
    "check-ws|$ws|check src"
    "format-big|$out|format --check big.p"
    "format-ws|$ws|format --check src"
    "conform-big|$out|conformance -j 1 big.p"
    "explain-lsp|$repo|explain resources/lsp_bench.p -I resources"
)

failed=0
# Run "$@" in directory $1; a status other than 0 or 1 is a failure.
run_in() {
    local dir=$1 status=0
    shift
    (cd "$dir" && "$@") >/dev/null 2>"$out/last.err" || status=$?
    if [ $status -gt 1 ]; then
        echo "cli.sh: '$*' in $dir exited $status:" >&2
        tail -5 "$out/last.err" >&2
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
    IFS='|' read -r name dir args <<< "$entry"
    if [ -n "$only" ]; then case ",$only," in *",$name,"*) ;; *) continue ;; esac; fi
    read -r -a argv <<< "$args"
    row=$name
    if wants time; then
        if run_in "$dir" "$gnu_time" -v -o "$out/$name.time" "$shipped" "${argv[@]}"; then
            wall=$(awk -F': ' '/Elapsed \(wall clock\)/ { n = split($2, p, ":"); s = 0; for (i = 1; i <= n; i++) s = s * 60 + p[i]; print s }' "$out/$name.time")
            rss=$(awk -F': ' '/Maximum resident set size/ { print $2 }' "$out/$name.time")
            row+="|$wall|$(mb "$rss")"
        else
            row+="|failed|failed"
        fi
    fi
    if wants heaptrack; then
        rm -f "$out/$name".heaptrack*
        if run_in "$dir" heaptrack -o "$out/$name.heaptrack" "$system" "${argv[@]}"; then
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
        if run_in "$dir" valgrind --tool=callgrind --callgrind-out-file="$out/$name.callgrind" "$shipped" "${argv[@]}"; then
            row+=$(python3 -I "$repo/scripts/bench/callgrind_summary.py" --row "$out/$name.callgrind")
        else
            row+="|failed|failed|failed|failed|failed|failed"
        fi
    fi
    rows+=("$row")
done

printf '%s\n' "${rows[@]}" | column -t -s '|'
exit $failed
