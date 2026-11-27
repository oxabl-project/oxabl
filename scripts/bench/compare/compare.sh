#!/usr/bin/env bash
# Parse one ABL tree with oxabl and with the two maintained Proparse lines, and
# print the wall time, peak RSS and failed files of each.
#
# Usage: scripts/bench/compare/compare.sh --root DIR --propath DIR[,DIR...]
#            --df FILE [--df FILE...] [--charset NAME] [--threads N]
#            [--passes N] [--only oxabl,riverside,consultingwerk] [--out DIR]
#
# Needs java (21), mvn, curl, unzip, python3 and GNU time on PATH, for example:
#   nix shell nixpkgs#jdk21 nixpkgs#maven nixpkgs#time -c scripts/bench/compare/compare.sh ...
# See README.md in this directory for what each tool does and what the numbers mean.

set -euo pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
repo=$(cd "$here/../../.." && pwd)
root='' propath='' charset=UTF-8 threads=$(nproc) passes=3 only=oxabl,riverside,consultingwerk
out=$repo/target/bench-compare
dfs=()
while (($#)); do
    case $1 in
        --root) root=$2; shift 2 ;;
        --propath) propath=$2; shift 2 ;;
        --df) dfs+=("$2"); shift 2 ;;
        --charset) charset=$2; shift 2 ;;
        --threads) threads=$2; shift 2 ;;
        --passes) passes=$2; shift 2 ;;
        --only) only=$2; shift 2 ;;
        --out) out=$2; shift 2 ;;
        -h | --help) sed -n '2,12p' "$0"; exit 0 ;;
        *) echo "unknown argument: $1" >&2; exit 2 ;;
    esac
done
[[ -d $root ]] || { echo "--root must name a directory" >&2; exit 2; }
[[ -n $propath ]] || propath=$root
root=$(cd "$root" && pwd)
mkdir -p "$out"
gnu_time=$(type -P time) || { echo "GNU time is not on PATH" >&2; exit 2; }
"$gnu_time" --version 2>&1 | grep -q GNU || { echo "$gnu_time is not GNU time" >&2; exit 2; }

wants() { [[ ,$only, == *,$1,* ]]; }

# Each tool parses the compile units below the root: .p .w .cls .v (oxabl's
# default root extensions). Proparse reads the includes through the PROPATH.
if wants oxabl; then
    oxabl=$out/oxabl
    cargo build --release -p oxabl --manifest-path "$repo/Cargo.toml" >&2
    cp "${CARGO_TARGET_DIR:-$repo/target}/release/oxabl" "$oxabl"
    echo "== oxabl $(git -C "$repo" rev-parse --short HEAD), $threads threads"
    inc=()
    IFS=, read -ra dirs <<<"$propath"
    for d in "${dirs[@]}"; do inc+=(-I "$d"); done
    for i in $(seq "$passes"); do
        (cd "$root" && "$gnu_time" -f "oxabl run $i: wall %e s, peak RSS %M KB" \
            "$oxabl" conformance --preprocess -j "$threads" "${inc[@]}" . 2>&1 |
            grep -E "^oxabl run|^Found|^  .*:[0-9]+:[0-9]+ " | sed 's/^  /  failed: /') || true
    done
fi

if wants riverside; then
    rs=$out/riverside
    mkdir -p "$rs"
    mvn -q -f "$here/riverside/pom.xml" -Dmaven.repo.local="$rs/m2" \
        dependency:copy-dependencies -DoutputDirectory="$rs/lib" >&2
    javac -cp "$rs/lib/*" -d "$rs/classes" "$here/riverside/src/main/java/RiversideBench.java"
    df_args=()
    for df in "${dfs[@]}"; do df_args+=(--df "$df"); done
    echo "== Proparse (Riverside), $threads threads"
    "$gnu_time" -f "riverside: wall %e s, peak RSS %M KB" \
        java -cp "$rs/classes:$rs/lib/*" RiversideBench --root "$root" --propath "$propath" \
        --charset "$charset" --passes "$passes" --threads "$threads" "${df_args[@]}" 2>&1 |
        grep -v '^SLF4J'
fi

if wants consultingwerk; then
    cw=$out/consultingwerk
    zip=$cw/proparse.java.zip
    mkdir -p "$cw/run"
    if [[ ! -f $zip ]]; then
        curl -fsSL -o "$zip.part" https://github.com/consultingwerk/proparse/releases/download/5.1.0/proparse.java.zip
        mv "$zip.part" "$zip"
    fi
    echo "9a0c96eb96ff0888e61db1c4b9eae513fbd8408067cffa57824051f4f9b4255b  $zip" | sha256sum -c --quiet
    unzip -q -o "$zip" -d "$cw"
    lib=$cw/proparse.java
    javac -cp "$lib/proparse.jar:$lib/lib/*" -d "$cw/classes" "$here/consultingwerk/CwBench.java"
    # The library reads a flat schema file, and finds the dictionary tables
    # (_file, _field, ...) only when the schema lists them.
    python3 -I "$here/df2schema.py" "$cw/schema.txt" \
        --meta "$repo/crates/oxabl_schema/resources/metaschema.df" "${dfs[@]}"
    # It keeps its settings in process-wide singletons, so one thread only. It
    # writes a listing file below the working directory on each parse.
    echo "== Proparse (Consultingwerk 5.1.0), 1 thread"
    (cd "$cw/run" && "$gnu_time" -f "consultingwerk: wall %e s, peak RSS %M KB" \
        java -cp "$cw/classes:$lib/proparse.jar:$lib/lib/*" CwBench --root "$root" \
        --propath "$propath" --schema "$cw/schema.txt" --charset "$charset" --passes "$passes" 2>&1)
fi
