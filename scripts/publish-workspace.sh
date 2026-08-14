#!/usr/bin/env bash

set -euo pipefail

declare -a publish_args=()

while IFS=$'\t' read -r package version; do
    status="$(
        curl \
            --silent \
            --output /dev/null \
            --write-out '%{http_code}' \
            --user-agent 'oxabl-release-ci (https://github.com/oxabl-project/oxabl)' \
            "https://crates.io/api/v1/crates/${package}/${version}"
    )"

    case "$status" in
        200)
            echo "publish-workspace: ${package}@${version} already exists; skipping."
            ;;
        404)
            publish_args+=(--package "$package")
            ;;
        *)
            echo "publish-workspace: crates.io returned HTTP ${status} for ${package}@${version}." >&2
            exit 1
            ;;
    esac
done < <(
    cargo metadata --no-deps --format-version 1 |
        jq -r '.packages[] | select(.publish != []) | [.name, .version] | @tsv'
)

if ((${#publish_args[@]} == 0)); then
    echo 'publish-workspace: all workspace versions already exist.'
    exit 0
fi

cargo publish "${publish_args[@]}" --locked "$@"
