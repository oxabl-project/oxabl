#!/usr/bin/env bash

set -euo pipefail

# crates.io meters publishing with a token bucket: a burst allowance that
# refills at roughly one publish per minute. This workspace uploads every
# crate it owns in a single run, so a release that lands soon after another
# one -- or a workspace that grows past the burst allowance -- can drain the
# bucket partway through and come back with HTTP 429.
#
# Recovering from that is just running the script again once the bucket has
# refilled, because each attempt asks crates.io which versions already exist
# and publishes only the missing ones. So the handling here is an outer retry
# loop rather than any per-crate bookkeeping.
#
# The budget below covers a full refill for a workspace of this size with room
# to spare. A 429 that asks for longer than that is a quota rather than a
# burst, and waiting it out would idle a runner for no reason, so the run
# fails immediately and prints the deadline instead.
readonly MAX_TOTAL_WAIT_SECONDS=1800

# Never retry sooner than this, even when crates.io names an earlier deadline.
# A publish has to propagate to the registry index before the next attempt can
# see it, and retrying on top of a fresh upload risks re-deriving the publish
# set from state that has not caught up yet.
readonly MIN_WAIT_SECONDS=60
readonly MAX_BACKOFF_SECONDS=600

# Tolerance for clock skew between the runner and crates.io.
readonly CLOCK_SKEW_SECONDS=60

# Publishes every workspace version that crates.io does not already have.
# Safe to call repeatedly: versions that made it up on an earlier attempt are
# detected and skipped.
publish_once() {
    local -a publish_args=() pending=()
    local package version status

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
                pending+=("${package}@${version}")
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

    if ((${#pending[@]} == 0)); then
        echo 'publish-workspace: all workspace versions already exist.'
        return 0
    fi

    echo "publish-workspace: ${#pending[@]} version(s) left to publish: ${pending[*]}"
    cargo publish "${publish_args[@]}" --locked "$@"
}

# Echoes how many seconds to wait based on the deadline crates.io states in a
# rate-limit response ("Please try again after <date>"), or nothing when the
# response carries no deadline this can parse.
crates_io_deadline_wait() {
    local log="$1" stated deadline target now
    stated="$(grep -oiE 'try again after .*' "$log" | head -n 1)" || return 0
    [[ -n "$stated" ]] || return 0
    # Pull a date out of the sentence rather than reading to the end of it, so
    # the trailing prose ("or email ...") is not handed to date(1).
    deadline="$(
        printf '%s' "$stated" |
            grep -oE '[A-Za-z]{3}, [0-9]{1,2} [A-Za-z]{3} [0-9]{4} [0-9]{2}:[0-9]{2}:[0-9]{2} ?(GMT|UTC|[+-][0-9]{4})?|[0-9]{4}-[0-9]{2}-[0-9]{2}[T ][0-9]{2}:[0-9]{2}:[0-9]{2}([.][0-9]+)?(Z|[+-][0-9:]+)?' |
            head -n 1
    )" || return 0
    [[ -n "$deadline" ]] || return 0
    target="$(date -u -d "$deadline" +%s 2>/dev/null)" || return 0
    now="$(date -u +%s)"
    echo "$((target - now + CLOCK_SKEW_SECONDS))"
}

main() {
    local log attempt=0 waited=0 backoff=$MIN_WAIT_SECONDS
    local rc wait_seconds reason

    log="$(mktemp)"
    # shellcheck disable=SC2064 # $log is fixed at trap time on purpose.
    trap "rm -f '$log'" EXIT

    while :; do
        attempt=$((attempt + 1))
        echo "publish-workspace: attempt ${attempt}."

        rc=0
        if ! publish_once "$@" 2>&1 | tee "$log"; then
            rc="${PIPESTATUS[0]}"
        fi
        ((rc == 0)) && return 0

        # Only rate limiting is worth waiting out. An unauthorized token, a
        # name someone else already owns, a rejected manifest and a package
        # that fails to build are all permanent: a retry spends the budget and
        # then reports the same thing, with the real cause further up the log.
        if ! grep -qiE '(status )?429 too many requests|you have published too many' "$log"; then
            echo "publish-workspace: attempt ${attempt} failed with exit ${rc} and crates.io did not report a rate limit; the failure is not retryable." >&2
            return "$rc"
        fi

        wait_seconds="$(crates_io_deadline_wait "$log")"
        if [[ -n "$wait_seconds" ]] && ((wait_seconds > MIN_WAIT_SECONDS)); then
            reason='the deadline crates.io gave'
        else
            wait_seconds=$backoff
            reason='backoff (crates.io gave no usable deadline)'
        fi

        if ((waited + wait_seconds > MAX_TOTAL_WAIT_SECONDS)); then
            echo "publish-workspace: crates.io is rate limiting and wants another ${wait_seconds}s, which exceeds the ${MAX_TOTAL_WAIT_SECONDS}s retry budget (${waited}s already spent over ${attempt} attempt(s)). Giving up. Re-run this workflow once the limit clears; the versions already published will be skipped." >&2
            return "$rc"
        fi

        echo "publish-workspace: crates.io rate limited attempt ${attempt} (exit ${rc}). Waiting ${wait_seconds}s per ${reason}, then retrying; ${MAX_TOTAL_WAIT_SECONDS}s budget will have $((MAX_TOTAL_WAIT_SECONDS - waited - wait_seconds))s left."
        sleep "$wait_seconds"
        waited=$((waited + wait_seconds))
        backoff=$((backoff * 2 > MAX_BACKOFF_SECONDS ? MAX_BACKOFF_SECONDS : backoff * 2))
    done
}

main "$@"
