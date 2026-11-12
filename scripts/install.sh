#!/bin/sh
# Installs the oxabl binary for Linux x86_64 (a static musl build).
#
#   curl -fsSL https://raw.githubusercontent.com/oxabl-project/oxabl/master/scripts/install.sh | sh
#
# It puts the newest release in ${OXABL_INSTALL_DIR:-$HOME/.local/bin}/oxabl and
# writes the install record ${XDG_DATA_HOME:-$HOME/.local/share}/oxabl/install.json,
# which lets that binary update itself (`oxabl self-update`).
#
# OXABL_VERSION=X.Y.Z installs that version. OXABL_UPDATE_API_URL replaces the
# GitHub API for a test; it is used only for a loopback host.
#
# The download is checked against the SHA-256 of the release and started once
# with --version before it replaces the old binary. Any failure leaves the old
# binary as it was.
set -eu

REPO=oxabl-project/oxabl
ASSET=oxabl-x86_64-unknown-linux-musl
TAG_PREFIX=oxabl-v
API=https://api.github.com

say() { printf '%s\n' "$*"; }
die() { printf 'error: %s\n' "$*" >&2; exit 1; }
have() { command -v "$1" >/dev/null 2>&1; }

case "$(uname -s)" in
  Linux) ;;
  MINGW* | MSYS* | CYGWIN*)
    die "this installer needs Linux. On Windows, run it inside WSL, or use \`cargo install oxabl\`." ;;
  Darwin)
    die "there is no macOS binary. Use \`cargo install oxabl\`." ;;
  *)
    die "unsupported system $(uname -s). Use \`cargo install oxabl\`." ;;
esac
case "$(uname -m)" in
  x86_64 | amd64) ;;
  *) die "unsupported architecture $(uname -m): the release has a Linux x86_64 binary only. Use \`cargo install oxabl\`." ;;
esac

have sha256sum || die "sha256sum is needed"
if have curl; then
  fetcher=curl
elif have wget; then
  fetcher=wget
else
  die "curl or wget is needed"
fi

# Loopback hosts only, for tests.
loopback() {
  case "$1" in
    http://127.0.0.1 | http://127.0.0.1[:/]* | http://localhost | http://localhost[:/]* | http://\[::1\] | http://\[::1\][:/]*) return 0 ;;
    https://127.0.0.1 | https://127.0.0.1[:/]* | https://localhost | https://localhost[:/]* | https://\[::1\] | https://\[::1\][:/]*) return 0 ;;
  esac
  return 1
}
# https, or http to a loopback host.
allowed() {
  case "$1" in
    https://*) return 0 ;;
  esac
  loopback "$1"
}

if [ -n "${OXABL_UPDATE_API_URL:-}" ]; then
  if loopback "$OXABL_UPDATE_API_URL"; then
    API=${OXABL_UPDATE_API_URL%/}
  else
    printf 'warning: OXABL_UPDATE_API_URL is not a loopback URL; ignored\n' >&2
  fi
fi

if [ -n "${OXABL_INSTALL_DIR:-}" ]; then
  install_dir=$OXABL_INSTALL_DIR
elif [ -n "${HOME:-}" ]; then
  install_dir=$HOME/.local/bin
else
  die "neither OXABL_INSTALL_DIR nor HOME is set"
fi
if [ -n "${XDG_DATA_HOME:-}" ]; then
  data_dir=$XDG_DATA_HOME/oxabl
elif [ -n "${HOME:-}" ]; then
  data_dir=$HOME/.local/share/oxabl
else
  die "neither XDG_DATA_HOME nor HOME is set"
fi

work=
staged=
cleanup() {
  [ -z "$staged" ] || rm -f "$staged"
  [ -z "$work" ] || rm -rf "$work"
}
trap cleanup EXIT
trap 'exit 1' INT TERM HUP

mkdir -p "$install_dir" || die "cannot create $install_dir"
install_dir=$(cd "$install_dir" && pwd -P)
target=$install_dir/oxabl
work=$(mktemp -d "${TMPDIR:-/tmp}/oxabl-install.XXXXXX") || die "cannot make a temporary directory"

# fetch URL FILE [HEADER-FILE]: writes the body, and the headers when asked.
fetch() {
  allowed "$1" || die "refusing $1: not https"
  if [ "$fetcher" = curl ]; then
    if [ -n "${3:-}" ]; then
      curl -fsSL --proto '=http,https' --proto-redir '=https' --connect-timeout 15 --max-time 600 -D "$3" -o "$2" "$1" || return 1
    else
      curl -fsSL --proto '=http,https' --proto-redir '=https' --connect-timeout 15 --max-time 600 -o "$2" "$1" || return 1
    fi
  elif [ -n "${3:-}" ]; then
    wget -q -T 60 -S -O "$2" "$1" 2>"$3" || return 1
  else
    wget -q -T 60 -O "$2" "$1" || return 1
  fi
}

plain_version() {
  printf '%s\n' "$1" | grep -Eq '^[0-9]+\.[0-9]+\.[0-9]+$'
}

# The highest plain X.Y.Z among the tags oxabl-vX.Y.Z, following the pages.
newest_version() {
  url="$API/repos/$REPO/git/matching-refs/tags/$TAG_PREFIX?per_page=100"
  : >"$work/versions"
  pages=0
  while [ -n "$url" ]; do
    pages=$((pages + 1))
    [ "$pages" -le 50 ] || die "too many pages of tags"
    fetch "$url" "$work/refs" "$work/headers" || die "cannot read the list of releases from $url"
    grep -o "\"ref\": *\"refs/tags/${TAG_PREFIX}[^\"]*\"" "$work/refs" |
      sed "s|.*refs/tags/$TAG_PREFIX||; s|\"\$||" |
      grep -E '^[0-9]+\.[0-9]+\.[0-9]+$' >>"$work/versions" || true
    url=$(tr -d '\r' <"$work/headers" | grep -i '^ *link:' | tr ',' '\n' |
      grep 'rel="next"' | sed 's/^[^<]*<//; s/>.*//' | head -n 1) || url=
  done
  [ -s "$work/versions" ] || die "no release of oxabl was found"
  sort -t. -k1,1n -k2,2n -k3,3n "$work/versions" | tail -n 1
}

if [ -n "${OXABL_VERSION:-}" ]; then
  version=${OXABL_VERSION#v}
  plain_version "$version" || die "OXABL_VERSION must look like 5.2.0"
else
  version=$(newest_version)
fi

# The release must be final and carry both assets, or nothing is installed.
fetch "$API/repos/$REPO/releases/tags/$TAG_PREFIX$version" "$work/release" ||
  die "cannot read release $version"
grep -Eq '"draft": *false' "$work/release" || die "release $version is a draft"
grep -Eq '"prerelease": *false' "$work/release" || die "release $version is a pre-release"
asset_url() {
  grep -o "\"browser_download_url\": *\"[^\"]*$1\"" "$work/release" |
    head -n 1 | sed 's/^[^:]*: *"//; s/"$//'
}
binary_url=$(asset_url "$ASSET")
checksum_url=$(asset_url "$ASSET.sha256")
[ -n "$binary_url" ] && [ -n "$checksum_url" ] || die "release $version is incomplete: it lacks $ASSET or its .sha256"

fetch "$checksum_url" "$work/sum" || die "cannot download the checksum"
expected=$(awk 'NR == 1 { print tolower($1) }' "$work/sum")
printf '%s\n' "$expected" | grep -Eq '^[0-9a-f]{64}$' || die "the checksum file holds no SHA-256 digest"

staged=$(mktemp "$install_dir/.oxabl-install-XXXXXX") || die "cannot write in $install_dir"
fetch "$binary_url" "$staged" || die "cannot download $binary_url"
actual=$(sha256sum "$staged" | awk '{ print $1 }')
[ "$actual" = "$expected" ] || die "the SHA-256 of the download does not match the release"
chmod 0755 "$staged"

# The download must run and name the version it was meant to be.
said_file=$work/said
if have timeout; then
  timeout 15 "$staged" --version >"$said_file" 2>/dev/null || die "the downloaded binary failed on --version"
else
  "$staged" --version >"$said_file" 2>/dev/null &
  child=$!
  ( sleep 15; kill "$child" 2>/dev/null ) &
  timer=$!
  wait "$child" || die "the downloaded binary failed on --version"
  kill "$timer" 2>/dev/null || true
fi
said=$(cat "$said_file")
[ "$said" = "oxabl $version" ] || die "the downloaded binary says \"$said\", not \"oxabl $version\""

# One atomic step.
mv -f "$staged" "$target" || die "cannot replace $target"
staged=

# The install record, atomically: a temporary file and one rename.
mkdir -p "$data_dir" || die "cannot create $data_dir"
escaped=$(printf '%s' "$target" | sed 's/\\/\\\\/g; s/"/\\"/g')
record=$data_dir/install.json
printf '{"format":1,"installer":"install.sh","path":"%s","version":"%s"}\n' "$escaped" "$version" >"$record.tmp.$$" ||
  die "cannot write $record"
mv -f "$record.tmp.$$" "$record" || die "cannot write $record"

say "oxabl $version was installed to $target"
case ":${PATH:-}:" in
  *":$install_dir:"*) ;;
  *) say "warning: $install_dir is not on your PATH. Add it, for example: export PATH=\"$install_dir:\$PATH\"" ;;
esac
