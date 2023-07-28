#!/usr/bin/env bash
set -o errexit

main_dir="$(dirname "$(realpath "${BASH_SOURCE[0]}")")/.."

# Where to find the JS build:
build_dir="result/bin/frontend.jsexe/"

# Where to finde the static assets:
static_dir="static/"

#####################

function usage() {
    echo "$0 TARGETDIR"
    echo
    echo "Bundle reflex-platform's build output with the static assets"
    echo "in TARGETDIR, which can then be served by any web server."
}

if [[ "$1" == "--help" || "$1" == "-help" || "$1" == "-h" || -z "$1" ]]; then
    usage
    exit
fi

target_dir="$(realpath --canonicalize-missing "$1")"

#####################

mkdir --parents "$target_dir"

cd "$main_dir"
cp --force --target="$target_dir" --recursive "$static_dir"/*
cp --force --target="$target_dir" "$build_dir/all.js"

cd "$target_dir"
sed --in-place --regexp-extended -e '/^\s*<HEADER>/{r header.html.fragment' -e';d}' *.html
rm --force header.html.fragment

echo "Done. Test in web browser:"
echo "file://$(realpath "$target_dir/index.html")"
