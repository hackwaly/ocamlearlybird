#!/bin/sh
# Build a bytecode executable whose debug info uses dune's "/workspace_root"
# search-dir prefix, the way a dune >= 3.0 build does (map_workspace_root is on
# by default), but without needing a real dune build. This lets the integration
# test check that the adapter can still resolve such sources and bind
# breakpoints in them. See test/test_dune_source.ml.
#
# dune achieves the rewrite with BUILD_PATH_PREFIX_MAP, which the compiler reads;
# we do the same here. The mapped directory is created under the current
# directory and the source is compiled from inside it so that the recorded
# search dir is exactly "/workspace_root/test/fixtures". Physical paths (pwd -P)
# are used so the prefix matches what the compiler records even when the build
# runs under a symlinked directory (e.g. /tmp -> /private/tmp on macOS).
set -e

ocamlc=$1
out=$2
src=$3

orig=$(pwd -P)
case $out in
/*) ;;
*) out=$orig/$out ;;
esac

root=$orig/_wsroot
rm -rf "$root"
mkdir -p "$root/test/fixtures"
cp "$src" "$root/test/fixtures/wsroot.ml"

cd "$root/test/fixtures"
BUILD_PATH_PREFIX_MAP="/workspace_root=$root" \
  "$ocamlc" -g -I "$(pwd -P)" wsroot.ml -o "$out"
