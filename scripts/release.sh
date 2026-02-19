#!/bin/bash
set -euo pipefail

usage() {
  echo "Usage: $0 [--publish] <package>"
  echo ""
  echo "Packages: mcp-types, mcp, all"
  echo ""
  echo "Without --publish, uploads as a package candidate (dry run)."
  echo "With --publish, uploads as a published release."
  exit 1
}

# Parse args
publish_flag=""
package=""
for arg in "$@"; do
  case "$arg" in
    --publish) publish_flag="--publish" ;;
    -h|--help) usage ;;
    *) package="$arg" ;;
  esac
done

if [ -z "$package" ]; then
  usage
fi

# Validate package name
case "$package" in
  mcp-types|mcp|all) ;;
  *) echo "Error: unknown package '$package'. Must be mcp-types, mcp, or all." ; exit 1 ;;
esac

read -p "Username: " username
read -sp "Password: " password
echo ""

upload_package() {
  local name="$1"
  echo ""
  echo "=== Releasing $name ==="

  # Build docs and sdist for this package
  rm -rf dist-newstyle/"$name"-*-docs.tar.gz
  rm -rf dist-newstyle/sdist/"$name"-*.tar.gz
  cabal haddock --haddock-for-hackage "$name"
  cabal sdist "$name"

  # Upload source tarball
  cabal upload $publish_flag -u "$username" -p "$password" dist-newstyle/sdist/"$name"-*.tar.gz
  # Upload docs
  cabal upload $publish_flag -d -u "$username" -p "$password" dist-newstyle/"$name"-*-docs.tar.gz

  echo "=== Done: $name ==="
}

case "$package" in
  mcp-types)
    upload_package mcp-types
    ;;
  mcp)
    upload_package mcp
    ;;
  all)
    # Upload mcp-types first (mcp depends on it)
    upload_package mcp-types
    upload_package mcp
    ;;
esac
