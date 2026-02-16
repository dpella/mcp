#!/bin/bash


rm -rf dist-newstyle/*-docs.tar.gz
rm -rf dist-newstyle/sdist/*
cabal haddock --haddock-for-hackage all
cabal sdist all
read -p "Username: " username
read -sp "Password: " password

# Upload mcp-types first (mcp depends on it)
#cabal upload $1 -u "$username" -p "$password" dist-newstyle/sdist/mcp-types-*.tar.gz
#cabal upload $1 -d -u "$username" -p "$password" dist-newstyle/mcp-types-*-docs.tar.gz

# Upload mcp (server)
cabal upload $1 -u "$username" -p "$password" dist-newstyle/sdist/mcp-0.*.tar.gz
cabal upload $1 -d -u "$username" -p "$password" dist-newstyle/mcp-0.*-docs.tar.gz
