#!/bin/bash


rm -rf dist-newstyle/*-docs.tar.gz
rm -rf dist-newstyle/sdist/*
cabal haddock --haddock-for-hackage all
cabal sdist all
read -p "Username: " username
read -sp "Password: " password

# Upload mcp-protocol first (mcp depends on it)
cabal upload $1 -u "$username" -p "$password" dist-newstyle/sdist/mcp-protocol-*.tar.gz
cabal upload $1 -d -u "$username" -p "$password" dist-newstyle/mcp-protocol-*-docs.tar.gz

# Upload mcp (server)
cabal upload $1 -u "$username" -p "$password" dist-newstyle/sdist/mcp-*.tar.gz
cabal upload $1 -d -u "$username" -p "$password" dist-newstyle/mcp-*-docs.tar.gz
