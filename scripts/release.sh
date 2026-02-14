#!/bin/bash


rm -rf dist-newstyle/*-docs.tar.gz
rm -rf dist-newstyle/sdist/*
cabal haddock --haddock-for-hackage
cabal sdist
read -p "Username: " username
read -sp "Password: " password

cabal upload $1 -u "$username" -p "$password" dist-newstyle/sdist/mcp-*.tar.gz
cabal upload $1 -d -u "$username" -p "$password" dist-newstyle/mcp-*-docs.tar.gz
