#!/usr/bin/env bash

rm -rf dist-newstyle/
cabal new-build -f debug
cabal new-exec -- calligraphy -m Test.Reference --output-stdout | diff -u test/Test/Reference.dot -
