#!/bin/sh

cabal update
cabal install
cp dist/build/connected-mu-clique/connected-mu-clique .
