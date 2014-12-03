#!/bin/sh
rm -R dist/build/CTest && rm -R dist/build/AudioEngineTest && cabal build
