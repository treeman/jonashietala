#!/bin/bash

ghc --make src/site.hs -isrc
mv src/site .
./site clean
./site rebuild
