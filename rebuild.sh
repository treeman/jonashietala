#!/bin/bash

ghc --make src/site.hs
mv src/site .
./site clean
./site rebuild
