#!/bin/bash

ghc --make site.hs
./site clean
./site build
