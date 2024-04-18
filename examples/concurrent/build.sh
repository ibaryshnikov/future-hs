#!/bin/bash

ghc -Wall -i../.. \
  -odir ../../build \
  -hidir ../../build \
  ../../libfuture_hs.a \
  -threaded \
  main.hs
