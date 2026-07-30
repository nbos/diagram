#!/bin/bash
make debug; dist-newstyle/build/x86_64-linux/ghc-9.6.7/diagram-0.1.0.0/x/diagram/build/diagram/diagram ../code/enwik1 -s $1 +RTS -xc -RTS
