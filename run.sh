#!/bin/bash
make debug
./dist-newstyle/build/x86_64-linux/ghc-9.10.3/diagram-0.1.0.0/x/diagram/build/diagram/diagram ~/code/enwik2 -s 0 +RTS -xc -RTS
