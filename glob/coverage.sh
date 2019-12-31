#!/bin/bash

rm *.tix
ghc -fhpc GlobRegexTest
./GlobRegexTest
hpc report GlobRegexTest --exclude=Main --exclude=GlobRegexTest
hpc markup GlobRegexTest --exclude=Main --exclude=GlobRegexTest
