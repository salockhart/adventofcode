#!/usr/bin/env bash
set -euxo pipefail

stack build

year=$1
day=$2

stack runhaskell ./adventofcode$year/src/Day$day.hs
