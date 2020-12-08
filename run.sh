#!/bin/bash

cd "$(dirname "$0")"

day=$(printf "%02d" $1)

stack runhaskell src/Day$day.hs < in/day$day.txt
