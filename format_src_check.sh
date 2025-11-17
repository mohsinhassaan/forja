#!/usr/bin/env sh

set -x -e

./mill --meta-level 1 mill.scalalib.scalafmt/checkFormatAll
./mill forja.updateLimit22Apply --check true
./mill __.fix --check
./mill mill.scalalib.scalafmt/checkFormatAll