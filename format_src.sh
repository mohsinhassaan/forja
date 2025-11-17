#!/usr/bin/env sh

set -x -e

./mill --meta-level 1 mill.scalalib.scalafmt/
./mill forja.updateLimit22Apply
./mill __.fix
./mill mill.scalalib.scalafmt/
