#!/usr/bin/env sh

set -x -e

./mill --meta-level 1 mill.scalalib.scalafmt/
./mill __.fix
./mill mill.scalalib.scalafmt/
