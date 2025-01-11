#!/usr/bin/env bash

mkdir_and_cp() {
  mkdir -p $(dirname "$2") && cp -rf "$1" "$2"
}

mkdir_and_cp dist-newstyle/build/*/*/*/doc/html/helpa/ docs/reports/helpa/
#mkdir_and_cp dist-newstyle/build/*/*/*/t/helpa-test/hpc/vanilla/html/ docs/reports/helpa-test/
