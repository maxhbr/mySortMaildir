#!/usr/bin/env bash

pushd "$(dirname "$0")" >/dev/null
./mySortMaildir.hs
popd >/dev/null
