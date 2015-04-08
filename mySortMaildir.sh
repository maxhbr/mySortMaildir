#!/usr/bin/env bash

pushd "$(dirname "$0")/src" >/dev/null
./mySortMaildir.hs
popd >/dev/null
