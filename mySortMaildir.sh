#!/usr/bin/env bash

if [[ -f "$(dirname "$0")/src/mySortMaildir.hs" ]]; then
  # pushd "$(dirname "$0")/src" >/dev/null
  # ./mySortMaildir.hs
  # popd >/dev/null
  pushd "$(dirname "$0")" >/dev/null
  cabal run mySortMaildir
  popd >/dev/null
else
  cat << EOF
The file 
  $(dirname "$0")/src/mySortMaildir.hs
does not exist. This means, that you have to follow the README and build your
own set of rules.
EOF
  exit 1
fi
