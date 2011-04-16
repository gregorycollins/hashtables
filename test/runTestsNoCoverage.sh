#!/bin/sh

set -e

SUITE=./dist/build/testsuite/testsuite

export LC_ALL=C
export LANG=C

if [ ! -f $SUITE ]; then
    cat <<EOF
Testsuite executable not found, please run:
    cabal configure -ftest
then
    cabal build
EOF
    exit;
fi

./dist/build/testsuite/testsuite -j4 -a1000 $*
