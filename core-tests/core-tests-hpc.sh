#!/bin/bash

shopt -s globstar

HPC_DIR=core-tests-hpc
HTML_DIR=core-tests-hpc-html
HPCPP=../../hpcpp/dist-newstyle/build/x86_64-linux/ghc-8.10.2/hpc-bin-0.68.1/x/hpc/build/hpc/hpc

BUILD=../dist-newstyle/build/x86_64-linux/ghc-8.10.2

TASTY_TESTS_SRC=.
TASTY_TESTS_HPC=$BUILD/core-tests-0.1/hpc/vanilla/mix/tasty-core-tests

TASTY_SRC=../core
TASTY_HPC=$BUILD/tasty-1.4.2/hpc/dyn/mix/tasty-1.4.2

TASTY_QUICKCHECK_SRC=../quickcheck
TASTY_QUICKCHECK_HPC=$BUILD/tasty-quickcheck-0.10.1.1/hpc/dyn/mix/tasty-quickcheck-0.10.1.1

TASTY_HUNIT_SRC=../hunit
TASTY_HUNIT_HPC=$BUILD/tasty-hunit-0.10.0.2/hpc/dyn/mix/tasty-hunit-0.10.0.2

cabal clean
cabal configure --enable-tests --enable-coverage
cabal build
cabal exec tasty-core-tests -- --save-tix ${HPC_DIR}

$HPCPP markup-multi \
  --hpcdir ${TASTY_HPC}            --srcdir ${TASTY_SRC} \
  --hpcdir ${TASTY_HUNIT_HPC}      --srcdir ${TASTY_HUNIT_SRC} \
  --hpcdir ${TASTY_QUICKCHECK_HPC} --srcdir ${TASTY_QUICKCHECK_SRC} \
  --hpcdir ${TASTY_TESTS_HPC}      --srcdir ${TASTY_TESTS_SRC} \
  --destdir ${HTML_DIR} \
  ${HPC_DIR}/**/*.tix

