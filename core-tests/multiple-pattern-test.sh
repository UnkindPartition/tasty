#!/bin/sh

set -eux

if ! command -v multiple-pattern-test
then
  echo "multiple-pattern-test executable is not in PATH, aborting"
  exit 1
fi

[ "$(multiple-pattern-test -l | wc -l)" -eq 4 ]
[ "$(multiple-pattern-test -l -p red | wc -l)" -eq 2 ]
[ "$(multiple-pattern-test -l -p circle | wc -l)" -eq 2 ]
[ "$(multiple-pattern-test -l -p red -p circle | wc -l)" -eq 1 ]
[ "$(multiple-pattern-test -l -p red -p circle -p green | wc -l)" -eq 0 ]

# Edge case: the empty pattern matches everything
[ "$(multiple-pattern-test -l -p '' | wc -l)" -eq 4 ]
[ "$(multiple-pattern-test -l -p '' -p '' | wc -l)" -eq 4 ]
[ "$(multiple-pattern-test -l -p '' -p red | wc -l)" -eq 2 ]
[ "$(multiple-pattern-test -l -p red -p '' | wc -l)" -eq 2 ]
[ "$(multiple-pattern-test -l -p '' -p red -p '' | wc -l)" -eq 2 ]
[ "$(multiple-pattern-test -l -p red -p '' -p circle | wc -l)" -eq 1 ]

# Environment variable is entirely overridden by any command line options
[ "$(TASTY_PATTERN=red.circle multiple-pattern-test -l | wc -l)" -eq 1 ]
[ "$(TASTY_PATTERN=red.circle multiple-pattern-test -l -p square | wc -l)" -eq 2 ]
[ "$(TASTY_PATTERN=red.circle multiple-pattern-test -l -p square -p green | wc -l)" -eq 1 ]
