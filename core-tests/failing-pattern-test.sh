#!/bin/sh

set -eu

# Make sure the loop is interrupted by Ctrl-C
trap exit INT

for seed in $(seq 1 10); do
  failing-pattern-test --seed "$seed" | perl -lne '/Use (.*) to rerun this test only\./ && print $1' |
    { n_patterns=0
      while read -r pattern; do
        if ! eval "failing-pattern-test --seed $seed $pattern" | grep -q '1 out of 1 tests failed'
        then
          printf >&2 "Failed for seed %d, pattern %s\n" "$seed" "$pattern"
          exit 1
        else
          n_patterns=$(( n_patterns + 1 ))
        fi
      done
      printf "Seed %d: checked %d patterns\n" "$seed" "$n_patterns"
    }
done
