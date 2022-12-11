#!/bin/sh

set -ux

# timeout executable is not always available (e. g., macOS),
# so here is a portable workaround. While GNU timeout throws
# error code 124, this one returns SIGALRM which is usually 142.
if ! command -v timeout
then
  timeout() { perl -e 'alarm shift; exec @ARGV' "$@"; }
fi

if ! command -v exit-status-test
then
  echo "exit-status-test executable is not in PATH, aborting"
  exit 1
fi

exit-status-test --result=True > /dev/null || exit 1
exit-status-test --result=True --quiet || exit 1

! exit-status-test --result=False > /dev/null || exit 1
! exit-status-test --result=False --quiet || exit 1

# Testing the --quiet mode
# We try different number of fast tests in order to test different paths in the 'statusMapResult' function.
# The number of fast tests should not affect anything.

TIMEOUT=3 # seconds

for fast in $(seq 0 20); do
  # When the number of slow tests < number of threads, should find the failing
  # test and short-circuit
  timeout "$TIMEOUT" exit-status-test --result=False --slow=3 --fast="$fast" --num-threads=4 --quiet
  [ $? -eq 1 ] || exit 1
  # When the number of slow tests >= number of threads, cannot find the failing
  # test
  timeout "$TIMEOUT" exit-status-test --result=False --slow=4 --num-threads=4 --quiet
  exitcode=$?
  [ $exitcode -eq 124 ] || [ $exitcode -eq 142 ] || exit 1
  # Successful case (and no slow tests)
  timeout "$TIMEOUT" exit-status-test --result=True --fast="$fast" --num-threads=4 --quiet || exit 1
done
