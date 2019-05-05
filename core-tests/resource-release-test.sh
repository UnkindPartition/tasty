#!/bin/sh

set -ux

# This assumes that the compiled resource-release-test executable
# is in the PATH

# create a folder for the duration of the test, and make sure it starts empty
DIR=`dirname $0`/resource-release-test-files
rm -rf "$DIR"
mkdir "$DIR"
function finish {
  rm -rf "$DIR"
}
trap finish EXIT

# start resource-release-test and wait until it's ready
resource-release-test "$DIR" &
PID="$!"
while [ ! -f "$DIR/test-has-started" ]; do sleep 1; done

# kill resource-release-test and wait for it to release its resources.
kill -s INT "$PID"
wait "$PID"

# check that the resources were correctly released
[ -f "$DIR/resources-are-released" ] || exit 1
