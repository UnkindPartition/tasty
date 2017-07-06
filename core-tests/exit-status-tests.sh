#!/bin/sh

set -eux

# This assumes that the compiled exit-status-test executable
# is in the PATH

exit-status-test --result=True > /dev/null
exit-status-test --result=True --quiet

! exit-status-test --result=False > /dev/null
! exit-status-test --result=False --quiet
