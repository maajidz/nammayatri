#!/bin/bash

set -e

ROOTDIR="$(dirname ${0})/../.."

ln -sf ../../Backend/dev/pre-commit ${ROOTDIR}/.git/hooks/pre-commit

# this hook can significantly increase push time
# ln -sf ../../Backend/dev/pre-push ${ROOTDIR}/.git/hooks/pre-push
