#!/bin/bash

set -e

alr exec -- gnatprove \
    \-P ../cobs.gpr \
    --level=1 \
    -j0 \
    --checks-as-errors=on \
    --warnings=error