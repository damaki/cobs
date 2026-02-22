#!/bin/sh

set -e

alr build --profiles=*=validation

alr gnatcov instrument --level=stmt+mcdc --dump-trigger=atexit --projects cobs.gpr

alr build --profiles=*=validation -- --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts_full

alr exec -- bin/unit_tests

alr gnatcov coverage \
    --annotate=html+ \
    --output-dir gnatcov_out \
    --level=stmt+mcdc \
    --projects cobs.gpr \
    *.srctrace