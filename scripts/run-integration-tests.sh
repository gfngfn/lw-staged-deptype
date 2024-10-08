#!/bin/bash

# Must be run at the root of the repository

ERRORS=()

for FILE in integration_tests/success/*.lwsd; do
    echo "======== $FILE (should pass) ========"
    cabal run lw-staged-deptype -- lwsd --optimize "$FILE"
    if [ $? -ne 0 ]; then
        ERRORS+=("$FILE (should pass)")
    fi
done
for FILE in integration_tests/failure/*.lwsd; do
    echo "======== $FILE (should be rejected) ========"
    cabal run lw-staged-deptype -- lwsd --optimize "$FILE"
    if [ $? -eq 0 ]; then
        ERRORS+=("$FILE (should be rejected)")
    fi
done

for FILE in integration_tests/success/*.surf; do
    echo "======== $FILE (should pass) ========"
    cabal run lw-staged-deptype -- surface --optimize "$FILE"
    if [ $? -ne 0 ]; then
        ERRORS+=("$FILE (should pass)")
    fi
done
for FILE in integration_tests/failure/*.surf; do
    echo "======== $FILE (should be rejected) ========"
    cabal run lw-staged-deptype -- surface --optimize "$FILE"
    if [ $? -eq 0 ]; then
        ERRORS+=("$FILE (should be rejected)")
    fi
done

RET=0
for ERROR in "${ERRORS[@]}"; do
    RET=1
    echo "! FAILED: $ERROR"
done
if [ $RET -eq 0 ]; then
    echo "All tests have passed."
fi
exit $RET
