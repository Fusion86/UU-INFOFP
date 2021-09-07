#!/bin/bash

ASSIGNMENT_SRC="src/Assignment$1.hs"
INPUT_FILE="data/in_$1.txt"
RUN="stack run Assignment$1"

if [ -f $ASSIGNMENT_SRC ]; then
    if [ -f $INPUT_FILE ]; then
        $RUN < $INPUT_FILE
    else
        $RUN
    fi
else
    echo "Assignment $1 has no source file!"
fi
