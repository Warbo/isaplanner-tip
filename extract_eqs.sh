#!/usr/bin/env bash
set -e
set -o pipefail

function check {
    INPUT=$()
    grep 'BEGIN OUTPUT' > /dev/null || {
        echo "No 'BEGIN OUTPUT' sentinel found. Dumping whole output:" 1>&2
        echo "$INPUT" 1>&2
        exit 1
    }
}

tee >(check)                                |
tr -d '\n'                                  |
grep -o -P '\[BEGIN OUTPUT,.*?END OUTPUT\]' |
head -n1                                    |
sed -e 's/, /\n/g'                          |
grep -v "OUTPUT"
