#!/usr/bin/env bash

tr -d '\n'                                  |
grep -o -P '\[BEGIN OUTPUT,.*?END OUTPUT\]' |
head -n1                                    |
sed -e 's/, /\n/g'                          |
grep -v "OUTPUT"
