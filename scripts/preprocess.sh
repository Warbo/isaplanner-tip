#!/usr/bin/env bash
set -e
set -o pipefail

[[ -n "$smtlib" ]] || {
    echo "No smtlib variable, aborting" 1>&2
    exit 1
}

[[ -n "$FIXES" ]] || {
    echo "No FIXES variable, aborting" 1>&2
    exit 1
}

function remove {
    # Strip out $1, but only if followed by a newline, whitespace or paren, i.e.
    # if we're stripping 'times', don't strip 'timestamp'.
    DATA=$(echo "$DATA" | grep -ve "$1"'\($\|\s\|)\)')
}

DATA=$(cat "$smtlib")

# Remove types and functions which we can't translate to valid Isabelle
while read -r NAME
do
    remove "$NAME"
done < <(jq -r '(.dependents.encoded +
                 .nontypes.encoded   ) | .[]' < "$FIXES")

echo "$DATA"
