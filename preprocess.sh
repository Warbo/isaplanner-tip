#!/usr/bin/env bash
set -e

function remove {
    # Strip out $1, but only if followed by a newline, whitespace or paren, i.e.
    # if we're stripping 'times', don't strip 'timestamp'.
    DATA=$(echo "$DATA" | grep -ve "$1"'\($\|\s\|)\)')
}

DATA=$(cat)

# Remove types and functions which we can't translate to valid Isabelle
while read -r NAME
do
    remove "$NAME"
done < <(jq -r '(.unparseable.encoded    +
                 .dependents.encoded     +
                 .nonterminating.encoded +
                 .nontypes.encoded)      | .[]' < "$FIXES")

echo "$DATA"
