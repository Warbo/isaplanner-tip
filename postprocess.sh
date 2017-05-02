#!/usr/bin/env bash
set -e

function replace {
    # Perl options explained at: https://unix.stackexchange.com/a/26289/63735
    DATA=$(echo "$DATA" | perl -0777 -pe "$1")
}

echo "Reading broken Isabelle theory" 1>&2
DATA=$(cat)
echo "Fixing up theory" 1>&2

# Use the more powerful size_change termination checker for tricky functions
while read -r FUNC
do
    # Expand 'fun foo' to 'function (sequential) foo'
    replace "s/fun\s*($FUNC)\s/function (sequential) \$1 /sg"

    # Now we can (and must) specify the exhaustiveness and termination checkers
    # Use the same exhaustiveness checker as 'fun', but use size_change for
    # termination.
    # This regex looks for the above declaration, eats it and the rest of the
    # definition up the next 'datatype' or 'fun' keyword, then spits it back
    # out, followed by the new options, followed by the keyword it ate.
     SEARCH="(function \(sequential\) $FUNC.*?)(datatype|fun)"
    REPLACE='$1\nby pat_completeness auto\ntermination by size_change\n\n$2'
    replace "s/$SEARCH/$REPLACE/gs"
done < <(jq -r '.trickyterminating.encoded | .[]' < "$FIXES")

# Use 'definition' instead of 'fun' for nullary values ('Nil', 'True', etc.)
# See http://stackoverflow.com/questions/28113667/why-must-isabelle-functions-have-at-least-one-argument/28147895
while read -r CONSTANT
do
    replace "s/fun\s*($CONSTANT\s)/definition \$1/gs"
done < <(jq -r '.constants.encoded | .[]' < "$FIXES")

echo "$DATA"
