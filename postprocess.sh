#!/usr/bin/env bash

echo "Reading broken Isabelle theory" 1>&2
DATA=$(cat)
echo "Fixing up theory" 1>&2

# Remove any types which Isabelle doesn't support
while read -r TYPE
do
    # Perl options explained at: https://unix.stackexchange.com/a/26289/63735
    # The regex is as follows:
    #
    # s/   Performs a search/replace
    # /gs  Replaces multiple matches, if found (g), and makes '.' match \n (s)
    #
    # We look for the literal 'datatype' followed, at some point, by $TYPE. We
    # allow extra stuff in between, for type parameters, whitespace, etc. but we
    # don't allow '='. This should match 'datatype $TYPE', 'datatype 'a $TYPE',
    # etc. but not 'datatype foo = ... $TYPE'.
    #
    # After $TYPE we match everything up to the next appearance of 'datatype' or
    # 'fun'. Note that this wouldn't work if we're matching the last definition
    # in the file, since there would be no next definition. This is OK for our
    # purposes, since TIP puts all function definitions after the datatype
    # definitions, and having no function definitions would be a bit useless.
    # Also note that we use non-greedy repetition (*?) to take the smallest
    # match, since the default greedy repetition (*) would match the rest of the
    # file all the way up to the second-to-last definition!
    #
    # The "search" part of the regex should hence match all definitions of the
    # datatype $TYPE, as well as the following 'datatype' or 'fun' keyword.
    #
    # The "replace" part of the regex just spits out that following keyword (the
    # only part of the regex that's parenthesised into a capture group), to
    # avoid breaking the next definition.
    DATA=$(echo "$DATA" |
               perl -0777 -pe "s/datatype[^=]*$TYPE.*?(datatype|fun)/\$1/gs")
done < <(jq -r '.nontypes.entries | .[]' < "$FIXES")

# Use 'definition' instead of 'fun' for nullary values ('Nil', 'True', etc.)
# See http://stackoverflow.com/questions/28113667/why-must-isabelle-functions-have-at-least-one-argument/28147895
while read -r CONSTANT
do
    DATA=$(echo "$DATA" |
               perl -0777 -pe "s/fun\s*($CONSTANT\s)/definition \$1/gs")
done < <(jq -r '.constants.entries | .[]' < "$FIXES")

echo "$DATA"
