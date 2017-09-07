#!/usr/bin/env python
import json
import os
import re
import sys

# Read in our Isabelle input and split apart each definition

def splitAt(word, s):
    '''Takes a string 's' and splits it at all occurrences of the string 'word',
    as long as 'word' appears at the start of a line (not the first line) and is
    followed by whitespace.

    Unlike the '.split' method, we don't strip 'word' from the results; it is
    put back at the start of each string (except the first, since that doesn't
    come from a split).'''

    bits = re.split('\n' + word + '\s', s)
    head = [] if bits == [] else [bits[0]]
    tail = [word + ' ' + x for x in bits[1:]]

    return head + tail

def splitAll(word, strs):
    '''Takes a list of strings 'strs' and applies 'splitAt(word, ...)' to each;
    concatenating the results together.'''
    return reduce(lambda result, s: result + splitAt(word, s),
                  strs,
                  [])

def splitDefs(s):
    '''Split a string of Isabelle apart into standalone definitions. The first
    string in the resulting list is the preamble before the first definition and
    the last entry will contain the end of the theory.'''
    return reduce(lambda result, word: splitAll(word, result),
                  ['end', 'function', 'fun', 'datatype'],
                  [s])

defs = splitDefs(sys.stdin.read())

# Fix up the definitions we've found

fixes = json.loads(open(os.getenv('FIXES'), 'r').read())

def useSorry(d):
    '''Replace TIP's choice of proofs with 'sorry'.'''
    return d.replace('by pat_completeness auto', 'sorry\ntermination sorry')

def insertStrInto(into, insert, at, before, skip):
    '''Insert string 'insert' into string 'into'. If 'skip' is None, we insert
    before/after (depending on 'before') the first occurrence of 'at' in 'into'.
    If 'skip' is a string, we look for the first occurrence of 'skip' in 'into',
    then look for the subsequent occurrence of 'at', and do our insertion there.
    Hence 'skip' is useful if 'at' is ambiguous; since 'skip' can occur
    anywhere before 'at', we have more opportunity to find a unique string.

    To aid further insertions, we return a '(pre, post)' pair, where 'pre'
    contains everything up to and including the 'insert' and 'at', while 'post'
    contains everything after.'''

    # Go to the start location (either the start of the string, or 'skip')
    prefix, lookIn, skipBits = ('', into, None)
    if skip:
        skipBits = into.split(skip)
        prefix = skipBits[0] + skip
        lookIn = skip.join(skipBits[1:])

    assert prefix + lookIn == into, \
        'skip did not split correctly: ' + repr(locals())
    del skipBits

    # Look for 'at'
    bits   = lookIn.split(at)
    middle = bits[0]
    suffix = at.join(bits[1:])

    assert middle.count(at) == 0, \
        'Broken "middle": ' + repr(locals())

    assert prefix + middle + at + suffix == into, \
        'Did not split correctly: ' + repr(locals())

    new      = (insert if before else '') + at + (insert if not before else '')
    begin    = prefix + middle + new
    combined = begin + suffix

    assert combined.count(insert) == 1 + into.count(insert), \
        'Result count wrong: ' + repr(locals())

    return (begin, suffix)

def putInParens(d, config):
    '''Add '(' and ')' to 'd', based on position info given in 'config'.'''

    # Where to insert
    startBefore = 'startBefore' in config
    endBefore   = 'endBefore'   in config

    # Get the strings we're looking for
    start = config['startBefore' if startBefore else 'startAfter']
    end   = config['endBefore'   if endBefore   else 'endAfter']

    # If given, we should skip ahead to these, before looking for our start/end
    # strings. This is useful when the start/end locations are ambiguous.
    startSkip = config['startSkipTo'] if 'startSkipTo' in config else None
    endSkip   = config['endSkipTo']   if 'endSkipTo'   in config else None

    # Insert '(' and ')'
    prefix, rest   = insertStrInto(d,    '(', start, startBefore, startSkip)
    middle, suffix = insertStrInto(rest, ')', end,   endBefore,   endSkip)
    result         = prefix + middle + suffix

    assert result.count('(') == 1 + d.count('('), \
        'Final "(" count wrong: ' + repr(locals())

    assert result.count(')') == 1 + d.count(')'), \
        'Final ")" count wrong: ' + repr(locals())

    assert      d.replace('(', '').replace(')', '') == \
           result.replace('(', '').replace(')', ''),   \
           'Result does not match input, modulo parentheses: ' + repr(locals())

    return result

# Unit tests
for config, expect in [
        # Each before/after combination
        ({'startBefore':'A', 'endBefore':'C'}, '(AB)CDABCD'),
        ({'startAfter' :'A', 'endBefore':'C'}, 'A(B)CDABCD'),
        ({'startBefore':'A', 'endAfter' :'C'}, '(ABC)DABCD'),
        ({'startAfter' :'A', 'endAfter' :'C'}, 'A(BC)DABCD'),

        # Ensure end looks for occurrences after start
        ({'startBefore':'B', 'endAfter' :'A'},  'A(BCDA)BCD'),

        # Check that we can skip
        ({'startBefore':'A', 'endAfter':'D', 'startSkipTo':'C'}, 'ABCD(ABCD)'),
        ({'startBefore':'A', 'endAfter':'C', 'endSkipTo'  :'D'}, '(ABCDABC)D')]:
    assert putInParens('ABCDABCD', config) == expect, 'Test failed: ' + repr(locals())

def addParens(d):
    '''Wrap expressions in parentheses, if needed.'''
    if d.split()[0] != 'function':
        return d
    name = d.split()[2]  # function (sequential) name
    if name in fixes['parenthesise']['encoded'].keys():
        return reduce(putInParens,
                      fixes['parenthesise']['encoded'][name],
                      d)
    return d

print('\n'.join(reduce(lambda result, func: map(func, result),
                       [useSorry, addParens],
                       defs)))
