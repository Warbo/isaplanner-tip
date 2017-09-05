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

def defConstant(d):
    '''Use 'definition' instead of 'fun' for names given in FIXES. See
    http://stackoverflow.com/questions/28113667/why-must-isabelle-functions-have-at-least-one-argument/28147895
    '''

    # Ignore non-fun
    if d[:3] != 'fun':
        return d

    # Extract the name and check if it's meant to use 'definition'
    name = d.split()[1]
    if name in fixes['constants']['encoded']:
        return 'definition ' + d[3:]

    # Otherwise return unchanged
    return d

def funToFunction(d):
    '''Turns any given 'fun' definition into a 'function' definition.'''
    if d.startswith('fun') and not d.startswith('function'):
        pre  = 'function (sequential) '
        post = '\nsorry\ntermination sorry\n'
        return pre + d[3:] + post
    return d

def fixCases(d):
    '''Wrap 'case' expressions in parentheses, if needed.'''
    if d.split()[0] != 'function':
        return d
    name = d.split()[2]  # function (sequential) name
    if name in fixes['cases']['encoded']:
        bits = d.split('case')
        bobs = bits[2].split('"')
        return ''.join([
            bits[0],
            '(case ', bits[1], ') (case ', bobs[0], ')"',
            bobs[1]])
    return d

print('\n'.join(reduce(lambda result, func: map(func, result),
                       [useSorry, defConstant, funToFunction, fixCases],
                       defs)))
