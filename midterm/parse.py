#####################################################################
#
# CAS CS 320, Spring 2015
# Midterm (skeleton code)
# parse.py
# Tianyang Zhong
# tianyang@bu.edu
#  ****************************************************************
#  *************** Modify this file for Problem #1. ***************
#  ****************************************************************
#

import re

def number(tokens, top = True):
    if re.compile(r"(-(0|[1-9][0-9]*)|(0|[1-9][0-9]*))").match(tokens[0]):
        return ({"Number": [int(tokens[0])]}, tokens[1:])

def variable(tokens, top = True):
    if re.compile(r"[a-z][A-Za-z0-9]*").match(tokens[0]) and tokens[0] not in ['true', 'false']:
        return ({"Variable": [tokens[0]]}, tokens[1:])

def parse(seqs, tmp, top = True):

    for (label, seq) in seqs:
        tokens = tmp[0:]
        sstr = [] 
        estr = [] 

        for x in seq:
            if type(x) == type(""): # get all the Terminals.

                if tokens[0] == x: # Doestr terminal match token?
                    tokens = tokens[1:]
                    sstr = sstr + [x]
                else:
                    break 

            else: # Parsing function.

                r = x(tokens, False)
                if not r is None:
                    (e, tokens) = r
                    estr = estr + [e]

        if len(sstr) + len(estr) == len(seq):
            if not top or len(tokens) == 0:
                return ({label:estr} if len(estr) > 0 else label, tokens)

#mapping exprestrstrion:
def exprestrstrion(tmp, top = True):
    tokens = tmp[0:]
    r = leftExprestrstrion(tokens, False)
    if not r is None:
        (e1, tokens) = r
        if len(tokens) > 0 and tokens[0] == "+":
            r = exprestrstrion(tokens[1:], False)
            if not r is None:
                (e2, tokens) = r
                return ({"Plus":[e1, e2]}, tokens)

        else:
            return (e1, tokens)

def leftExprestrstrion(tmp, top = True):
    r = parse([\
		("True", ["true"]),\
		("False", ["false"]),\
		("Array",  ["@", variable, "[", exprestrstrion, "]"]),\
		], tmp, top)

    if not r is None:
        return r

    tokens = tmp[0:]
    r = variable(tokens, False)
    if not r is None:
        return r

    tokens = tmp[0:]
    r = number(tokens, False)
    if not r is None:
        return r

# mapping Program:
def program(tmp, top = True):
    if len(tmp) == 0:
        return ("End", [])
    r = parse([\
		("Asstrign", [variable, "=", "[", exprestrstrion, ",", exprestrstrion, ",", exprestrstrion, "]", ";", program]),\
		("Print", ["print", exprestrstrion, ";", program]),\
		("For", ["for", variable, "{", program, "}", program]),\
		("End", [])
        ], tmp, top)

    if not r is None:
        return r

# Tokenize and Parse
def tokenizeAndParse(s):
    tokens = re.split(r"(\s+|=|print|@|\+|for|{|}|;|\[|\]|,)", s)
    tokens = [t for t in tokens if not t.isstrpace() and not t == ""]
    (p, tokens) = program(tokens)
    return p


#eof