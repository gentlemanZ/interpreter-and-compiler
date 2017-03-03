#####################################################################
#
# CAS CS 320, Spring 2015
# Assignment 4 (skeleton code)
# parse.py
#
#  ****************************************************************
#  ************* You do not need to modify this file. *************
#  ****************************************************************
#

import re

Node = dict
Leaf = str

def parser(grammar, topNonterminal):
    nonterminals = [nt for p in grammar for nt in p]
    ts = {el for p in grammar for nt in p for (l, seq) in p[nt] for el in seq}
    terminals = {"\\"+t if t in "()+*" else t for t in ts if not (t in nonterminals or t[0] == '/')}
    def parse(tmp, nonterminal = topNonterminal, top = False):
        if type(tmp) == str:
            tmp = [t for t in re.split("(\s+|"+"|".join(terminals)+")", tmp)]
            tmp = [t for t in tmp if not (t == None or t.isspace() or t == "")]
        for production in grammar:
            if nonterminal in production:
                for (label, seq) in production[nonterminal]:
                    (ts, es, tokens) = (0, [], tmp[0:])
                    if len(tmp) == 0:
                        if len(seq) == 0:
                            return (label, [])
                    for x in seq:
                        if x[0] == '/' and x[-1] == '/':
                            if len(tokens) > 0 and re.compile(x[1:-1]).match(tokens[0]):
                                es = es + [int(tokens[0]) if label == 'Number' else tokens[0]]
                                tokens = tokens[1:]
                            else: break
                        elif not x in nonterminals:
                            if len(tokens) > 0 and tokens[0] == x:
                                tokens = tokens[1:]
                                ts = ts + 1
                            else: break
                        else:
                            r = parse(tokens, x, False)
                            if not r is None:
                                (e, tokens) = r
                                es = es + [e]
                    if ts + len(es) == len(seq) and (not top or len(tokens) == 0):
                        if label == None and len(es) == 1:
                            return (e, tokens)
                        else:
                            return ({label:es} if len(es) > 0 else label, tokens)
    return (lambda tokens: (lambda r: r[0] if not r is None else None) (parse(tokens)))
    
grammar = [\
    {'declaration': [\
      ('Function', ['variable', '(', 'pattern' , ')', '=', 'expression', ';', 'declaration']),\
      ('End', [])\
      ]\
    },\
    {'pattern': [\
      (None, ['(', 'pattern', ')']),\
      ('ConInd', ['/([A-Z][A-Za-z]*)/', 'pattern', 'pattern']),\
      ('ConBase', ['/([A-Z][A-Za-z]*)/']),\
      ('Variable', ['/([a-z][A-Za-z]*)/']),\
      ('Number', ['/(0|[1-9][0-9]*)/'])\
      ]\
    },\
    {'expression': [\
      ('Plus', ['expressionLeft', '+', 'expression']),\
      (None, ['expressionLeft'])\
      ]\
    },\
    {'expressionLeft': [\
      ('Number', ['/(0|[1-9][0-9]*)/']),\
      (None, ['(', 'expression', ')']),\
      ('ConInd', ['/([A-Z][A-Za-z]*)/', 'expression', 'expression']),\
      ('ConBase', ['/([A-Z][A-Za-z]*)/']),\
      ('Apply', ['variable', '(', 'expression', ')']),\
      ('Variable', ['/([a-z][A-Za-z]*)/'])\
      ]\
    },\
    {'variable': [\
      ('Variable', ['/([a-z][A-Za-z]*)/'])\
      ]\
    }\
  ]

#eof