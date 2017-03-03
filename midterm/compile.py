#####################################################################
#
# CAS CS 320, Spring 2015
# Midterm (skeleton code)
# compile.py
# Tianyang Zhong
# tianyang@bu.edu
#  ****************************************************************
#  *************** Modify this file for Problem #3. ***************
#  ****************************************************************
#

from random import randint
exec(open('parse.py').read())
exec(open('interpret.py').read())
exec(open('optimize.py').read())
exec(open('machine.py').read())
exec(open('analyze.py').read())
Leaf = str
Node = dict

def freshStr():
    return str(randint(0,10000000))

def compileExpression(env, e, heap):
    if type(e) == Node:
        for label in e:
            children = e[label]
            if label == 'Number':
                n = children[0]
                heap = heap + 1
                return (['set ' + str(heap) + ' ' + str(n)], heap, heap)
            elif label == 'Variable':
                if children[0] in env:
                    return ([], env[children[0]], heap)
                else:
                    print("Unbound!")
                    exit()
            elif label == 'Plus':
                (i1, a1, heap) = compileExpression(env, children[0], heap)
                (i2, a2, heap) = compileExpression(env, children[1], heap)
                heap = heap + 1
                instructions = i1 + i2 + copy(a1, 1) + copy(a2, 2) + \
                               ['add'] + copy(0, heap)
                return (instructions, heap, heap)
            elif label == 'Array':
                x = children[0]
                e = children[1]
                (i1, a1, heap) = compileExpression(env, e, heap)
                (i2, a2, heap) = compileExpression(env, x, heap)
                instsThree = copy(a1, 1) + ['set 2 ' + str(a2), 'add']
                heap = heap + 1
                instsFour = copyFromRef(0, heap)
                return (i1 + i2 + instsThree + instsFour, heap, heap)

def compileProgram(env, s, heap = 8): # Set initial heap default address.
    if type(s) == Leaf:
        if s == 'End':
            return (env, [], heap)
    if type(s) == Node:
        for label in s:
            children = s[label]
            if label == 'Print':
                [e, p] = children
                (instsE, addr, heap) = compileExpression(env, e, heap)
                (env, instsP, heap) = compileProgram(env, p, heap)
                return (env, instsE + copy(addr, 5) + instsP, heap)
            elif label == 'Assign':
                [x, e0, e1, e2, prog] = children
                (i1, a1, heap) = compileExpression(env, e0, heap)
                (i2, a2, heap) = compileExpression(env, e1, heap)
                (instsThree, addrThree, heap) = compileExpression(env, e2, heap)
                start = heap + 1
                heap = heap + 1
                instsFour = copy(a1, heap)
                heap = heap + 1
                instsFour = instsFour + copy(a2, heap)
                heap = heap + 1
                instsFour = instsFour + copy(addrThree, heap)
                env[x['Variable'][0]] = start
                (env, instsFive, heap) = compileProgram(env, prog, heap)
                return (env, i1 + i2 + instsThree + instsFour + instsFive, heap)

    
def compile(s):
    p = tokenizeAndParse(s)
    #print(p)
    if typeProgram({}, p) != 'Void':
        print('you use the wrong type in the program!!!')
        return None
    #print("hello")
    p = foldConstants(p)
    p = unrollLoops(p)

    (env, insts, heap) = compileProgram({}, p)
    return insts

def compileAndSimulate(s):
    return simulate(compile(s))
#print(compileAndSimulate("print 5; x = [2, 2, 2]; for i { print @ x [i]; }"))
#eof