'''
cs320
Assignment#2:Compilation and Review of Interpretation
student: tianyang zhong
BU ID:U19285500
due date:3/2/2015
Problem 1 :interpreter
'''

Node = dict
Leaf = str
exec(open("parse.py").read())
#part a:
def evalTerm(env, e):
    if type(e) == Node:
        for label in e:
            children = e[label]
            if label == 'Number':
                f = children[0]
                return f
            elif label == 'Plus':
                f1 = children[0]
                v1 = evalTerm(env, f1)
                f2 = children[1]
                v2 = evalTerm(env, f2)
                return v1+v2
            elif label == 'Multiply':
                f1 = children[0]
                v1 = evalTerm(env, f1)
                f2 = children[1]
                v2 = evalTerm(env, f2)
                return v1*v2
            elif label == 'Variable':
                x = children[0]
                if x in env:
                    return env[x]
                else:
                    print(x+"is unbound.")
                    exit()
            elif label == 'Int':
                f = children[0]

                v = evalTerm(env, f)
                if v == 'True':
                    return {'Number': [1]}
                elif v == 'False':
                    return {'Number': [0]}

            elif label == 'Parens':
                f = children[0]
                v = evalTerm(env, f)
                return v


    elif type(e) == Leaf:
        if e =='True':
            return 'True'
        if e == 'False':
            return 'False'

#the helper function for evalFoumula:
def vnot(v):
    if v == 'True':  return 'False'
    if v == 'False': return 'True'

def vand(v1, v2):
    if v1 == 'True'  and v2 == 'True':  return 'True'
    if v1 == 'True'  and v2 == 'False': return 'False'
    if v1 == 'False' and v2 == 'True':  return 'False'
    if v1 == 'False' and v2 == 'False': return 'False'

def vor(v1, v2):
    if v1 == 'True'  and v2 == 'True':  return 'True'
    if v1 == 'True'  and v2 == 'False': return 'True'
    if v1 == 'False' and v2 == 'True':  return 'True'
    if v1 == 'False' and v2 == 'False': return 'False'
def vxor(v1,v2):
    if v1 == 'True'  and v2 == 'True':  return 'False'
    if v1 == 'True'  and v2 == 'False': return 'True'
    if v1 == 'False' and v2 == 'True':  return 'True'
    if v1 == 'False' and v2 == 'False': return 'False'

#part b:
def evalFormula(env, e):
    if type(e) == Node:
        for label in e:
            children = e[label]
            if label == 'Not':
                f = children[0]
                v = evalFormula(env, f)
                return vnot(v)
            elif label == 'And':
                f1 = children[0]
                v1 = evalFormula(env, f1)
                f2 = children[1]
                v2 = evalFormula(env, f2)
                return vand(v1, v2)
            elif label == 'Or':
                f2 = children[1]
                v2 = evalFormula(env, f2)
                f1 = children[0]
                v1 = evalFormula(env, f1)
                return vor(v1, v2)
            elif label == 'Xor':
                f2 = children[1]
                v2 = evalFormula(env, f2)
                f1 = children[0]
                v1 = evalFormula(env, f1)
                return vxor(v1, v2)
            elif label == 'Bool':
                f = children[0]
                v = evalFormula(env, f)
                if v != 0:
                    return 'True'

                if v == 0:
                    return 'False'
            elif label == 'Variable':
                x = children[0]
                if x in env:
                    return env[x]
                else:
                    print(x + " is unbound.")
                    exit()
    elif type(e) == Leaf:
        if e == 'True':
            return 'True'
        if e == 'False':
            return 'False'

def evalExpression(env, e): # Useful helper function.
    v = evalFormula(env,e)
    if not v is None:
        return v
    else:
        v=evalTerm(env,e)
        if not v is None:
            return v

def execProgram(env, c):
    if type(c) == Leaf:
        if c == 'End':
            return (env, [])
    elif type(c) == Node:
        for label in c:
            if label == 'Print':
                children = c[label]
                f = children[0]
                p = children[1]
                v = evalExpression(env, f)
                (env, o) = execProgram(env, p)
                return (env, [v] + o)
            if label == 'Assign':
                children = c[label]
                f = children[0]['Variable'][0]
                p = children[1]
                d = children[2]
                v = evalExpression(env, p)
                env[f] = v
                (env, o) = execProgram(env, d)
                return (env, o)
            if label == 'If':
                children = c[label]
                condition = children[0]
                body = children[1]
                rest = children[2]
                env1 = env
                if evalExpression(env1,condition) == 'False':
                    (env, o) = execProgram(env1, rest)
                    return (env,o)
                elif evalExpression(env1,condition) == 'True':
                    (env2,o1)= execProgram(env1,body)
                    (env3,o2)=execProgram(env2,rest)
                    return (env3,o1+o2)
            if label == 'While':
                children = c[label]
                condition = children[0]
                body = children[1]
                rest = children[2]
                env1 = env
                if evalExpression(env1,condition) == 'False':
                    (env, o) = execProgram(env1, rest)
                    return (env,o)
                elif evalExpression(env1,condition) == 'True':
                    (env2,o1)= execProgram(env1,body)
                    (env3,o2)= execProgram(env2,{'while':[children]})
                    return (env3,o1+o2)
            if label =='Procedure':
                children = c[label]
                x = children[0]['Variable'][0]
                f = children[1]
                p = children[2]
                env[x] = f
                (env2, o) = execProgram(env, p)
                return (env2, o)
            if label == 'Call':
                children = c[label]
                body = children[0]['Variable'][0]
                rest = children[1]
                p1 = env[body]
                (env2,o1)= execProgram(env,p1)
                (env3,o2)=execProgram(env2,rest)
                return (env3,o1+o2)
def interpret(s):
    (env, o) = execProgram({}, tokenizeAndParse(s))
    return o
#eof