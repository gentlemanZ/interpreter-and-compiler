'''
cs320
Assignment#2:More Parsing Algorithms and Interpreters
student: tianyang zhong
BU ID:U19285500
due date:2/17/2015
this is the interpreter
'''

exec(open('parse.py').read())
Node = dict
Leaf1 = str
Leaf2 = int
def evalTerm(env,t):
    if type(t) == Node:
        for label in t:
            children = t[label]
            if label == 'Number':
                f = children[0]
                #print(env, f)
                v = evalTerm(env, f)
                return v
            elif label == 'Parens':
                f1 = children[0]
                v1 = evalTerm(env, f1)
                return v1
            elif label == 'Add':
                f1 = children[0]
                v1 = evalTerm(env, f1)
                f2 = children[1]
                v2 = evalTerm(env, f2)
                return v1+v2
            elif label == 'Multiply':
                f2 = children[1]
                v2 = evalTerm(env, f2)
                f1 = children[0]
                v1 = evalTerm(env, f1)
                return v1*v2
            elif label == 'Int':
                f1 = children[0]
                v1 = evalTerm(env, f1)
                if v1:
                    return {'Number':[1]}
                else:
                    return {'Number':[0]}
            elif label == 'Variable':
                x = children[0]
                if x in env:
                    return env[x]
                else:
                    print(x+"is unbound.")
                    exit()

    elif type(t) == Leaf1:
        if t =='True':
            return True
        if t == 'False':
            return False
    elif type(t) ==Leaf2:
        return t
    
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

def evalFormula(env,f):
    if type(f) == Node:
        for label in f:
            children = f[label]
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
            elif label =='Xor':
                f2 = children[1]
                v2 = evalFormula(env, f2)
                f1 = children[0]
                v1 = evalFormula(env, f1)
                return vxor(v1, v2)
            elif label =='Bool':
                f = children[0]
                v = evalFormula(env, f)
                if v ==0:
                    return 'False'
                else:
                    return 'True'
            elif label == 'Variable':
                x = children[0]
                if x in env:
                    return env[x]
                else:
                    print(x + " is unbound.")
                    exit()
    elif type(f) == Leaf1:
        if f == 'True':
            return 'True'
        if f == 'False':
            return 'False'



def execProgram(env,c):
    if type(c) == Leaf1:
        if c == 'End':
            return (env, [])
    elif type(c) == Node:
        for label in c:
            if label == 'Print':
                children = c[label]
                f = children[0]
                p = children[1]


                v = evalFormula(env, f)
                if not v is None:
                    (env, o) = execProgram(env, p)
                    return (env, [v] + o)
                if v is None:
                    v=evalTerm(env, f)
                    if not v is None:
                        (env, o) = execProgram(env, p)
                        return (env, [v] + o)
            if label == 'Assign':
                children = c[label]
                x = children[0]['Variable'][0]
                f = children[1]
                p = children[2]
                v = evalFormula(env, f)
                if not v is None:
                    env[x] = v
                    (env, o) = execProgram(env, p)
                    return (env, o)
                if v is None:
                    v = evalTerm(env, f)
                    env[x] = v
                    (env, o) = execProgram(env, p)
                    return (env, o)
            if label == 'RepeatTwice':
                children = c[label]
                body = children[0]
                rest = children[1]
                env1 = env
                (env2, o1) = execProgram(env1, body)
                (env3, o2) = execProgram(env2, body)
                (env4, o3) = execProgram(env3, rest)
                return (env4, o1 + o2 + o3)
            if label == 'If':
                children = c[label]
                condition = children[0]
                body = children[1]
                rest = children[2]
                env1 = env
                if evalFormula(env1,condition) == 'False':
                    (env, o) = execProgram(env1, rest)
                    return (env,o)
                elif evalFormula(env1,condition) == 'True':
                    (env2,o1)= execProgram(env1,body)
                    (env3,o2)=execProgram(env2,rest)
                    return (env3,o1+o2)
            if label == 'While':
                children = c[label]
                condition = children[0]
                body = children[1]
                rest = children[2]
                env1 = env
                if evalFormula(env1,condition) == 'False':
                    (env, o) = execProgram(env1, rest)
                    return (env,o)
                elif evalFormula(env1,condition) == 'True':
                    (env2,o1)= execProgram(env1,body)
                    (env3,o2)= execProgram(env2,{'While':children})
                    return (env3,o1+o2)
def interpret(s):
    terminals=['xor','bool','(',')','true','false','+','*','int','print',';','assign','=','if','{','}','while']
    #print(program(tokenize(terminals,s))[0])
    return execProgram({},program(tokenize(terminals,s))[0])[1]

    
#print(interpret("assign x = true; while x { print x; assign x = false; } print x;"))
