#####################################################################
#
# CAS CS 320, Spring 2015
# Midterm 
# interpret.py
# Tianyang Zhong
# U 19 28 5500
#

exec(open("parse.py").read())

Node = dict
Leaf = str

def evaluate(env, e):
    if type(e) == Node:
        for label in e:
            children = e[label]
            if label == 'Number':
                f = children[0]
                #print(env, f)
                return int(f)
            elif label == 'Plus':
                f1 = children[0]
                v1 = evaluate(env, f1)
                f2 = children[1]
                v2 = evaluate(env, f2)
                return v1+v2
            elif label == 'Array':
                x = children[0]['Variable'][0]
                k = int(evaluate(env, children[1]))
                if x in env:
                    return env[x][k]
                else:
                    print(x + " is unbound.")
                    exit()
            elif label == 'Variable':
                x = children[0]
                if x in env:
                    return env[x]
                else:
                    print(x+"is unfounded.")
                    exit()

                    
    elif type(e) == Leaf:
        if e == 'True':
            return 'True'
        if e == 'False':
            return 'False'
    elif type(e) == int:
        return e

def execute(env, s):
    if type(s) == Leaf:
        if s == 'End':
            return (env, [])
    elif type(s) == Node:
        for label in s:
            children = s[label]
            if label == 'Print':
                v = evaluate(env, children[0])
                #print(execute(env, children[1]))
                (env, o) = execute(env, children[1])
                return (env, [v] + o)
            if label == 'Assign':
                #children = s[label]
                #print(children)
                x = children[0]['Variable'][0]
                f1 = children[1]
                f2 = children[2]
                f3 = children[3]
                p = children[4]
                #print(f3)
                v1 = evaluate(env, f1)
                v2 = evaluate(env, f2)
                v3 = evaluate(env, f3)
                env[x]= [v1,v2,v3]
                return execute(env, p)
            if label == 'For':
                x = children[0]['Variable'][0]
                p1 = children[1]
                p2 = children[2]
                env[x] = 0
                (env2, o1) = execute(env, p1)
                env2[x] = 1
                (env3, o2) = execute(env2, p1)
                env3[x] = 2
                (env4, o3) = execute(env3, p1)
                (env5, o4) = execute(env4, p2)
                return (env5, o1 + o2 + o3 + o4)

def interpret(s):
    #print(tokenizeAndParse(s))
    (env, o) = execute({}, tokenizeAndParse(s))
    return o
#print(interpret("a = [1+1,2+8,3];for i {print @a[i];}"))
#eof