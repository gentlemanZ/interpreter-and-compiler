#####################################################################
#
# CAS CS 320, Spring 2015
# Assignment 4 
# interpret.py
# Tianyang Zhong
# tianyang@bu.edu
# U 19285500
# 

exec(open("parse.py").read())

def subst(s, a):

    if type(a) == Node:
        for label in a:
            children = a[label]
            b = children[0]
            if label == 'Variable':
                if s != None and b in s:
                    return s[b]
                else:
                    return a
            else:
                return {label: [subst(s, b) for b in children]}
    elif type(a) == Leaf:
        return a

def unify(a, b):
    if type(a) == Leaf and a == b:
        return {}
    elif type(a) == Node and type(b) == Node:
        for labelA in a:
            for labelB in b:
                child1 = a[labelA]
                child2 = b[labelB]
                #print(child1)
                #print(child2)
                if 'Variable' == labelA:
                    return {child1[0]: b}
                if 'Variable' == labelB:
                    return {child2[0]: a}
                if labelA == labelB and len(child1) == len(child2):
                    combined = []
                    #orginze=[]
                    #print(child1)
                    #print(child2)
                    for i in range (0, len(child1)):
                        (c,d) = (child1[i],child2[i])
                        if unify(c, d) != None:
                            for x in unify(c, d):
                                combined += [(x, unify(c, d)[x])]
                    #print(combined)
                    return dict(combined)


def build(m, d):
    if type(d) == Node:
        for label in d:
            children = d[label]
            if label == 'Function':
                f = children[0]['Variable'][0]
                p = children[1]
                e = children[2]
                if f in m:
                    m[f] = m[f] + [(p, e)]
                else:
                    m[f] = [(p, e)]
                return build(m, children[3])
    elif type(d) == Leaf:
        if d == 'End':
            return m
  
def evaluate(m, env, e):
    if type(e) == Node:
        for label in e:
            children = e[label]
            if label == 'ConInd':
                v1 = evaluate(m, env, children[1])
                v2 = evaluate(m, env, children[2])
                return {'ConInd': [children[0], v1, v2]}
            elif label == 'ConBase':
                return e
            elif label == 'Number':
                return evaluate(m, env, children[0])
            elif label == 'Variable':
                if children[0] in env:
                    return env[children[0]]
            elif label == 'Plus':
                n1 = evaluate(m, env, children[0])
                n2 = evaluate(m, env, children[1])
                return n1 + n2
            elif label == 'Apply':
                f = children[0]['Variable'][0]
                v1 = evaluate(m, env, children[1])
                for (p, e2) in m[f]:
                    if subst(unify(p, v1), p) == subst(unify(p, v1), v1):
                        return evaluate(m, unify(p, v1), e2)
    else:
        return e

def interact(s):
    # Build the module definition.
    m = build({}, parser(grammar, 'declaration')(s))

    # Interactive loop.
    while True:
        # Prompt the user for a query.
        s = input('> ') 
        if s == ':quit':
            break
        
        # Parse and evaluate the query.
        e = parser(grammar, 'expression')(s)
        if not e is None:
            print(evaluate(m, {}, e))
        else:
            print("Unknown input.")
#(subst({'x': {'Number': [5]}}, {'Variable': ['x']}))
#eof