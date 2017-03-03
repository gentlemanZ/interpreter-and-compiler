#####################################################################
#
# CAS CS 320, Spring 2015
# Midterm 
# analyze.py
# Tianyang Zhong
# tianyang@bu.edu
# U19 28 5500
#
#

exec(open("parse.py").read())

Node = dict
Leaf = str

def typeExpression(env, e):
    if type(e) == Leaf:
        if e =='True' or e =='False':
            return 'Boolean'

    if type(e) == Node:
        for label in e:
            children = e[label]
            if label == 'Number':
                return 'Number'

            if label == 'Variable':
                x = children[0]
                if x in env and env[x] =='Number':
                    return 'Number'

            elif label == 'Array':
                [x, e] = children
                x = x['Variable'][0]
                if x in env and env[x] == 'Array' and typeExpression(env, e) == 'Number':
                    return 'Number'

            elif label == 'Plus':
                if typeExpression(env,children[0]) =='Number':
                    if typeExpression(env,children[1]) == 'Number':
                        return 'Number'

def typeProgram(env, s):
    #print(s)
    if type(s) == Leaf:
        if s == 'End':
            return 'Void'
    elif type(s) == Node:
        for label in s:
            if label == 'Print':
            
                [e, p] = s[label]
                print(s)
                if typeExpression(env,e) =='Number' or typeExpression(env,e) =='Boolean':
                    if typeProgram(env,p) == 'Void':
                        return 'Void'
            if label == 'Assign':
                [x, e0, e1, e2, p] = s[label]
                x = x['Variable'][0]
                if typeExpression(env, e0) == 'Number' and\
                   typeExpression(env, e1) == 'Number' and\
                   typeExpression(env, e2) == 'Number':
                     env[x] = 'Array'
                     if typeProgram(env, p) == 'Void':
                           return 'Void'

            if label == 'For':
                [x, p1, p2] = s[label]
                x = x['Variable'][0]
                env[x] = 'Number'
                if typeProgram(env, p1) == 'Void':
                    if typeProgram(env, p2) == 'Void':
                        return 'Void'

#print(typeProgram({}, {'Print': [{'Number': [5]}, {'Assign': [{'Variable': ['x']}, {'Number': [2]}, {'Number': [2]}, {'Number': [2]}, {'For': [{'Variable': ['i']}, {'Print': [{'Array': [{'Variable': ['x']}, {'Variable': ['i']}]}, 'End']}, 'End']}]}]}))


#eof