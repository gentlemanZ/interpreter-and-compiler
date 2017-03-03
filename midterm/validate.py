#####################################################################
#
# CAS CS 320, Spring 2015
# Midterm (skeleton code)
# validate.py
# Tianyang Zhong
# tianyang@bu.edu
#

exec(open('interpret.py').read())
exec(open('compile.py').read())

def expressions(n):
    if n <= 0:
        []
    elif n == 1:
        return [{'Number':[2]}] 
    else:
        es = expressions(n-1)
        esN = []
        esN += [{'Array':[{'Variable':['a']}, e]} for e in es]
        return es + esN

def programs(n):
    if n <= 0:
        []
    elif n == 1:
        return ['End']
    else:
        ps = programs(n-1)
        es = expressions(n-1)
        psN = []
        psN += [{'Assign':[{'Variable':['a']}, e, e, e, p]} for p in ps for e in es]
        psN += [{'Print':[e, p]} for p in ps for e in es]
        psN += [{'For':[{'Variable':['x']}, p1, p2]} for p1 in ps for p2 in ps]

        return ps + psN



def defaultAssigns(p):
    return \
      {'Assign':[\
        {'Variable':['a']}, {'Number':[2]}, {'Number':[2]}, {'Number':[2]}, p\
      ]}

# Compute the formula that defines correct behavior for the
# compiler for all program parse trees of depth at most 4.
# Any outputs indicate that the behavior of the compiled
# program does not match the behavior of the interpreted
# program.
# use this file we can test all the possible case to test if our compiler and interpreter
# is working.
for p in [defaultAssigns(p) for p in programs(4)]:
    try:
        if simulate(compileProgram({}, unrollLoops(p))[1]) != execute({}, p)[1]:
            print('\nIncorrect behavior on: ' + str(p))
    except:
        print('\nError on: ' + str(p))
print("you have reach the end of the the tsting")

#eof