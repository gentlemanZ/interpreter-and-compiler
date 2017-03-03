'''
#CAS CS 320, Spring 2015
#Assignment 3 (the final one)
#Tianyang Zhong
#U19285500
# compile.py
'''
Node = dict
Leaf = str
exec(open("parse.py").read())
exec(open("machine.py").read())
import random
#compileTerm function. takes env,t,and heap. Notice that it never change env.
def compileTerm(env,t,heap):
    if type(t) == Node:
        for label in t:
            children = t[label]
            if label == 'Number':
                heap = heap+1
                inst = 'set ' + str(heap) +' '+ str(children[0])
                addr = heap 
                return ([inst], addr, heap)
            if label == 'Variable':
                #print(children)
                t=children[0]
                return ([],env[t],heap)
            if label == 'Plus':
                f1 = children[0]
                f2 = children[1]
                (insts1, addr1, heap2) = compileTerm(env,f1,heap)
                (insts2,addr2,heap3) = compileTerm(env,f2,heap2)
                heap4 =heap3+1
                instsPlus =\
                    copy(addr1,1)+\
                    copy(addr2,2)+\
                    ['add',]\
                     +copy(0,heap4)
                addr3 = heap4
                return (insts1+insts2+instsPlus,addr3,heap4)
    elif type(t) == Leaf:
        if t =='True':
            heap = heap +1
            inst = 'set '+str(heap)+' 1'
            addr = heap
            return ([inst], addr, heap)
        if t == 'False':
            heap = heap +1
            inst = 'set ' +str(heap)+' 0'
            addr = heap
            return ([inst], addr, heap)
#compileFormula
def compileFormula(env,t,heap):
    if type(t) == Leaf:
        if t =='True':
            heap = heap +1
            inst = 'set '+str(heap)+' 1'
            addr = heap
            return ([inst], addr, heap)
        if t == 'False':
            heap = heap +1
            inst = 'set ' +str(heap)+' 0'
            addr = heap
            return ([inst], addr, heap)
    elif type(t) == Node:
        for label in t:
            children = t[label]
            if label == 'Variable':
                #print(children)
                t=children[0]
                return ([],env[t],heap)
            if label =='Not':
                fresh = random.random()
                t = children[0]
                print(t)
                (insts,addr,heap2) = compileFormula(env,t,heap)
                print(addr)
                heap3=heap2+1
                instsNot =\
                    ['branch setZero ' +str(addr),\
                     'set ' + str(heap3) +' 1',\
                     'goto finish'+str(fresh),\
                     'label setZero',\
                     'set ' + str(heap3) + ' 0',\
                     'label finish'+str(fresh)\
                    ]
                addr=heap3
                return (insts+instsNot, addr, heap3)
            if label =='Or':
                fresh = random.random()
                f1 = children[0]
                f2 = children [1]
                (insts1,addr1,heap2) = compileFormula(env,f1,heap)
                (insts2,addr2, heap3) = compileFormula(env,f2,heap2)
                heap4=heap3+1
                instsOr =\
                    copy(addr1,1)+\
                    copy(addr2,2)+[\
                     'add',\
                     'branch setOne '+ str(0),\
                     'goto finish'+str(fresh),\
                     'label setOne',\
                     'set 0 1',\
                     'label finish'+str(fresh),\
                     ]+\
                     copy(0,heap4)
                addr = heap4
                return (insts1 + insts2 + instsOr, addr,heap4)
            if label =='And':
                fresh=random.random()
                f1 = children[0]
                f2 = children [1]
                #print(compileFormula(env,f1,heap))
                (insts1,addr1,heap2) = compileFormula(env,f1,heap)
                (insts2,addr2, heap3) = compileFormula(env,f2,heap2)
                heap4=heap3+1
                instsAnd =\
                   copy(addr1,0)+\
                    [\
                     'branch next '+ '0',\
                     'goto finish'+str(fresh),\
                     'label next']+\
                     copy(addr2,0)+\
                     ['branch setOne'+str(fresh)+' 0',\
                     'goto finish'+str(fresh),\
                     'label setOne'+str(fresh),\
                     'set 0 1',\
                     'label finish'+str(fresh),]+\
                     copy(0,heap4)
                addr = heap4
                return (insts1 + insts2 + instsAnd, addr,heap4)

#function compileExpression, useful helper fuction.
def compileExpression(env,s,heap):
    v = compileFormula(env,s,heap)
    if not v is None:
        return v
    else:
        v=compileTerm(env,s,heap)
        if not v is None:
            return v
#compileProgram  remember to 
def compileProgram(env, s , heap):
    if type(s) == Leaf:
        if s== 'End':
            return (env,[],heap)
    elif type(s) == Node:
        for label in s:
            children = s[label]
            if label == 'Print':
                f1=children[0]
                f2=children[1]
                (insts1,addr,heap2) = compileExpression(env,f1,heap)
                instsPrint=\
                    copy(addr,5)
                (env,insts2,heap3) = compileProgram(env,f2,heap2)
                return (env,insts1+instsPrint+insts2,heap3)
            if label == 'Assign':
                f = children[0]['Variable'][0]
                p = children[1]
                d = children[2]
                (insts,addr,heap2) = compileExpression(env,p,heap)
                env[f] = addr
                (env,insts2,heap3) = compileProgram(env,d,heap2)
                return (env,insts+insts2,heap3)         
            if label == 'If':
                fresh = random.random()
                condition = children[0]
                body = children[1]
                rest = children[2]
                (insts1,addr1,heap1) = compileExpression(env,condition,heap)
                (env1,insts2,heap2) = compileProgram(env,body,heap1)
                heap3 = heap2+1
                instsIf=\
                    ['branch setbody'+ str(heap3)+' '+str(addr1),\
                     'branch finish'+str(heap3)+' 5',\
                     'label setbody'+ str(heap3)]+\
                     insts2+\
                    ['label finish'+str(heap3)]
                (env2,insts3,heap4) =compileProgram(env1,rest,heap3)
                     
                return (env2, insts1+instsIf+insts3, heap4)
            if label == 'While':
                fresh = random.random()
                condition = children[0]
                body = children[1]
                rest = children[2]
                (insts1,addr1,heap1) = compileExpression(env,condition,heap)
                (env1,insts2,heap2) = compileProgram(env,body,heap1)
                    
                heap3 = heap2+1
                instsIf=\
                    ['branch setbody'+ str(heap3)+' '+str(addr1),\
                     'branch finish'+str(heap3)+' 5',\
                     'label setbody'+ str(heap3)]+\
                     insts2+\
                    ['label finish'+str(heap3)]
                (env2,insts3,heap4) =compileProgram(env1,rest,heap3)
                     
                return (env2, insts1+instsIf+insts3, heap4)
            if label =='Procedure':
                name = children[0]['Variable'][0]
                body = children[1]
                rest = children[2]
                (env, insts1, heap1) = compileProgram(env, body, heap)
                insts = procedure(name, insts1)
                (env, insts2, heap2) = compileProgram(env, rest, heap1)
                heap = heap2
                insts = insts + insts2
                return (env, insts, heap)
            if label == 'Call':
               name = children[0]['Variable'][0]
               rest = children[1]
               insts = call(name)
               (env, insts1, heap1) = compileProgram(env, rest, heap)
               heap = heap1
               insts = insts + insts1
               return (env, insts, heap)
#final wrap fucntion. remember to set 7 to 0.
def compile(s):
    (env,insts,heap) = compileProgram({},tokenizeAndParse(s),heap=7)
    insts= ['set 7 0'] + insts
    return insts
print( simulate(compile("procedure example {print 4;} call example;")))