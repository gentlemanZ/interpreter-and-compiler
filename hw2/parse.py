'''
cs320
Assignment#2:More Parsing Algorithms and Interpreters
student: tianyang zhong
BU ID:U19285500
due date:2/17/2015
this is the parse
'''
import re
#problem 1 part a:
def number(tokens):
    if re.match(r"^([0-9][0-9]*)$", tokens[0]):
        return (int(tokens[0]), tokens[1:])
		
def variable(tokens):
	if re.match(r"a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z", tokens[0]):
		return (tokens[0],tokens[1:])
        
def tokenize(P,S):
    str=""
    for t in P:
        if (t=="(" or t==")"or t=="+"or t=="-"or t=="*"):
            str=str+"|"+"\\"+t
        else:
        	str=str+"|"+t
    #print(type("(\s+"+str+")"))
    #print("(\s+"+str+")")
    tokens=[t for t in re.split(r"(\s+|\d+|\w+"+str+")",S)]
    return [t for t in tokens if not t.isspace() and not t ==""]

#problem 1 part b:
def left(tokens):
    if tokens[0]=="true":
        return("True",tokens[1:])
    if tokens[0] == "false":
        return("False",tokens[1:])
    if tokens[0] =="bool" and tokens[1] =="(":
        (e1,tokens) = term(tokens[2:],False)
        if tokens[0]==")":
            return ({"Bool":[e1]},tokens[1:])
    else:
        r=variable(tokens)
        if not r is None:
            (e1,tokens)=r
            return ({'Variable':[e1]},tokens[0:])
            
def formula(tmp, top = True):
    tokens = tmp[0:]
    r=left(tokens)
    if not r is None :
        (e1,tokens)=r
        if len(tokens) !=0 and tokens[0] == "xor":
            r2 = formula(tokens[1:],False)
            if not r2 is None:
                (e2,tokens) =r2
                if not top or len(tokens) == 0:
                    return ({"Xor":[e1,e2]},tokens[0:])
        else:
            if not top or len(tokens) == 0:
                return (e1,tokens)

            
#problem 1 part c:
def term(tmp, top =True):
    tokens = tmp[0:]
    r=factor(tokens,False)
    if not r is None:
        (e1,tokens)=r
        if len(tokens) != 0 and tokens[0] =="+":
            r2 = term(tokens[1:],False)
            if not r2 is None:
                (e2,tokens) = r2
                if not top or len(tokens) == 0:
                    return ({"Add":[e1,e2]},tokens[0:])
        else:
            if not top or len(tokens) == 0:
                return (e1,tokens)
            
def factor(tmp, top = True):
    tokens = tmp[0:]
    r=left2(tokens,False)
    if not r is None:
        (e1,tokens)=r
        if len(tokens) != 0 and tokens[0] == "*":
            r2 = factor(tokens[1:],False)
            if not r2 is None:
                (e2,tokens) =r2
                if not top or len(tokens) == 0:
                    return ({"Multiply":[e1,e2]},tokens[0:])
        else:
            if not top or len(tokens) == 0:
                return (e1,tokens)
def left2(tmp, top = True):
    tokens = tmp[0:]
    if tokens[0] == 'int' and tokens[1] == '(':
        tokens = tokens[2:]
        r = formula(tokens, False)
        if not r is None:
            (e1, tokens) = r
            if tokens[0] == ')':
                tokens = tokens[1:]
                if not top or len(tokens) == 0:
                    return ({'Int':[e1]}, tokens)
                
    tokens = tmp[0:]
    if tokens[0] == '(':
        tokens = tokens[1:]
        r = term(tokens, False)
        if not r is None:
            (e1,tokens)= r
            if(tokens[0] ==")"):
                tokens = tokens[1:]
                if not top or len(tokens) ==0:
                    return ({'Parens':[e1]},tokens)
    tokens = tmp[0:]
    r= variable (tokens)
    if not r is None:
        (e1,tokens) =r
        if not top or len(tokens) ==0:
            return ({'Variable':[e1]},tokens)
    
    tokens = tmp[0:]
    r= number (tokens)
    if not r is None:
        (e1,tokens) =r
        if not top or len(tokens) ==0:
            return ({'Number':[e1]},tokens)

            
#problem 1 part d:

def program(tmp, top = True):
    if tmp == [ ]:
        return ('End',[])
    tokens = tmp[0:]
    if tokens[0] == '}':
        r = program(tokens[1:],False)
        if not r is None:
            return ('End',tokens[0:])
    tokens = tmp[0:]
    if tokens[0] == 'print':
        tokens = tokens[1:]
        r = expression(tokens, False)
        if not r is None:
            (e1, tokens) = r
            if tokens[0] == ';':
                r =  program(tokens[1:],False)
                if not r is None:
                    (e2,tokens) = r
                    if not top or len(tokens) == 0 or e2 is None:
                        return ({'Print':[e1,e2]}, tokens)
                    
    tokens = tmp[0:]
    if tokens[0] == 'assign':
        
        tokens = tokens[1:]
        r = variable(tokens)
        if not r is None:
            (e1, tokens) = r
            if tokens[0] == '=':
                (e2,tokens) = expression(tokens[1:],False)
                if not e2 is None and tokens[0] == ';':
                    (e3,tokens) = program(tokens[1:],False)
                    if not top or len(tokens) == 0 or e3 is None:
                        return ({'Assign':[{'Variable':[e1]},e2,e3]}, tokens)
                    
    tokens = tmp[0:]
    if tokens[0] == 'if':
        tokens = tokens[1:]
        r = expression(tokens, False)
        if not r is None:
            (e1, tokens) = r
            if tokens[0] == '{':
                (e2,tokens) = program(tokens[1:],False)
                if not e2 is None and tokens[0] == '}':
                    (e3,tokens) = program(tokens[1:],False)
                    if not top or len(tokens) == 0 or e3 is None:
                        return ({'If':[e1,e2,e3]}, tokens)
    
    tokens = tmp[0:]
    if tokens[0] == 'while':
        tokens = tokens[1:]
        r = expression(tokens, False)
        if not r is None:
            (e1, tokens) = r
            if tokens[0] == '{':
                (e2,tokens) = program(tokens[1:],False)
                if not e2 is None and tokens[0] == '}':
                    (e3,tokens) = program(tokens[1:],False)
                    if not top or len(tokens) == 0 or e3 is None:
                        return ({'While':[e1,e2,e3]}, tokens)
                        
                        
def expression(tmp,top = True):
    tokens = tmp[0:]
    r= formula (tokens,False)
    if not r is None:
        (e1,tokens) =r
        if (not top or len(tokens)==0) and (tokens[0] =='{' or tokens[0] ==';'):
            return (e1,tokens)
    
    tokens = tmp[0:]
    r= term (tokens,False)
    if not r is None:
        (e1,tokens) =r
        if not top or len(tokens) and (tokens[0] =='{' or tokens[0] ==';'  )==0:
            return (e1,tokens)
