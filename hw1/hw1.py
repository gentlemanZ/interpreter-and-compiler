'''
cs320
Assignment#1:Grammars,Lexing,and Parsing
student: tianyang zhong
BU ID:U19285500
due date:2/2/2015
'''
import re
#problem 1 part a:
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
def transformation(S):
    if S[0] == "finish":
        return ("Finish",S[1:])
    if S[0] == "projection" and S[1] == ";":
        (e1,S)= transformation(S[2:])
        return ({"Projection":[e1]},S[1:])
    if S[0] == "reflection" and S[1]==";":
        (e1,S)= transformation(S[2:])
        return ({"Reflection":[e1]},S[1:])
    if S[0] == "left" and S[1]=="rotation" and S[2] == ";":
        (e1,S) = transformation(S[3:])
        return ({"LeftRotation":[e1]},S[1:]) 
    if S[0] == "right"and S[1] =="rotation" and S[2]==";":
        (e1,S)= transformation(S[3:])
        return ({"RightRotation":[e1]},S[1:])



#problem 2 part a:
def number(tokens):
    if re.match(r"^([1-9][0-9]*)$", tokens[0]):
        return ({"Number": [int(tokens[0])]}, tokens[1:])
		
def variable(tokens):
	if re.match(r"a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z", tokens[0]):
		return ({"Variable":[tokens[0]]},tokens[1:])
#old code for problem 2, did not use back tracking on this two function.
'''
#problem 2 part b:
def term(tokens):
	if tokens[0] == "#":
		return (number(tokens[1:]))
	if tokens[0] == "@":
		return (variable(tokens[1:]))
	if tokens[0] =="add" and tokens[1] == "(":
		(e1,tokens)= term(tokens[2:])
		if tokens[0] ==",":
			(e2,tokens) = term(tokens[1:])
			if tokens[0] == ")":
				return ({"Add":[e1,e2]},tokens[1:])
	if tokens[0] == "subtract" and tokens[1] == "(":
		(e1,tokens) = term(tokens[2:])
		if tokens[0] ==",":
			(e2,tokens) = term(tokens[1:])
			if tokens[0] == ")":
				return({"Subtract":[e1,e2]},tokens[1:])
	if tokens[0] == "abs" and tokens[1] =="(":
		(e1,tokens) = term(tokens[2:])
		if tokens[0] == ")":
			return({"Abs":[e1]},tokens[1:])
#unit test for problem 2 part b:
#print(term(['abs', '(', 'subtract', '(', '#', '2', ',', '#', '3', ')', ')']))



#problem 2 part c:

def formula(tokens):
	if tokens[0]=="true":
		return("True",tokens[1:])
	if tokens[0] == "false":
		return("False",tokens[1:])
	if tokens[0] =="not" and tokens[1] =="(":
		(e1,tokens) = formula(tokens[2:])
		if tokens[0]==")":
			return ({"Not":[e1]},tokens[1:])
	if tokens[0] == "and" and tokens[1]=="(":
		(e1,tokens)= formula(tokens[2:])
		if tokens[0]==",":
			(e2,tokens)=formula(tokens[1:])
			if tokens[0] ==")":
				return ({"And":[e1,e2]},tokens[1:])
			
	if tokens[0] == "or" and tokens[1]=="(":
		(e1,tokens) = formula(tokens[2:])
		if tokens[0] == ",":
			(e2,tokens)=formula(tokens[1:])
			if tokens[0] ==")":
				return ({"Or":[e1,e2]},tokens[1:])
	if tokens[0] == "compare" and tokens[1]=="(":
		(e1,tokens) = term(tokens[2:])
		if tokens[0] == ",":
			(e2,tokens)=term(tokens[1:])
			if tokens[0] ==")":
				return ({"Compare":[e1,e2]},tokens[1:])
	if tokens[0] == "less" and tokens[1]=="than"and tokens[2]=="(":
		(e1,tokens) = term(tokens[3:])
		if tokens[0] == ",":
			(e2,tokens)=term(tokens[1:])
			if tokens[0] ==")":
				return ({"LessThan":[e1,e2]},tokens[1:])
	if tokens[0] == "greater" and tokens[1]=="than"and tokens[2]=="(":
		(e1,tokens) = term(tokens[3:])
		if tokens[0] == ",":
			(e2,tokens)=term(tokens[1:])
			if tokens[0] ==")":
				return ({"GreaterThan":[e1,e2]},tokens[1:])
'''
#problem2 part d:
#should be the same structure as the first problem, but we may need to use 
#backtracking recursive descent.
def program(tokens):
    if tokens[0] =="end":
        return ("End",tokens[1:])
    if tokens[0] =="assign" and tokens[1] =="@":
        (e1,tokens) = variable(tokens[2:])
        if tokens[0] == "=":
                (e2,tokens)=term(tokens[1:],False)
                if tokens[0] ==";":
                    (e3,tokens)=program(tokens[1:])
                    return ({"Assign":[e1,e2,e3]},tokens[1:])
    if tokens[0] =="print" and (tokens[1] == "add" or tokens[1]== "subtract" or tokens[1]=="abs"or tokens[1]== "@"or tokens[1]=="#"):
        (e1,tokens)=term(tokens[1:],False) 
        if tokens[0]==";":
            (e2,tokens)=program(tokens[1:])
            return ({"Print":[e1,e2]},tokens[1:])
    if tokens[0]=="print" and (tokens[1]=="true"or tokens[1]=="false"or tokens[1]=="not"or tokens[1]=="and"or tokens[1]=="or"or tokens[1]=="compare"or tokens[1]=="less"or tokens[1]=="greater"):
        (e1,tokens)=formula(tokens[1:],False)
        if tokens[0]==";":
            (e2,tokens)=program(tokens[1:])
            return ({"Print":[e1,e2]},tokens[1:])
    if tokens[0] =="print" and tokens[1] =="(":
        #print(tokens[1:])
        r=formula(tokens[1:],False)
       # print(r)
        if not r == None:
            (e1,tokens) = r
            if tokens[0]==";":
                (e2,tokens)=program(tokens[1:])
                return ({"Print":[e1,e2]},tokens[1:])
        else:
            (e1,tokens) = term(tokens[1:],False)
            if tokens[0]==";":
                (e2,tokens)=program(tokens[1:])
                return ({"Print":[e1,e2]},tokens[1:])
#problem 2 part c:
def complete(string):
    terminals=['print',';','assign','@','=','end','true','false','not','and',',','(',')','or','compare','less','than','greater','than','add','subtract','abs','@','#']
    s=tokenize(terminals,string)
    if s[0] =="add" or s[0] =="subtract" or s[0] =="abs" or s[0] =="@" or s[0] =="#" or s[0] =="|":
        return term(s,True)
    if s[0] =="true" or s[0] =="false" or s[0] =="not" or s[0] =="and" or s[0] =="or" or s[0]=="compare" or s[0]=="less" or s[0]=="greater":
        return formula(s,True)
    if s[0]=='print' or s[0] == 'assign' or s[0] =='end':
        return program(s)

        
#problem 3 extend and modify:
#new formula function



def formula(tmp, top = True):
#handle the base case true and false
    tokens = tmp[0:]
    if tokens[0] == 'true':
        tokens = tokens[1:]
        if not top or len(tokens) == 0:
            return ('True', tokens)

    tokens = tmp[0:]
    if tokens[0] == 'false':
        tokens = tokens[1:]
        if not top or len(tokens) == 0:
            return ('False', tokens)
#handle the case not(formula)
    tokens = tmp[0:]
    if tokens[0] == 'not' and tokens[1] == '(':
        tokens = tokens[2:]
        r = formula(tokens, False)
        if not r is None:
            (e1, tokens) = r
            if tokens[0] == ')':
                tokens = tokens[1:]
                if not top or len(tokens) == 0:
                    return ({'Not':[e1]}, tokens)
#handle and(formula,formula) & or(formula,formula) compare(term,term) 
#less than(term,term) & greater than(term,term)
    tokens = tmp[0:]
    if tokens[0] == 'and' and tokens[1] =='(':
        tokens = tokens[2:]
        r=formula(tokens,False)
        if not r is None and r[1][0] ==',':
            r2=formula(r[1][1:],False)
            if not r2 is None:
                (e1,tokens)=r
                (e2,tokens)=r2
                if tokens[0]==')':
                    tokens=tokens[1:]
                    if not top or len(tokens) == 0:
                            return ({'And':[e1,e2]},tokens)
    
    tokens = tmp[0:]
    if tokens[0] == 'or' and tokens[1] =='(':
        tokens = tokens[2:]
        r=formula(tokens,False)
        if not r is None and r[1][0] ==',':
            r2=formula(r[1][1:],False)
            if not r2 is None:
                (e1,tokens)=r
                (e2,tokens)=r2
                if tokens[0]==')':
                    tokens=tokens[1:]
                    if not top or len(tokens) == 0:
                        return ({'Or':[e1,e2]},tokens)
    
    tokens = tmp[0:]
    if tokens[0] == 'compare' and tokens[1] =='(':
        tokens = tokens[2:]
        r=term(tokens,False)
        if not r is None and r[1][0] ==',':
            r2=term(r[1][1:],False)
            if not r2 is None:
                (e1,tokens)=r
                (e2,tokens)=r2
                if tokens[0]==')':
                    tokens=tokens[1:]
                    if not top or len(tokens) == 0:
                        return ({'Compare':[e1,e2]},tokens)
        
    
    tokens = tmp[0:]
    if tokens[0] == 'less' and tokens[1] =='than' and tokens[2]=='(':
        tokens = tokens[3:]
        r=term(tokens,False)
        if not r is None and r[1][0] ==',':
            r2=term(r[1][1:],False)
            if not r2 is None:
                (e1,tokens)=r
                (e2,tokens)=r2
                if tokens[0]==')':
                    tokens=tokens[1:]
                    if not top or len(tokens) == 0:
                        return ({'LessThan':[e1,e2]},tokens)\
    
    
    tokens = tmp[0:]
    if tokens[0] == 'greater' and tokens[1] =='than' and tokens[2]=='(':
        tokens = tokens[3:]
        r=term(tokens,False)
        if not r is None and r[1][0] ==',':
            r2=term(r[1][1:],False)
            if not r2 is None:
                (e1,tokens)=r
                (e2,tokens)=r2
                if tokens[0]==')':
                    tokens=tokens[1:]
                    if not top or len(tokens) == 0:
                        return ({'GreaterThan':[e1,e2]},tokens)
#handle the case ( formula && formula ) &( formula || formula ) &( term = term ) & ( term < term ) & ( term > term )

    tokens = tmp[0:]
    if tokens[0] == '(':
        tokens = tokens[1:]
        r = term(tokens, False)
        if not r is None:
            (e1, tokens) = r
            if tokens[0] == '<':
                tokens = tokens[1:]
                r = term(tokens, False)
                if not r is None:
                    (e2, tokens) = r
                    if tokens[0] == ')':
                        tokens = tokens[1:]
                        if not top or len(tokens) == 0:
                            return ({'LessThan':[e1,e2]}, tokens)

    tokens = tmp[0:]
    if tokens[0] == '(':
        tokens = tokens[1:]
        r = term(tokens, False)
        if not r is None:
            (e1, tokens) = r
            if tokens[0] == '=':
                tokens = tokens[1:]
                r = term(tokens, False)
                if not r is None:
                    (e2, tokens) = r
                    if tokens[0] == ')':
                        tokens = tokens[1:]
                        if not top or len(tokens) == 0:
                            return ({'Compare':[e1,e2]}, tokens)
	
    tokens = tmp[0:]
    if tokens[0] == '(':
        tokens = tokens[1:]
        r = term(tokens, False)
        if not r is None:
            (e1, tokens) = r
            if tokens[0] == '>':
                tokens = tokens[1:]
                r = term(tokens, False)
                if not r is None:
                    (e2, tokens) = r
                    if tokens[0] == ')':
                        tokens = tokens[1:]
                        if not top or len(tokens) == 0:
                            return ({'GreaterThan':[e1,e2]}, tokens)
    tokens = tmp[0:]
    if tokens[0] == '(':
        tokens = tokens[1:]
        r = term(tokens, False)
        if r is None:
            #print(tokens)
            (e1, tokens) = formula(tokens,False)
            #print(tokens)
            if tokens[0] == '&&':
                tokens = tokens[1:]
                r = formula(tokens, False)
                if not r is None:
                    (e2, tokens) = r
                    if tokens[0] == ')':
                        tokens = tokens[1:]
                        if not top or len(tokens) == 0:
                            return ({'And':[e1,e2]}, tokens)
							
							
    tokens = tmp[0:]
    if tokens[0] == '(':
        tokens = tokens[1:]
        r = term(tokens, False)
        if r is None:
            (e1, tokens) = formula(tokens,False)
            if tokens[0] == '||':
                tokens = tokens[1:]
                r = formula(tokens, False)
                if not r is None:
                    (e2, tokens) = r
                    if tokens[0] == ')':
                        tokens = tokens[1:]
                        if not top or len(tokens) == 0:
                            return ({'Or':[e1,e2]}, tokens)	
'''
'''
#the new term 							
def term(tmp, top = True):
    tokens = tmp[0:]
    if tokens[0] == 'add' and tokens[1] =='(':
        tokens = tokens[2:]
        r=term(tokens,False)
        if not r is None and r[1][0] ==',':
            r2=term(r[1][1:],False)
            if not r2 is None:
                (e1,tokens)=r
                (e2,tokens)=r2
                if tokens[0]==')':
                    tokens=tokens[1:]
                    if not top or len(tokens) == 0:
                        return ({'Add':[e1,e2]},tokens)


    tokens = tmp[0:]
    if tokens[0] == 'subtract' and tokens[1] =='(':
        tokens = tokens[2:]
        r=term(tokens,False)
        if not r is None and r[1][0] ==',':
            r2=term(r[1][1:],False)
            if not r2 is None:
                (e1,tokens)=r
                (e2,tokens)=r2
                if tokens[0]==')':
                    tokens=tokens[1:]
                    if not top or len(tokens) == 0:
                        return ({'Subtract':[e1,e2]},tokens)	


    tokens = tmp[0:]
    if tokens[0] == 'abs' and tokens[1] == '(':
        tokens = tokens[2:]
        r = term(tokens, False)
        if not r is None:
            (e1, tokens) = r
            if tokens[0] == ')':
                tokens = tokens[1:]
                if not top or len(tokens) == 0:
                    return ({'Abs':[e1]}, tokens)
    
    tokens = tmp[0:]
    if tokens[0] == '@':
        tokens = tokens[1:]
        if not top or len(tokens) == 0:
            return variable(tokens)
    
    tokens = tmp[0:]
    if tokens[0] == '#':
        tokens = tokens[1:]
        if not top or len(tokens) == 0:
            return number(tokens)
        
    tokens = tmp[0:]
    if tokens[0] == '(':
        tokens = tokens[1:]
        r = term(tokens, False)
        if not r is None:
            (e1, tokens) = r
            if tokens[0] == '+':
                tokens = tokens[1:]
                r = term(tokens, False)
                if not r is None:
                    (e2, tokens) = r
                    if tokens[0] == ')':
                        tokens = tokens[1:]
                        if not top or len(tokens) == 0:
                            return ({'Add':[e1,e2]}, tokens)
    
    tokens = tmp[0:]
    if tokens[0] == '(':
        tokens = tokens[1:]
        r = term(tokens, False)
        if not r is None:
            (e1, tokens) = r
            if tokens[0] == '-':
                tokens = tokens[1:]
                r = term(tokens, False)
                if not r is None:
                    (e2, tokens) = r
                    if tokens[0] == ')':
                        tokens = tokens[1:]
                        if not top or len(tokens) == 0:
                            return ({'Subtract':[e1,e2]}, tokens)
                            
    tokens = tmp[0:]
    if tokens[0] == '|':
        tokens = tokens[1:]
        r = term(tokens, False)
        if not r is None:
            (e1, tokens) = r
            if tokens[0] == '|':
                tokens = tokens[1:]
                if not top or len(tokens) == 0:
                    return ({'abs':[e1]}, tokens)
 

def number(tokens):
    if re.match(r"-|^([1-9][0-9]*)$", tokens[0]):
        return ({"Number": [int(tokens[0])]}, tokens[1:])

