from sppl.compilers.ast_to_spe import Id
from sppl.compilers.sppl_to_python import SPPL_Compiler

compiler = SPPL_Compiler('''
from sppl.sym_util import binspace

bias_0 = beta(a=1, b=1)
switch (bias_0) cases (p in binspace(0, 1, 10)):
    votes_0 ~= binom(p=p.right, n=1000)

ansBias_0 = bias_0 
switch (ansBias_0) cases (p in binspace(0, 1, 10)):
    answer_0 ~= bernoulli(p=p.right)    

condition(answer_0 == 1)            
                           ''')

namespace = compiler.execute_module()
e  = Id('answer_0')
event = (e < 0.5)
print(namespace.model.prob(event))