from sppl.compilers.ast_to_spe import Id
from sppl.compilers.sppl_to_python import SPPL_Compiler

compiler = SPPL_Compiler('''
Nationality   ~= choice({'India': 0.5, 'USA': 0.5})
if (Nationality == 'India'):
    Perfect       ~= bernoulli(p=0.10)
    if (Perfect == 1):  GPA ~= atomic(loc=10)
    else:               GPA ~= uniform(loc=0, scale=10)
else:
    Perfect       ~= bernoulli(p=0.15)
    if (Perfect == 1):  GPA ~= atomic(loc=4)
    else:               GPA ~= uniform(loc=0, scale=4)   
condition(GPA == 2)      
                           ''')

namespace = compiler.execute_module()
e  = Id('GPA')
event = (e < 1)
print(namespace.model.prob(event))