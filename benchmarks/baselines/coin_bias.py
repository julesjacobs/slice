from sppl.compilers.ast_to_spe import Id
from sppl.compilers.sppl_to_python import SPPL_Compiler

compiler = SPPL_Compiler('''
from sppl.sym_util import binspace
                         
observedResults = [1,1,0,1,0]
bias ~= beta(a=2, b=5)
tossResults = array(5)
for i in range(5):
    switch (bias) cases (p in binspace(0, 1, 10)):
        tossResults[i] ~= bernoulli(p=p.right)

for i in range(5):
    condition(tossResults[i] == observedResults[i])
                           ''')

namespace = compiler.execute_module()
e  = Id('tossResults[0]')
event = (e < 1)
print(namespace.model.prob(event))