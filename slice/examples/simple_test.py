from sppl.compilers.ast_to_spe import Id
from sppl.compilers.sppl_to_python import SPPL_Compiler

compiler = SPPL_Compiler('''
perfB1 ~= binom(n=3, p=.8)

                           ''')

namespace = compiler.execute_module()
e  = Id('perfB1')
event = (e < 2)
print(namespace.model.prob(event))