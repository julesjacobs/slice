from sppl.compilers.ast_to_spe import Id
from sppl.compilers.ast_to_spe import IfElse
from sppl.compilers.ast_to_spe import Otherwise
from sppl.compilers.ast_to_spe import Sample
from sppl.compilers.ast_to_spe import Sequence
from sppl.distributions import bernoulli
from sppl.distributions import uniform
from sppl.compilers.sppl_to_python import SPPL_Compiler

x    = Id('x')
y  = Id('y')
z      = Id('z')

program = Sequence(
    Sample(x,   uniform(loc=0, scale=1)),
    IfElse(
        x << {0.5}, Sample(y,   uniform(loc=0, scale=1)),
        Otherwise, Sample(y,   uniform(loc=0, scale=1)))
        )
model = program.interpret()

compiler = SPPL_Compiler('''
x   ~= uniform(loc=0, scale=1)
if (x < 0.5):
    y ~= uniform(loc=0, scale=1)
else:
    y ~= uniform(loc=0, scale=1)
                           ''')
namespace = compiler.execute_module()
event = (y < 0.5)
print(namespace.model.prob(event))
