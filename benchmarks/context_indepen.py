from sppl.compilers.ast_to_spe import Id
from sppl.compilers.ast_to_spe import IfElse
from sppl.compilers.ast_to_spe import Otherwise
from sppl.compilers.ast_to_spe import Sample
from sppl.compilers.ast_to_spe import Sequence
from sppl.distributions import bernoulli
from sppl.distributions import uniform
from sppl.compilers.sppl_to_python import SPPL_Compiler

a    = Id('a')
b  = Id('b')
c      = Id('c')
d    = Id('d')
e  = Id('e')
f      = Id('f')
g    = Id('g')
h  = Id('h')
i      = Id('i')
j    = Id('j')
k  = Id('k')
l      = Id('l')
m      = Id('m')
n      = Id('n')
o      = Id('o')
p      = Id('p')
q     = Id('q')
r      = Id('r')
s      = Id('s')
t      = Id('t')
u      = Id('u')
v      = Id('v')
w      = Id('w')
x      = Id('x')
y      = Id('y')
z      = Id('z')

compiler = SPPL_Compiler('''
a   ~= uniform(loc=0, scale=1)
if (a < 0.5):
    b ~= uniform(loc=0, scale=2)
else:
    b ~= uniform(loc=0, scale=3)
if (b < 0.5):
    c ~= uniform(loc=0, scale=4)
else:
    c ~= uniform(loc=0, scale=5)
if (c < 0.5):
    d ~= uniform(loc=0, scale=6)
else:
    d ~= uniform(loc=0, scale=7)
if (d < 0.5):
    e ~= uniform(loc=0, scale=8)
else:
    e ~= uniform(loc=0, scale=9)
if (e < 0.5):
    f ~= uniform(loc=0, scale=10)
else:
    f ~= uniform(loc=0, scale=11)
if (f < 0.5):
    g ~= uniform(loc=0, scale=12)
else:
    g ~= uniform(loc=0, scale=13)
if (g < 0.5):
    h ~= uniform(loc=0, scale=14)
else:
    h ~= uniform(loc=0, scale=15)
if (h < 0.5):
    i ~= uniform(loc=0, scale=16)
else:
    i ~= uniform(loc=0, scale=17)
if (i < 0.5):
    j ~= uniform(loc=0, scale=18)
else:
    j ~= uniform(loc=0, scale=19)
if (j < 0.5):
    k ~= uniform(loc=0, scale=20)
else:
    k ~= uniform(loc=0, scale=21)
if (k < 0.5):
    l ~= uniform(loc=0, scale=22)
else:
    l ~= uniform(loc=0, scale=23)
if (l < 0.5):
    m ~= uniform(loc=0, scale=24)
else:
    m ~= uniform(loc=0, scale=25)
if (m < 0.5):
    n ~= uniform(loc=0, scale=26)
else:
    n ~= uniform(loc=0, scale=27)
if (n < 0.5):
    o ~= uniform(loc=0, scale=28)
else:
    o ~= uniform(loc=0, scale=29)
if (o < 0.5):
    p ~= uniform(loc=0, scale=30)
else:
    p ~= uniform(loc=0, scale=31)
if (p < 0.5):
    q ~= uniform(loc=0, scale=32)
else:
    q ~= uniform(loc=0, scale=33)
if (q < 0.5):
    r ~= uniform(loc=0, scale=34)
else:
    r ~= uniform(loc=0, scale=35)
if (r < 0.5):
    s ~= uniform(loc=0, scale=36)
else:
    s ~= uniform(loc=0, scale=37)
if (s < 0.5):
    t ~= uniform(loc=0, scale=38)
else:
    t ~= uniform(loc=0, scale=39)
if (t < 0.5):
    u ~= uniform(loc=0, scale=40)
else:
    u ~= uniform(loc=0, scale=41)
if (u < 0.5):
    v ~= uniform(loc=0, scale=42)
else:
    v ~= uniform(loc=0, scale=43)
if (v < 0.5):
    w ~= uniform(loc=0, scale=44)
else:
    w ~= uniform(loc=0, scale=45)
if (w < 0.5):
    x ~= uniform(loc=0, scale=46)
else:
    x ~= uniform(loc=0, scale=47)
if (x < 0.5):
    y ~= uniform(loc=0, scale=48)
else:
    y ~= uniform(loc=0, scale=49)
if (y < 0.5):
    z ~= uniform(loc=0, scale=50)
else:
    z ~= uniform(loc=0, scale=51)
                           ''')
namespace = compiler.execute_module()
event = (n.z << {0.5})
print(namespace.model.prob(event))
