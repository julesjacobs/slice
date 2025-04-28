from sppl.compilers.ast_to_spe import Id
from sppl.compilers.ast_to_spe import IfElse
from sppl.compilers.ast_to_spe import Otherwise
from sppl.compilers.ast_to_spe import Sample
from sppl.compilers.ast_to_spe import Sequence
from sppl.distributions import bernoulli
from sppl.distributions import uniform
from sppl.compilers.sppl_to_python import SPPL_Compiler

import argparse
import sys

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
if (z < 0.5):
 a1 ~= uniform(loc=0, scale=52)
else:
 a1 ~= uniform(loc=0, scale=53)
if (a1 < 0.5):
 b1 ~= uniform(loc=0, scale=54)
else:
 b1 ~= uniform(loc=0, scale=55)
if (b1 < 0.5):
 c1 ~= uniform(loc=0, scale=56)
else:
 c1 ~= uniform(loc=0, scale=57)
if (c1 < 0.5):
 d1 ~= uniform(loc=0, scale=58)
else:
 d1 ~= uniform(loc=0, scale=59)
if (d1 < 0.5):
 e1 ~= uniform(loc=0, scale=60)
else:
 e1 ~= uniform(loc=0, scale=61)
if (e1 < 0.5):
 f1 ~= uniform(loc=0, scale=62)
else:
 f1 ~= uniform(loc=0, scale=63)
if (f1 < 0.5):
 g1 ~= uniform(loc=0, scale=64)
else:
 g1 ~= uniform(loc=0, scale=65)
if (g1 < 0.5):
 h1 ~= uniform(loc=0, scale=66)
else:
 h1 ~= uniform(loc=0, scale=67)
if (h1 < 0.5):
 i1 ~= uniform(loc=0, scale=68)
else:
 i1 ~= uniform(loc=0, scale=69)
if (i1 < 0.5):
 j1 ~= uniform(loc=0, scale=70)
else:
 j1 ~= uniform(loc=0, scale=71)
if (j1 < 0.5):
 k1 ~= uniform(loc=0, scale=72)
else:
 k1 ~= uniform(loc=0, scale=73)
if (k1 < 0.5):
 l1 ~= uniform(loc=0, scale=74)
else:
 l1 ~= uniform(loc=0, scale=75)
if (l1 < 0.5):
 m1 ~= uniform(loc=0, scale=76)
else:
 m1 ~= uniform(loc=0, scale=77)
if (m1 < 0.5):
 n1 ~= uniform(loc=0, scale=78)
else:
 n1 ~= uniform(loc=0, scale=79)
if (n1 < 0.5):
 o1 ~= uniform(loc=0, scale=80)
else:
 o1 ~= uniform(loc=0, scale=81)
if (o1 < 0.5):
 p1 ~= uniform(loc=0, scale=82)
else:
 p1 ~= uniform(loc=0, scale=83)
if (p1 < 0.5):
 q1 ~= uniform(loc=0, scale=84)
else:
 q1 ~= uniform(loc=0, scale=85)
if (q1 < 0.5):
 r1 ~= uniform(loc=0, scale=86)
else:
 r1 ~= uniform(loc=0, scale=87)
if (r1 < 0.5):
 s1 ~= uniform(loc=0, scale=88)
else:
 s1 ~= uniform(loc=0, scale=89)
if (s1 < 0.5):
 t1 ~= uniform(loc=0, scale=90)
else:
 t1 ~= uniform(loc=0, scale=91)
if (t1 < 0.5):
 u1 ~= uniform(loc=0, scale=92)
else:
 u1 ~= uniform(loc=0, scale=93)
if (u1 < 0.5):
 v1 ~= uniform(loc=0, scale=94)
else:
 v1 ~= uniform(loc=0, scale=95)
if (v1 < 0.5):
 w1 ~= uniform(loc=0, scale=96)
else:
 w1 ~= uniform(loc=0, scale=97)
if (w1 < 0.5):
 x1 ~= uniform(loc=0, scale=98)
else:
 x1 ~= uniform(loc=0, scale=99)
if (x1 < 0.5):
 y1 ~= uniform(loc=0, scale=100)
else:
 y1 ~= uniform(loc=0, scale=101)
if (y1 < 0.5):
 z1 ~= uniform(loc=0, scale=102)
else:
 z1 ~= uniform(loc=0, scale=103)
                        ''')
namespace = compiler.execute_module()
z1      = Id('z1')
event = (z1 < 0.5)
print(namespace.model.prob(event))
    
    
# Could be useful down the line?
def generate_sppl_program(n):
    """
    Generate SPPL program with n variables and conditional distributions.
    Args:
        n: Number of variables to generate
    Returns:
        Generated SPPL program as a string
    """
    var_names = []
    for i in range(min(26, n)):
        var_names.append(chr(97 + i)) 
    
    # For n > 26, use v0, v1, v2, etc. for the remaining variables
    for i in range(26, n):
        var_names.append(f"v{i-26}")
    
    id_declarations = []
    for var in var_names:
        id_declarations.append(f"{var} = Id('{var}')")
    
    sppl_program = []
    sppl_program.append(f"{var_names[0]} ~= uniform(loc=0, scale=1)")
    
    for i in range(1, n):
        prev_var = var_names[i-1]
        curr_var = var_names[i]
        scale1 = 2 * i
        scale2 = 2 * i + 1
        
        sppl_program.append(f"if ({prev_var} < 0.5):")
        sppl_program.append(f" {curr_var} ~= uniform(loc=0, scale={scale1})")
        sppl_program.append(f"else:")
        sppl_program.append(f" {curr_var} ~= uniform(loc=0, scale={scale2})")
    
    return sppl_program, var_names[-1]


