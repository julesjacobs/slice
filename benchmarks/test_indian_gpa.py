# Copyright 2020 MIT Probabilistic Computing Project.
# See LICENSE.txt

'''
Indian GPA example from:

Discrete-Continuous Mixtures in Probabilistic Programming: Generalized
Semantics and Inference Algorithms, Wu et. al., ICML 2018.
https://arxiv.org/pdf/1806.02027.pdf
'''

from sppl.compilers.ast_to_spe import Id
from sppl.compilers.ast_to_spe import IfElse
from sppl.compilers.ast_to_spe import Sample
from sppl.compilers.ast_to_spe import Sequence
from sppl.compilers.sppl_to_python import SPPL_Compiler
from sppl.distributions import atomic
from sppl.distributions import choice
from sppl.distributions import uniform
from sppl.math_util import allclose
from sppl.sets import Interval
from sppl.spe import ExposedSumSPE

Nationality = Id('Nationality')
Perfect     = Id('Perfect')
GPA         = Id('GPA')

def model_ifelse_exhuastive_compiled():
    compiler = SPPL_Compiler('''
Nationality   ~= choice({'India': 0.5, 'USA': 0.5})
Perfect       ~= choice({'True': 0.01, 'False': 0.99})
if (Nationality == 'India') & (Perfect == 'False'):
    GPA ~= uniform(loc=0, scale=10)
elif (Nationality == 'India') & (Perfect == 'True'):
    GPA ~= atomic(loc=10)
elif (Nationality == 'USA') & (Perfect == 'False'):
    GPA ~= uniform(loc=0, scale=4)
elif (Nationality == 'USA') & (Perfect == 'True'):
    GPA ~= atomic(loc=4)
    ''')
    namespace = compiler.execute_module()
    return namespace.model

def model_ifelse_non_exhuastive_compiled():
    compiler = SPPL_Compiler('''
Nationality   ~= choice({'India': 0.5, 'USA': 0.5})
Perfect       ~= choice({'True': 0.01, 'False': 0.99})
if (Nationality == 'India') & (Perfect == 'False'):
    GPA ~= uniform(loc=0, scale=10)
elif (Nationality == 'India') & (Perfect == 'True'):
    GPA ~= atomic(loc=10)
elif (Nationality == 'USA') & (Perfect == 'False'):
    GPA ~= uniform(loc=0, scale=4)
else:
    GPA ~= atomic(loc=4)
    ''')
    namespace = compiler.execute_module()
    return namespace.model

def model_ifelse_nested_compiled():
    compiler = SPPL_Compiler('''
Nationality   ~= choice({'India': 0.5, 'USA': 0.5})
Perfect       ~= choice({'True': 0.01, 'False': 0.99})
if (Nationality == 'India'):
    if (Perfect == 'False'):
        GPA ~= uniform(loc=0, scale=10)
    else:
        GPA ~= atomic(loc=10)
elif (Nationality == 'USA'):
    if (Perfect == 'False'):
        GPA ~= uniform(loc=0, scale=4)
    elif (Perfect == 'True'):
        GPA ~= atomic(loc=4)
    ''')
    namespace = compiler.execute_module()
    return namespace.model

def model_perfect_nested_compiled():
    compiler = SPPL_Compiler('''
Nationality   ~= choice({'India': 0.5, 'USA': 0.5})
if (Nationality == 'India'):
    Perfect       ~= choice({'True': 0.01, 'False': 0.99})
    if (Perfect == 'False'):
        GPA ~= uniform(loc=0, scale=10)
    else:
        GPA ~= atomic(loc=10)
elif (Nationality == 'USA'):
    Perfect       ~= choice({'True': 0.01, 'False': 0.99})
    if (Perfect == 'False'):
        GPA ~= uniform(loc=0, scale=4)
    else:
        GPA ~= atomic(loc=4)
    ''')
    namespace = compiler.execute_module()
    return namespace.model


model = model_ifelse_nested_compiled()
result = model.prob(GPA < 1.0)
print(result)
    
