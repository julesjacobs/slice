from sppl.compilers.ast_to_spe import Id
from sppl.compilers.sppl_to_python import SPPL_Compiler

# Original n,k = 20

compiler = SPPL_Compiler('''
n = 4
k = 4
isEffective ~= bernoulli(p=.5)
probControl ~= randint(low=0, high=k)
probTreated ~= randint(low=0, high=k)
probAll ~= randint(low=0, high=k)

controlGroup = array(n)
treatedGroup = array(n)

if (isEffective == 1):
    for i in range(n):
        switch (probControl) cases (p in range(k)):
            controlGroup[i] ~= bernoulli(p=p/k)
        switch (probTreated) cases (p in range(k)):
            treatedGroup[i] ~= bernoulli(p=p/k)
else:
    for i in range(n):
        switch (probAll) cases (p in range(k)):
            controlGroup[i] ~= bernoulli(p=p/k)
            treatedGroup[i] ~= bernoulli(p=p/k)
                           ''')

namespace = compiler.execute_module()
e  = Id('controlGroup[0]')
event = (e < 1)
print(namespace.model.prob(event))
