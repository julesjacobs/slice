from sppl.compilers.sppl_to_python import SPPL_Compiler
from sppl.compilers.ast_to_spe import Id

compiler = SPPL_Compiler('''
# Population model
age ~= norm(loc=38.5816, scale=186.0614)
relationship ~= choice({
    '0': 0.0481,
    '1': 0.1557,
    '2': 0.4051,
    '3': 0.2551,
    '4': 0.0301,
    '5': 0.1059
})
sex ~= choice({'female': 0.3307, 'male': 0.6693})

condition(sex == 'female')
condition(age > 18)

# Decision model
if relationship == '0':
    t ~= atomic(loc=1)
elif relationship == '1':
    if age < 21.5:
        t ~= atomic(loc=1)
    elif age < 47.5:
        t ~= atomic(loc=1)
    else:
        t ~= atomic(loc=0)
elif relationship == '2':
    t ~= atomic(loc=1)
elif relationship == '3':
    if age < 50.5:
        t ~= atomic(loc=1)
    else:
        t ~= atomic(loc=0)
elif relationship == '4':
    if age < 49.5:
        t ~= atomic(loc=1)
    else:
        t ~= atomic(loc=0)
else:
    t ~= atomic(loc=1)
''')

n = compiler.execute_module()
t = Id('t')
event = (t < 0.5)
print(n.model.prob(event))
