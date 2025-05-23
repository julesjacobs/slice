from sppl.compilers.sppl_to_python import SPPL_Compiler
from sppl.compilers.ast_to_spe import Id

compiler = SPPL_Compiler('''
# Population model
age ~= norm(loc=38.5816, scale=186.0614)

education ~= choice({
    '0': 0.1644, '1': 0.2239, '2': 0.0361, '3': 0.3225,
    '4': 0.0177, '5': 0.0328, '6': 0.0424, '7': 0.0158,
    '8': 0.0198, '9': 0.0133, '10': 0.0530, '11': 0.0051,
    '12': 0.0287, '13': 0.0127, '14': 0.0102, '15': 0.0016
})

relationship ~= choice({
    '0': 0.0481, '1': 0.1557, '2': 0.4051,
    '3': 0.2551, '4': 0.0301, '5': 0.1059
})

sex ~= choice({'female': 0.3307, 'male': 0.6693})

capital_gain ~= norm(loc=1077.6488, scale=54542539.1784)

condition(sex == 'female')
condition(age > 18)

# Decision model
if relationship == '0':
    if education == '0': t ~= atomic(loc=0)
    elif education == '1': t ~= atomic(loc=1)
    elif education == '2': t ~= atomic(loc=1)
    elif education == '3': t ~= atomic(loc=1)
    elif education == '4': t ~= atomic(loc=0)
    elif education == '5': t ~= atomic(loc=0)
    elif education == '6': t ~= atomic(loc=0)
    elif education == '7': t ~= atomic(loc=1)
    elif education == '8': t ~= atomic(loc=1)
    elif education == '9': t ~= atomic(loc=1)
    elif education == '10': t ~= atomic(loc=0)
    elif education == '11': t ~= atomic(loc=1)
    elif education == '12': t ~= atomic(loc=1)
    elif education == '13': t ~= atomic(loc=0)
    elif education == '14': t ~= atomic(loc=1)
    else: t ~= atomic(loc=1)
elif relationship == '1':
    if capital_gain < 4718.5: t ~= atomic(loc=1)
    else: t ~= atomic(loc=0)
elif relationship == '2':
    if education == '0': t ~= atomic(loc=0)
    elif education == '1': t ~= atomic(loc=1)
    elif education == '2': t ~= atomic(loc=1)
    elif education == '3': t ~= atomic(loc=1)
    elif education == '4': t ~= atomic(loc=0)
    elif education == '5': t ~= atomic(loc=1)
    elif education == '6': t ~= atomic(loc=1)
    elif education == '7': t ~= atomic(loc=1)
    elif education == '8': t ~= atomic(loc=1)
    elif education == '9': t ~= atomic(loc=1)
    elif education == '10': t ~= atomic(loc=0)
    elif education == '11': t ~= atomic(loc=1)
    elif education == '12': t ~= atomic(loc=1)
    elif education == '13': t ~= atomic(loc=0)
    elif education == '14': t ~= atomic(loc=1)
    else: t ~= atomic(loc=1)
elif relationship == '3':
    if capital_gain < 8296: t ~= atomic(loc=1)
    else: t ~= atomic(loc=0)
elif relationship == '4':
    t ~= atomic(loc=1)
else:
    if capital_gain < 4668.5: t ~= atomic(loc=1)
    else: t ~= atomic(loc=0)
''')

n = compiler.execute_module()
t = Id('t')
event = (t < 0.5)
print(n.model.prob(event))