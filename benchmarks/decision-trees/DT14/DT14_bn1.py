from sppl.compilers.sppl_to_python import SPPL_Compiler
from sppl.compilers.ast_to_spe import Id

compiler = SPPL_Compiler('''
# Population model
sex ~= choice({'female': 0.3307, 'male': 0.6693})

if (sex == 'female'):
    capital_gain ~= norm(loc=568.4105, scale=24248365.5428)
    if capital_gain < 7298.0:
        age ~= norm(loc=38.4208, scale=184.9151)
        relationship ~= choice({
            '0': 0.0491,
            '1': 0.1556,
            '2': 0.4012,
            '3': 0.2589,
            '4': 0.0294,
            '5': 0.1058
        })
    else:
        age ~= norm(loc=38.8125, scale=193.4918)
        relationship ~= choice({
            '0': 0.0416,
            '1': 0.1667,
            '2': 0.4583,
            '3': 0.2292,
            '4': 0.0166,
            '5': 0.0876
        })
else:
    capital_gain ~= norm(loc=1329.3700, scale=69327473.1006)
    if capital_gain < 5178.0:
        age ~= norm(loc=38.6361, scale=187.2435)
        relationship ~= choice({
            '0': 0.0497,
            '1': 0.1545,
            '2': 0.4021,
            '3': 0.2590,
            '4': 0.0294,
            '5': 0.1053
        })
    else:
        age ~= norm(loc=38.2668, scale=187.2747)
        relationship ~= choice({
            '0': 0.0417,
            '1': 0.1624,
            '2': 0.3976,
            '3': 0.2606,
            '4': 0.0356,
            '5': 0.1021
        })

condition(sex == 'female')
condition(age > 18)

# Decision model
if relationship == '0':
    t ~= atomic(loc=1)
elif relationship == '1':
    if age < 21.5:
        t ~= atomic(loc=1)
    else:
        if age < 47.5:
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
