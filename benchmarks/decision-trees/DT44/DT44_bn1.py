from sppl.compilers.sppl_to_python import SPPL_Compiler
from sppl.compilers.ast_to_spe import Id

compiler = SPPL_Compiler('''
# Population model
sex ~= choice({'female': 0.3307, 'male': 0.6693})

if (sex == 'female'):
    capital_gain ~= norm(loc=568.4105, scale=24248365.5428)
    if capital_gain < 7298.0000:
        age ~= norm(loc=38.4208, scale=184.9151)
        education ~= choice({
            '0': 0.1638, '1': 0.2308, '2': 0.0354, '3': 0.3230,
            '4': 0.0173, '5': 0.0321, '6': 0.0412, '7': 0.0156,
            '8': 0.0200, '9': 0.0112, '10': 0.0528, '11': 0.0050,
            '12': 0.0290, '13': 0.0119, '14': 0.0092, '15': 0.0017
        })
        relationship ~= choice({
            '0': 0.0491, '1': 0.1556, '2': 0.4012,
            '3': 0.2589, '4': 0.0294, '5': 0.1058
        })
    else:
        age ~= norm(loc=38.8125, scale=193.4918)
        education ~= choice({
            '0': 0.1916, '1': 0.2000, '2': 0.0500, '3': 0.3542,
            '4': 0.0208, '5': 0.0125, '6': 0.0375, '7': 0.0125,
            '8': 0.0292, '9': 0.0042, '10': 0.0541, '11': 0.0000,
            '12': 0.0250, '13': 0.0042, '14': 0.0042, '15': 0.0000
        })
        relationship ~= choice({
            '0': 0.0416, '1': 0.1667, '2': 0.4583,
            '3': 0.2292, '4': 0.0166, '5': 0.0876
        })
else:
    capital_gain ~= norm(loc=1329.3700, scale=69327473.1006)
    if capital_gain < 5178.0000:
        age ~= norm(loc=38.6361, scale=187.2435)
        education ~= choice({
            '0': 0.1670, '1': 0.2239, '2': 0.0358, '3': 0.3267,
            '4': 0.0159, '5': 0.0320, '6': 0.0426, '7': 0.0155,
            '8': 0.0198, '9': 0.0121, '10': 0.0518, '11': 0.0047,
            '12': 0.0287, '13': 0.0125, '14': 0.0096, '15': 0.0014
        })
        relationship ~= choice({
            '0': 0.0497, '1': 0.1545, '2': 0.4021,
            '3': 0.2590, '4': 0.0294, '5': 0.1053
        })
    else:
        age ~= norm(loc=38.2668, scale=187.2747)
        education ~= choice({
            '0': 0.1569, '1': 0.2205, '2': 0.0417, '3': 0.3071,
            '4': 0.0255, '5': 0.0302, '6': 0.0409, '7': 0.0155,
            '8': 0.0178, '9': 0.0147, '10': 0.0619, '11': 0.0062,
            '12': 0.0317, '13': 0.0139, '14': 0.0139, '15': 0.0016
        })
        relationship ~= choice({
            '0': 0.0417, '1': 0.1624, '2': 0.3976,
            '3': 0.2606, '4': 0.0356, '5': 0.1021
        })

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