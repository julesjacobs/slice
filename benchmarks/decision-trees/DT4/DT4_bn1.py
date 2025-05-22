from sppl.compilers.sppl_to_python import SPPL_Compiler

compiler = SPPL_Compiler('''
# Population model
sex ~= choice({'female': 0.3307, 'male': 0.6693})
if (sex == 'female'):
    capital_gain ~= norm(loc=568.4105, scale=24248365.5428)
    if capital_gain < 7298.0000:
        age ~= norm(loc=38.4208, scale=184.9151)
    else:
        age ~= norm(loc=38.8125, scale=193.4918)
else:
    capital_gain ~= norm(loc=1329.3700, scale=69327473.1006)
    if capital_gain < 5178.0000:
        age ~= norm(loc=38.6361, scale=187.2435)
    else:
        age ~= norm(loc=38.2668, scale=187.2747)

# Qualification condition
# condition(age > 18)

# Decision model
if capital_gain >= 7073.5:
    if age < 20:
        t ~= atomic(loc=1)
    else:
        t ~= atomic(loc=0)
else:
    t ~= atomic(loc=1)
''')

n = compiler.execute_module()
model = n.model
model_c1 = model.prob(n.t << {0})
print(model_c1)
