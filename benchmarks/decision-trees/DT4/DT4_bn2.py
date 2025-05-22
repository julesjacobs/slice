from sppl.compilers.sppl_to_python import SPPL_Compiler

compiler = SPPL_Compiler('''
# Population model
sex ~= choice({'female': 0.3307, 'male': 0.6693})

if (sex == 'female'):
    capital_gain ~= norm(loc=568.4105, scale=24248365.5428)
    if capital_gain < 7298.0000:
        age ~= norm(loc=38.4208, scale=184.9151)
        education_num ~= norm(loc=10.0827, scale=6.5096)
    else:
        age ~= norm(loc=38.8125, scale=193.4918)
        education_num ~= norm(loc=10.1041, scale=6.1522)
else:
    capital_gain ~= norm(loc=1329.3700, scale=69327473.1006)
    if capital_gain < 5178.0000:
        age ~= norm(loc=38.6361, scale=187.2435)
        education_num ~= norm(loc=10.0817, scale=6.4841)
    else:
        age ~= norm(loc=38.2668, scale=187.2747)
        education_num ~= norm(loc=10.0974, scale=7.1793)

# Ensure education_num <= age by rejecting cases where education_num > age
# reject(education_num > age)

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

# Calculate probability that t < 0.5 (which corresponds to t == 0)
model_c1 = model.prob(n.t << {0})
print(model_c1)