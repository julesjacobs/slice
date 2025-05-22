from sppl.compilers.sppl_to_python import SPPL_Compiler

compiler = SPPL_Compiler('''
# Population model
age ~= norm(loc=38.5816, scale=186.0614)
sex ~= choice({'female': 0.3307, 'male': 0.6693})
capital_gain ~= norm(loc=1077.6488, scale=54542539.1784)

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