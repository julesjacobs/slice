from sppl.compilers.ast_to_spe import Id
from sppl.compilers.sppl_to_python import SPPL_Compiler

compiler = SPPL_Compiler('''
from sppl.sym_util import binspace

p_similar ~= beta(a=1, b=1)   # Prior probabilty URLs A and B are similar. (in [0,1])

# Five file pairs
similar         = array(5)    # Are url_A[i] and url_B[i] similar? (binary)
p_click_url_A   = array(5)    # Probability user clicks url_A[i]. (in [0,1])
p_click_url_B   = array(5)    # Probability user clicks url_B[i]. (in [0,1])
click_url_A     = array(5)    # Did user click url_A[i]? (binary)
click_url_B     = array(5)    # Did user click url_B[i]? (binary)

for i in range(5):
    switch (p_similar) cases (p in binspace(0, 1, 20)):
        similar[i] ~= bernoulli(p=p.right)

    if similar[i] == 1:
        p_click_url_A[i] ~= uniform(loc=0, scale=1)
        p_click_url_B[i] ~= p_click_url_A[i]
    else:
        p_click_url_A[i] ~= uniform(loc=0, scale=1)
        p_click_url_B[i] ~= uniform(loc=0, scale=1)

    switch (p_click_url_A[i]) cases (p in binspace(0, 1, 10)):
        click_url_A[i] ~= bernoulli(p=p.right)

    switch (p_click_url_B[i]) cases (p in binspace(0, 1, 10)):
        click_url_B[i] ~= bernoulli(p=p.right)
                           ''')

namespace = compiler.execute_module()
e  = Id('click_url_B[0]')
event = (e < 1)
print(namespace.model.prob(event))
