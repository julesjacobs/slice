import argparse
import sys
import time
from sppl.compilers.sppl_to_python import SPPL_Compiler
from sppl.compilers.ast_to_spe import Id

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--time", action="store_true", help="Print only the execution time to stderr")
    args = parser.parse_args()

    # ---------- Translation Section ----------
    start_translate = time.time()
    compiler = SPPL_Compiler('''
# Population model
relationship ~= choice({
    '0': 0.0481,
    '1': 0.1557, 
    '2': 0.4051,
    '3': 0.2551,
    '4': 0.0301,
    '5': 0.1059
})

sex ~= choice({'female': 0.3307, 'male': 0.6693})

capital_gain ~= norm(loc=1077.6488, scale=54542539.1784)

# Decision model
if relationship == '0':
    if capital_gain < 5095.5:
        t_initial ~= atomic(loc=1)
    else:
        t_initial ~= atomic(loc=0)
elif relationship == '1':
    if capital_gain < 4718.5:
        t_initial ~= atomic(loc=1)
    else:
        t_initial ~= atomic(loc=0)
elif relationship == '2':
    if capital_gain < 5095.5:
        t_initial ~= atomic(loc=1)
    else:
        t_initial ~= atomic(loc=0)
elif relationship == '3':
    if capital_gain < 8296.0:
        t_initial ~= atomic(loc=1)
    else:
        t_initial ~= atomic(loc=0)
elif relationship == '4':
    t_initial ~= atomic(loc=1)
else:
    if capital_gain < 4668.5:
        t_initial ~= atomic(loc=1)
    else:
        t_initial ~= atomic(loc=0)

if t_initial > 0.5 and sex == 'female':
    # Equivalent to step([(0,1,0.15),(1,2,0.85)])
    fl ~= choice({'flip_false': 0.15, 'flip_true': 0.85})
    if fl == 'flip_false':
        t ~= atomic(loc=0)
    else:
        t = t_initial
else:
    # Dummy draw to keep symbol sets aligned
    fl ~= choice({'flip_false': 1.0, 'flip_true': 0.0})
    t = t_initial    
                           ''')

    namespace = compiler.execute_module()
    model = namespace.model
    translate_time = time.time() - start_translate

    # ---------- Conditioning Section ----------
    start_condition = time.time()
    model_c0 = model.condition(namespace.sex << {'female'})
    model_c1 = model.condition(namespace.sex << {'male'})
    condition_time = time.time() - start_condition

    # ---------- Querying Section ----------
    start_prob = time.time()
    p_given_minority = model_c0.prob(namespace.t << {1})
    p_given_majority = model_c1.prob(namespace.t << {1})
    fairness = p_given_minority / p_given_majority
    prob_time = time.time() - start_prob

    # ---------- Output ----------
    print(fairness)

    if args.time:
        print(
            f"{translate_time:.3f},"
            f"{condition_time:.3f},"
            f"{prob_time:.3f}",
            file=sys.stderr
        )

if __name__ == "__main__":
    main()