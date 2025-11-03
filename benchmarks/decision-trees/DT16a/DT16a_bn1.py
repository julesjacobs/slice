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
sex ~= choice({'female': 0.3307, 'male': 0.6693})

if sex == 'female':
    capital_gain ~= norm(loc=568.4105, scale=24248365.5428)
    if capital_gain < 7298.0000:
        relationship ~= choice({
            '0': 0.0491, '1': 0.1556, '2': 0.4012,
            '3': 0.2589, '4': 0.0294, '5': 0.1058
        })
    else:
        relationship ~= choice({
            '0': 0.0416, '1': 0.1667, '2': 0.4583,
            '3': 0.2292, '4': 0.0166, '5': 0.0876
        })
else:
    capital_gain ~= norm(loc=1329.3700, scale=69327473.1006)
    if capital_gain < 5178.0000:
        relationship ~= choice({
            '0': 0.0497, '1': 0.1545, '2': 0.4021,
            '3': 0.2590, '4': 0.0294, '5': 0.1053
        })
    else:
        relationship ~= choice({
            '0': 0.0417, '1': 0.1624, '2': 0.3976,
            '3': 0.2606, '4': 0.0356, '5': 0.1021
        })

# Decision model
if relationship == '1':
    if capital_gain < 5095.5:
        t_initial ~= atomic(loc=1)
    else:
        t_initial ~= atomic(loc=0)
elif relationship == '2':
    if capital_gain < 4718.5:
        t_initial ~= atomic(loc=1)
    else:
        t_initial ~= atomic(loc=0)
elif relationship == '3':
    if capital_gain < 5095.5:
        t_initial ~= atomic(loc=1)
    else:
        t_initial ~= atomic(loc=0)
elif relationship == '4':
    if capital_gain < 8296.0:
        t_initial ~= atomic(loc=1)
    else:
        t_initial ~= atomic(loc=0)
elif relationship == '5':
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