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

if (sex == 'female'):
    capital_gain ~= norm(loc=568.4105, scale=24248365.5428)
    if capital_gain < 7298.0:
        age ~= norm(loc=38.4208, scale=184.9151)
        education_num ~= norm(loc=10.0827, scale=6.5096)
        relationship ~= choice({
            '0': 0.0491, '1': 0.1556, '2': 0.4012,
            '3': 0.2589, '4': 0.0294, '5': 0.1058
        })
    else:
        age ~= norm(loc=38.8125, scale=193.4918)
        education_num ~= norm(loc=10.1041, scale=6.1522)
        relationship ~= choice({
            '0': 0.0416, '1': 0.1667, '2': 0.4583,
            '3': 0.2292, '4': 0.0166, '5': 0.0876
        })
else:
    capital_gain ~= norm(loc=1329.3700, scale=69327473.1006)
    if capital_gain < 5178.0:
        age ~= norm(loc=38.6361, scale=187.2435)
        education_num ~= norm(loc=10.0817, scale=6.4841)
        relationship ~= choice({
            '0': 0.0497, '1': 0.1545, '2': 0.4021,
            '3': 0.2590, '4': 0.0294, '5': 0.1053
        })
    else:
        age ~= norm(loc=38.2668, scale=187.2747)
        education_num ~= norm(loc=10.0974, scale=7.1793)
        relationship ~= choice({
            '0': 0.0417, '1': 0.1624, '2': 0.3976,
            '3': 0.2606, '4': 0.0356, '5': 0.1021
        })

condition(age > 18)

# Decision model
if relationship == '1':
    t ~= atomic(loc=1)
elif relationship == '2':
    if age < 21.5:
        t ~= atomic(loc=1)
    elif age < 47.5:
        t ~= atomic(loc=1)
    else:
        t ~= atomic(loc=0)
elif relationship == '3':
    t ~= atomic(loc=1)
elif relationship == '4':
    if age < 50.5:
        t ~= atomic(loc=1)
    else:
        t ~= atomic(loc=0)
elif relationship == '5':
    if age < 49.5:
        t ~= atomic(loc=1)
    else:
        t ~= atomic(loc=0)
else:
    t ~= atomic(loc=1)
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