import argparse
import sys
import time
from sppl.compilers.ast_to_spe import Id
from sppl.compilers.sppl_to_python import SPPL_Compiler

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--time", action="store_true", help="Print timing info to stderr")
    args = parser.parse_args()

    # ---------- Translation Section ----------
    start_translate = time.time()
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

condition(age > 18)

# Decision model
if capital_gain >= 7073.5:
    if age < 20:
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
