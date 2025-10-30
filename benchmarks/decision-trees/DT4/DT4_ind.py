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
age ~= norm(loc=38.5816, scale=186.0614)
sex ~= choice({'female': 0.3307, 'male': 0.6693})
capital_gain ~= norm(loc=1077.6488, scale=54542539.1784)

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