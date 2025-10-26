import argparse
import sys
import time
from sppl.compilers.sppl_to_python import SPPL_Compiler
from sppl.compilers.ast_to_spe import Id

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--time", action="store_true", help="Print only the execution time to stderr")
    args = parser.parse_args()

    start_time = time.time()
    compiler = SPPL_Compiler('''
# Population model.
sex ~= choice({'female': .3307, 'male': .6693})
if (sex == 'female'):
    capital_gain ~= norm(loc=568.4105, scale=24248365.5428)
    if capital_gain < 7298.0000:
        age ~= norm(loc=38.4208, scale=184.9151)
        relationship ~= choice({
            '0': .0491, '1': .1556, '2': .4012,
            '3': .2589, '4': .0294, '5': .1058
        })
    else:
        age ~= norm(loc=38.8125, scale=193.4918)
        relationship ~= choice({
            '0': .0416, '1': .1667, '2': .4583,
            '3': .2292, '4': .0166, '5': .0876
        })
else:
    capital_gain ~= norm(loc=1329.3700, scale=69327473.1006)
    if capital_gain < 5178.0000:
        age ~= norm(loc=38.6361, scale=187.2435)
        relationship ~= choice({
            '0': .0497, '1': .1545, '2': .4021,
            '3': .2590, '4': .0294, '5': .1053
        })
    else:
        age ~= norm(loc=38.2668, scale=187.2747)
        relationship ~= choice({
            '0': .0417, '1': .1624, '2': .3976,
            '3': .2606, '4': .0356, '5': .1021
        })

condition(sex == 'female')
condition(age > 18)

# Decision model.
if relationship == '0':
    if capital_gain < 5095.5:
        t ~= atomic(loc=1)
    else:
        t ~= atomic(loc=0)
elif relationship == '1':
    if capital_gain < 4718.5:
        t ~= atomic(loc=1)
    else:
        t ~= atomic(loc=0)
elif relationship == '2':
    if capital_gain < 5095.5:
        t ~= atomic(loc=1)
    else:
        t ~= atomic(loc=0)
elif relationship == '3':
    if capital_gain < 8296:
        t ~= atomic(loc=1)
    else:
        t ~= atomic(loc=0)
elif relationship == '4':
    t ~= atomic(loc=1)
else:
    if capital_gain < 4668.5:
        t ~= atomic(loc=1)
    else:
        t ~= atomic(loc=0)
                           ''')

    namespace = compiler.execute_module()
    e = Id('t')
    event = (e < 0.5)
    prob = namespace.model.prob(event)
    elapsed = time.time() - start_time

    print(prob)
    if args.time:
        print(f"{elapsed:.3f}", file=sys.stderr)

if __name__ == "__main__":
    main()