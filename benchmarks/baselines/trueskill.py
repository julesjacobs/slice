import argparse
import sys
import time
from sppl.compilers.ast_to_spe import Id
from sppl.compilers.sppl_to_python import SPPL_Compiler

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--time", action="store_true", help="Print only the execution time to stderr")
    args = parser.parse_args()

    start_time = time.time()
    compiler = SPPL_Compiler('''
perfB1 ~= binom(n=100, p=.9); condition (98 <= perfB1)
skillA ~= poisson(mu=100)
condition ((77 <= skillA) <  80)
switch (skillA) cases (s in range(77, 80)):
    perfA1 ~= binom(n=s, p=.9)
    switch (perfB1) cases (b in range(98, 101)):
        if (perfA1 > b):
            result ~= atomic(loc=1)
        else:
            result ~= atomic(loc=0)
''')
    
    namespace = compiler.execute_module()
    e = Id('result')
    event = (e < 0.5)
    prob = namespace.model.prob(event)
    elapsed = time.time() - start_time

    print(prob)
    if args.time:
        print(f"{elapsed:.3f}", file=sys.stderr)

if __name__ == "__main__":
    main()




