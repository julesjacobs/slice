import argparse
import sys
import time
from sppl.compilers.sppl_to_python import SPPL_Compiler

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--time", action="store_true", help="Print only the execution time to stderr")
    args = parser.parse_args()

    start_time = time.time()
    compiler = SPPL_Compiler('''
perfB1 ~= binom(n=100, p=.9); condition (80 <= perfB1)
skillA ~= poisson(mu=100)
condition ((77 <= skillA) <  125)
switch (skillA) cases (s in range(77, 125)):
    perfA1 ~= binom(n=s, p=.9)
    switch (perfB1) cases (b in range(80, 101)):
        if (perfA1 > b):
            result ~= atomic(loc=1)
        else:
            result ~= atomic(loc=0)
''')
    
    namespace = compiler.execute_module()
    modelc = namespace.model.condition(namespace.result << {0})
    prob = modelc.prob(namespace.perfA1 << {1})
    elapsed = time.time() - start_time

    print(prob)
    if args.time:
        print(f"{elapsed:.3f}", file=sys.stderr)

if __name__ == "__main__":
    main()


