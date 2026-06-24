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
from sppl.sym_util import binspace
n = 4000
param ~= randint(low=250, high=350)
switch (param) cases (b in range(250, 350)):
    p ~= beta(a=277, b=b)
switch (p) cases (x in binspace(0, 1, 20)):
    votes ~= binom(n=n, p=(x.left + x.right)/2)
win ~= votes > 0.5*n
        ''')
    
    namespace = compiler.execute_module()
    prob = namespace.model.prob(namespace.win << {1})
    elapsed = time.time() - start_time

    print(prob)
    if args.time:
        print(f"{elapsed:.3f}", file=sys.stderr)

if __name__ == "__main__":
    main()
