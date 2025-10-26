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

bias_0 = beta(a=1, b=1)
switch (bias_0) cases (p in binspace(0, 1, 10)):
    votes_0 ~= binom(p=p.right, n=1000)

ansBias_0 = bias_0 
switch (ansBias_0) cases (p in binspace(0, 1, 10)):
    answer_0 ~= bernoulli(p=p.right)    

condition(answer_0 == 1)   
''')
    
    namespace = compiler.execute_module()
    e = Id('answer_0')
    event = (e < 0.5)
    prob = namespace.model.prob(event)
    elapsed = time.time() - start_time

    print(prob)
    if args.time:
        print(f"{elapsed:.3f}", file=sys.stderr)

if __name__ == "__main__":
    main()
