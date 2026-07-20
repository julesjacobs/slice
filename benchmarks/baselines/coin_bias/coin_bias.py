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
                        
observedResults = [1,1,0,1,0]
bias ~= beta(a=2, b=5)
tossResults = array(5)
for i in range(5):
    switch (bias) cases (p in binspace(0, 1, 21)):
        tossResults[i] ~= bernoulli(p=p.right)

for i in range(5):
    condition(tossResults[i] == observedResults[i])
''')
    
    namespace = compiler.execute_module()
    prob = namespace.model.prob(namespace.bias < 0.3)
    elapsed = time.time() - start_time

    print(prob)
    if args.time:
        print(f"{elapsed:.3f}", file=sys.stderr)

if __name__ == "__main__":
    main()
