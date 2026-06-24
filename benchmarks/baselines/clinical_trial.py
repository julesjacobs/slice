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
n = 20
k = 20
controlGroup = array(n)
treatedGroup = array(n)
                        
isEffective ~= bernoulli(p=.5)
probControl ~= randint(low=0, high=k)
probTreated ~= randint(low=0, high=k)
probAll ~= randint(low=0, high=k)

if (isEffective == 1):
    for i in range(n):
        switch (probControl) cases (p in range(k)):
            controlGroup[i] ~= bernoulli(p=p/k)
        switch (probTreated) cases (p in range(k)):
            treatedGroup[i] ~= bernoulli(p=p/k)
else:
    for i in range(n):
        switch (probAll) cases (p in range(k)):
            controlGroup[i] ~= bernoulli(p=p/k)
            treatedGroup[i] ~= bernoulli(p=p/k)
        ''')
    
    namespace = compiler.execute_module()
    e = Id('treatedGroup[0]')
    event = (e < 1)
    prob = namespace.model.prob(event)
    elapsed = time.time() - start_time

    print(prob)
    if args.time:
        print(f"{elapsed:.3f}", file=sys.stderr)

if __name__ == "__main__":
    main()
