import argparse
import sys
import time
from sppl.compilers.ast_to_spe import Id
from sppl.compilers.sppl_to_python import SPPL_Compiler

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--time", action="store_true", help="Print timing info to stderr")
    parser.add_argument("--n", type=int, default=4, help="Number of HMM steps (sequence length)")
    args = parser.parse_args()

    start_time = time.time()
    compiler = SPPL_Compiler(f'''
k = 3
n = 4
theta = [[0.7, 0.2, 0.1], [0.2, 0.5, 0.3], [0.1, 0.1, 0.8]]
mu = [0.5, 1.0, 2.0]
Z = array(n)
Y = array(n)
                             
#for i in range(0,k):
#    mu[i] ~= binom(n=3, p=0.5)
                                         
Z[0] ~= discrete({{
            0: theta[0][0],
            1: theta[0][1],
            2: theta[0][2]
}})

# Generate the hidden state sequence
for i in range(1, n):
    switch (Z[i-1]) cases (prev in [0,1,2]):
        Z[i] ~= discrete({{
            0: theta[prev][0],
            1: theta[prev][1],
            2: theta[prev][2]
        }})

# Generate continuous emissions
for i in range(0, n):
    switch (Z[i]) cases (z in [0, 1, 2]):
        Y[i] ~= norm(loc=mu[z], scale=1)                                     

''')

    namespace = compiler.execute_module()
    e = Id(f'Y[{args.n - 1}]')
    event = (e < 0.5)
    prob = namespace.model.prob(event)
    elapsed = time.time() - start_time

    print(prob)
    if args.time:
        print(f"{elapsed:.3f}", file=sys.stderr)

if __name__ == "__main__":
    main()
