import argparse
import sys
import time
from sppl.compilers.ast_to_spe import Id
from sppl.compilers.sppl_to_python import SPPL_Compiler

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--time", action="store_true", help="Print timing info to stderr")
    args = parser.parse_args()

    start_time = time.time()
    compiler = SPPL_Compiler(f'''
n = 100
p_transition = [.2, .8]
mu_x = [[5, 7], [5, 15]]
mu_y = [[5, 8], [3, 8]]

X = array(n)
Y = array(n)
Z = array(n)

seperated ~= bernoulli(p=.4)
switch (seperated) cases (s in [0,1]):

    # Sample initial point.
    Z[0] ~= bernoulli(p=.5)
    switch (Z[0]) cases (z in [0, 1]):
        X[0] ~= norm(loc=mu_x[s][z])
        Y[0] ~= poisson(mu=mu_y[s][z])

    for i in range(1, n):
        switch (Z[i-1]) cases (z in [0, 1]):
            Z[i] ~= bernoulli(p=p_transition[z])
            switch (Z[i]) cases (z in [0, 1]):
                X[i] ~= norm(loc=mu_x[s][z])
                Y[i] ~= poisson(mu=mu_y[s][z])

''')

    namespace = compiler.execute_module()
    e = Id(f'Z[0]')
    event = (e < 0.5)
    prob = namespace.model.prob(event)
    elapsed = time.time() - start_time

    print(prob)
    if args.time:
        print(f"{elapsed:.3f}", file=sys.stderr)

if __name__ == "__main__":
    main()
