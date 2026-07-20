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

numPopulations = 2
n = 5
population = [1000, 2000]
gender = [0, 1]
personGender = [0, 1, 0, 1, 0]
dataAnswer = [1, 0, 1, 1, 0]

bias = array(numPopulations)
votes = array(numPopulations)
ansBias = array(n)
answer = array(n)

for i in range(numPopulations):
    bias[i] ~= beta(a=1, b=1)

for i in range(numPopulations):
    switch (bias[i]) cases (p in binspace(0, 1, 21)):
        votes[i] ~= binom(n=population[i], p=p.right)

for i in range(n):
    ansBias[i] = bias[personGender[i]]

for i in range(n):
    switch (ansBias[i]) cases (p in binspace(0, 1, 21)):
        answer[i] ~= bernoulli(p=p.right)

for i in range(n):
    condition(answer[i] == dataAnswer[i])
''')
    
    namespace = compiler.execute_module()
    e = Id('bias[0]')
    event = (e < 0.5)
    prob = namespace.model.prob(event)
    elapsed = time.time() - start_time

    print(prob)
    if args.time:
        print(f"{elapsed:.3f}", file=sys.stderr)

if __name__ == "__main__":
    main()
