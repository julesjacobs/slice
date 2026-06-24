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
from scipy.stats import poisson as p

# Students.
num_students       = 10
student_perfect    = array(num_students)
student_gpa        = array(num_students)
student_interviews = array(num_students)
student_offers     = array(num_students)

# For each student, sample GPA.
for s in range(num_students):
    student_perfect[s] ~= bernoulli(p=.2)
    if (student_perfect[s] == 0):
        student_gpa[s] ~= beta(a=7, b=3, scale=4)
    else:
        student_gpa[s] ~= atomic(loc=4)
        
# Sample number of recruiters
num_recruiters ~= poisson(mu=25)
condition ((10 <= num_recruiters) <= 20)

# Switch on number of recruiters.
switch (num_recruiters) cases (n in range(10, 21)):

    # For each student, sample interviews and offers.
    for j in range(num_students):
        student_interviews[j] ~= \
            binom(n=n, p=0.9) if (student_gpa[j] == 4)  else \
            binom(n=n, p=0.6) if ((3.5 < student_gpa[j]) < 4) else \
            binom(n=n, p=0.5)

        # Switch on number of interviews.
        switch (student_interviews[j]) cases (k in range(0, n+1)):
            student_offers[j] ~= binom(n=k, p=0.4)
''')
    
    namespace = compiler.execute_module()
    model = namespace.model.condition((namespace.student_offers[0] << {1}) & (namespace.num_recruiters > 10))
    e = Id('student_gpa[0]')
    event = (e < 0.5)
    prob = model.prob(event)
    elapsed = time.time() - start_time

    print(prob)
    if args.time:
        print(f"{elapsed:.3f}", file=sys.stderr)

if __name__ == "__main__":
    main()
