from sppl.distributions import discrete
from sppl.compilers.ast_to_spe import Id
from sppl.compilers.ast_to_spe import IfElse
from sppl.compilers.ast_to_spe import Sample
from sppl.compilers.ast_to_spe import Sequence
from sppl.compilers.sppl_to_python import SPPL_Compiler
from sppl.distributions import atomic
from sppl.distributions import choice
from sppl.distributions import uniform
from sppl.math_util import allclose
from sppl.sets import Interval
from sppl.spe import ExposedSumSPE


from pathlib import Path
import time
import os
import subprocess

'''
let A = uniform(loc=0, scale=10) in
let S = uniform(loc=0, scale=10) in
let E = if ((A == int(2, 0))) then (if ((S == int(1, 0))) then (uniform(loc=0, scale=10)) else (uniform(loc=0, scale=10))) else (if ((A == int(2, 1))) then (if ((S == int(1, 0))) then (uniform(loc=0, scale=10)) else (uniform(loc=0, scale=10))) else (if ((S == int(1, 0))) then (uniform(loc=0, scale=10)) else (uniform(loc=0, scale=10)))) in
let R = if ((E == int(1, 0))) then (uniform(loc=0, scale=10)) else (uniform(loc=0, scale=10)) in
let O = if ((E == int(1, 0))) then (uniform(loc=0, scale=10)) else (uniform(loc=0, scale=10)) in
let T = if ((O == int(1, 0))) then (if ((R == int(1, 0))) then (uniform(loc=0, scale=10)) else (uniform(loc=0, scale=10))) else (if ((R == int(1, 0))) then (uniform(loc=0, scale=10)) else (uniform(loc=0, scale=10))) in
(T,(O,(R,(E,(S,A)))))
'''

path = Path("test.cdice").resolve()
dice_dir = Path(__file__).resolve().parent.parent.parent / "dice"
os.chdir(dice_dir)

command = ["./run_dice.sh", str(path)]
start = time.time()
result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
output = result.stdout.decode('utf-8')
duration = time.time() - start
print(f"contdice time {duration}")

start = time.time()
compiler = SPPL_Compiler('''
# Root variables
Pollution = discrete({0: 0.5, 1: 0.4, 2: 0.1})  
Smoker = discrete({0: 0.3, 1: 0.7})             

# Cancer conditional on Pollution and Smoker
if Pollution == 0:  # High pollution
    if Smoker == 0:  # Non-smoker
        Cancer = discrete({0: 0.05, 1: 0.95})    
    else:  # Smoker
        Cancer = discrete({0: 0.02, 1: 0.98})    
elif Pollution == 1:  # Medium pollution
    if Smoker == 0:  # Non-smoker
        Cancer = discrete({0: 0.03, 1: 0.97})    
    else:  # Smoker
        Cancer = discrete({0: 0.001, 1: 0.999})  
else:  # Pollution == 2 (Low pollution)
    if Smoker == 0:  # Non-smoker
        Cancer = discrete({0: 0.03, 1: 0.97})    
    else:  # Smoker
        Cancer = discrete({0: 0.001, 1: 0.999})  

if Cancer == 0:  # No cancer
    Dyspnoea = discrete({0: 0.3, 1: 0.7})       
else:  # Cancer
    Dyspnoea = discrete({0: 0.65, 1: 0.35})     

if Cancer == 0:  # No cancer
    Xray = discrete({0: 0.2, 1: 0.8})           
else:  # Cancer
    Xray = discrete({0: 0.9, 1: 0.1})           
                           ''')

namespace = compiler.execute_module()
Xray  = Id('Xray')
event = (Xray < 0.5)
print(namespace.model.prob(event))
duration = time.time() - start

print(f"sppl time {duration}")

