#!/usr/bin/env python3
import subprocess
import time
import sys
import os
from pathlib import Path
import matplotlib.pyplot as plt
from gen import *  # custom library for generating programs

from sppl.compilers.ast_to_spe import Id
from sppl.compilers.sppl_to_python import SPPL_Compiler

'''
$ python3 run.py: tests asymptotic scaling for a variety of programs
$ python3 run.py --print-outputs: to verify that the outputs between slice and sppl are the same
'''

'''
These are for functions in the gen.py library that have one argument.
'''
def test_asymptotic_scaling_1(test_func):
    # List to store benchmark results: (program_size, (slice_duration, total_duration), sppl_duration)  
    benchmark_results = []
    
    # Generate programs of increasing size, linearly
    # Currently testing: programs of size 1-200 incremented by 5
    for i in range(1,50,5):
        prog_size = i
        
        # --- SLICE ---
        program_slice, last_var = test_func(i)
        path = (original_dir / "scale.slice").resolve()
        path.write_text(program_slice)
        
        # Run slice component
        os.chdir(str(original_dir / "slice"))
        command = ["./run_slice.sh", str(path)]
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL) # warm-up run
        start = time.time()
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        slice_duration = time.time() - start
        output = result.stdout.decode('utf-8')
        
        # Run dice component
        os.chdir(str(original_dir / "dice"))
        command = ["./run_dice.sh", "../output.dice"]
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL) # warm-up run
        start = time.time()
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        dice_duration = time.time() - start
        slice_output = result.stdout.decode('utf-8')
        total_duration = slice_duration + dice_duration
        
        # --- SPPL ---
        os.chdir(str(original_dir / "slice"))
        command = ["bash", "-c", "eval $(opam env --switch=4.14.1) && dune exec -- bin/main.exe --to-sppl ../scale.slice"]
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        output = result.stdout.decode('utf-8')
        start = time.time()
        compiler = SPPL_Compiler(f'''{output}''')
        namespace = compiler.execute_module()
        model = Id('model')
        event = (model >= 1.0)
        sppl_output = namespace.model.prob(event)
        sppl_duration = time.time() - start
        os.chdir(str(original_dir))
        
        benchmark_results.append((prog_size, (slice_duration, total_duration), sppl_duration))
        
        # Print results
        print(f"---- PROGRAM SIZE: {prog_size} ----")
        if print_outputs_flag: print(f"sppl -- sppl output: {sppl_output}\nslice -- slice output: {slice_output}")
        print(f"slice -- slice time: {slice_duration}")
        print(f"slice -- total time: {total_duration}")
        print(f"sppl -- sppl time: {sppl_duration}\n")
         
    return benchmark_results


'''
These are for functions in the gen.py library that have two arguments.
'''
def test_asymptotic_scaling_2(test_func):
    # List to store benchmark results: (program_size, (slice_duration, total_duration), sppl_duration)  
    benchmark_results = []
    
    # Generate programs of increasing size, linearly
    # Currently testing: programs of size 1-50 incremented by 5, with alternating guards incremented by 1 each program
    for i in range(1, 20):
        prog_size = i*5
        
        # --- SLICE ---
        program_slice, last_var = test_func(i*5, i)
        path = (original_dir / "scale.slice").resolve()
        path.write_text(program_slice)
        
        # Run slice component
        os.chdir(str(original_dir / "slice"))
        command = ["./run_slice.sh", str(path)]
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL) # warm-up run
        start = time.time()
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        slice_duration = time.time() - start
        output = result.stdout.decode('utf-8')
        
        # Run dice component
        os.chdir(str(original_dir / "dice"))
        command = ["./run_dice.sh", "../output.dice"]
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL) # warm-up run
        start = time.time()
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        dice_duration = time.time() - start
        slice_output = result.stdout.decode('utf-8')
        total_duration = slice_duration + dice_duration
        
        # --- SPPL ---
        os.chdir(str(original_dir / "slice"))
        command = ["bash", "-c", "eval $(opam env --switch=4.14.1) && dune exec -- bin/main.exe --to-sppl ../scale.slice"]
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        output = result.stdout.decode('utf-8')
        start = time.time()
        compiler = SPPL_Compiler(f'''{output}''')
        namespace = compiler.execute_module()
        model = Id('model')
        event = (model >= 1.0)
        sppl_output = namespace.model.prob(event)
        sppl_duration = time.time() - start
        os.chdir(str(original_dir))
        
        benchmark_results.append((prog_size, (slice_duration, total_duration), sppl_duration))
        
        # Print results
        print(f"---- PROGRAM SIZE: {prog_size} ----")
        if print_outputs_flag: print(f"sppl -- sppl output: {sppl_output}\nslice -- slice output: {slice_output}")
        print(f"slice -- slice time: {slice_duration}")
        print(f"slice -- total time: {total_duration}")
        print(f"sppl -- sppl time: {sppl_duration}\n")
         
    return benchmark_results


def generate_line_graphs(benchmark_results, output_path):
    '''
    Args:
    benchmark_results: list of tuples (prog_size, (slice_times, total_times), sppl_times)
    output_path: .png file
    '''
    plt.figure(figsize=(10, 6))
    
    prog_size = [b[0] if b[0] is not None else 0 for b in benchmark_results]
    slice_times = [b[1][0] if b[1] and b[1][0] is not None else 0 for b in benchmark_results]
    total_times = [b[1][1] if b[1] and b[1][1] is not None else 0 for b in benchmark_results]
    sppl_times = [b[2] if b[2] is not None else 0 for b in benchmark_results]
    plt.plot(prog_size, sppl_times, label="SPPL", color="red")
    plt.plot(prog_size, slice_times, label="Slice", color="darkblue")
    plt.plot(prog_size, total_times, label="Slice+Dice", color="lightblue")

    plt.xlabel("Program Size")
    plt.ylabel("Execution Time (seconds)")
    plt.title("SPPL vs Slice Execution Time Scaling")
    plt.legend()
    plt.grid(True)
    plt.savefig(output_path, dpi=300, bbox_inches="tight")
    plt.show()
    print(f"Saved scaling plot to {output_path}")


print_outputs_flag = False
# Get the project root directory more reliably
script_dir = Path(__file__).resolve().parent
original_dir = script_dir.parent.parent  # Go up two levels from benchmarks/scaling to project root

def main():
    global print_outputs_flag
    
    if "--print-outputs" in sys.argv:
        print_outputs_flag = True
        
    tests = [build_conditional_independent_contdice, build_conditional_random_independent_contdice_1, build_conditional_random_independent_contdice_2, build_alternating_guard_contdice_1, build_alternating_guard_contdice_2, build_alternating_guard_contdice_3, build_random_alternating_guard_contdice]
    
    test_func = build_random_alternating_guard_contdice
    print(f"Testing asymptotic scaling for {test_func.__name__}...\n")
    benchmark_results = test_asymptotic_scaling_2(test_func) # adjust accordingly for one or two args
    if benchmark_results:
        output_file = original_dir / "images" / "scaling" / f"{test_func.__name__}.png"
        output_file.parent.mkdir(parents=True, exist_ok=True)
        generate_line_graphs(benchmark_results, str(output_file))
    

if __name__ == "__main__":
    main()