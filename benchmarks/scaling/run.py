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
$ python3 run.py --print-outputs: to verify that the outputs between contdice and sppl are the same
'''

def test_asymptotic_scaling_1():
    # List to store benchmark results: (program_size, (cdice_duration, contdice_duration), sppl_duration)  
    benchmark_results = []
    original_dir = os.path.dirname(os.path.dirname(os.getcwd()))
    os.chdir(original_dir)
    
    # Generate programs of increasing size, linearly
    for i in range(1,100,5):
        prog_size = i
        
        # --- CONTDICE ---
        program_contdice, last_var = build_conditional_independent_contdice(i)
        path = Path("scale.cdice").resolve()
        path.write_text(program_contdice)
        
        # Run cdice component
        os.chdir("./cdice")
        command = ["./run_cdice.sh", str(path)]
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL) # warm-up run
        start = time.time()
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        cdice_duration = time.time() - start
        output = result.stdout.decode('utf-8')
        if print_outputs_flag: print(output)
        print(f"contdice -- cdice time {cdice_duration}")
        
        # Run dice component
        os.chdir(os.path.dirname(os.path.dirname(original_dir)))
        os.chdir("./dice")
        command = ["./run_dice.sh", "../output.dice"]
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL) # warm-up run
        start = time.time()
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        dice_duration = time.time() - start
        output = result.stdout.decode('utf-8')
        if print_outputs_flag: print(output)
        contdice_duration = cdice_duration + dice_duration
        print(f"contdice -- contdice time {contdice_duration}")
        os.chdir(original_dir)
        
        # --- SPPL ---
        os.chdir("./cdice")
        command = ["dune", "exec", "--", "bin/main.exe", "--to-sppl", "../scale.cdice"]
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        output = result.stdout.decode('utf-8')
        start = time.time()
        compiler = SPPL_Compiler(f'''{output}''')
        namespace = compiler.execute_module()
        model = Id('model')
        event = (model >= 1.0)
        output = namespace.model.prob(event)
        sppl_duration = time.time() - start
        
        if print_outputs_flag: print(output)
        print(f"sppl time {sppl_duration}")
        os.chdir(original_dir)
        
        benchmark_results.append((prog_size, (cdice_duration, contdice_duration), sppl_duration)) 
    return benchmark_results



def test_asymptotic_scaling_2():
    # List to store benchmark results: (program_size, (cdice_duration, contdice_duration), sppl_duration)  
    benchmark_results = []
    original_dir = os.getcwd()
    
    home_dir = os.path.dirname(os.path.dirname(original_dir))
    # Generate programs of increasing size, linearly
    for i in range(1, 11):
        prog_size = i*5
        
        # --- CONTDICE ---
        program_contdice, last_var = build_alternating_guard_contdice_3(i*5, i)
        path = Path("scale.cdice").resolve()
        path.write_text(program_contdice)
        # Run cdice component
        os.chdir("./cdice")
        command = ["./run_cdice.sh", str(path)]
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL) # warm-up run
        total_cdice_time = 0
        for _ in range(10):
            start = time.time()
            result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
            duration = time.time() - start
            total_cdice_time += duration
        cdice_duration = total_cdice_time / 10
        output = result.stdout.decode('utf-8')
        if print_outputs_flag: print(output)
        print(f"contdice -- cdice time {cdice_duration}")
        # Run dice component
        os.chdir(original_dir)
        os.chdir("./dice")
        command = ["./run_dice.sh", "../output.dice"]
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL) # warm-up run
        total_dice_time = 0
        for _ in range(10):
            start = time.time()
            result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
            duration = time.time() - start
            total_dice_time += duration
        dice_duration = total_dice_time / 10
        output = result.stdout.decode('utf-8')
        if print_outputs_flag: print(output)
        print(f"contdice -- dice time {dice_duration}")
        os.chdir(original_dir)
        
        # --- SPPL ---
        total_sppl_time = 0
        os.chdir("./cdice")
        for _ in range(10):
            # Convert the contdice program to an sppl program using --to-sppl
            command = ["dune", "exec", "--", "bin/main.exe", "--to-sppl", "../scale.cdice"]
            result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
            output = result.stdout.decode('utf-8')
            start = time.time()
            compiler = SPPL_Compiler(f'''{output}''')
            namespace = compiler.execute_module()
            model = Id('model')
            event = (model >= 1.0)
            output = namespace.model.prob(event)
            duration = time.time() - start
            total_sppl_time += duration
            
        sppl_duration = total_sppl_time / 10
        if print_outputs_flag: print(output)
        print(f"sppl time {sppl_duration}")
        os.chdir(original_dir)
        
        benchmark_results.append((prog_size, (cdice_duration, dice_duration), sppl_duration)) 
    return benchmark_results
    


def generate_line_graphs(benchmark_results, output_path):
    '''
    Args:
    benchmark_results: list of tuples (prog_size, contdice_times, sppl_times) or (prog_size, (cdice_times, dice_times), sppl_times)
    output_path: .png file
    '''
    plt.figure(figsize=(10, 6))
    
    prog_size = [b[0] if b[0] is not None else 0 for b in benchmark_results]
    contdice_times = [b[1] if b[1] is not None else 0 for b in benchmark_results]
    sppl_times = [b[2] if b[2] is not None else 0 for b in benchmark_results]
    plt.plot(prog_size, sppl_times, label="SPPL", color="red", marker="o")
    plt.plot(prog_size, contdice_times, label="ContDice", color="blue", marker="o")

    plt.xlabel("Program Size")
    plt.ylabel("Execution Time (seconds)")
    plt.title("Sppl vs Contdice Execution Time Scaling")
    plt.legend()
    plt.grid(True)
    plt.savefig(output_path, dpi=300, bbox_inches="tight")
    plt.show()
    print(f"Saved scaling plot to {output_path}")


def main():
    global print_outputs_flag
    
    if "--print-outputs" in sys.argv:
        print_outputs_flag = True
    
    print("Testing asymptotic scaling...\n")
    benchmark_results = test_asymptotic_scaling_1()
    
    if benchmark_results:
        output_file = "images/scaling.png"
        Path("images").mkdir(exist_ok=True)
        generate_line_graphs(benchmark_results, output_file)

if __name__ == "__main__":
    main()