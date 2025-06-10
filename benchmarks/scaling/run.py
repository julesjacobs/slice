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
Usage:
If no backend is specified, by default benchmarks both dice and roulette

$ python3 run.py [--dice|--roulette]: tests asymptotic scaling for a variety of programs
$ python3 run.py --print-outputs [--dice|--roulette]: to verify that the outputs between slice and sppl are the same
$ python3 run.py --timeout N [--dice|--roulette]: set timeout to N seconds per benchmark (default: 300)
'''

def run_with_timeout(command, timeout, stderr=subprocess.DEVNULL):
    """Run a command with timeout handling"""
    try:
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, 
                              stderr=stderr, timeout=timeout)
        return result
    except subprocess.TimeoutExpired:
        return None

'''
These are for functions in the gen.py library that have one argument.
'''
def test_asymptotic_scaling_1(test_func, timeout=300):
    # List to store benchmark results: (program_size, ((slice_duration_dice, total_dice_duration), (slice_duration_roulette, total_roulette_duration)), sppl_duration)  
    benchmark_results = []
    
    # Track if slice or sppl has timed out
    slice_timed_out = False
    sppl_timed_out = False
    
    # Generate programs of increasing size, linearly
    # Currently testing: programs of size 1-200 incremented by 5
    for i in range(1,50,5):
        prog_size = i
        print(f"---- PROGRAM SIZE: {prog_size} ----", flush=True)
        
        # --- SLICE ---
        slice_duration_dice = None
        slice_duration_roulette = None
        dice_duration = None
        roulette_duration = None
        total_dice_duration = None
        total_roulette_duration = None
        slice_output = None
        
        if not slice_timed_out:
            program_slice, last_var = test_func(i)
            path = (original_dir / "scale.slice").resolve()
            path.write_text(program_slice)
            
            # Run slice component
            print(f"  Running Slice...", end='', flush=True)
            os.chdir(str(original_dir / "slice"))

            if dice_backend:
                command = ["./run_slice.sh", str(path)]
                result = run_with_timeout(command, timeout) # warm-up run
                if not result:
                    print(f" TIMEOUT (warm-up)", flush=True)
                    slice_timed_out = True
                else:
                    start = time.time()
                    result = run_with_timeout(command, timeout)
                    if not result:
                        print(f" TIMEOUT after {timeout}s", flush=True)
                        slice_timed_out = True
                    else:
                        slice_duration_dice = time.time() - start
                        output = result.stdout.decode('utf-8')

            if roulette_backend:
                command = ["./run_slice.sh", "--roulette", str(path)]
                result = run_with_timeout(command, timeout) # warm-up run
                if not result:
                    print(f" TIMEOUT (warm-up)", flush=True)
                    slice_timed_out = True
                else:
                    start = time.time()
                    result = run_with_timeout(command, timeout)
                    if not result:
                        print(f" TIMEOUT after {timeout}s", flush=True)
                        slice_timed_out = True
                    else:
                        slice_duration_roulette = time.time() - start
                        output = result.stdout.decode('utf-8')

            if dice_backend and roulette_backend and not slice_timed_out:
                print(f" dice: {slice_duration_dice:.3f}s / roulette: {slice_duration_roulette:.3f}s", flush=True)
            elif dice_backend and not slice_timed_out:
                print(f" {slice_duration_dice:.3f}s", flush=True)
            elif roulette_backend and not slice_timed_out:
                print(f" {slice_duration_roulette:.3f}s", flush=True)

            # Run dice / roulette component if slice succeeded
            if not slice_timed_out:
                if dice_backend:
                    print(f"  Running Dice...", end='', flush=True)
                    os.chdir(str(original_dir / "dice"))
                    command = ["./run_dice.sh", "../output.dice"]
                    result = run_with_timeout(command, timeout) # warm-up run
                    if not result:
                        print(f" TIMEOUT (warm-up)", flush=True)
                        slice_timed_out = True
                    else:
                        start = time.time()
                        result = run_with_timeout(command, timeout)
                        if not result:
                            print(f" TIMEOUT after {timeout}s", flush=True)
                            slice_timed_out = True
                        else:
                            dice_duration = time.time() - start
                            slice_output = result.stdout.decode('utf-8')
                            total_dice_duration = slice_duration_dice + dice_duration
                            print(f" {dice_duration:.3f}s (total: {total_dice_duration:.3f}s)", flush=True)
                
                if roulette_backend:
                    print(f"  Running Roulette...", end='', flush=True)
                    os.chdir(str(original_dir / "roulette"))
                    command = ["./run_roulette.sh", "../output.rkt"]
                    result = run_with_timeout(command, timeout) # warm-up run
                    if not result:
                        print(f" TIMEOUT (warm-up)", flush=True)
                        slice_timed_out = True
                    else:
                        start = time.time()
                        result = run_with_timeout(command, timeout)
                        if not result:
                            print(f" TIMEOUT after {timeout}s", flush=True)
                            slice_timed_out = True
                        else:
                            roulette_duration = time.time() - start
                            slice_output = result.stdout.decode('utf-8')
                            total_roulette_duration = slice_duration_roulette + roulette_duration
                            print(f" {roulette_duration:.3f}s (total: {total_roulette_duration:.3f}s)", flush=True)

            else:
                print(f"  Skipping Slice (previous timeout)", flush=True)
        
        # --- SPPL ---
        sppl_duration = None
        sppl_output = None
        
        if not sppl_timed_out:
            print(f"  Running SPPL...", end='', flush=True)
            os.chdir(str(original_dir / "slice"))
            command = ["bash", "-c", "eval $(opam env --switch=4.14.1) && dune exec -- bin/main.exe --to-sppl ../scale.slice"]
            result = run_with_timeout(command, timeout)
            
            if not result:
                print(f" TIMEOUT after {timeout}s (conversion)", flush=True)
                sppl_timed_out = True
            else:
                output = result.stdout.decode('utf-8')
                start = time.time()
                try:
                    # Apply timeout to SPPL execution using threading
                    import threading
                    sppl_result = [None, None]  # [output, error]
                    
                    def run_sppl():
                        try:
                            compiler = SPPL_Compiler(f'''{output}''')
                            namespace = compiler.execute_module()
                            model = Id('model')
                            event = (model >= 1.0)
                            sppl_result[0] = namespace.model.prob(event)
                        except Exception as e:
                            sppl_result[1] = e
                    
                    thread = threading.Thread(target=run_sppl)
                    thread.daemon = True
                    thread.start()
                    thread.join(timeout)
                    
                    if thread.is_alive():
                        print(f" TIMEOUT after {timeout}s", flush=True)
                        sppl_timed_out = True
                    elif sppl_result[1]:
                        print(f" ERROR: {sppl_result[1]}", flush=True)
                    else:
                        sppl_duration = time.time() - start
                        sppl_output = sppl_result[0]
                        print(f" {sppl_duration:.3f}s", flush=True)
                except Exception as e:
                    print(f" ERROR: {e}", flush=True)
        else:
            print(f"  Skipping SPPL (previous timeout)", flush=True)
        
        os.chdir(str(original_dir))
        
        # Only append results if we have slice data (even if SPPL timed out)
        if (slice_duration_dice and slice_duration_roulette) is not None and (total_dice_duration and total_roulette_duration) is not None:
            benchmark_results.append((prog_size, ((slice_duration_dice, total_dice_duration), (slice_duration_roulette, total_roulette_duration)), sppl_duration))
        
        # Print detailed output if requested
        if print_outputs_flag and sppl_output is not None: 
            print(f"  Outputs: sppl={sppl_output}, slice={slice_output}", flush=True)
        print("", flush=True)
         
    return benchmark_results


'''
These are for functions in the gen.py library that have two arguments.
'''
def test_asymptotic_scaling_2(test_func, timeout=300):
    # List to store benchmark results: (program_size, ((slice_duration_dice, total_dice_duration), (slice_duration_roulette, total_roulette_duration)), sppl_duration)  
    benchmark_results = []
    
    # Track if slice or sppl has timed out
    slice_timed_out = False
    sppl_timed_out = False
    
    # Generate programs of increasing size, linearly
    # Currently testing: programs of size 1-50 incremented by 5, with alternating guards incremented by 1 each program
    for i in range(1, 20):
        prog_size = i*5
        print(f"---- PROGRAM SIZE: {prog_size} ----", flush=True)
        
        # --- SLICE ---
        slice_duration_dice = None
        slice_duration_roulette = None
        dice_duration = None
        roulette_duration = None
        total_dice_duration = None
        total_roulette_duration = None
        slice_output = None
        
        if not slice_timed_out:
            program_slice, last_var = test_func(i*5, i)
            path = (original_dir / "scale.slice").resolve()
            path.write_text(program_slice)
            
            # Run slice component
            print(f"  Running Slice...", end='', flush=True)
            os.chdir(str(original_dir / "slice"))

            if dice_backend:
                command = ["./run_slice.sh", str(path)]
                result = run_with_timeout(command, timeout) # warm-up run
                if not result:
                    print(f" TIMEOUT (warm-up)", flush=True)
                    slice_timed_out = True
                else:
                    start = time.time()
                    result = run_with_timeout(command, timeout)
                    if not result:
                        print(f" TIMEOUT after {timeout}s", flush=True)
                        slice_timed_out = True
                    else:
                        slice_duration_dice = time.time() - start
                        output = result.stdout.decode('utf-8')

            if roulette_backend:
                command = ["./run_slice.sh", "--roulette", str(path)]
                result = run_with_timeout(command, timeout) # warm-up run
                if not result:
                    print(f" TIMEOUT (warm-up)", flush=True)
                    slice_timed_out = True
                else:
                    start = time.time()
                    result = run_with_timeout(command, timeout)
                    if not result:
                        print(f" TIMEOUT after {timeout}s", flush=True)
                        slice_timed_out = True
                    else:
                        slice_duration_roulette = time.time() - start
                        output = result.stdout.decode('utf-8')

            if dice_backend and roulette_backend and not slice_timed_out:
                print(f" dice: {slice_duration_dice:.3f}s / roulette: {slice_duration_roulette:.3f}s", flush=True)
            elif dice_backend and not slice_timed_out:
                print(f" {slice_duration_dice:.3f}s", flush=True)
            elif roulette_backend and not slice_timed_out:
                print(f" {slice_duration_roulette:.3f}s", flush=True)

            # Run dice / roulette component if slice succeeded
            if not slice_timed_out:
                if dice_backend:
                    print(f"  Running Dice...", end='', flush=True)
                    os.chdir(str(original_dir / "dice"))
                    command = ["./run_dice.sh", "../output.dice"]
                    result = run_with_timeout(command, timeout) # warm-up run
                    if not result:
                        print(f" TIMEOUT (warm-up)", flush=True)
                        slice_timed_out = True
                    else:
                        start = time.time()
                        result = run_with_timeout(command, timeout)
                        if not result:
                            print(f" TIMEOUT after {timeout}s", flush=True)
                            slice_timed_out = True
                        else:
                            dice_duration = time.time() - start
                            slice_output = result.stdout.decode('utf-8')
                            total_dice_duration = slice_duration_dice + dice_duration
                            print(f" {dice_duration:.3f}s (total: {total_dice_duration:.3f}s)", flush=True)
                
                if roulette_backend:
                    print(f"  Running Roulette...", end='', flush=True)
                    os.chdir(str(original_dir / "roulette"))
                    command = ["./run_roulette.sh", "../output.rkt"]
                    result = run_with_timeout(command, timeout) # warm-up run
                    if not result:
                        print(f" TIMEOUT (warm-up)", flush=True)
                        slice_timed_out = True
                    else:
                        start = time.time()
                        result = run_with_timeout(command, timeout)
                        if not result:
                            print(f" TIMEOUT after {timeout}s", flush=True)
                            slice_timed_out = True
                        else:
                            roulette_duration = time.time() - start
                            slice_output = result.stdout.decode('utf-8')
                            total_roulette_duration = slice_duration_roulette + roulette_duration
                            print(f" {roulette_duration:.3f}s (total: {total_roulette_duration:.3f}s)", flush=True)
        else:
            print(f"  Skipping Slice (previous timeout)", flush=True)
        
        # --- SPPL ---
        sppl_duration = None
        sppl_output = None
        
        if not sppl_timed_out:
            print(f"  Running SPPL...", end='', flush=True)
            os.chdir(str(original_dir / "slice"))
            command = ["bash", "-c", "eval $(opam env --switch=4.14.1) && dune exec -- bin/main.exe --to-sppl ../scale.slice"]
            result = run_with_timeout(command, timeout)
            
            if not result:
                print(f" TIMEOUT after {timeout}s (conversion)", flush=True)
                sppl_timed_out = True
            else:
                output = result.stdout.decode('utf-8')
                start = time.time()
                try:
                    # Apply timeout to SPPL execution using threading
                    import threading
                    sppl_result = [None, None]  # [output, error]
                    
                    def run_sppl():
                        try:
                            compiler = SPPL_Compiler(f'''{output}''')
                            namespace = compiler.execute_module()
                            model = Id('model')
                            event = (model >= 1.0)
                            sppl_result[0] = namespace.model.prob(event)
                        except Exception as e:
                            sppl_result[1] = e
                    
                    thread = threading.Thread(target=run_sppl)
                    thread.daemon = True
                    thread.start()
                    thread.join(timeout)
                    
                    if thread.is_alive():
                        print(f" TIMEOUT after {timeout}s", flush=True)
                        sppl_timed_out = True
                    elif sppl_result[1]:
                        print(f" ERROR: {sppl_result[1]}", flush=True)
                    else:
                        sppl_duration = time.time() - start
                        sppl_output = sppl_result[0]
                        print(f" {sppl_duration:.3f}s", flush=True)
                except Exception as e:
                    print(f" ERROR: {e}", flush=True)
        else:
            print(f"  Skipping SPPL (previous timeout)", flush=True)
        
        os.chdir(str(original_dir))
        
        # Only append results if we have slice data (even if SPPL timed out)
        if (slice_duration_dice and slice_duration_roulette) is not None and (total_dice_duration and total_roulette_duration) is not None:
            benchmark_results.append((prog_size, ((slice_duration_dice, total_dice_duration), (slice_duration_roulette, total_roulette_duration)), sppl_duration))
        
        # Print detailed output if requested
        if print_outputs_flag and sppl_output is not None: 
            print(f"  Outputs: sppl={sppl_output}, slice={slice_output}", flush=True)
        print("", flush=True)
         
    return benchmark_results


def generate_line_graphs(benchmark_results, output_path):
    '''
    Args:
    benchmark_results: list of tuples (prog_size, ((slice_dice, total_dice), (slice_roulette, total_roulette)), sppl_times)
    output_path: .png file
    dice_backend: bool - whether dice backend is specified
    roulette_backend: bool - whether roulette backend is specified
    '''
    plt.figure(figsize=(10, 6))
    
    # Separate data for SPPL (which may have timeouts) and Slice (which continues)
    all_prog_sizes = [b[0] for b in benchmark_results]
    
    # For SPPL, only plot non-timeout values
    sppl_prog_sizes = []
    sppl_times = []
    for b in benchmark_results:
        if b[2] is not None:
            sppl_prog_sizes.append(b[0])
            sppl_times.append(b[2])
    
    # Plot SPPL if we have data
    if sppl_times:
        plt.plot(sppl_prog_sizes, sppl_times, label="SPPL", color="red", marker='o')
    
    # Plot slice times, default to using dice slice times if both backends are specified
    if dice_backend:
        slice_times = [b[1][0][0] for b in benchmark_results]  # slice_dice times
        plt.plot(all_prog_sizes, slice_times, label="Slice", color="darkblue", marker='s')
    
    # Plot backend times
    if dice_backend and roulette_backend:
        # Both backends - plot both total times
        dice_times = [b[1][0][1] for b in benchmark_results]  # total_dice times
        roulette_times = [b[1][1][1] for b in benchmark_results]  # total_roulette times
        
        plt.plot(all_prog_sizes, dice_times, label="Slice+Dice", color="lightblue", marker='^')
        plt.plot(all_prog_sizes, roulette_times, label="Slice+Roulette", color="lightgreen", marker='D')
    elif dice_backend:
        # Only dice backend
        dice_times = [b[1][0][1] for b in benchmark_results]  # total_dice times
        plt.plot(all_prog_sizes, dice_times, label="Slice+Dice", color="lightblue", marker='^')
    elif roulette_backend:
        # Only roulette backend
        roulette_times = [b[1][1][1] for b in benchmark_results]  # total_roulette times
        plt.plot(all_prog_sizes, roulette_times, label="Slice+Roulette", color="lightgreen", marker='D')

    plt.xlabel("Program Size")
    plt.ylabel("Execution Time (seconds)")
    
    # Set appropriate title based on backends used
    if dice_backend and roulette_backend:
        plt.title("SPPL vs Slice Execution Time Scaling (Both Backends)")
    elif dice_backend:
        plt.title("SPPL vs Slice Execution Time Scaling (Dice Backend)")
    else:
        plt.title("SPPL vs Slice Execution Time Scaling (Roulette Backend)")
    
    plt.legend()
    plt.grid(True)
    plt.savefig(output_path, dpi=300, bbox_inches="tight")
    plt.show()
    print(f"Saved scaling plot to {output_path}", flush=True)


print_outputs_flag = False
dice_backend = True
roulette_backend = True
# Get the project root directory more reliably
script_dir = Path(__file__).resolve().parent
original_dir = script_dir.parent.parent  # Go up two levels from benchmarks/scaling to project root

def main():
    global print_outputs_flag, dice_backend, roulette_backend
    
    # Parse command line arguments
    timeout = 300  # Default timeout per benchmark
    if "--timeout" in sys.argv:
        idx = sys.argv.index("--timeout")
        if idx + 1 < len(sys.argv):
            timeout = int(sys.argv[idx + 1])
            print(f"Using timeout: {timeout}s per benchmark", flush=True)
    
    if "--print-outputs" in sys.argv:
        print_outputs_flag = True
    
    if "--dice" in sys.argv:
        dice_backend = True
        roulette_backend = False
    
    if "--roulette" in sys.argv:
        roulette_backend = True
        dice_backend = False

    # Define all tests with their argument count
    tests_1_arg = [
        build_conditional_independent_slice,
        build_conditional_random_independent_slice_1,
        build_conditional_random_independent_slice_2
    ]
    
    tests_2_args = [
        build_alternating_guard_slice_1,
        build_alternating_guard_slice_2,
        build_alternating_guard_slice_3,
        build_random_alternating_guard_slice
    ]
    
    # Run all benchmarks
    print(f"Running all {len(tests_1_arg) + len(tests_2_args)} scaling benchmarks...", flush=True)
    print("=" * 60, flush=True)
    
    # Run single-argument tests
    for test_func in tests_1_arg:
        print(f"\nTesting asymptotic scaling for {test_func.__name__}...", flush=True)
        print(f"This will test programs of size 1 to 245 in increments of 5.", flush=True)
        print("", flush=True)
        benchmark_results = test_asymptotic_scaling_1(test_func, timeout=timeout)
        if benchmark_results:
            output_file = original_dir / "images" / "scaling" / f"{test_func.__name__}.png"
            output_file.parent.mkdir(parents=True, exist_ok=True)
            generate_line_graphs(benchmark_results, str(output_file))
    
    # Run two-argument tests
    for test_func in tests_2_args:
        print(f"\nTesting asymptotic scaling for {test_func.__name__}...", flush=True)
        print(f"This will test programs of size 5 to 95 in increments of 5.", flush=True)
        print("", flush=True)
        benchmark_results = test_asymptotic_scaling_2(test_func, timeout=timeout)
        if benchmark_results:
            output_file = original_dir / "images" / "scaling" / f"{test_func.__name__}.png"
            output_file.parent.mkdir(parents=True, exist_ok=True)
            generate_line_graphs(benchmark_results, str(output_file))
    
    print("\n" + "=" * 60, flush=True)
    print("All scaling benchmarks completed!", flush=True)
    print(f"Results saved to: {original_dir / 'images' / 'scaling'}/", flush=True)
    

if __name__ == "__main__":
    main()