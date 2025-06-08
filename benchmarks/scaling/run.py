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
$ python3 run.py --timeout N: set timeout to N seconds per benchmark (default: 300)
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
    # List to store benchmark results: (program_size, (slice_duration, total_duration), sppl_duration)  
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
        slice_duration = None
        dice_duration = None
        total_duration = None
        slice_output = None
        
        if not slice_timed_out:
            program_slice, last_var = test_func(i)
            path = (original_dir / "scale.slice").resolve()
            path.write_text(program_slice)
            
            # Run slice component
            print(f"  Running Slice...", end='', flush=True)
            os.chdir(str(original_dir / "slice"))
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
                    slice_duration = time.time() - start
                    output = result.stdout.decode('utf-8')
                    print(f" {slice_duration:.3f}s", flush=True)
            
            # Run dice component if slice succeeded
            if not slice_timed_out:
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
                        total_duration = slice_duration + dice_duration
                        print(f" {dice_duration:.3f}s (total: {total_duration:.3f}s)", flush=True)
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
        if slice_duration is not None and total_duration is not None:
            benchmark_results.append((prog_size, (slice_duration, total_duration), sppl_duration))
        
        # Print detailed output if requested
        if print_outputs_flag and sppl_output is not None: 
            print(f"  Outputs: sppl={sppl_output}, slice={slice_output}", flush=True)
        print("", flush=True)
         
    return benchmark_results


'''
These are for functions in the gen.py library that have two arguments.
'''
def test_asymptotic_scaling_2(test_func, timeout=300):
    # List to store benchmark results: (program_size, (slice_duration, total_duration), sppl_duration)  
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
        slice_duration = None
        dice_duration = None
        total_duration = None
        slice_output = None
        
        if not slice_timed_out:
            program_slice, last_var = test_func(i*5, i)
            path = (original_dir / "scale.slice").resolve()
            path.write_text(program_slice)
            
            # Run slice component
            print(f"  Running Slice...", end='', flush=True)
            os.chdir(str(original_dir / "slice"))
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
                    slice_duration = time.time() - start
                    output = result.stdout.decode('utf-8')
                    print(f" {slice_duration:.3f}s", flush=True)
            
            # Run dice component if slice succeeded
            if not slice_timed_out:
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
                        total_duration = slice_duration + dice_duration
                        print(f" {dice_duration:.3f}s (total: {total_duration:.3f}s)", flush=True)
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
        if slice_duration is not None and total_duration is not None:
            benchmark_results.append((prog_size, (slice_duration, total_duration), sppl_duration))
        
        # Print detailed output if requested
        if print_outputs_flag and sppl_output is not None: 
            print(f"  Outputs: sppl={sppl_output}, slice={slice_output}", flush=True)
        print("", flush=True)
         
    return benchmark_results


def generate_line_graphs(benchmark_results, output_path):
    '''
    Args:
    benchmark_results: list of tuples (prog_size, (slice_times, total_times), sppl_times)
    output_path: .png file
    '''
    plt.figure(figsize=(10, 6))
    
    # Separate data for SPPL (which may have timeouts) and Slice (which continues)
    all_prog_sizes = [b[0] for b in benchmark_results]
    slice_times = [b[1][0] for b in benchmark_results]
    total_times = [b[1][1] for b in benchmark_results]
    
    # For SPPL, only plot non-timeout values
    sppl_prog_sizes = []
    sppl_times = []
    for b in benchmark_results:
        if b[2] is not None:
            sppl_prog_sizes.append(b[0])
            sppl_times.append(b[2])
    
    # Plot with markers to show data points
    if sppl_times:
        plt.plot(sppl_prog_sizes, sppl_times, label="SPPL", color="red", marker='o')
    plt.plot(all_prog_sizes, slice_times, label="Slice", color="darkblue", marker='s')
    plt.plot(all_prog_sizes, total_times, label="Slice+Dice", color="lightblue", marker='^')

    plt.xlabel("Program Size")
    plt.ylabel("Execution Time (seconds)")
    plt.title("SPPL vs Slice Execution Time Scaling")
    plt.legend()
    plt.grid(True)
    plt.savefig(output_path, dpi=300, bbox_inches="tight")
    plt.show()
    print(f"Saved scaling plot to {output_path}", flush=True)


print_outputs_flag = False
# Get the project root directory more reliably
script_dir = Path(__file__).resolve().parent
original_dir = script_dir.parent.parent  # Go up two levels from benchmarks/scaling to project root

def main():
    global print_outputs_flag
    
    # Parse command line arguments
    timeout = 300  # Default timeout per benchmark
    if "--timeout" in sys.argv:
        idx = sys.argv.index("--timeout")
        if idx + 1 < len(sys.argv):
            timeout = int(sys.argv[idx + 1])
            print(f"Using timeout: {timeout}s per benchmark", flush=True)
    
    if "--print-outputs" in sys.argv:
        print_outputs_flag = True
        
    # Define all tests with their argument count
    tests_1_arg = [
        build_conditional_independent_contdice,
        build_conditional_random_independent_contdice_1,
        build_conditional_random_independent_contdice_2
    ]
    
    tests_2_args = [
        build_alternating_guard_contdice_1,
        build_alternating_guard_contdice_2,
        build_alternating_guard_contdice_3,
        build_random_alternating_guard_contdice
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