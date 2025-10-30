#!/usr/bin/env python3
import subprocess
import time
import sys
import os
from pathlib import Path
import matplotlib.pyplot as plt

# custom library for generating programs
try:
    from .gen import *
except ImportError:
    from gen import *

from sppl.compilers.ast_to_spe import Id
from sppl.compilers.sppl_to_python import SPPL_Compiler

'''
Usage:
If no backend is specified, by default benchmarks both dice and roulette

$ python3 run.py [--dice|--roulette]: tests asymptotic scaling for a variety of programs
$ python3 run.py --print-outputs [--dice|--roulette]: to verify that the outputs between slice and sppl are the same
$ python3 run.py --print-programs [--dice|--roulette]: to print the input programs that are generated
$ python3 run.py --timeout N [--dice|--roulette]: set timeout to N seconds per benchmark (default: 300)
'''

def run_with_timeout(command, timeout):
    """Run a command with timeout handling"""
    try:
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, 
                            stderr=subprocess.PIPE, timeout=timeout)
        # return stdout for output, stderr result for timing (if --time is specified)
        return result.stdout, result.stderr
    except subprocess.TimeoutExpired:
        return None


def run_slice_to_dice(path, timeout):
    """
    Runs Slice on a .slice file if --dice backend is specified.
    Args:
        path: path to .slice file
        timeout: timeout for running .slice file
    Output:
        slice duration, total slice+dice duration, output, timed out : bool
    """
    slice_duration_dice = None
    dice_duration = None
    total_duration_dice = None
    slice_output = None
    slice_timed_out = False

    # Run slice component
    print(f"  Running Slice...", end='', flush=True)
    os.chdir(str(original_dir / "slice"))
    command = ["./run_slice.sh", "--time", str(path)]
    result = run_with_timeout(command, timeout)
    if not result:
        print(f" TIMEOUT after {timeout}s", flush=True)
        slice_timed_out = True
    else:
        slice_duration_dice = float(result[1].strip())

    if not slice_timed_out:
        # Run dice component (if slice succeeded)
        print(f"  Running Dice...", end='', flush=True)
        os.chdir(str(original_dir / "dice"))
        command = ["./run_dice.sh", "--time", "../output.dice"]
        result = run_with_timeout(command, timeout)
        if not result:
            print(f" TIMEOUT after {timeout}s", flush=True)
            slice_timed_out = True
        else:
            # Extract time from stderr, output from stdout
            dice_duration = float(result[1].strip())
            slice_output = result[0].decode('utf-8')
            total_duration_dice = slice_duration_dice + dice_duration
            print(f"   slice: {slice_duration_dice:.3f}s / dice: {dice_duration:.3f}s (total: {total_duration_dice:.3f}s)", flush=True)
    else:
        print(f"  Skipping Dice (previous timeout)", flush=True)
    
    os.chdir(str(original_dir))
    return slice_duration_dice, total_duration_dice, slice_output, slice_timed_out


def run_slice_to_roulette(path, timeout):
    """
    Runs Slice on a .slice file if --roulette backend is specified.
    Args:
        path: path to .slice file
        timeout: timeout for running .slice file
    Output:
        duration, output, timed out : bool
    """
    slice_duration_roulette = None
    roulette_duration = None
    total_duration_roulette = None
    slice_output = None
    slice_timed_out = False
    
    # Run slice component
    print(f"  Running Slice...", end='', flush=True)
    os.chdir(str(original_dir / "slice"))
    command = ["./run_slice.sh", "--roulette", "--time", str(path)]
    result = run_with_timeout(command, timeout)
    if not result:
        print(f" TIMEOUT after {timeout}s", flush=True)
        slice_timed_out = True
    else:
        slice_duration_roulette = float(result[1].strip())

    if not slice_timed_out:
        # Run roulette component (if slice succeeded)
        print(f"  Running Roulette...", end='', flush=True)
        os.chdir(str(original_dir / "roulette"))
        command = ["./run_roulette.sh", "--time", "../output.rkt"]
        result = run_with_timeout(command, timeout)
        if not result:
            print(f" TIMEOUT after {timeout}s", flush=True)
            slice_timed_out = True
        else:
            # Extract time from stderr, output from stdout
            roulette_duration = float(result[1].strip())
            slice_output = result[0].decode('utf-8')
            total_duration_roulette = slice_duration_roulette + roulette_duration
            print(f"   slice: {slice_duration_roulette:.3f}s / roulette: {roulette_duration:.3f}s (total: {total_duration_roulette:.3f}s)", flush=True)
    else:
        print(f"  Skipping Roulette (previous timeout)", flush=True)
    
    os.chdir(str(original_dir))
    return slice_duration_roulette, total_duration_roulette, slice_output, slice_timed_out


def convert_to_sppl_from_slice(timeout):
    """Runs the --to-sppl conversion, converting a slice program to an sppl program
    Output:
        sppl program : string"""
    os.chdir(str(original_dir / "slice"))
    command = ["bash", "-c", "eval $(opam env --switch=4.14.1) && dune exec -- bin/main.exe --to-sppl ../scale.slice"]
    result = run_with_timeout(command, timeout=timeout)
    output = result[0].decode('utf-8')
    return output
    

def run_sppl(sppl_program, timeout):
    """Runs SPPL.
    Args:
        sppl_program: sppl program : string
    Output:
        duration, output, timed out : bool"""
    sppl_duration = None
    sppl_output = None
    sppl_timed_out = False

    print(f"  Running SPPL...", end='', flush=True)
    start = time.time()
    try:
        # Apply timeout to SPPL execution using threading
        import threading
        sppl_result = [None, None]  # [output, error]
        
        def run_sppl():
            try:
                compiler = SPPL_Compiler(f'''{sppl_program}''')
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

    return sppl_duration, sppl_output, sppl_timed_out


'''
These are for functions in the gen.py library that have one argument.
'''
def test_asymptotic_scaling_1_arg(test_func, timeout=120, num_runs=10):
    # List to store benchmark results: (program_size, ((slice_duration_dice, total_duration_dice), (slice_duration_roulette, total_duration_roulette)), sppl_duration)  
    benchmark_results = []

    # Track if slice or sppl has timed out
    slice_timed_out_dice = False
    slice_timed_out_roulette = False
    sppl_timed_out = False
    
    # Generate programs of increasing size, linearly
    # Currently testing: programs of size 1-50 incremented by 10
    for i in range(0, 51, 5):
        prog_size = i
        print(f"---- PROGRAM SIZE: {prog_size} ----", flush=True)
        
        # Lists to store multiple run results
        slice_durations_dice = []
        slice_durations_roulette = []
        total_durations_dice = []
        total_durations_roulette = []
        sppl_durations = []

        if i==0: 
            benchmark_results.append((i, ((0, 0), (0, 0)), 0))
            continue
        
        program_slice, _ = test_func(i)
        path = (original_dir / "scale.slice").resolve()
        path.write_text(program_slice)

        # Run multiple times and collect results
        for run in range(num_runs):
            print(f"  Run {run+1}/{num_runs}...", flush=True)
            
            # --- SLICE ---
            slice_duration_dice = None
            slice_duration_roulette = None
            total_duration_dice = None
            total_duration_roulette = None
            slice_output_dice = None
            slice_output_roulette = None
            
            print(f"    Running Slice...", end='', flush=True)
            os.chdir(str(original_dir / "slice"))

            # Run Dice backend if enabled and not timed out
            if dice_backend:
                if not slice_timed_out_dice:
                    slice_duration_dice, total_duration_dice, slice_output_dice, slice_timed_out_dice = run_slice_to_dice(path, timeout)
                    if not slice_timed_out_dice:
                        slice_durations_dice.append(slice_duration_dice)
                        total_durations_dice.append(total_duration_dice)
                else:
                    print(f"    Skipping Slice+Dice (previous timeout)", flush=True)

            # Run Roulette backend if enabled and not timed out
            if roulette_backend:
                if not slice_timed_out_roulette:
                    slice_duration_roulette, total_duration_roulette, slice_output_roulette, slice_timed_out_roulette = run_slice_to_roulette(path, timeout)
                    if not slice_timed_out_roulette:
                        slice_durations_roulette.append(slice_duration_roulette)
                        total_durations_roulette.append(total_duration_roulette)
                else:
                    print(f"    Skipping Slice+Roulette (previous timeout)", flush=True)
            
            # --- SPPL ---
            sppl_duration = None
            sppl_output = None
            
            if not sppl_timed_out:
                sppl_program = convert_to_sppl_from_slice(timeout)
                sppl_duration, sppl_output, sppl_timed_out = run_sppl(sppl_program, timeout)
                if not sppl_timed_out:
                    sppl_durations.append(sppl_duration)
            else:
                print(f"    Skipping SPPL (previous timeout)", flush=True)
            
            os.chdir(str(original_dir))
            
            # Print detailed output if requested (only for first run to avoid repetition)
            if print_outputs_flag and run == 0 and sppl_output is not None: 
                print(f"    Outputs: sppl={sppl_output}, slice+dice={slice_output_dice}, slice+roulette={slice_output_roulette}", flush=True)
        
        # Calculate averages for this program size
        avg_slice_duration_dice = sum(slice_durations_dice) / len(slice_durations_dice) if slice_durations_dice else None
        avg_slice_duration_roulette = sum(slice_durations_roulette) / len(slice_durations_roulette) if slice_durations_roulette else None
        avg_total_duration_dice = sum(total_durations_dice) / len(total_durations_dice) if total_durations_dice else None
        avg_total_duration_roulette = sum(total_durations_roulette) / len(total_durations_roulette) if total_durations_roulette else None
        avg_sppl_duration = sum(sppl_durations) / len(sppl_durations) if sppl_durations else None
        
        # Only append results if we have at least one successful run
        if ((avg_slice_duration_dice is not None or avg_slice_duration_roulette is not None) and 
            (avg_total_duration_dice is not None or avg_total_duration_roulette is not None)):
            benchmark_results.append((
                prog_size, 
                ((avg_slice_duration_dice, avg_total_duration_dice), 
                 (avg_slice_duration_roulette, avg_total_duration_roulette)), 
                avg_sppl_duration
            ))
            
            print(f"  Averages after {num_runs} runs:", flush=True)
            if avg_slice_duration_dice is not None:
                print(f"    Slice+Dice - Slice: {avg_slice_duration_dice:.4f}s, Total: {avg_total_duration_dice:.4f}s", flush=True)
            if avg_slice_duration_roulette is not None:
                print(f"    Slice+Roulette - Slice: {avg_slice_duration_roulette:.4f}s, Total: {avg_total_duration_roulette:.4f}s", flush=True)
            if avg_sppl_duration is not None:
                print(f"    SPPL: {avg_sppl_duration:.4f}s", flush=True)
        else:
            print(f"  No successful runs for program size {prog_size}", flush=True)
            
        print("", flush=True)
         
    return benchmark_results

'''
These are for functions in the gen.py library that have two arguments.
'''
def test_asymptotic_scaling_2_arg(test_func, timeout=120, num_runs=10):
    # List to store benchmark results: (program_size, ((slice_duration_dice, total_duration_dice), (slice_duration_roulette, total_duration_roulette)), sppl_duration)  
    benchmark_results = []

    # Track if slice or sppl has timed out
    slice_timed_out_dice = False
    slice_timed_out_roulette = False
    sppl_timed_out = False
    
    # Generate programs of increasing size, linearly
    # Currently testing: programs of size 1-100 incremented by 5, with alternating guards incremented by 1 each program
    for i in range(0, 11):
        prog_size = i * 5
        print(f"---- PROGRAM SIZE: {prog_size} ----", flush=True)
        
        # Lists to store multiple run results
        slice_durations_dice = []
        slice_durations_roulette = []
        total_durations_dice = []
        total_durations_roulette = []
        sppl_durations = []

        if i==0: 
            benchmark_results.append((i, ((0, 0), (0, 0)), 0))
            continue
        
        program_slice, _ = test_func(i * 5, i)
        path = (original_dir / "scale.slice").resolve()
        path.write_text(program_slice)

        # Run multiple times and collect results
        for run in range(num_runs):
            print(f"  Run {run+1}/{num_runs}...", flush=True)
            
            # --- SLICE ---
            slice_duration_dice = None
            slice_duration_roulette = None
            total_duration_dice = None
            total_duration_roulette = None
            slice_output_dice = None
            slice_output_roulette = None
            
            print(f"    Running Slice...", end='', flush=True)
            os.chdir(str(original_dir / "slice"))

            # Run Dice backend if enabled and not timed out
            if dice_backend:
                if not slice_timed_out_dice:
                    slice_duration_dice, total_duration_dice, slice_output_dice, slice_timed_out_dice = run_slice_to_dice(path, timeout)
                    if not slice_timed_out_dice:
                        slice_durations_dice.append(slice_duration_dice)
                        total_durations_dice.append(total_duration_dice)
                else:
                    print(f"    Skipping Slice+Dice (previous timeout)", flush=True)

            # Run Roulette backend if enabled and not timed out
            if roulette_backend:
                if not slice_timed_out_roulette:
                    slice_duration_roulette, total_duration_roulette, slice_output_roulette, slice_timed_out_roulette = run_slice_to_roulette(path, timeout)
                    if not slice_timed_out_roulette:
                        slice_durations_roulette.append(slice_duration_roulette)
                        total_durations_roulette.append(total_duration_roulette)
                else:
                    print(f"    Skipping Slice+Roulette (previous timeout)", flush=True)

            # --- SPPL ---
            sppl_duration = None
            sppl_output = None
            
            if not sppl_timed_out:
                sppl_program = convert_to_sppl_from_slice(timeout)
                sppl_duration, sppl_output, sppl_timed_out = run_sppl(sppl_program, timeout)
                if not sppl_timed_out:
                    sppl_durations.append(sppl_duration)
            else:
                print(f"    Skipping SPPL (previous timeout)", flush=True)
            
            os.chdir(str(original_dir))
            
            # Print detailed output if requested (only for first run to avoid repetition)
            if print_outputs_flag and run == 0 and sppl_output is not None: 
                print(f"    Outputs: sppl={sppl_output}, slice+dice={slice_output_dice}, slice+roulette={slice_output_roulette}", flush=True)
        
        # Calculate averages for this program size
        avg_slice_duration_dice = sum(slice_durations_dice) / len(slice_durations_dice) if slice_durations_dice else None
        avg_slice_duration_roulette = sum(slice_durations_roulette) / len(slice_durations_roulette) if slice_durations_roulette else None
        avg_total_duration_dice = sum(total_durations_dice) / len(total_durations_dice) if total_durations_dice else None
        avg_total_duration_roulette = sum(total_durations_roulette) / len(total_durations_roulette) if total_durations_roulette else None
        avg_sppl_duration = sum(sppl_durations) / len(sppl_durations) if sppl_durations else None
        
        # Only append results if we have at least one successful run
        if ((avg_slice_duration_dice is not None or avg_slice_duration_roulette is not None) and 
            (avg_total_duration_dice is not None or avg_total_duration_roulette is not None)):
            benchmark_results.append((
                prog_size, 
                ((avg_slice_duration_dice, avg_total_duration_dice), 
                 (avg_slice_duration_roulette, avg_total_duration_roulette)), 
                avg_sppl_duration
            ))
            
            print(f"  Averages after {num_runs} runs:", flush=True)
            if avg_slice_duration_dice is not None:
                print(f"    Slice+Dice - Slice: {avg_slice_duration_dice:.4f}s, Total: {avg_total_duration_dice:.4f}s", flush=True)
            if avg_slice_duration_roulette is not None:
                print(f"    Slice+Roulette - Slice: {avg_slice_duration_roulette:.4f}s, Total: {avg_total_duration_roulette:.4f}s", flush=True)
            if avg_sppl_duration is not None:
                print(f"    SPPL: {avg_sppl_duration:.4f}s", flush=True)
        else:
            print(f"  No successful runs for program size {prog_size}", flush=True)
            
        print("", flush=True)
         
    return benchmark_results


def generate_line_graphs(benchmark_results, output_path,
                         dice_backend=True, roulette_backend=True):
    '''
    Args:
    benchmark_results: list of tuples (prog_size, ((slice_dice, total_dice), (slice_roulette, total_roulette)), sppl_times)
    output_path: .png file
    dice_backend: bool - whether dice backend is specified
    roulette_backend: bool - whether roulette backend is specified
    '''
    plt.figure(figsize=(10, 6))
    
    # Set larger font sizes globally for this figure
    plt.rcParams.update({
        'axes.labelsize': 18,     # axis labels
        'axes.titlesize': 18,     # plot title (if any)
        'xtick.labelsize': 14,    # x tick labels
        'ytick.labelsize': 14,    # y tick labels
        'legend.fontsize': 14,    # legend text
        'lines.markersize': 8     # marker size
    })
    
    all_prog_sizes = [b[0] for b in benchmark_results]
    
    sppl_prog_sizes = []
    sppl_times = []
    for b in benchmark_results:
        if b[2] is not None:
            sppl_prog_sizes.append(b[0])
            sppl_times.append(b[2])
    
    if sppl_times:
        plt.plot(sppl_prog_sizes, sppl_times, label="SPPL", color="red", marker='o', markevery=[-1], linewidth=3.0)
    
    if dice_backend:
        slice_times = [b[1][0][0] for b in benchmark_results]  # slice_dice times
        plt.plot(all_prog_sizes, slice_times, label="Slice", color="peru", marker='s', markevery=[-1], linewidth=3.0)
    
    if dice_backend and roulette_backend:
        dice_times = [b[1][0][1] for b in benchmark_results]  # total_dice times
        roulette_times = [b[1][1][1] for b in benchmark_results]  # total_roulette times
        
        plt.plot(all_prog_sizes, dice_times, label="Slice+Dice", color="darkblue", marker='^', markevery=[-1], linewidth=3.0)
        plt.plot(all_prog_sizes, roulette_times, label="Slice+Roulette", color="forestgreen", marker='D', markevery=[-1], linewidth=3.0)
    elif dice_backend:
        dice_times = [b[1][0][1] for b in benchmark_results]  # total_dice times
        plt.plot(all_prog_sizes, dice_times, label="Slice+Dice", color="darkblue", marker='^', markevery=[-1], linewidth=3.0)
    elif roulette_backend:
        roulette_times = [b[1][1][1] for b in benchmark_results]  # total_roulette times
        plt.plot(all_prog_sizes, roulette_times, label="Slice+Roulette", color="forestgreen", marker='D', markevery=[-1], linewidth=3.0)

    plt.xlabel("Program Size")
    plt.ylabel("Execution Time (seconds)")
    
    # Remove top and right borders
    plt.gca().spines['top'].set_visible(False)
    plt.gca().spines['right'].set_visible(False)
    
    # Make grid lines faint but still visible
    plt.grid(True, alpha=0.3)
    
    plt.legend()
    plt.savefig(output_path, dpi=300, bbox_inches="tight")
    plt.show()
    print(f"Saved scaling plot to {output_path}", flush=True)


print_outputs_flag = False
print_programs_flag = False
dice_backend = True
roulette_backend = True
# Get the project root directory more reliably
script_dir = Path(__file__).resolve().parent
original_dir = script_dir.parent.parent  # Go up two levels from benchmarks/scaling to project root

def main():
    global print_outputs_flag, print_programs_flag, dice_backend, roulette_backend
    
    # Parse command line arguments
    timeout = 120  # Default timeout per benchmark
    if "--timeout" in sys.argv:
        idx = sys.argv.index("--timeout")
        if idx + 1 < len(sys.argv):
            timeout = int(sys.argv[idx + 1])
            print(f"Using timeout: {timeout}s per benchmark", flush=True)
    
    if "--print-outputs" in sys.argv:
        print_outputs_flag = True

    if "--print-programs" in sys.argv:
        print_programs_flag = True
    
    if "--dice" in sys.argv:
        dice_backend = True
        roulette_backend = False
    
    if "--roulette" in sys.argv:
        roulette_backend = True
        dice_backend = False

    # Define all tests with their argument count
    tests_1_arg = [
        #build_conditional_independent_slice,
        #build_conditional_independent_fork_slice,
        #build_conditional_random_independent_slice_1,
        #build_conditional_random_independent_slice_2,
    ]
    
    tests_2_args = [
        #build_alternating_guard_slice_1,
        build_alternating_guard_slice_2,
        #build_alternating_guard_slice_3,
        #build_random_alternating_guard_slice
    ]
    
    # Run all benchmarks
    print(f"Running all {len(tests_1_arg) + len(tests_2_args)} scaling benchmarks...", flush=True)
    print("=" * 60, flush=True)
    
    # Run single-argument tests
    for test_func in tests_1_arg:
        print(f"\nTesting asymptotic scaling for {test_func.__name__}...", flush=True)
        print(f"This will test programs of size 1 to 245 in increments of 5.", flush=True)
        print("", flush=True)
        benchmark_results = test_asymptotic_scaling_1_arg(test_func, timeout=timeout)
        if benchmark_results:
            output_file = original_dir / "images" / "scaling" / f"{test_func.__name__}.png"
            output_file.parent.mkdir(parents=True, exist_ok=True)
            generate_line_graphs(benchmark_results, str(output_file))

    # Run two-argument tests
    for test_func in tests_2_args:
        print(f"\nTesting asymptotic scaling for {test_func.__name__}...", flush=True)
        print(f"This will test programs of size 5 to 95 in increments of 5.", flush=True)
        print("", flush=True)
        benchmark_results = test_asymptotic_scaling_2_arg(test_func, timeout=timeout)
        if benchmark_results:
            output_file = original_dir / "images" / "scaling" / f"{test_func.__name__}.png"
            output_file.parent.mkdir(parents=True, exist_ok=True)
            generate_line_graphs(benchmark_results, str(output_file))
    
    print("\n" + "=" * 60, flush=True)
    print("All scaling benchmarks completed!", flush=True)
    print(f"Results saved to: {original_dir / 'images' / 'scaling'}/", flush=True)
    

if __name__ == "__main__":
    main()