#!/usr/bin/env python3
"""
Slice - Unified CLI for the Slice probabilistic programming system

This script consolidates all functionality from various shell scripts into
a single, cohesive command-line interface.
"""

import argparse
import subprocess
import sys
import os
import csv
import json
import time
import statistics
from pathlib import Path
from datetime import datetime

# Global backend configuration - both enabled by default
USE_DICE_BACKEND = True
USE_ROULETTE_BACKEND = True

class BenchmarkRunner:
    """Integrated benchmark runner for Slice"""
    
    def __init__(self, output_dir="artifact_results", timeout=300):
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(exist_ok=True)
        self.results = []
        self.num_runs = 5  # Number of times to run each benchmark
        self.timeout = timeout  # Timeout in seconds
        
    def run_command(self, cmd, cwd=None):
        """Run a command and return timing information"""
        try:
            # Warm-up run
            subprocess.run(cmd, shell=True, capture_output=True, cwd=cwd, timeout=self.timeout)
            
            # Timed runs
            times = []
            for _ in range(self.num_runs):
                start = time.time()
                result = subprocess.run(
                    cmd, 
                    shell=True, 
                    capture_output=True, 
                    text=True,
                    cwd=cwd,
                    timeout=self.timeout
                )
                end = time.time()
                
                if result.returncode == 0:
                    times.append(end - start)
                else:
                    print(f"    Error running {cmd}: {result.stderr}")
                    return None
                    
            if times:
                return {
                    'mean': statistics.mean(times),
                    'stdev': statistics.stdev(times) if len(times) > 1 else 0,
                    'min': min(times),
                    'max': max(times),
                    'times': times
                }
            return None
            
        except subprocess.TimeoutExpired:
            print(f"    Timeout after {self.timeout}s running {cmd}")
            return None
        except Exception as e:
            print(f"    Exception running {cmd}: {e}")
            return None
    
    def run_slice_on_file(self, slice_file, backend):
        """Run Slice on a .slice file"""

        if backend == 'dice':
            # Check if binaries are in PATH (Docker) or need local paths
            if subprocess.run("which main.exe", shell=True, capture_output=True).returncode == 0:
                # Docker environment - binaries in PATH
                slice_cmd = f"main.exe {os.path.abspath(slice_file)} > output.dice"
                dice_cmd = "dice.exe output.dice -show-size -flip-lifting"
            else:
                # Local environment
                slice_cmd = f"cd slice && ./_build/default/bin/main.exe {os.path.abspath(slice_file)} | tee ../output.dice"
                cd_out = "cd .."
                dice_cmd = "cd dice && ./_build/default/bin/dice.exe '../output.dice' -show-size -flip-lifting"
            
            full_cmd = f"{slice_cmd} && {cd_out} && {dice_cmd}"
            return self.run_command(full_cmd)

        if backend == 'roulette':
            add_header_cmd = 'echo "#lang roulette/example/disrupt" > output.rkt'
            slice_cmd = f"cd slice && ./_build/default/bin/main.exe --backend roulette {os.path.abspath(slice_file)}"
            append_output_cmd = f"{slice_cmd} >> ../output.rkt"
            roulette_cmd = "racket ../output.rkt"
            full_cmd = f"{add_header_cmd} && {append_output_cmd} && {roulette_cmd}"
            return self.run_command(full_cmd)
    
    def run_python_file(self, py_file):
        """Run a Python file (SPPL benchmark)"""
        cmd = f"python3 {py_file}"
        return self.run_command(cmd)
    
    def run_decision_tree_benchmarks(self):
        """Run decision tree benchmarks comparing Slice and SPPL"""
        print("Running decision tree benchmarks...", flush=True)
        print("-" * 50, flush=True)
        
        dt_dir = Path("benchmarks/decision-trees")
        
        for size in ["DT4", "DT14", "DT16", "DT44"]:
            size_dir = dt_dir / size
            if not size_dir.exists():
                continue
                
            for variant in ["ind", "bn1", "bn2"]:
                benchmark_name = f"{size}_{variant}"
                
                # Run Slice version
                slice_file = size_dir / f"{benchmark_name}.slice"
                if slice_file.exists():
                    if USE_DICE_BACKEND:
                        print(f"  Running {benchmark_name} (Slice / dice)...", flush=True)
                        timing = self.run_slice_on_file(slice_file, 'dice')
                        if timing:
                            self.results.append({
                                'benchmark': benchmark_name,
                                'category': 'decision_tree',
                                'size': size,
                                'variant': variant,
                                'tool': 'slice-dice',
                                'mean_time': timing['mean'],
                                'stdev': timing['stdev'],
                                'min_time': timing['min'],
                                'max_time': timing['max']
                            })
                            print(f"    Mean: {timing['mean']:.3f}s ± {timing['stdev']:.3f}s")
                    if USE_ROULETTE_BACKEND:
                        print(f"  Running {benchmark_name} (Slice / roulette)...", flush=True)
                        timing = self.run_slice_on_file(slice_file, 'roulette')
                        if timing:
                            self.results.append({
                                'benchmark': benchmark_name,
                                'category': 'decision_tree',
                                'size': size,
                                'variant': variant,
                                'tool': 'slice-roulette',
                                'mean_time': timing['mean'],
                                'stdev': timing['stdev'],
                                'min_time': timing['min'],
                                'max_time': timing['max']
                            })
                            print(f"    Mean: {timing['mean']:.3f}s ± {timing['stdev']:.3f}s")
                
                # Run SPPL version
                py_file = size_dir / f"{benchmark_name}.py"
                if py_file.exists():
                    print(f"  Running {benchmark_name} (SPPL)...", flush=True)
                    timing = self.run_python_file(py_file)
                    if timing:
                        self.results.append({
                            'benchmark': benchmark_name,
                            'category': 'decision_tree',
                            'size': size,
                            'variant': variant,
                            'tool': 'sppl',
                            'mean_time': timing['mean'],
                            'stdev': timing['stdev'],
                            'min_time': timing['min'],
                            'max_time': timing['max']
                        })
                        print(f"    Mean: {timing['mean']:.3f}s ± {timing['stdev']:.3f}s")
    
    def run_baseline_benchmarks(self):
        """Run baseline benchmarks comparing Slice and SPPL"""
        print("\nRunning baseline benchmarks...", flush=True)
        print("-" * 50, flush=True)
        
        baseline_dir = Path("benchmarks/baselines")
        
        for benchmark in ["clickgraph", "clinical_trial", "coin_bias", "trueskill"]:
            # Run Slice version
            slice_file = baseline_dir / f"{benchmark}.slice"
            if slice_file.exists():
                if USE_DICE_BACKEND:
                    print(f"  Running {benchmark} (Slice / dice)...", flush=True)
                    timing = self.run_slice_on_file(slice_file, 'dice')
                    if timing:
                        self.results.append({
                            'benchmark': benchmark,
                            'category': 'baseline',
                            'tool': 'slice-dice',
                            'mean_time': timing['mean'],
                            'stdev': timing['stdev'],
                            'min_time': timing['min'],
                            'max_time': timing['max']
                        })
                        print(f"    Mean: {timing['mean']:.3f}s ± {timing['stdev']:.3f}s")
                if USE_ROULETTE_BACKEND:
                    print(f"  Running {benchmark} (Slice / roulette)...", flush=True)
                    timing = self.run_slice_on_file(slice_file, 'roulette')
                    if timing:
                        self.results.append({
                            'benchmark': benchmark,
                            'category': 'baseline',
                            'tool': 'slice-roulette',
                            'mean_time': timing['mean'],
                            'stdev': timing['stdev'],
                            'min_time': timing['min'],
                            'max_time': timing['max']
                        })
                        print(f"    Mean: {timing['mean']:.3f}s ± {timing['stdev']:.3f}s")
            
            # Run SPPL version
            py_file = baseline_dir / f"{benchmark}.py"
            if py_file.exists():
                print(f"  Running {benchmark} (SPPL)...", flush=True)
                timing = self.run_python_file(py_file)
                if timing:
                    self.results.append({
                        'benchmark': benchmark,
                        'category': 'baseline',
                        'tool': 'sppl',
                        'mean_time': timing['mean'],
                        'stdev': timing['stdev'],
                        'min_time': timing['min'],
                        'max_time': timing['max']
                    })
                    print(f"    Mean: {timing['mean']:.3f}s ± {timing['stdev']:.3f}s")
    
    def run_scaling_benchmarks(self):
        """Run scaling benchmarks"""
        print("\nRunning scaling benchmarks...", flush=True)
        print("-" * 50, flush=True)
        
        scaling_dir = Path("benchmarks/scaling")
        if (scaling_dir / "run.py").exists():
            print("  Running scaling experiments...", flush=True)
            if USE_DICE_BACKEND and USE_ROULETTE_BACKEND:
                # The scaling runner creates its own graphs
                cmd = f"cd {scaling_dir} && python3 -u run.py"
                # Use subprocess.Popen for real-time output
                process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, 
                                        universal_newlines=True, bufsize=1)
                for line in process.stdout:
                    print(f"    {line.rstrip()}", flush=True)
                process.wait()
                print("  Scaling graphs saved to images/scaling/", flush=True)
            elif USE_DICE_BACKEND and not USE_ROULETTE_BACKEND:
                # The scaling runner creates its own graphs
                cmd = f"cd {scaling_dir} && python3 -u run.py --dice"
                # Use subprocess.Popen for real-time output
                process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, 
                                        universal_newlines=True, bufsize=1)
                for line in process.stdout:
                    print(f"    {line.rstrip()}", flush=True)
                process.wait()
                print("  Scaling graphs saved to images/scaling/", flush=True)
            else:
                # The scaling runner creates its own graphs
                cmd = f"cd {scaling_dir} && python3 -u run.py --roulette"
                # Use subprocess.Popen for real-time output
                process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, 
                                        universal_newlines=True, bufsize=1)
                for line in process.stdout:
                    print(f"    {line.rstrip()}", flush=True)
                process.wait()
                print("  Scaling graphs saved to images/scaling/", flush=True)

    
    def calculate_speedups(self):
        """Calculate speedups for decision tree benchmarks"""
        print("\nCalculating speedups...")
        print("-" * 50)
        
        # Group results by benchmark
        benchmarks = {}
        for r in self.results:
            if r['category'] == 'decision_tree':
                key = r['benchmark']
                if key not in benchmarks:
                    benchmarks[key] = {}
                benchmarks[key][r['tool']] = r['mean_time']
        
        # Calculate speedups
        speedups = []
        for benchmark, times in sorted(benchmarks.items()):
            if 'slice' in times and 'sppl' in times:
                speedup = times['sppl'] / times['slice']
                speedups.append({
                    'benchmark': benchmark,
                    'slice_time': times['slice'],
                    'sppl_time': times['sppl'],
                    'speedup': speedup
                })
                print(f"  {benchmark}: {speedup:.1f}x speedup")
        
        return speedups
    
    def save_results(self):
        """Save results to CSV and JSON"""
        # Save detailed results
        csv_file = self.output_dir / "benchmark_results.csv"
        if self.results:
            with open(csv_file, 'w', newline='') as f:
                writer = csv.DictWriter(f, fieldnames=self.results[0].keys())
                writer.writeheader()
                writer.writerows(self.results)
            print(f"\nDetailed results saved to: {csv_file}")
        
        # Save speedup summary
        speedups = self.calculate_speedups()
        if speedups:
            speedup_file = self.output_dir / "speedup_summary.csv"
            with open(speedup_file, 'w', newline='') as f:
                writer = csv.DictWriter(f, fieldnames=speedups[0].keys())
                writer.writeheader()
                writer.writerows(speedups)
            print(f"Speedup summary saved to: {speedup_file}")
        
        # Save everything as JSON
        json_file = self.output_dir / "all_results.json"
        with open(json_file, 'w') as f:
            json.dump({
                'timestamp': datetime.now().isoformat(),
                'num_runs': self.num_runs,
                'results': self.results,
                'speedups': speedups
            }, f, indent=2)
        print(f"All results saved to: {json_file}")
    
    def generate_latex_table(self):
        """Generate LaTeX table for the paper"""
        speedups = self.calculate_speedups()
        if not speedups:
            return
            
        latex_file = self.output_dir / "results_table.tex"
        with open(latex_file, 'w') as f:
            f.write("\\begin{tabular}{lrrr}\n")
            f.write("\\toprule\n")
            f.write("Benchmark & Slice (s) & SPPL (s) & Speedup \\\\\n")
            f.write("\\midrule\n")
            
            for s in speedups:
                f.write(f"{s['benchmark']} & "
                       f"{s['slice_time']:.3f} & "
                       f"{s['sppl_time']:.3f} & "
                       f"{s['speedup']:.1f}× \\\\\n")
            
            f.write("\\bottomrule\n")
            f.write("\\end{tabular}\n")
        
        print(f"LaTeX table saved to: {latex_file}")
    
    def run_all(self):
        """Run all benchmarks"""
        print("Slice Artifact Evaluation - Benchmark Runner")
        print("=" * 50)
        print(f"Running each benchmark {self.num_runs} times")
        print(f"Results will be saved to: {self.output_dir}/")
        print()
        
        self.run_decision_tree_benchmarks()
        self.run_scaling_benchmarks()
        
        self.save_results()
        self.generate_latex_table()
        
        print("\nBenchmark run complete!")


class SliceCLI:
    """Main CLI handler for Slice"""
    
    def __init__(self):
        self.verbose = False
        self.quiet = False
        self.opam_switch = "4.14.1"
        self.setup_environment()
    
    def setup_environment(self):
        """Set up OCaml environment"""
        # Get opam environment variables
        try:
            result = subprocess.run(
                f"opam env --switch={self.opam_switch} --set-switch",
                shell=True,
                capture_output=True,
                text=True
            )
            if result.returncode == 0:
                # Parse and set environment variables
                for line in result.stdout.strip().split('\n'):
                    if line.startswith('export'):
                        # Parse export FOO=bar or export FOO="bar"
                        parts = line[7:].split('=', 1)
                        if len(parts) == 2:
                            key = parts[0]
                            value = parts[1].strip('"').strip("'")
                            os.environ[key] = value
        except Exception as e:
            if self.verbose:
                print(f"Warning: Could not set up opam environment: {e}")
    
    def run_command(self, cmd, cwd=None, check=True):
        """Run a shell command and return result"""
        if self.verbose and not self.quiet:
            print(f"Running: {cmd}")
        
        result = subprocess.run(
            cmd,
            shell=True,
            capture_output=True,
            text=True,
            cwd=cwd
        )
        
        if check and result.returncode != 0:
            print(f"Error: Command failed with return code {result.returncode}")
            if result.stderr:
                print(f"stderr: {result.stderr}")
            sys.exit(1)
        
        return result
    
    def cmd_run(self, args):
        """Execute a .slice file through the full pipeline"""
        global USE_DICE_BACKEND, USE_ROULETTE_BACKEND
        
        # Update global backend flags based on command line args
        if args.dice:
            USE_DICE_BACKEND = True
            USE_ROULETTE_BACKEND = False
        elif args.roulette:
            USE_DICE_BACKEND = False
            USE_ROULETTE_BACKEND = True
        # If neither flag is specified, keep both enabled (default)
        
        if not os.path.exists(args.file):
            print(f"Error: File '{args.file}' not found")
            sys.exit(1)
        
        file_abs = os.path.abspath(args.file)
        
        # Run slice
        if not self.quiet:
            print(f"Running Slice on {args.file}...")
        
        if USE_DICE_BACKEND:
            # Check if we're in Docker (slice binary is in PATH)
            slice_binary = "main.exe"
            if subprocess.run("which main.exe", shell=True, capture_output=True).returncode != 0:
                # Not in PATH, use local build
                slice_binary = "./slice/_build/default/bin/main.exe"
            
            slice_cmd = f"{slice_binary} '{file_abs}'"
            if args.print_all:
                slice_cmd = f"{slice_binary} --print-all '{file_abs}'"
            
            if args.to_sppl:
                slice_cmd = f"{slice_binary} --to-sppl '{file_abs}'"
                result = self.run_command(slice_cmd)
                print(result.stdout)
                return
            
            result = self.run_command(slice_cmd)
            slice_output = result.stdout
            
            if args.no_dice:
                print(slice_output)
                return
            
            # Save slice output for dice
            with open("output.dice", "w") as f:
                f.write(slice_output)
            
            # Run dice
            if not self.quiet:
                print("Running Dice inference...")
            
            # Check if we're in Docker (dice binary is in PATH)
            dice_binary = "dice.exe"
            if subprocess.run("which dice.exe", shell=True, capture_output=True).returncode != 0:
                # Not in PATH, use local build
                dice_binary = "./_build/default/bin/dice.exe"
                dice_cmd = f"cd dice && {dice_binary} '../output.dice' -show-size -flip-lifting"
                if args.dice_args:
                    dice_cmd = f"cd dice && {dice_binary} '../output.dice' {args.dice_args}"
            else:
                dice_cmd = f"{dice_binary} 'output.dice' -show-size -flip-lifting"
                if args.dice_args:
                    dice_cmd = f"{dice_binary} 'output.dice' {args.dice_args}"
            
            result = self.run_command(dice_cmd)
            
            if args.output:
                with open(args.output, "w") as f:
                    f.write(result.stdout)
                if not self.quiet:
                    print(f"Output saved to {args.output}")
            else:
                print(result.stdout)

        if USE_ROULETTE_BACKEND:
            # Check if we're in Docker (slice binary is in PATH)
            slice_binary = "main.exe"
            if subprocess.run("which main.exe", shell=True, capture_output=True).returncode != 0:
                # Not in PATH, use local build
                slice_binary = "./slice/_build/default/bin/main.exe --backend roulette"
            
            slice_cmd = f"{slice_binary} '{file_abs}'"
            if args.print_all:
                slice_cmd = f"{slice_binary} --print-all '{file_abs}'"
            
            if args.to_sppl:
                slice_cmd = f"{slice_binary} --to-sppl '{file_abs}'"
                result = self.run_command(slice_cmd)
                print(result.stdout)
                return
            
            result = self.run_command(slice_cmd)
            slice_output = result.stdout
            slice_output = "#lang roulette/example/disrupt\n" + result.stdout
            
            if args.no_dice:
                print(slice_output)
                return
            
            # Save slice output for roulette
            with open("output.rkt", "w") as f:
                f.write(slice_output)
            
            # Run dice
            if not self.quiet:
                print("Running Roulette inference...")
            
            roulette_cmd = "racket output.rkt"
            result = self.run_command(roulette_cmd)
            
            if args.output:
                with open(args.output, "w") as f:
                    f.write(result.stdout)
                if not self.quiet:
                    print(f"Output saved to {args.output}")
            else:
                print(result.stdout)

    
    def cmd_test(self, args):
        """Test installation of components"""
        print("=== Slice Installation Test ===\n")
        
        components = ['slice', 'dice', 'roulette', 'python', 'hyperfine']
        if args.component and args.component != 'all':
            components = [args.component]
        
        all_passed = True
        
        # Test Slice
        if 'slice' in components:
            print("1. Testing Slice...")
            result = self.run_command(
                "cd slice && dune exec -- bin/main.exe examples/coin.slice",
                check=False
            )
            if result.returncode == 0:
                print("   ✓ Slice is working")
            else:
                print("   ✗ Slice failed")
                if args.verbose:
                    print(f"   Error: {result.stderr}")
                all_passed = False
        
        # Test Dice
        if 'dice' in components:
            print("2. Testing Dice...")
            result = self.run_command(
                "cd dice && dune exec dice resources/example.dice",
                check=False
            )
            if result.returncode == 0:
                print("   ✓ Dice is working")
            else:
                print("   ✗ Dice failed")
                if args.verbose:
                    print(f"   Error: {result.stderr}")
                all_passed = False

        # Test Roulette
        if 'roulette' in components:
            print("3. Testing Roulette...")
            tmp_path = "tmp.rkt"
            with open(tmp_path, "w") as f:
                f.write("#lang roulette/example/disrupt")
            try:
                result = self.run_command(
                    f"racket {tmp_path}",
                    check=False
                )
            finally:
                os.remove(tmp_path)

            if result.returncode == 0:
                print("   ✓ Roulette is working")
            else:
                print("   ✗ Roulette failed")
                if args.verbose:
                    print(f"   Error: {result.stderr}")
                all_passed = False
        
        # Test Python packages
        if 'python' in components:
            print("4. Testing Python environment...")
            result = self.run_command(
                'python3 -c "import numpy, scipy, matplotlib, pandas, sppl"',
                check=False
            )
            if result.returncode == 0:
                print("   ✓ Python packages installed")
            else:
                print("   ✗ Python packages missing")
                if args.verbose:
                    print(f"   Error: {result.stderr}")
                all_passed = False
        
        # Test hyperfine
        if 'hyperfine' in components:
            print("5. Testing hyperfine...")
            result = self.run_command("which hyperfine", check=False)
            if result.returncode == 0:
                print("   ✓ hyperfine is available")
            else:
                print("   ✗ hyperfine not found")
                all_passed = False
        
        print()
        if all_passed:
            print("All tests passed! ✓")
        else:
            print("Some tests failed. ✗")
            sys.exit(1)
    
    def cmd_benchmark(self, args):
        """Run benchmarks"""
        output_dir = Path(args.output_dir)
        output_dir.mkdir(exist_ok=True)
        
        if args.type in ['all', 'decision-trees', 'dt']:
            if not self.quiet:
                print("Running decision tree benchmarks...")
            
            if args.hyperfine:
                # Use hyperfine for benchmarking
                self.run_hyperfine_benchmarks(output_dir)
            else:
                # Use integrated benchmark runner
                runner = BenchmarkRunner(str(output_dir), timeout=args.timeout if args.timeout else 300)
                runner.num_runs = args.runs
                runner.run_decision_tree_benchmarks()
                runner.save_results()
                
                if args.format == 'latex':
                    runner.generate_latex_table()
        
        if args.type in ['all', 'baseline']:
            if not self.quiet:
                print("\nRunning baseline benchmarks...")
            
            if args.hyperfine:
                # Use hyperfine for benchmarking
                self.run_hyperfine_benchmarks(output_dir)
            else:
                # Use integrated benchmark runner
                runner = BenchmarkRunner(str(output_dir), timeout=args.timeout if args.timeout else 300)
                runner.num_runs = args.runs
                runner.run_baseline_benchmarks()
                runner.save_results()
                
                if args.format == 'latex':
                    runner.generate_latex_table()
        
        if args.type in ['all', 'scaling']:
            if not self.quiet:
                print("\nRunning scaling benchmarks...", flush=True)
            # Use subprocess with real-time output
            cmd = f"python3 -u run.py"
            if args.timeout:
                cmd += f" --timeout {args.timeout}"
            try:
                process = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                         universal_newlines=True, bufsize=1, cwd="benchmarks/scaling")
                
                # Read output line by line without overall timeout
                while True:
                    line = process.stdout.readline()
                    if not line and process.poll() is not None:
                        break
                    if line:
                        print(line.rstrip(), flush=True)
                
                process.wait()
            except Exception as e:
                print(f"Error running scaling benchmarks: {e}", flush=True)
        
        if not self.quiet:
            print(f"\nBenchmark results saved to {output_dir}/")
    
    def run_hyperfine_benchmarks(self, output_dir):
        """Run decision tree benchmarks using hyperfine"""
        results_file = output_dir / "hyperfine_results.md"
        cmd = f"hyperfine -w3 --export-markdown {results_file}"
        
        # Add benchmark commands
        for size in ["DT4", "DT14", "DT16", "DT44"]:
            dt_dir = Path(f"benchmarks/decision-trees/{size}")
            if dt_dir.exists():
                # SPPL benchmarks
                for py_file in dt_dir.glob("*.py"):
                    name = py_file.stem
                    cmd += f' --command-name "{name} (sppl)" "python3 {py_file}"'
                
                # Slice benchmarks
                for slice_file in dt_dir.glob("*.slice"):
                    name = slice_file.stem
                    cmd += f' --command-name "{name} (slice)" "./run_slice.sh {slice_file}"'
        
        self.run_command(cmd)
        print(f"Hyperfine results saved to {results_file}")
    
    def cmd_build(self, args):
        """Build components"""
        components = ['slice', 'dice'] if args.component == 'all' else [args.component]
        
        for component in components:
            if not self.quiet:
                print(f"Building {component}...")
            
            build_cmd = f"cd {component} && dune build"
            if args.clean:
                self.run_command(f"cd {component} && dune clean")
            
            self.run_command(build_cmd)
            if not self.quiet:
                print(f"✓ {component} built successfully")
    
    def cmd_convert(self, args):
        """Convert between formats"""
        if not os.path.exists(args.input):
            print(f"Error: Input file '{args.input}' not found")
            sys.exit(1)
        
        input_abs = os.path.abspath(args.input)
        
        if args.to == 'sppl':
            cmd = f"cd slice && ./_build/default/bin/main.exe --to-sppl '{input_abs}'"
            result = self.run_command(cmd)
            
            if args.output:
                with open(args.output, 'w') as f:
                    f.write(result.stdout)
                if not self.quiet:
                    print(f"Converted output saved to {args.output}")
            else:
                print(result.stdout)
        else:
            print(f"Error: Conversion to '{args.to}' not yet implemented")
            sys.exit(1)
    
    def cmd_examples(self, args):
        """List or run examples"""
        examples_dir = Path("examples")
        
        if args.list:
            print("Available examples:")
            for category in ['tutorial', 'paper', 'features', 'tests']:
                cat_dir = examples_dir / category
                if cat_dir.exists():
                    files = list(cat_dir.glob("*.slice"))
                    if files:
                        print(f"\n{category}:")
                        for f in sorted(files):
                            print(f"  {f.relative_to(examples_dir)}")
        
        elif args.run:
            # Find the example file
            example_path = None
            for category in ['', 'tutorial', 'paper', 'features', 'tests']:
                for suffix in ['', '.slice']:
                    if category:
                        path = examples_dir / category / f"{args.run}{suffix}"
                    else:
                        path = examples_dir / f"{args.run}{suffix}"
                    if path.exists() and path.is_file():
                        example_path = path
                        break
                if example_path:
                    break
            
            if not example_path:
                print(f"Error: Example '{args.run}' not found")
                sys.exit(1)
            
            # Run the example
            if not self.quiet:
                print(f"Running example: {example_path}")
            self.cmd_run(argparse.Namespace(
                file=str(example_path),
                output=None,
                no_dice=False,
                print_all=False,
                to_sppl=False,
                dice_args=None
            ))


def main():
    parser = argparse.ArgumentParser(
        description="Slice - Unified CLI for probabilistic programming",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python3 slice.py run examples/coin.slice                      # Run with both backends
  python3 slice.py --dice run examples/coin.slice               # Run with Dice backend only
  python3 slice.py --roulette run examples/coin.slice           # Run with Roulette backend only
  python3 slice.py test                                         # Test installation
  python3 slice.py benchmark                                    # Run all benchmarks
  python3 slice.py examples --list                              # List examples
  python3 slice.py run file.slice --to-sppl                     # Convert to SPPL
        """
    )
    
    # Global options
    parser.add_argument('--verbose', '-v', action='store_true', help='Enable verbose output')
    parser.add_argument('--quiet', '-q', action='store_true', help='Suppress non-essential output')
    parser.add_argument('--opam-switch', default='4.14.1', help='OCaml switch to use')
    parser.add_argument('--dice', action='store_true', help='Use only Dice backend (disables Roulette)')
    parser.add_argument('--roulette', action='store_true', help='Use only Roulette backend (disables Dice)')
    
    # Subcommands
    subparsers = parser.add_subparsers(dest='command', help='Commands')
    
    # Run command
    run_parser = subparsers.add_parser('run', help='Execute a .slice file')
    run_parser.add_argument('file', help='Path to .slice file')
    run_parser.add_argument('--output', '-o', help='Save output to file')
    run_parser.add_argument('--no-dice', action='store_true', help='Only run slice, skip dice')
    run_parser.add_argument('--print-all', action='store_true', help='Print all intermediate steps')
    run_parser.add_argument('--to-sppl', action='store_true', help='Convert to SPPL instead of running')
    run_parser.add_argument('--dice-args', help='Additional arguments for dice')
    
    # Test command
    test_parser = subparsers.add_parser('test', help='Test installation')
    test_parser.add_argument('--component', choices=['slice', 'dice', 'python', 'hyperfine', 'all'],
                           default='all', help='Component to test')
    
    # Benchmark command
    bench_parser = subparsers.add_parser('benchmark', help='Run benchmarks')
    bench_parser.add_argument('type', nargs='?', default='all',
                            choices=['all', 'decision-trees', 'dt', 'baseline', 'scaling'],
                            help='Type of benchmarks to run')
    bench_parser.add_argument('--output-dir', default='artifact_results', help='Output directory')
    bench_parser.add_argument('--runs', type=int, default=5, help='Number of runs per benchmark')
    bench_parser.add_argument('--format', choices=['csv', 'json', 'latex', 'markdown'],
                            default='csv', help='Output format')
    bench_parser.add_argument('--hyperfine', action='store_true', help='Use hyperfine for benchmarks')
    bench_parser.add_argument('--timeout', type=int, default=300, help='Timeout per benchmark in seconds (default: 300)')
    
    # Build command
    build_parser = subparsers.add_parser('build', help='Build components')
    build_parser.add_argument('component', nargs='?', default='all',
                            choices=['slice', 'dice', 'all'], help='Component to build')
    build_parser.add_argument('--clean', action='store_true', help='Clean before building')
    
    # Convert command
    convert_parser = subparsers.add_parser('convert', help='Convert between formats')
    convert_parser.add_argument('input', help='Input file')
    convert_parser.add_argument('--to', required=True, choices=['dice', 'sppl', 'julia'],
                              help='Target format')
    convert_parser.add_argument('--output', '-o', help='Output file')
    
    # Examples command
    examples_parser = subparsers.add_parser('examples', help='List or run examples')
    examples_parser.add_argument('--list', action='store_true', help='List available examples')
    examples_parser.add_argument('--run', help='Run a specific example')
    examples_parser.add_argument('--category', help='Filter by category')
    
    args = parser.parse_args()
    
    # Update global backend flags based on command line args
    global USE_DICE_BACKEND, USE_ROULETTE_BACKEND
    if args.dice:
        USE_DICE_BACKEND = True
        USE_ROULETTE_BACKEND = False
    elif args.roulette:
        USE_DICE_BACKEND = False
        USE_ROULETTE_BACKEND = True
    # If neither flag is specified, keep both enabled (default)
    
    # Default to help if no command
    if not args.command:
        parser.print_help()
        sys.exit(0)
    
    # Create CLI handler
    cli = SliceCLI()
    cli.verbose = args.verbose
    cli.quiet = args.quiet
    cli.opam_switch = args.opam_switch
    
    # Route to appropriate command
    if args.command == 'run':
        cli.cmd_run(args)
    elif args.command == 'test':
        cli.cmd_test(args)
    elif args.command == 'benchmark':
        cli.cmd_benchmark(args)
    elif args.command == 'build':
        cli.cmd_build(args)
    elif args.command == 'convert':
        cli.cmd_convert(args)
    elif args.command == 'examples':
        if not args.list and not args.run:
            args.list = True
        cli.cmd_examples(args)


if __name__ == '__main__':
    main()