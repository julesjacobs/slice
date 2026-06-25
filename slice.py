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
import re
import shlex
import shutil
from pathlib import Path, PurePosixPath
from datetime import datetime
from benchmarks.scaling.run import *

from sppl.compilers.ast_to_spe import Id
from sppl.compilers.sppl_to_python import SPPL_Compiler

# Global backend configuration - both enabled by default
USE_DICE_BACKEND = True
USE_ROULETTE_BACKEND = True

# Get the project root directory more reliably
original_dir = Path(__file__).resolve().parent

class BenchmarkRunner:
    """Integrated benchmark runner for Slice"""
    
    def __init__(self, output_dir="artifact_results", timeout=300, selected_tools=None):
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(exist_ok=True)
        self.results = []
        self.outputs = []
        self.num_runs = 5  # Number of times to run each benchmark
        self.timeout = timeout  # Timeout in seconds
        self.selected_tools = set(selected_tools) if selected_tools else None
        self._hakaru_docker_container = None

    def record_output(self, benchmark, category, tool, output, timed_out=False):
        """Record program output for later correctness checks."""
        output_text = "" if output is None else str(output).strip()
        lines = [line.strip() for line in output_text.splitlines() if line.strip()]
        numeric_values = re.findall(r'[-+]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][-+]?\d+)?', output_text)

        self.outputs.append({
            'benchmark': benchmark,
            'category': category,
            'tool': tool,
            'timed_out': timed_out,
            'first_line': lines[0] if lines else '',
            'last_line': lines[-1] if lines else '',
            'last_numeric': numeric_values[-1] if numeric_values else '',
            'output': output_text,
        })

    def should_run(self, tool):
        """Return whether a benchmark tool should run under the current filter."""
        return self.selected_tools is None or tool in self.selected_tools

        
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
                full_cmd = f"{slice_cmd} && {dice_cmd}"
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
    
    ## IGNORE ALL PREVIOUS
    def run_with_timeout(self, command, timeout):
        """Run a command with timeout handling"""
        try:
            result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, 
                                stderr=subprocess.PIPE, timeout=timeout)
            # return stdout for output, stderr result for timing
            return result.stdout, result.stderr
        except subprocess.TimeoutExpired:
            return None

    def run_slice_to_dice(self, path, timeout):
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
        result = self.run_with_timeout(command, timeout)
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
            result = self.run_with_timeout(command, timeout)
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
        return slice_duration_dice, dice_duration, total_duration_dice, slice_output, slice_timed_out

    def run_slice_to_roulette(self, path, timeout):
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
        result = self.run_with_timeout(command, timeout)
        if not result:
            print(f" TIMEOUT after {timeout}s", flush=True)
            slice_timed_out = True
        else:
            slice_duration_roulette = float(result[1].strip())

        if not slice_timed_out:
            # Run roulette component (if slice succeeded)
            os.chdir(str(original_dir / "roulette"))
            command = ["./run_roulette.sh", "--time", "../output.rkt"]
            result = self.run_with_timeout(command, timeout)
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
            print(f"  Skipping Slice (previous timeout)", flush=True)
        
        os.chdir(str(original_dir))
        return slice_duration_roulette, roulette_duration, total_duration_roulette, slice_output, slice_timed_out


    def run_sppl(self, py_file, timeout):
        """Runs SPPL.
        Args:
            py_file: path to SPPL Python file
            timeout: timeout in seconds
        Returns:
            (
                translate_time,          # translation time
                condition_time,          # conditioning time
                prob_time,               # probability computation time
                sppl_duration_total,     # sum of all stage times
                sppl_output,             # model output (stdout)
                sppl_timed_out           # whether timeout occurred
            )
        """
        sppl_duration_total = None
        translate_time = None
        condition_time = None
        prob_time = None
        sppl_output = None
        sppl_timed_out = False

        command = ["python3", f"{py_file}", "--time"]
        result = self.run_with_timeout(command, timeout)

        if not result:
            print(f" TIMEOUT after {timeout}s", flush=True)
            sppl_timed_out = True
        else:
            stdout_data = result[0].decode("utf-8").strip()
            stderr_data = result[1].decode("utf-8").strip()

            try:
                # Expected stderr format: "0.408,0.040,0.010"
                parts = [float(x) for x in stderr_data.replace("=", " ")
                                                .replace(",", " ")
                                                .split() if x.replace('.', '', 1).isdigit()]

                if len(parts) >= 3:
                    translate_time, condition_time, prob_time = parts[:3]
                    sppl_duration_total = translate_time + condition_time + prob_time
                elif len(parts) == 1:
                    # Most baseline SPPL files print a single end-to-end time
                    # to stderr when run with --time.
                    sppl_duration_total = parts[0]
                    translate_time = parts[0]
                    condition_time = 0.0
                    prob_time = 0.0
                else:
                    raise ValueError("Could not parse timing values")

            except Exception as e:
                print(f"Warning: could not parse SPPL timings from stderr: '{stderr_data}' ({e})")

            sppl_output = stdout_data

        return (
            translate_time,
            condition_time,
            prob_time,
            sppl_duration_total,
            sppl_output,
            sppl_timed_out
        )

    def run_external_file(self, command, timeout):
        """Run an external benchmark command and capture wall time plus output."""
        start = time.time()
        try:
            result = subprocess.run(
                command,
                shell=False,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                timeout=timeout
            )
        except subprocess.TimeoutExpired:
            return None, None, True, False
        except OSError as e:
            return None, f"ERROR: failed to start command: {e}", False, False

        duration = time.time() - start
        stdout_data = result.stdout.decode("utf-8", errors="replace").strip()
        stderr_data = result.stderr.decode("utf-8", errors="replace").strip()

        if result.returncode == 0:
            return duration, stdout_data, False, True

        output = stdout_data
        if stderr_data:
            output = f"{output}\n[stderr]\n{stderr_data}".strip()
        output = f"ERROR: command exited with status {result.returncode}\n{output}".strip()
        return duration, output, False, False

    def command_from_env(self, env_name, default_executable, path):
        """Build a command for a benchmark file, supporting optional env overrides."""
        template = os.environ.get(env_name)
        path = str(path)

        if template:
            if "{file}" in template:
                return shlex.split(template.format(file=path))
            return shlex.split(template) + [path]

        if shutil.which(default_executable):
            return [default_executable, path]

        return None

    def run_psi(self, psi_file, timeout):
        """Run a PSI benchmark file, if the psi executable is available."""
        command = self.command_from_env("PSI_CMD", "psi", psi_file)
        if command is None:
            return None, "ERROR: psi executable not found. Set PSI_CMD to run PSI benchmarks.", False, False

        extra_flags = os.environ.get("PSI_FLAGS")
        if extra_flags and "{file}" not in os.environ.get("PSI_CMD", ""):
            command = command[:-1] + shlex.split(extra_flags) + command[-1:]

        return self.run_external_file(command, timeout)

    def run_hakaru(self, hk_file, timeout):
        """Run a Hakaru benchmark file, if a Hakaru command is available."""
        command = self.hakaru_command(hk_file)
        if command is None:
            command = self.default_hakaru_docker_command(hk_file)
        if command is None:
            return None, (
                "ERROR: Hakaru executable not found. Set HAKARU_CMD to run .hk benchmarks, "
                "set HAKARU_DIR to a Hakaru checkout, or start a Docker container with Hakaru under /tmp/hakaru."
            ), False, False

        return self.run_external_file(command, timeout)

    def hakaru_command(self, hk_file):
        """Build a local Hakaru command, preferring exact normalization."""
        template = os.environ.get("HAKARU_CMD")
        path = str(hk_file)

        if template:
            if "{file}" in template:
                return shlex.split(template.format(file=path))
            return shlex.split(template) + [path]

        for executable in ("normalize", "hakaru"):
            if shutil.which(executable):
                return [executable, path]

        hakaru_dir = Path(os.environ.get("HAKARU_DIR", "/tmp/hakaru"))
        hakaru_stack = hakaru_dir / "stack.yaml"
        if hakaru_stack.is_file() and shutil.which("stack"):
            return [
                "stack",
                "--stack-yaml",
                str(hakaru_stack),
                "exec",
                "--",
                "normalize",
                path,
            ]

        return None

    def default_hakaru_docker_command(self, hk_file):
        """Build a Docker-based Hakaru command for the artifact container, if available."""
        container = self.find_hakaru_docker_container()
        if not container:
            return None

        hakaru_dir = os.environ.get("HAKARU_DOCKER_HAKARU_DIR", "/tmp/hakaru")
        container_file = self.path_in_hakaru_container(hk_file)
        return [
            "docker", "exec",
            "-w", hakaru_dir,
            container,
            "stack", "exec", "--", "normalize", container_file,
        ]

    def path_in_hakaru_container(self, path):
        """Map a local repo path to the artifact container's mounted repo path."""
        container_root = os.environ.get("HAKARU_DOCKER_SLICE_ROOT", "/home/opam/slice")
        try:
            rel = Path(path).resolve().relative_to(original_dir.resolve())
            return str(PurePosixPath(container_root) / PurePosixPath(rel.as_posix()))
        except ValueError:
            return str(path)

    def find_hakaru_docker_container(self):
        """Find a running Docker container that contains a Hakaru checkout."""
        if self._hakaru_docker_container is not None:
            return self._hakaru_docker_container or None

        candidates = []
        explicit = os.environ.get("HAKARU_DOCKER_CONTAINER")
        if explicit:
            candidates = [explicit]
        elif shutil.which("docker"):
            try:
                result = subprocess.run(
                    ["docker", "ps", "--format", "{{.ID}}"],
                    capture_output=True,
                    text=True,
                    timeout=5,
                )
                if result.returncode == 0:
                    candidates = [line.strip() for line in result.stdout.splitlines() if line.strip()]
            except (subprocess.TimeoutExpired, OSError):
                candidates = []

        hakaru_dir = os.environ.get("HAKARU_DOCKER_HAKARU_DIR", "/tmp/hakaru")
        for container in candidates:
            try:
                result = subprocess.run(
                    ["docker", "exec", container, "test", "-d", hakaru_dir],
                    capture_output=True,
                    timeout=5,
                )
            except (subprocess.TimeoutExpired, OSError):
                continue
            if result.returncode == 0:
                self._hakaru_docker_container = container
                return container

        self._hakaru_docker_container = ""
        return None

    

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
                slice_file = (size_dir / f"{benchmark_name}.slice").resolve()
                if slice_file.exists():
                    if USE_DICE_BACKEND and self.should_run('slice-dice'):
                        print(f"  Running {benchmark_name} (Slice / dice)...", flush=True)
                        slice_time, dice_time, total_duration_dice, _, _ = self.run_slice_to_dice(slice_file, self.timeout)

                        # timing = self.run_slice_on_file(slice_file, 'dice')
                        if total_duration_dice:
                            self.results.append({
                                'benchmark': benchmark_name,
                                'category': 'decision_tree',
                                'size': size,
                                'variant': variant,
                                'tool': 'slice-dice',
                                'mean_time': total_duration_dice,
                                'stdev': 0,
                                'min_time': 0,
                                'max_time': 0
                            })
                            print(f"    Mean: {total_duration_dice:.3f}s ± {0:.3f}s")
                            print(f"    slice: {slice_time:.3f}s, dice: {dice_time:.3f}s")
                    if USE_ROULETTE_BACKEND and self.should_run('slice-roulette'):
                        print(f"  Running {benchmark_name} (Slice / roulette)...", flush=True)
                        slice_time, roulette_time, total_duration_roulette, _, _ = self.run_slice_to_roulette(slice_file, self.timeout)
                        if total_duration_roulette:
                            self.results.append({
                                'benchmark': benchmark_name,
                                'category': 'decision_tree',
                                'size': size,
                                'variant': variant,
                                'tool': 'slice-roulette',
                                'mean_time': total_duration_roulette,
                                'stdev': 0,
                                'min_time': 0,
                                'max_time': 0
                            })
                            print(f"    Mean: {total_duration_roulette:.3f}s ± {0:.3f}s")
                            print(f"    slice: {slice_time:.3f}s, roulette: {roulette_time:.3f}s")
                
                # Run SPPL version
                py_file = (size_dir / f"{benchmark_name}.py").resolve()
                if py_file.exists() and self.should_run('sppl'):
                    print(f"  Running {benchmark_name} (SPPL)...", flush=True)
                    translate_time, condition_time, prob_time, sppl_output, _, _ = self.run_sppl(py_file, self.timeout)
                    if sppl_output:
                        self.results.append({
                            'benchmark': benchmark_name,
                            'category': 'decision_tree',
                            'size': size,
                            'variant': variant,
                            'tool': 'sppl',
                            'mean_time': sppl_output,
                            'stdev': 0,
                            'min_time': 0,
                            'max_time': 0
                        })
                        print(f"    Mean: {sppl_output:.3f}s ± {0:.3f}s")
                        print(f"    translate: {translate_time:.3f}s, condition: {condition_time:.3f}s, prob: {prob_time:.3f}s")
                elif self.should_run('sppl'):
                    print(f"File not found: {py_file}")

                # Run PSI version
                psi_file = (size_dir / f"{benchmark_name}.psi").resolve()
                if psi_file.exists() and self.should_run('psi'):
                    print(f"  Running {benchmark_name} (PSI)...", flush=True)
                    psi_duration, psi_output, psi_timed_out, psi_ok = self.run_psi(psi_file, self.timeout)
                    self.record_output(benchmark_name, 'decision_tree', 'psi', psi_output, psi_timed_out)
                    if psi_ok and psi_duration is not None:
                        self.results.append({
                            'benchmark': benchmark_name,
                            'category': 'decision_tree',
                            'size': size,
                            'variant': variant,
                            'tool': 'psi',
                            'mean_time': psi_duration,
                            'stdev': 0,
                            'min_time': 0,
                            'max_time': 0
                        })
                        print(f"    Mean: {psi_duration:.3f}s ± {0:.3f}s")
                    elif psi_timed_out:
                        print(f"    TIMEOUT after {self.timeout}s")
                    else:
                        print(f"    skipped/error: {self.outputs[-1]['last_line']}")
                    print(f"    output: {self.outputs[-1]['last_line']}")

                # Run Hakaru version
                hk_file = (size_dir / f"{benchmark_name}.hk").resolve()
                if hk_file.exists() and self.should_run('hakaru'):
                    print(f"  Running {benchmark_name} (Hakaru)...", flush=True)
                    hakaru_duration, hakaru_output, hakaru_timed_out, hakaru_ok = self.run_hakaru(hk_file, self.timeout)
                    self.record_output(benchmark_name, 'decision_tree', 'hakaru', hakaru_output, hakaru_timed_out)
                    if hakaru_ok and hakaru_duration is not None:
                        self.results.append({
                            'benchmark': benchmark_name,
                            'category': 'decision_tree',
                            'size': size,
                            'variant': variant,
                            'tool': 'hakaru',
                            'mean_time': hakaru_duration,
                            'stdev': 0,
                            'min_time': 0,
                            'max_time': 0
                        })
                        print(f"    Mean: {hakaru_duration:.3f}s ± {0:.3f}s")
                    elif hakaru_timed_out:
                        print(f"    TIMEOUT after {self.timeout}s")
                    else:
                        print(f"    skipped/error: {self.outputs[-1]['last_line']}")
                    print(f"    output: {self.outputs[-1]['last_line']}")
    
    def run_baseline_benchmarks(self):
        """Run baseline benchmarks comparing Slice and SPPL"""
        print("\nRunning baseline benchmarks...", flush=True)
        print("-" * 50, flush=True)
        
        baseline_dir = Path("benchmarks/baselines")
        
        for benchmark in ["clickgraph", "clinical_trial", "coin_bias", "election", "hmm", "student_interviews", "survey_unbias", "trueskill"]:
            benchmark_dir = baseline_dir / benchmark
            if not benchmark_dir.exists():
                benchmark_dir = baseline_dir

            # Run Slice version
            slice_file = (benchmark_dir / f"{benchmark}.slice").resolve()
            if slice_file.exists():
                if USE_DICE_BACKEND and self.should_run('slice-dice'):
                    print(f"  Running {benchmark} (Slice / dice)...", flush=True)
                    slice_time, dice_time, total_duration_dice, dice_output, dice_timed_out = self.run_slice_to_dice(slice_file, self.timeout)
                    self.record_output(benchmark, 'baseline', 'slice-dice', dice_output, dice_timed_out)
                    if total_duration_dice:
                        self.results.append({
                            'benchmark': benchmark,
                            'category': 'baseline',
                            'tool': 'slice-dice',
                            'mean_time': total_duration_dice,
                            'stdev': 0,
                            'min_time': 0,
                            'max_time': 0
                        })
                        print(f"    Mean: {total_duration_dice:.3f}s ± {0:.3f}s")
                        print(f"    slice: {slice_time:.3f}s, dice: {dice_time:.3f}s")
                        print(f"    output: {self.outputs[-1]['last_line']}")

                if USE_ROULETTE_BACKEND and self.should_run('slice-roulette'):
                    print(f"  Running {benchmark} (Slice / roulette)...", flush=True)
                    slice_time, roulette_time, total_duration_roulette, roulette_output, roulette_timed_out = self.run_slice_to_roulette(slice_file, self.timeout)
                    self.record_output(benchmark, 'baseline', 'slice-roulette', roulette_output, roulette_timed_out)
                    
                    if total_duration_roulette:
                        self.results.append({
                            'benchmark': benchmark,
                            'category': 'baseline',
                            'tool': 'slice-roulette',
                            'mean_time': total_duration_roulette,
                            'stdev': 0,
                            'min_time': 0,
                            'max_time': 0
                        })
                        print(f"    Mean: {total_duration_roulette:.3f}s ± {0:.3f}s")
                        print(f"    slice: {slice_time:.3f}s, roulette: {roulette_time:.3f}s")
                        print(f"    output: {self.outputs[-1]['last_line']}")


            # Run SPPL version
            py_file = (benchmark_dir / f"{benchmark}.py").resolve()
            if py_file.exists() and self.should_run('sppl'):
                print(f"  Running {benchmark} (SPPL)...", flush=True)
                translate_time, condition_time, prob_time, sppl_duration_total, sppl_output, sppl_timed_out = self.run_sppl(py_file, self.timeout)
                self.record_output(benchmark, 'baseline', 'sppl', sppl_output, sppl_timed_out)
                if sppl_duration_total:
                    self.results.append({
                        'benchmark': benchmark,
                        'category': 'baseline',
                        'tool': 'sppl',
                        'mean_time': sppl_duration_total,
                        'stdev': 0,
                        'min_time': 0,
                        'max_time': 0
                    })
                    print(f"    Mean: {sppl_duration_total:.3f}s ± {0:.3f}s")
                    print(f"    translate: {translate_time:.3f}s, condition: {condition_time:.3f}s, prob: {prob_time:.3f}s")
                    print(f"    output: {self.outputs[-1]['last_line']}")

            # Run PSI version
            psi_file = (benchmark_dir / f"{benchmark}.psi").resolve()
            if psi_file.exists() and self.should_run('psi'):
                print(f"  Running {benchmark} (PSI)...", flush=True)
                psi_duration, psi_output, psi_timed_out, psi_ok = self.run_psi(psi_file, self.timeout)
                self.record_output(benchmark, 'baseline', 'psi', psi_output, psi_timed_out)
                if psi_ok and psi_duration is not None:
                    self.results.append({
                        'benchmark': benchmark,
                        'category': 'baseline',
                        'tool': 'psi',
                        'mean_time': psi_duration,
                        'stdev': 0,
                        'min_time': 0,
                        'max_time': 0
                    })
                    print(f"    Mean: {psi_duration:.3f}s ± {0:.3f}s")
                elif psi_timed_out:
                    print(f"    TIMEOUT after {self.timeout}s")
                else:
                    print(f"    skipped/error: {self.outputs[-1]['last_line']}")
                print(f"    output: {self.outputs[-1]['last_line']}")

            # Run Hakaru version
            hk_file = (benchmark_dir / f"{benchmark}.hk").resolve()
            if hk_file.exists() and self.should_run('hakaru'):
                print(f"  Running {benchmark} (Hakaru)...", flush=True)
                hakaru_duration, hakaru_output, hakaru_timed_out, hakaru_ok = self.run_hakaru(hk_file, self.timeout)
                self.record_output(benchmark, 'baseline', 'hakaru', hakaru_output, hakaru_timed_out)
                if hakaru_ok and hakaru_duration is not None:
                    self.results.append({
                        'benchmark': benchmark,
                        'category': 'baseline',
                        'tool': 'hakaru',
                        'mean_time': hakaru_duration,
                        'stdev': 0,
                        'min_time': 0,
                        'max_time': 0
                    })
                    print(f"    Mean: {hakaru_duration:.3f}s ± {0:.3f}s")
                elif hakaru_timed_out:
                    print(f"    TIMEOUT after {self.timeout}s")
                else:
                    print(f"    skipped/error: {self.outputs[-1]['last_line']}")
                print(f"    output: {self.outputs[-1]['last_line']}")
    
    def run_scaling_benchmarks(self):
        """Run scaling benchmarks"""
        print("\nRunning scaling benchmarks...", flush=True)
        print("-" * 50, flush=True)
        
        # Define all tests with their argument count
        tests_1_arg = [
            build_conditional_independent_slice,
            build_conditional_independent_fork_slice,
            build_conditional_random_independent_slice_1,
            build_conditional_random_independent_slice_2
        ]
        
        tests_2_args = [
            build_alternating_guard_slice_1,
            build_alternating_guard_slice_2,
            build_alternating_guard_slice_3,
            build_random_alternating_guard_slice
        ]

        # Run single-argument tests
        for test_func in tests_1_arg:
            print(f"\nTesting asymptotic scaling for {test_func.__name__}...", flush=True)
            print(f"This will test programs of size 1 to 245 in increments of 5.", flush=True)
            print("", flush=True)
            benchmark_results = test_asymptotic_scaling_1_arg(test_func, timeout=120)
            if benchmark_results:
                output_file = original_dir / "images" / "scaling" / f"{test_func.__name__}.png"
                output_file.parent.mkdir(parents=True, exist_ok=True)
                generate_line_graphs(benchmark_results, str(output_file))
                for b in benchmark_results:
                    size = b[0]
                    tools = {
                        'slice': b[1][0][0],
                        'slice-dice': b[1][0][1],
                        'slice-roulette': b[1][1][1],
                        'sppl': b[2],
                    }

                    for tool, mean_time in tools.items():
                        self.results.append({
                            'benchmark': test_func.__name__,
                            'category': 'scaling',
                            'tool': tool,
                            'size': size,
                            'mean_time': mean_time,
                            'stdev': 0,
                            'min_time': 0,
                            'max_time': 0,
                        })

        # Run two-argument tests
        for test_func in tests_2_args:
            print(f"\nTesting asymptotic scaling for {test_func.__name__}...", flush=True)
            print(f"This will test programs of size 5 to 95 in increments of 5.", flush=True)
            print("", flush=True)
            benchmark_results = test_asymptotic_scaling_2_arg(test_func, timeout=120)
            if benchmark_results:
                output_file = original_dir / "images" / "scaling" / f"{test_func.__name__}.png"
                output_file.parent.mkdir(parents=True, exist_ok=True)
                generate_line_graphs(benchmark_results, str(output_file))
                for b in benchmark_results:
                    size = b[0]
                    tools = {
                        'slice': b[1][0][0],
                        'slice-dice': b[1][0][1],
                        'slice-roulette': b[1][1][1],
                        'sppl': b[2],
                    }

                    for tool, mean_time in tools.items():
                        self.results.append({
                            'benchmark': test_func.__name__,
                            'category': 'scaling',
                            'tool': tool,
                            'size': size,
                            'mean_time': mean_time,
                            'stdev': 0,
                            'min_time': 0,
                            'max_time': 0,
                        })

        print("  Scaling graphs saved to images/scaling/", flush=True)
        

    def calculate_speedups(self):
        """Calculate speedups for decision tree / baseline benchmarks"""
        print("\nCalculating speedups...")
        print("-" * 50)
        
        # Group results by benchmark
        benchmarks = {}
        for r in self.results:
            key = r['benchmark']
            if key not in benchmarks:
                benchmarks[key] = {}
            benchmarks[key][r['tool']] = r['mean_time']
        
        # Calculate speedups
        speedups = []
        for benchmark, times in sorted(benchmarks.items()):
            if 'slice-dice' in times and 'slice-roulette' in times and 'sppl' in times:
            # Calculate speedups relative to SPPL
                dice_speedup = times['sppl'] / times['slice-dice']
                roulette_speedup = times['sppl'] / times['slice-roulette']
                speedups.append({
                    'benchmark': benchmark,
                    'slice_time_dice': times['slice-dice'],
                    'slice_time_roulette': times['slice-roulette'],
                    'sppl_time': times['sppl'],
                    'dice_speedup': dice_speedup,
                    'roulette_speedup': roulette_speedup
                })
                print(f"  {benchmark}: {dice_speedup:.1f}x dice speedup, {roulette_speedup:.1f}x roulette speedup")
        
        return speedups
    
    def save_results(self, type):
        """Save results to CSV and JSON"""
        # Save detailed results
        csv_file = self.output_dir / f"{type}_benchmark_results.csv"
        print(os.getcwd())
        if self.results:
            fieldnames = sorted({key for row in self.results for key in row.keys()})
            with open(csv_file, 'w', newline='') as f:
                writer = csv.DictWriter(f, fieldnames=fieldnames)
                writer.writeheader()
                writer.writerows(self.results)
            print(f"\nDetailed results saved to: {csv_file}")

        category_for_type = {
            'dt': 'decision_tree',
            'baseline': 'baseline',
            'scaling': 'scaling',
        }.get(type, type)

        matching_outputs = [
            row for row in self.outputs
            if row.get('category') == category_for_type
        ]

        if matching_outputs:
            outputs_csv = self.output_dir / f"{type}_outputs.csv"
            output_fieldnames = [
                'benchmark',
                'category',
                'tool',
                'timed_out',
                'first_line',
                'last_line',
                'last_numeric',
                'output',
            ]
            with open(outputs_csv, 'w', newline='') as f:
                writer = csv.DictWriter(f, fieldnames=output_fieldnames)
                writer.writeheader()
                writer.writerows(matching_outputs)
            print(f"Program outputs saved to: {outputs_csv}")

            outputs_json = self.output_dir / f"{type}_outputs.json"
            with open(outputs_json, 'w') as f:
                json.dump(matching_outputs, f, indent=2)
            print(f"Program outputs JSON saved to: {outputs_json}")

            if type == 'baseline':
                comparison_file = self.output_dir / "baseline_output_comparison.csv"
                benchmarks = sorted({row['benchmark'] for row in matching_outputs})
                tools = sorted({row['tool'] for row in matching_outputs})
                by_key = {
                    (row['benchmark'], row['tool']): row
                    for row in matching_outputs
                }
                with open(comparison_file, 'w', newline='') as f:
                    fieldnames = (
                        ['benchmark'] +
                        [f'{tool}_last_line' for tool in tools] +
                        [f'{tool}_last_numeric' for tool in tools]
                    )
                    writer = csv.DictWriter(f, fieldnames=fieldnames)
                    writer.writeheader()
                    for benchmark in benchmarks:
                        row = {'benchmark': benchmark}
                        for tool in tools:
                            output_row = by_key.get((benchmark, tool), {})
                            row[f'{tool}_last_line'] = output_row.get('last_line', '')
                            row[f'{tool}_last_numeric'] = output_row.get('last_numeric', '')
                        writer.writerow(row)
                print(f"Baseline output comparison saved to: {comparison_file}")
        
        # Save speedup summary
        speedups = self.calculate_speedups()
        if speedups:
            speedup_file = self.output_dir / f"{type}_speedup_summary.csv"
            with open(speedup_file, 'w', newline='') as f:
                writer = csv.DictWriter(f, fieldnames=speedups[0].keys())
                writer.writeheader()
                writer.writerows(speedups)
            print(f"Speedup summary saved to: {speedup_file}")
        
        # Save everything as JSON
        json_file = self.output_dir / f"{type}_all_results.json"
        with open(json_file, 'w') as f:
            json.dump({
                'timestamp': datetime.now().isoformat(),
                'num_runs': self.num_runs,
                'results': self.results,
                'outputs': matching_outputs,
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
            f.write("    \\toprule\n")
            f.write("    Benchmark & Slice+Dice (s) & Slice+Roulette (s) & SPPL (s) \\\\\n")
            f.write("    \\midrule\n")

            for s in speedups:
                f.write(
                    f"    {s['benchmark']}  & "
                    f"{s['slice_time_dice']:.3f} (\\textbf{{{s['dice_speedup']:.1f}×}}) & "
                    f"{s['slice_time_roulette']:.3f} ({s['roulette_speedup']:.1f}×) & "
                    f"{s['sppl_time']:.3f} \\\\\n"
                )

            f.write("    \\bottomrule\n")
            f.write("    \\end{tabular}\n")

        print(f"LaTeX table saved to: {latex_file}")
    
    def run_all(self):
        """Run all benchmarks"""
        print("Slice Artifact Evaluation - Benchmark Runner")
        print("=" * 50)
        print(f"Running each benchmark {self.num_runs} times")
        print(f"Results will be saved to: {self.output_dir}/")
        print()
        
        self.run_decision_tree_benchmarks()
        self.save_results('dt')

        self.run_baseline_benchmarks()
        self.save_results('baseline')

        self.run_scaling_benchmarks()
        self.save_results('scaling')

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

            if args.print_all:
                slice_cmd = f"{slice_binary} --print-all '{file_abs}'"
                result = self.run_command(slice_cmd)
                print(result.stdout)

            slice_cmd = f"{slice_binary} '{file_abs}'"
            
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
            
            if args.print_all:
                slice_cmd = f"{slice_binary} --print-all '{file_abs}'"
                result = self.run_command(slice_cmd)
                print(result.stdout)
            
            slice_cmd = f"{slice_binary} '{file_abs}'"

            if args.to_sppl:
                slice_cmd = f"{slice_binary} --to-sppl '{file_abs}'"
                result = self.run_command(slice_cmd)
                print(result.stdout)
                return
            
            result = self.run_command(slice_cmd)
            slice_output = result.stdout
            slice_output = "#lang roulette/example/disrupt\n" + result.stdout
            
            if args.no_roulette:
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
                "cd slice && dune exec -- bin/main.exe ../examples/tutorial/coin.slice",
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
        selected_tools = self.selected_benchmark_tools(args)

        if selected_tools is not None and args.hyperfine:
            print("ERROR: benchmark system flags are not supported with --hyperfine. Remove --hyperfine to filter by system.")
            sys.exit(2)
        if selected_tools is not None and args.type == 'scaling':
            print("ERROR: benchmark system flags are only supported for baseline and decision-tree benchmarks.")
            sys.exit(2)
        
        if args.type in ['all', 'decision-trees', 'dt']:
            if not self.quiet:
                print("Running decision tree benchmarks...")
            
            if args.hyperfine:
                # Use hyperfine for benchmarking
                self.run_hyperfine_benchmarks(output_dir)
            else:
                # Use integrated benchmark runner
                runner = BenchmarkRunner(str(output_dir), timeout=args.timeout if args.timeout else 300, selected_tools=selected_tools)
                runner.num_runs = args.runs
                runner.run_decision_tree_benchmarks()
                runner.save_results('dt')
                
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
                runner = BenchmarkRunner(str(output_dir), timeout=args.timeout if args.timeout else 300, selected_tools=selected_tools)
                runner.num_runs = args.runs
                runner.run_baseline_benchmarks()
                runner.save_results('baseline')
                
                if args.format == 'latex':
                    runner.generate_latex_table()
        
        if args.type in ['all', 'scaling'] and selected_tools is None:
            if not self.quiet:
                print("\nRunning scaling benchmarks...", flush=True)
            
            if args.hyperfine:
                # Use hyperfine for benchmarking
                self.run_hyperfine_benchmarks(output_dir)
            else:
                # Use integrated benchmark runner
                runner = BenchmarkRunner(str(output_dir), timeout=args.timeout if args.timeout else 300)
                runner.num_runs = args.runs
                runner.run_scaling_benchmarks()
                runner.save_results('scaling')
                
                if args.format == 'latex':
                    runner.generate_latex_table()

    def selected_benchmark_tools(self, args):
        """Return selected benchmark tools from CLI flags, or None for all tools."""
        selected = set()
        if args.slice:
            selected.update({'slice-dice', 'slice-roulette'})
        if args.slice_dice:
            selected.add('slice-dice')
        if args.slice_roulette:
            selected.add('slice-roulette')
        if args.sppl:
            selected.add('sppl')
        if args.psi:
            selected.add('psi')
        if args.hakaru:
            selected.add('hakaru')
        return selected or None

    
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
                no_roulette=False,
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
  python3 slice.py run examples/tutorial/coin.slice             # Run with both backends
  python3 slice.py --dice run examples/tutorial/coin.slice      # Run with Dice backend only
  python3 slice.py --roulette run examples/tutorial/coin.slice  # Run with Roulette backend only
  python3 slice.py run examples/tutorial/coin.slice             # Run with both Dice and Roulette
  python3 slice.py test                                         # Test installation
  python3 slice.py benchmark                                    # Run all benchmarks
  python3 slice.py benchmark baseline --psi                     # Run only PSI baseline benchmarks
  python3 slice.py benchmark decision-trees --hakaru            # Run only Hakaru decision-tree benchmarks
  python3 slice.py examples --list                              # List examples
  python3 slice.py run file.slice --to-sppl                     # Convert to SPPL

  python3 slice.py [--dice | --roulette] run file.slice                                                  # Prints output only
  python3 slice.py [--dice | --roulette] run --print-all file.slice                                      # Prints type annotations and output
  python3 slice.py [--dice | --roulette] run --print-all [--no-dice | --no-roulette] file.slice          # Prints type annotations only
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
    run_parser.add_argument('--no-roulette', action='store_true', help='Only run slice, skip roulette')
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
    bench_systems = bench_parser.add_argument_group('benchmark system selection')
    bench_systems.add_argument('--slice', action='store_true',
                               help='Run only Slice systems (Slice+Dice and Slice+Roulette)')
    bench_systems.add_argument('--slice-dice', action='store_true',
                               help='Run only Slice+Dice benchmarks')
    bench_systems.add_argument('--slice-roulette', action='store_true',
                               help='Run only Slice+Roulette benchmarks')
    bench_systems.add_argument('--sppl', action='store_true',
                               help='Run only SPPL benchmarks')
    bench_systems.add_argument('--psi', action='store_true',
                               help='Run only PSI benchmarks')
    bench_systems.add_argument('--hakaru', action='store_true',
                               help='Run only Hakaru benchmarks')
    
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
