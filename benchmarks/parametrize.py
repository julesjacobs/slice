# Scripts for generating context dependent programs in sppl and contdice
# Parametrizes the variables by n and plots a curve of contdice vs sppl that shows better asymptotic scaling

from sppl.compilers.ast_to_spe import Id
from sppl.compilers.sppl_to_python import SPPL_Compiler

from pathlib import Path
import time
import subprocess
import matplotlib.pyplot as plt


# ===== for SPPL =====
def build_context_dependent_sppl(vars):
    lines = []
    counter = [1]

    # Step 0: Variable bindings
    for idx, var in enumerate(vars):
        lines.append(f"{var} ~= uniform(loc=0,scale={idx+1})")

    # Step 1: Now define the nested structure
    def build(depth):
        if depth == len(vars):
            last_var = vars[-1]
            next_var = chr(ord(last_var) + 1)
            lines.append(f"{'  ' * depth}{next_var} ~= uniform(loc=0,scale={counter[0]})")
            counter[0] += 1
            return
        var = vars[depth]
        lines.append(f"{'  ' * depth}if ({var} < 0.5):")
        build(depth + 1)
        lines.append(f"{'  ' * depth}else:")
        build(depth + 1)

    build(0)
    return "\n".join(lines)


# ===== for CONTDICE =====
def build_context_dependent_contdice(variables):
    code = []
    # Variable bindings
    for idx, var in enumerate(variables):
        code.append(f"let {var} = uniform(0,{idx + 1}) in")
    
    # Compute the result variable: next after the last one
    last_var = variables[-1]
    result_var = chr(ord(last_var) + 1)

    code.append(f"let {result_var} =")  # Use the computed variable

    # Build nested if-else structure
    indent = 2  # Starting indent (one more after `let result_var =`)
    counter = [1]  # Using list for mutable integer inside inner function

    def build(depth):
        if depth == len(variables):
            code.append(" " * indent + f"uniform(0,{counter[0]})")
            counter[0] += 1
            return
        var = variables[depth]
        code.append(" " * indent + f"if {var} < 0.5 then")
        indent_increase()
        build(depth + 1)
        indent_decrease()
        code.append(" " * indent + "else")
        indent_increase()
        build(depth + 1)
        indent_decrease()

    def indent_increase():
        nonlocal indent
        indent += 2

    def indent_decrease():
        nonlocal indent
        indent -= 2

    build(0)
    # Add the conditional query
    code.append(f"in {result_var} < 0.5")
    return "\n".join(code)


# ===== Parametrize =====
# program = build_context_dependent_contdice(['a', 'b', 'c'])
# print(program)

# program = build_context_dependent_sppl(['a','b','c','d'])
# print(program)

def generate_variable_list_up_to(end_letter):
    return [chr(c) for c in range(ord('a'), ord(end_letter) + 1)]

def main():
    # Lists to collect timings
    sppl_times = []
    contdice_times = []
    num_vars = []

    end_letter = 'm'
    for c in range(ord('a'), ord(end_letter) + 1):
        var_list = generate_variable_list_up_to(chr(c))
        num_vars.append(len(var_list))  # Track how many variables

        # --- SPPL ---
        program_sppl = build_context_dependent_sppl(var_list)
        start = time.time()
        compiler = SPPL_Compiler(f'''{program_sppl}''')
        namespace = compiler.execute_module()
        last_var = var_list[-1]
        result_var = Id(chr(ord(last_var) + 1))
        event = (result_var < 0.5)  
        output = namespace.model.prob(event)
        duration = time.time() - start
        sppl_times.append(duration)
        print(f"sppl time {duration}")

        # --- CONTDICE ---
        program_contdice = build_context_dependent_contdice(var_list)
        path = Path("parametrize.cdice")
        path.write_text(program_contdice)
        command = ["./run_contdice.sh", str(path)]
        subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)  # build step
        start = time.time()
        result = subprocess.run(command, shell=False, check=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        duration = time.time() - start
        contdice_times.append(duration)
        print(f"contdice time {duration}")
        output = result.stdout.decode('utf-8')

    # --- Plotting ---
    plt.plot(num_vars, sppl_times, label="SPPL", color="red", marker="o")
    plt.plot(num_vars, contdice_times, label="ContDice", color="blue", marker="o")

    plt.xlabel("Number of Variables")
    plt.ylabel("Execution Time (seconds)")
    plt.title("Sppl vs Contdice Execution Time")
    plt.legend()
    plt.grid(True)
    plt.savefig("timings.png", dpi=300, bbox_inches="tight")
    plt.show()


if __name__ == "__main__":
    main()

