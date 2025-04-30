def build_conditional_dependent_contdice(variables):
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


def build_conditional_dependent_sppl(vars):
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

def build_conditional_independent_contdice(variables):
    code = []
    counter = 1

    for idx, var in enumerate(variables):
        if idx == 0:
            code.append(f"let {var} = uniform(0,{counter}) in")
            counter += 1
        else:
            prev = variables[idx - 1]
            code.append(f"let {var} = if {prev} < 0.5 then uniform(0,{counter}) else uniform(0,{counter + 1}) in")
            counter += 2

    last_var = variables[-1]
    code.append(f"{last_var} < 0.5")
    return "\n".join(code)

def build_conditional_independent_sppl(vars):
    lines = []
    counter = 1
    first_var = vars[0]
    lines.append(f"{first_var} ~= uniform(loc=0, scale={counter})")
    counter += 1
    
    for i in range(1, len(vars)):
        prev = vars[i - 1]
        curr = vars[i]
        lines.append(f"if ({prev} < 0.5):")
        lines.append(f"    {curr} ~= uniform(loc=0, scale={counter})")
        counter += 1
        lines.append("else:")
        lines.append(f"    {curr} ~= uniform(loc=0, scale={counter})")
        counter += 1
    return "\n".join(lines)

program = build_conditional_dependent_contdice(['a', 'b','c'])
print(program)