'''
A library of Python functions that generate variants of probabilistic programs, 
parametrized by the number of comparison tests (i.e. <).
Note: only generates linearly sized programs.
'''

import random

'''
Generate programs with conditional independence. 
Each variable in the if-else guard depends on its immediate predecessor.
'''
def build_conditional_independent_contdice(comparison_count):
    code = []
    counter = 1
    variables = [f"x{n}" for n in range(comparison_count + 1)]

    for idx in range(comparison_count + 1):
        if idx == 0:
            code.append(f"let {variables[idx]} = uniform(0,{counter}) in")
            counter += 1
        else:
            prev = variables[idx - 1]
            code.append(f"let {variables[idx]} = if {prev} < 0.5 then uniform(0,{counter}) else uniform(0,{counter + 1}) in")
            counter += 2

    last_var = variables[-1]
    code.append(f"{last_var} < 0.5")
    return "\n".join(code)


def build_conditional_independent_sppl(comparison_count):
    # Alternatively, can use the --to-sppl flag in cdice
    lines = []
    counter = 1
    variables = [f"x{n}" for n in range(comparison_count + 1)]
    
    lines.append(f"{variables[0]} ~= uniform(loc=0, scale={counter})")
    counter += 1
    
    for i in range(1, comparison_count + 1):
        prev = variables[i - 1]
        curr = variables[i]
        lines.append(f"if ({prev} < 0.5):")
        lines.append(f"    {curr} ~= uniform(loc=0, scale={counter})")
        counter += 1
        lines.append("else:")
        lines.append(f"    {curr} ~= uniform(loc=0, scale={counter})")
        counter += 1
    return "\n".join(lines)


'''
Each variable in the if-else guard depends on any previous variable chosen at random.
May lead to unused fragments.
'''
def build_conditional_random_independent_contdice_1(comparison_count):
    code = []
    counter = 1
    variables = [f"x{n}" for n in range(comparison_count + 1)]

    for idx in range(comparison_count + 1):
        if idx == 0:
            code.append(f"let {variables[idx]} = uniform(0,{counter}) in")
            counter += 1
        else:
            # Choose a random previous variable index
            rand_idx = random.randint(0, idx - 1)
            prev = variables[rand_idx]
            code.append(f"let {variables[idx]} = if {prev} < 0.5 then uniform(0,{counter}) else uniform(0,{counter + 1}) in")
            counter += 2

    last_var = variables[-1]
    code.append(f"{last_var} < 0.5")
    return "\n".join(code)


def build_conditional_random_independent_contdice_2(depth):
    # Unlike build_conditional_random_independent_contdice_1, uses variables in the then and else bodies
    counter = [0]
    prev_vars = []

    def gen_var():
        name = f"x{counter[0]}"
        counter[0] += 1
        return name
    
    def uniform(min_, max_):
        return f"uniform({min_}, {max_})"

    def ite(cond, thenb, elseb):
        return f"if {cond} then {thenb} else {elseb}"

    def lt(a, b):
        return f"{a} < {b}"

    def const(value):
        return f"{value}"

    def let(expr, fn):
        var_name = gen_var()
        prev_vars.append(var_name)
        body, final_var = fn(var_name)
        return f"let {var_name} = {expr} in\n{body}", final_var

    def gen_expr(d):
        if d == 0:
            var = random.choice(prev_vars[-5:]) if prev_vars else gen_var()
            return lt(var, const(0.5)), var

        if random.random() < 0.5 or len(prev_vars) == 0:
            quantity = uniform(0, random.randint(1, 10))
        else:
            v1 = random.choice(prev_vars[-5:])
            v2 = random.choice(prev_vars[-5:])
            quantity = ite(lt(v1, const(0.5)), v1, v2)

        return let(quantity, lambda x: gen_expr(d - 1))

    # Generate the full program
    program_str, last_var = gen_expr(depth)
    return program_str


program = build_conditional_random_independent_contdice_2(6)
print(program)