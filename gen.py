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
    variables = [f"x{n}" for n in range(1, comparison_count + 1)]

    for idx in range(comparison_count):
        var = variables[idx]
        if idx == 0:
            code.append(f"let {var} = uniform(0,{counter}) in")
            counter += 1
        else:
            prev = variables[idx - 1]
            code.append(f"let {var} = if {prev} < 0.5 then uniform(0,{counter}) else uniform(0,{counter + 1}) in")
            counter += 2

    last_var = variables[-1]
    code.append(f"{last_var} < 0.5")
    return "\n".join(code), last_var


def build_conditional_independent_sppl(comparison_count):
    lines = []
    counter = 1
    variables = [f"x{n}" for n in range(1, comparison_count + 1)]

    lines.append(f"{variables[0]} ~= uniform(loc=0, scale={counter})")
    counter += 1

    for i in range(1, comparison_count):
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
    variables = [f"x{n}" for n in range(1, comparison_count + 1)]

    for idx in range(comparison_count):
        var = variables[idx]
        if idx == 0:
            code.append(f"let {var} = uniform(0,{counter}) in")
            counter += 1
        else:
            rand_idx = random.randint(0, idx - 1)
            prev = variables[rand_idx]
            code.append(f"let {var} = if {prev} < 0.5 then uniform(0,{counter}) else uniform(0,{counter + 1}) in")
            counter += 2

    last_var = variables[-1]
    code.append(f"{last_var} < 0.5")
    return "\n".join(code), last_var


def build_conditional_random_independent_contdice_2(depth):
    # Uses more randomization than build_conditional_random_independent_contdice_1
    counter = [0]
    def gen_var():
        counter[0] += 1
        return f"x{counter[0]}"
    
    class CDiceGenerator:
        def let(self, a, fn):
            var_name = gen_var()
            body = fn(var_name)
            return f"let {var_name} = {a} in\n{body}"
        
        def uniform(self, min_, max_):
            return f"uniform({min_}, {max_})"
        
        def ite(self, cond, thenb, elseb):
            return f"if {cond} then {thenb} else {elseb}"
        
        def lt(self, a, b):
            return f"{a} < {b}"
        
        def const(self, value):
            return f"{value}"
    
    g = CDiceGenerator()
    last_comparison_var = [None]  # To keep track of the last variable used in comparison
    guard_info = {i: 0 for i in range(1, depth + 1)} # Eventually to map to the correct last variable used in SPPL
    valid_flag = [False]
    
    def prev_var_or_uniform(prev_vars):
        if not prev_vars or random.random() < 0.5:
            return g.uniform(0, random.randint(1, 10))
        else:
            return random.choice(prev_vars[-5:])
    
    def gen_expr(d, prev_vars):
        if d == 0:
            expr = prev_var_or_uniform(prev_vars)
            last_comparison_var[0] = expr
            return g.lt(expr, g.const(0.5))
            
        if not prev_vars:
            quantity = g.uniform(0, random.randint(1, 10))
        else:
            guard_expr = prev_var_or_uniform(prev_vars)
            thenb_expr = prev_var_or_uniform(prev_vars)
            elseb_expr = prev_var_or_uniform(prev_vars)
            
            if not valid_flag[0]:
                # Keep regenerating thenb_expr until it's a uniform distribution
                while elseb_expr.startswith("uniform"):
                    elseb_expr = prev_var_or_uniform(prev_vars)
                valid_flag[0] = True
                        
            quantity = g.ite(
                g.lt(guard_expr, g.const(0.5)),
                thenb_expr,
                elseb_expr
            )
            # Update count on how many uniform guards there are up to the current variable
            if guard_expr.startswith("uniform"):
                prev_var_num = int(prev_vars[-1][1:])
                prev_var_count = guard_info[prev_var_num]
                guard_info[prev_var_num + 1] = prev_var_count + 1
            else:
                prev_var_num = int(prev_vars[-1][1:])
                prev_var_count = guard_info[prev_var_num]
                guard_info[prev_var_num + 1] = prev_var_count
                
        return g.let(quantity, lambda x: gen_expr(d - 1, prev_vars + [x]))
    
    program = gen_expr(depth, [])
    # Adjust the last comparison variable so it is compatible with SPPL
    last_comparison_var = last_comparison_var[0]
    if last_comparison_var.startswith("uniform"):
        last_comparison_var_num = len(guard_info)
        num_of_uniform_guards = list(guard_info.values())[-1]
        adjusted_last = f"x{last_comparison_var_num + num_of_uniform_guards + 1}"
    else:
        last_comparison_var_num = int(last_comparison_var[1:])
        num_of_uniform_guards = guard_info[last_comparison_var_num]
        adjusted_last = f"x{last_comparison_var_num + num_of_uniform_guards}"
        
    return program, adjusted_last


def main():
    program, last_var = build_conditional_random_independent_contdice_2(4)
    print(program)
    print(last_var)

if __name__ == "__main__":
    main()