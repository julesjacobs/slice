import random


class Generator:
    pass

# Generator for variable names
counter = 0
def gen_var():
    global counter
    counter += 1
    return f"x{counter}"

class CDiceGenerator(Generator):
    def let(self, a, fn):
        var_name = gen_var()
        body = fn(var_name)
        return f"let {var_name} = {a} in\n{body}"

    def uniform(self, min, max):
        return f"uniform({min}, {max})"

    def ite(self, cond, thenb, elseb):
        return f"if {cond} then {thenb} else {elseb}"

    def lt(self, a, b):
        return f"{a} < {b}"
    
    def const(self, value):
        return f"{value}"


class SPPLGenerator(Generator):
    pass # TODO

g = CDiceGenerator()

# Generate a sequence of let expressions, where each is either a uniform, or an if-then-else expression
# with a lt condition

    
def gen_expr(depth, prev_vars = []):
    if depth == 0:
        if prev_vars == []:
            return g.uniform(0, 1)
        else:
            return random.choice(prev_vars[-5:])
    
    if prev_vars == []:
        quantity = g.uniform(0, random.randint(1, 10))
    else:
        quantity = g.ite(g.lt(g.uniform(0, random.randint(1, 10)), g.const(0.5)), random.choice(prev_vars[-5:]), random.choice(prev_vars[-5:]))
    return g.let(quantity, lambda x: gen_expr(depth - 1, prev_vars + [x]))

print(gen_expr(50))