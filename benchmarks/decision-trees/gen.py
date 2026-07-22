def _slice_float_for_t_value(v):
    """
    Convert SPPL atomic(loc=v) value into Slice float value.
    SPPL uses loc={i % 2}, so:
      i even -> 0
      i odd  -> 1
    """
    return "1.0" if v == 1 else "0.0"


def _indent_block(text, spaces):
    prefix = " " * spaces
    return "\n".join(prefix + line if line else line for line in text.splitlines())


def _condition_hakaru_with_disint(program):
    """Condition generated Hakaru code on the two categorical sex values.

    Hakaru cannot disintegrate Boolean comparisons such as ``age > 18``.
    Those continuous event observations therefore remain as weights, while
    the equivalent categorical observations are represented by conditioning
    directly on ``sex_f = 0`` and ``sex_m = 1``.
    """
    observe_prefix = "_ <~ observe_bool("
    return_prefix = "return ((), "
    sex_conditions = {"sex_f < 1", "sex_m >= 1"}
    found_sex_conditions = set()
    body = []

    for line in program.strip("\n").splitlines():
        stripped = line.strip()
        if stripped.startswith(observe_prefix) and stripped.endswith(")"):
            condition = stripped[len(observe_prefix):-1]
            if condition in sex_conditions:
                found_sex_conditions.add(condition)
                continue
        body.append(line)

    if found_sex_conditions != sex_conditions:
        raise ValueError("generated Hakaru program lacks the expected sex observations")

    for index in range(len(body) - 1, -1, -1):
        stripped = body[index].strip()
        if stripped.startswith(return_prefix) and stripped.endswith(")"):
            target = stripped[len(return_prefix):-1]
            indentation = body[index][:len(body[index]) - len(body[index].lstrip())]
            body[index] = f"{indentation}return ((sex_f, sex_m), {target})"
            break
    else:
        raise ValueError("generated Hakaru program has no unit/target return")

    return (
        "posterior =\n"
        "  disint(\n"
        f"{_indent_block(chr(10).join(body), 4)}\n"
        "  )\n\n"
        "posterior((0, 1))\n"
    )


def _generate_psi_assignment_if_chain(cases, default_stmt, indent):
    if not cases:
        return _indent_block(default_stmt, indent)

    condition, body = cases[0]
    rest = _generate_psi_assignment_if_chain(cases[1:], default_stmt, indent + 4)
    return (
        f"{' ' * indent}if {condition} {{\n"
        f"{_indent_block(body, indent + 4)}\n"
        f"{' ' * indent}}} else {{\n"
        f"{rest}\n"
        f"{' ' * indent}}}"
    )


def _generate_psi_threshold_assignments(target_var, capital_gain_var, values, else_value, indent):
    cases = [
        (
            f"{capital_gain_var} < {100 + (i * 100)}.0",
            f"{target_var} = {value};",
        )
        for i, value in enumerate(values)
    ]
    return _generate_psi_assignment_if_chain(cases, f"{target_var} = {else_value};", indent)


def _generate_psi_decision_tree(n, category_var, capital_gain_var, target_var):
    num_categories = 5
    conditionals_per_category = n // num_categories
    remainder = n % num_categories

    cases = []
    for cat in range(num_categories):
        cat_conditionals = conditionals_per_category + (1 if cat < remainder else 0)
        if cat_conditionals == 0:
            body = f"{target_var} = 0.0;"
        else:
            values = [_slice_float_for_t_value(i % 2) for i in range(cat_conditionals)]
            else_value = _slice_float_for_t_value(cat_conditionals % 2)
            body = _generate_psi_threshold_assignments(
                target_var,
                capital_gain_var,
                values,
                else_value,
                0,
            )
        cases.append((f"{category_var} == {cat}", body))

    default_stmt = (
        f"if {capital_gain_var} < 4668.5 {{\n"
        f"    {target_var} = 1.0;\n"
        f"}} else {{\n"
        f"    {target_var} = 0.0;\n"
        f"}}"
    )
    return _generate_psi_assignment_if_chain(cases, default_stmt, 4)


def _generate_hakaru_if_expr(cases, default_expr, indent):
    if not cases:
        return _indent_block(default_expr, indent)

    condition, body = cases[0]
    rest = _generate_hakaru_if_expr(cases[1:], default_expr, indent + 4)
    return (
        f"{' ' * indent}if {condition}:\n"
        f"{_indent_block(body, indent + 4)}\n"
        f"{' ' * indent}else:\n"
        f"{rest}"
    )


def _generate_hakaru_threshold_expr(capital_gain_var, values, else_value, indent):
    cases = [
        (
            f"{capital_gain_var} < {100 + (i * 100)}.0",
            f"return {value}",
        )
        for i, value in enumerate(values)
    ]
    return _generate_hakaru_if_expr(cases, f"return {else_value}", indent)


def _generate_hakaru_decision_tree(n, category_var, capital_gain_var, indent=0):
    num_categories = 5
    conditionals_per_category = n // num_categories
    remainder = n % num_categories

    cases = []
    for cat in range(num_categories):
        cat_conditionals = conditionals_per_category + (1 if cat < remainder else 0)
        if cat_conditionals == 0:
            body = "return 0.0"
        else:
            values = [_slice_float_for_t_value(i % 2) for i in range(cat_conditionals)]
            else_value = _slice_float_for_t_value(cat_conditionals % 2)
            body = _generate_hakaru_threshold_expr(capital_gain_var, values, else_value, 0)
        cases.append((f"{category_var} == {cat}", body))

    default_expr = (
        f"if {capital_gain_var} < 4668.5:\n"
        f"    return 1.0\n"
        f"else:\n"
        f"    return 0.0"
    )
    return _generate_hakaru_if_expr(cases, default_expr, indent)


def _generate_slice_decision_tree(n, category_var, capital_gain_var, category_cardinality):
    """
    Generate the Slice decision model shared by the female and male branches.

    This matches the generated SPPL decision model:

        if category == '0':
            if capital_gain < 100: t ~= atomic(loc=0)
            elif capital_gain < 200: t ~= atomic(loc=1)
            ...
            else: t ~= atomic(loc=cat_conditionals % 2)
        elif category == '1':
            ...
        ...
        else:
            if capital_gain < 4668.5: t ~= atomic(loc=1)
            else: t ~= atomic(loc=0)

    Since both SPPL and Slice query t = 1, the generated
    Slice program should return:

        t_f >= 1.0
        t_m >= 1.0
    """
    num_categories = 5
    conditionals_per_category = n // num_categories
    remainder = n % num_categories

    code = ""

    for cat in range(num_categories):
        cat_conditionals = conditionals_per_category + (1 if cat < remainder else 0)

        if cat == 0:
            code += f"  if {category_var} ==#{category_cardinality} {cat}#{category_cardinality} then\n"
        else:
            code += f"  else if {category_var} ==#{category_cardinality} {cat}#{category_cardinality} then\n"

        if cat_conditionals == 0:
            # This case makes the generator robust for n < 5.
            # The original SPPL generator would produce invalid code for n < 5,
            # so this is just a safe fallback.
            code += "    0.0\n"
            continue

        for i in range(cat_conditionals):
            threshold = 100 + (i * 100)
            loc_value = _slice_float_for_t_value(i % 2)

            if i == 0:
                code += f"    if {capital_gain_var} < {threshold}.0 then {loc_value}\n"
            else:
                code += f"    else if {capital_gain_var} < {threshold}.0 then {loc_value}\n"

        else_loc = _slice_float_for_t_value(cat_conditionals % 2)
        code += f"    else {else_loc}\n"

    code += "  else\n"
    code += f"    if {capital_gain_var} < 4668.5 then 1.0 else 0.0"

    return code


def generate_ind_slice(n):
    """
    Generate an independent variables Slice program with duplicated female
    and male branches.

    This corresponds to conditioning on sex = female and sex = male separately,
    producing:

        (t_f >= 1.0, t_m >= 1.0)

    so the query matches t = 1.
    """

    decision_f = _generate_slice_decision_tree(
        n=n,
        category_var="category_f",
        capital_gain_var="capital_gain_f",
        category_cardinality=6,
    )

    decision_m = _generate_slice_decision_tree(
        n=n,
        category_var="category_m",
        capital_gain_var="capital_gain_m",
        category_cardinality=6,
    )

    program = f"""let sex_f = discrete(0.3307, 0.6693) in
let _1 = observe(sex_f <#2 1#2) in

let age_f = gaussian(38.5816, 186.0614) in

let education_f = discrete(
  0.1644, 0.2239, 0.0361, 0.3225,
  0.0177, 0.0328, 0.0424, 0.0158,
  0.0198, 0.0133, 0.0530, 0.0051,
  0.0287, 0.0127, 0.0102, 0.0016) in

let category_f = discrete(
  0.0481, 0.1557, 0.4051,
  0.2551, 0.0301, 0.1059) in

let capital_gain_f = gaussian(1077.6488, 54542539.1784) in

let _2 = observe(age_f > 18.0) in

let t_f =
{decision_f}
in

let sex_m = discrete(0.3307, 0.6693) in
let _3 = observe(sex_m >=#2 1#2) in

let age_m = gaussian(38.5816, 186.0614) in

let education_m = discrete(
  0.1644, 0.2239, 0.0361, 0.3225,
  0.0177, 0.0328, 0.0424, 0.0158,
  0.0198, 0.0133, 0.0530, 0.0051,
  0.0287, 0.0127, 0.0102, 0.0016) in

let category_m = discrete(
  0.0481, 0.1557, 0.4051,
  0.2551, 0.0301, 0.1059) in

let capital_gain_m = gaussian(1077.6488, 54542539.1784) in

let _4 = observe(age_m > 18.0) in

let t_m =
{decision_m}
in

(t_f >= 1.0, t_m >= 1.0)
"""

    return program


def generate_bn1_slice(n):
    """
    Generate a Bayesian network Slice program, BN1, with duplicated female
    and male branches.

    This produces:

        (t_f >= 1.0, t_m >= 1.0)

    so the query matches t = 1.
    """

    decision_f = _generate_slice_decision_tree(
        n=n,
        category_var="category_f",
        capital_gain_var="capital_gain_f",
        category_cardinality=5,
    )

    decision_m = _generate_slice_decision_tree(
        n=n,
        category_var="category_m",
        capital_gain_var="capital_gain_m",
        category_cardinality=5,
    )

    program = f"""let sex_f = discrete(0.3307, 0.6693) in
let _1 = observe(sex_f <#2 1#2) in

let capital_gain_f = gaussian(5000.0, 1000000.0) in

let age_f =
  if capital_gain_f < 7000.0 then
    gaussian(40.0, 10.0)
  else
    gaussian(45.0, 10.0) in

let category_f =
  if capital_gain_f < 7000.0 then
    discrete(0.2, 0.2, 0.2, 0.2, 0.2)
  else
    discrete(0.2, 0.2, 0.2, 0.2, 0.2) in

let _2 = observe(age_f > 18.0) in

let t_f =
{decision_f}
in

let sex_m = discrete(0.3307, 0.6693) in
let _3 = observe(sex_m >=#2 1#2) in

let capital_gain_m = gaussian(6000.0, 1000000.0) in

let age_m =
  if capital_gain_m < 5000.0 then
    gaussian(35.0, 10.0)
  else
    gaussian(42.0, 10.0) in

let category_m =
  if capital_gain_m < 5000.0 then
    discrete(0.2, 0.2, 0.2, 0.2, 0.2)
  else
    discrete(0.2, 0.2, 0.2, 0.2, 0.2) in

let _4 = observe(age_m > 18.0) in

let t_m =
{decision_m}
in

(t_f >= 1.0, t_m >= 1.0)
"""

    return program


def generate_bn2_slice(n):
    """
    Generate a Bayesian network Slice program, BN2, with duplicated female
    and male branches.

    This produces:

        (t_f >= 1.0, t_m >= 1.0)

    so the query matches t = 1.
    """

    decision_f = _generate_slice_decision_tree(
        n=n,
        category_var="category_f",
        capital_gain_var="capital_gain_f",
        category_cardinality=5,
    )

    decision_m = _generate_slice_decision_tree(
        n=n,
        category_var="category_m",
        capital_gain_var="capital_gain_m",
        category_cardinality=5,
    )

    program = f"""let sex_f = discrete(0.3307, 0.6693) in
let _1 = observe(sex_f <#2 1#2) in

let capital_gain_f = gaussian(5000.0, 1000000.0) in

let age_f =
  if capital_gain_f < 7000.0 then
    gaussian(40.0, 10.0)
  else
    gaussian(45.0, 10.0) in

let education_num_f =
  if capital_gain_f < 7000.0 then
    gaussian(10.0827, 6.5096)
  else
    gaussian(10.1041, 6.1522) in

let category_f =
  if capital_gain_f < 7000.0 then
    discrete(0.2, 0.2, 0.2, 0.2, 0.2)
  else
    discrete(0.2, 0.2, 0.2, 0.2, 0.2) in

let _2 = observe(age_f > 18.0) in

let t_f =
{decision_f}
in

let sex_m = discrete(0.3307, 0.6693) in
let _3 = observe(sex_m >=#2 1#2) in

let capital_gain_m = gaussian(6000.0, 1000000.0) in

let age_m =
  if capital_gain_m < 5000.0 then
    gaussian(35.0, 10.0)
  else
    gaussian(42.0, 10.0) in

let education_num_m =
  if capital_gain_m < 5000.0 then
    gaussian(10.0817, 6.4841)
  else
    gaussian(10.0974, 7.1793) in

let category_m =
  if capital_gain_m < 5000.0 then
    discrete(0.2, 0.2, 0.2, 0.2, 0.2)
  else
    discrete(0.2, 0.2, 0.2, 0.2, 0.2) in

let _4 = observe(age_m > 18.0) in

let t_m =
{decision_m}
in

(t_f >= 1.0, t_m >= 1.0)
"""

    return program



def generate_ind_psi(n):
    """
    Generate an independent variables PSI program with duplicated female
    and male branches, matching generate_ind_slice.
    """
    decision_f = _generate_psi_decision_tree(
        n=n,
        category_var="category_f",
        capital_gain_var="capital_gain_f",
        target_var="t_f",
    )
    decision_m = _generate_psi_decision_tree(
        n=n,
        category_var="category_m",
        capital_gain_var="capital_gain_m",
        target_var="t_m",
    )

    return f"""// generated from the paired Slice query.
def main() {{
    sex_f := categorical([0.3307, 0.6693]);
    observe(sex_f < 1);
    age_f := gauss(38.5816, 186.0614);
    education_f := categorical([0.1644, 0.2239, 0.0361, 0.3225, 0.0177, 0.0328, 0.0424, 0.0158, 0.0198, 0.0133, 0.0530, 0.0051, 0.0287, 0.0127, 0.0102, 0.0016]);
    category_f := categorical([0.0481, 0.1557, 0.4051, 0.2551, 0.0301, 0.1059]);
    capital_gain_f := gauss(1077.6488, 54542539.1784);
    observe(age_f > 18.0);
    t_f := 0;
{decision_f}
    sex_m := categorical([0.3307, 0.6693]);
    observe(sex_m >= 1);
    age_m := gauss(38.5816, 186.0614);
    education_m := categorical([0.1644, 0.2239, 0.0361, 0.3225, 0.0177, 0.0328, 0.0424, 0.0158, 0.0198, 0.0133, 0.0530, 0.0051, 0.0287, 0.0127, 0.0102, 0.0016]);
    category_m := categorical([0.0481, 0.1557, 0.4051, 0.2551, 0.0301, 0.1059]);
    capital_gain_m := gauss(1077.6488, 54542539.1784);
    observe(age_m > 18.0);
    t_m := 0;
{decision_m}
    return (t_f >= 1.0, t_m >= 1.0);
}}
"""


def generate_bn1_psi(n):
    """
    Generate a BN1 PSI program with duplicated female and male branches,
    matching generate_bn1_slice.
    """
    decision_f = _generate_psi_decision_tree(
        n=n,
        category_var="category_f",
        capital_gain_var="capital_gain_f",
        target_var="t_f",
    )
    decision_m = _generate_psi_decision_tree(
        n=n,
        category_var="category_m",
        capital_gain_var="capital_gain_m",
        target_var="t_m",
    )

    return f"""// generated from the paired Slice query.
def main() {{
    sex_f := categorical([0.3307, 0.6693]);
    observe(sex_f < 1);
    capital_gain_f := gauss(5000.0, 1000000.0);
    age_f := 0;
    if capital_gain_f < 7000.0 {{
        age_f = gauss(40.0, 10.0);
    }} else {{
        age_f = gauss(45.0, 10.0);
    }}
    category_f := 0;
    if capital_gain_f < 7000.0 {{
        category_f = categorical([0.2, 0.2, 0.2, 0.2, 0.2]);
    }} else {{
        category_f = categorical([0.2, 0.2, 0.2, 0.2, 0.2]);
    }}
    observe(age_f > 18.0);
    t_f := 0;
{decision_f}
    sex_m := categorical([0.3307, 0.6693]);
    observe(sex_m >= 1);
    capital_gain_m := gauss(6000.0, 1000000.0);
    age_m := 0;
    if capital_gain_m < 5000.0 {{
        age_m = gauss(35.0, 10.0);
    }} else {{
        age_m = gauss(42.0, 10.0);
    }}
    category_m := 0;
    if capital_gain_m < 5000.0 {{
        category_m = categorical([0.2, 0.2, 0.2, 0.2, 0.2]);
    }} else {{
        category_m = categorical([0.2, 0.2, 0.2, 0.2, 0.2]);
    }}
    observe(age_m > 18.0);
    t_m := 0;
{decision_m}
    return (t_f >= 1.0, t_m >= 1.0);
}}
"""


def generate_bn2_psi(n):
    """
    Generate a BN2 PSI program with duplicated female and male branches,
    matching generate_bn2_slice.
    """
    decision_f = _generate_psi_decision_tree(
        n=n,
        category_var="category_f",
        capital_gain_var="capital_gain_f",
        target_var="t_f",
    )
    decision_m = _generate_psi_decision_tree(
        n=n,
        category_var="category_m",
        capital_gain_var="capital_gain_m",
        target_var="t_m",
    )

    return f"""// generated from the paired Slice query.
def main() {{
    sex_f := categorical([0.3307, 0.6693]);
    observe(sex_f < 1);
    capital_gain_f := gauss(5000.0, 1000000.0);
    age_f := 0;
    if capital_gain_f < 7000.0 {{
        age_f = gauss(40.0, 10.0);
    }} else {{
        age_f = gauss(45.0, 10.0);
    }}
    education_num_f := 0;
    if capital_gain_f < 7000.0 {{
        education_num_f = gauss(10.0827, 6.5096);
    }} else {{
        education_num_f = gauss(10.1041, 6.1522);
    }}
    category_f := 0;
    if capital_gain_f < 7000.0 {{
        category_f = categorical([0.2, 0.2, 0.2, 0.2, 0.2]);
    }} else {{
        category_f = categorical([0.2, 0.2, 0.2, 0.2, 0.2]);
    }}
    observe(age_f > 18.0);
    t_f := 0;
{decision_f}
    sex_m := categorical([0.3307, 0.6693]);
    observe(sex_m >= 1);
    capital_gain_m := gauss(6000.0, 1000000.0);
    age_m := 0;
    if capital_gain_m < 5000.0 {{
        age_m = gauss(35.0, 10.0);
    }} else {{
        age_m = gauss(42.0, 10.0);
    }}
    education_num_m := 0;
    if capital_gain_m < 5000.0 {{
        education_num_m = gauss(10.0817, 6.4841);
    }} else {{
        education_num_m = gauss(10.0974, 7.1793);
    }}
    category_m := 0;
    if capital_gain_m < 5000.0 {{
        category_m = categorical([0.2, 0.2, 0.2, 0.2, 0.2]);
    }} else {{
        category_m = categorical([0.2, 0.2, 0.2, 0.2, 0.2]);
    }}
    observe(age_m > 18.0);
    t_m := 0;
{decision_m}
    return (t_f >= 1.0, t_m >= 1.0);
}}
"""


def generate_ind_hakaru(n):
    """
    Generate an independent variables Hakaru program with duplicated female
    and male branches, matching generate_ind_slice.
    """
    decision_f = _generate_hakaru_decision_tree(
        n=n,
        category_var="category_f",
        capital_gain_var="capital_gain_f",
        indent=7,
    )
    decision_m = _generate_hakaru_decision_tree(
        n=n,
        category_var="category_m",
        capital_gain_var="capital_gain_m",
        indent=7,
    )

    program = f"""# generated from the paired Slice query.

def observe_bool(b bool):
    if b:
        weight(1, return ())
    else:
        weight(0, return ())

sex_f <~ categorical([0.3307, 0.6693])

_ <~ observe_bool(sex_f < 1)

age_f <~ normal(38.5816, 186.0614)

education_f <~ categorical([0.1644, 0.2239, 0.0361, 0.3225, 0.0177, 0.0328, 0.0424, 0.0158, 0.0198, 0.0133, 0.0530, 0.0051, 0.0287, 0.0127, 0.0102, 0.0016])

category_f <~ categorical([0.0481, 0.1557, 0.4051, 0.2551, 0.0301, 0.1059])

capital_gain_f <~ normal(1077.6488, 54542539.1784)

_ <~ observe_bool(age_f > 18.0)

t_f <~ {decision_f.lstrip()}

sex_m <~ categorical([0.3307, 0.6693])

_ <~ observe_bool(sex_m >= 1)

age_m <~ normal(38.5816, 186.0614)

education_m <~ categorical([0.1644, 0.2239, 0.0361, 0.3225, 0.0177, 0.0328, 0.0424, 0.0158, 0.0198, 0.0133, 0.0530, 0.0051, 0.0287, 0.0127, 0.0102, 0.0016])

category_m <~ categorical([0.0481, 0.1557, 0.4051, 0.2551, 0.0301, 0.1059])

capital_gain_m <~ normal(1077.6488, 54542539.1784)

_ <~ observe_bool(age_m > 18.0)

t_m <~ {decision_m.lstrip()}

return ((), (t_f >= 1.0, t_m >= 1.0))
"""
    return _condition_hakaru_with_disint(program)


def generate_bn1_hakaru(n):
    """
    Generate a BN1 Hakaru program with duplicated female and male branches,
    matching generate_bn1_slice.
    """
    decision_f = _generate_hakaru_decision_tree(
        n=n,
        category_var="category_f",
        capital_gain_var="capital_gain_f",
        indent=7,
    )
    decision_m = _generate_hakaru_decision_tree(
        n=n,
        category_var="category_m",
        capital_gain_var="capital_gain_m",
        indent=7,
    )

    program = f"""# generated from the paired Slice query.

def observe_bool(b bool):
    if b:
        weight(1, return ())
    else:
        weight(0, return ())

sex_f <~ categorical([0.3307, 0.6693])

_ <~ observe_bool(sex_f < 1)

capital_gain_f <~ normal(5000.0, 1000000.0)

age_f <~ if capital_gain_f < 7000.0:
             normal(40.0, 10.0)
         else:
             normal(45.0, 10.0)

category_f <~ if capital_gain_f < 7000.0:
                  categorical([0.2, 0.2, 0.2, 0.2, 0.2])
              else:
                  categorical([0.2, 0.2, 0.2, 0.2, 0.2])

_ <~ observe_bool(age_f > 18.0)

t_f <~ {decision_f.lstrip()}

sex_m <~ categorical([0.3307, 0.6693])

_ <~ observe_bool(sex_m >= 1)

capital_gain_m <~ normal(6000.0, 1000000.0)

age_m <~ if capital_gain_m < 5000.0:
             normal(35.0, 10.0)
         else:
             normal(42.0, 10.0)

category_m <~ if capital_gain_m < 5000.0:
                  categorical([0.2, 0.2, 0.2, 0.2, 0.2])
              else:
                  categorical([0.2, 0.2, 0.2, 0.2, 0.2])

_ <~ observe_bool(age_m > 18.0)

t_m <~ {decision_m.lstrip()}

return ((), (t_f >= 1.0, t_m >= 1.0))
"""
    return _condition_hakaru_with_disint(program)


def generate_bn2_hakaru(n):
    """
    Generate a BN2 Hakaru program with duplicated female and male branches,
    matching generate_bn2_slice.
    """
    decision_f = _generate_hakaru_decision_tree(
        n=n,
        category_var="category_f",
        capital_gain_var="capital_gain_f",
        indent=7,
    )
    decision_m = _generate_hakaru_decision_tree(
        n=n,
        category_var="category_m",
        capital_gain_var="capital_gain_m",
        indent=7,
    )

    program = f"""# generated from the paired Slice query.

def observe_bool(b bool):
    if b:
        weight(1, return ())
    else:
        weight(0, return ())

sex_f <~ categorical([0.3307, 0.6693])

_ <~ observe_bool(sex_f < 1)

capital_gain_f <~ normal(5000.0, 1000000.0)

age_f <~ if capital_gain_f < 7000.0:
             normal(40.0, 10.0)
         else:
             normal(45.0, 10.0)

education_num_f <~ if capital_gain_f < 7000.0:
                       normal(10.0827, 6.5096)
                   else:
                       normal(10.1041, 6.1522)

category_f <~ if capital_gain_f < 7000.0:
                  categorical([0.2, 0.2, 0.2, 0.2, 0.2])
              else:
                  categorical([0.2, 0.2, 0.2, 0.2, 0.2])

_ <~ observe_bool(age_f > 18.0)

t_f <~ {decision_f.lstrip()}

sex_m <~ categorical([0.3307, 0.6693])

_ <~ observe_bool(sex_m >= 1)

capital_gain_m <~ normal(6000.0, 1000000.0)

age_m <~ if capital_gain_m < 5000.0:
             normal(35.0, 10.0)
         else:
             normal(42.0, 10.0)

education_num_m <~ if capital_gain_m < 5000.0:
                       normal(10.0817, 6.4841)
                   else:
                       normal(10.0974, 7.1793)

category_m <~ if capital_gain_m < 5000.0:
                  categorical([0.2, 0.2, 0.2, 0.2, 0.2])
              else:
                  categorical([0.2, 0.2, 0.2, 0.2, 0.2])

_ <~ observe_bool(age_m > 18.0)

t_m <~ {decision_m.lstrip()}

return ((), (t_f >= 1.0, t_m >= 1.0))
"""
    return _condition_hakaru_with_disint(program)


def generate_psi(n, variant):
    if variant == "ind":
        return generate_ind_psi(n)
    if variant == "bn1":
        return generate_bn1_psi(n)
    if variant == "bn2":
        return generate_bn2_psi(n)
    raise ValueError(f"unknown decision-tree variant: {variant}")


def generate_hakaru(n, variant):
    if variant == "ind":
        return generate_ind_hakaru(n)
    if variant == "bn1":
        return generate_bn1_hakaru(n)
    if variant == "bn2":
        return generate_bn2_hakaru(n)
    raise ValueError(f"unknown decision-tree variant: {variant}")


generate_ind_hk = generate_ind_hakaru
generate_bn1_hk = generate_bn1_hakaru
generate_bn2_hk = generate_bn2_hakaru
generate_hk = generate_hakaru


def generate_ind_sppl(n):
    """
    Generate an independent variables program with n conditionals.
    
    Args:
        n: Number of conditionals in the decision model
    
    Returns:
        String containing the complete program
    """
    program = """# Population model
age ~= norm(loc=38.5816, scale=186.0614)

education ~= choice({
    '0': 0.1644, '1': 0.2239, '2': 0.0361, '3': 0.3225,
    '4': 0.0177, '5': 0.0328, '6': 0.0424, '7': 0.0158,
    '8': 0.0198, '9': 0.0133, '10': 0.0530, '11': 0.0051,
    '12': 0.0287, '13': 0.0127, '14': 0.0102, '15': 0.0016
})

category ~= choice({
    '0': 0.0481, '1': 0.1557, '2': 0.4051,
    '3': 0.2551, '4': 0.0301, '5': 0.1059
})

sex ~= choice({'female': 0.3307, 'male': 0.6693})

capital_gain ~= norm(loc=1077.6488, scale=54542539.1784)

condition(age > 18)

# Decision model
"""
    
    # Number of categories to split conditionals across
    num_categories = 5
    conditionals_per_category = n // num_categories
    remainder = n % num_categories
    
    for cat in range(num_categories):
        cat_conditionals = conditionals_per_category + (1 if cat < remainder else 0)
        
        if cat == 0:
            program += f"if category == '{cat}':\n"
        else:
            program += f"elif category == '{cat}':\n"
        
        # Generate conditionals for this category
        for i in range(cat_conditionals):
            threshold = 100 + (i * 100)
            if i == 0:
                program += f"    if capital_gain < {threshold}: t ~= atomic(loc={i % 2})\n"
            else:
                program += f"    elif capital_gain < {threshold}: t ~= atomic(loc={i % 2})\n"
        
        # Add else clause for last conditional in category
        program += f"    else: t ~= atomic(loc={cat_conditionals % 2})\n"
    
    # Add final else clause
    program += "else:\n"
    program += "    if capital_gain < 4668.5: t ~= atomic(loc=1)\n"
    program += "    else: t ~= atomic(loc=0)\n"
    
    return program


def generate_bn1_sppl(n):
    """
    Generate a Bayesian network program (BN1) with n conditionals.
    
    Args:
        n: Number of conditionals in the decision model
    
    Returns:
        String containing the complete program
    """
    program = """# Population model
sex ~= choice({'female': 0.3307, 'male': 0.6693})

if (sex == 'female'):
    capital_gain ~= norm(loc=5000, scale=1000000)
    if capital_gain < 7000:
        age ~= norm(loc=40, scale=10)
        category ~= choice({'0': 0.2, '1': 0.2, '2': 0.2, '3': 0.2, '4': 0.2})
    else:
        age ~= norm(loc=45, scale=10)
        category ~= choice({'0': 0.2, '1': 0.2, '2': 0.2, '3': 0.2, '4': 0.2})
else:
    capital_gain ~= norm(loc=6000, scale=1000000)
    if capital_gain < 5000:
        age ~= norm(loc=35, scale=10)
        category ~= choice({'0': 0.2, '1': 0.2, '2': 0.2, '3': 0.2, '4': 0.2})
    else:
        age ~= norm(loc=42, scale=10)
        category ~= choice({'0': 0.2, '1': 0.2, '2': 0.2, '3': 0.2, '4': 0.2})

condition(age > 18)

# Decision model
"""
    
    # Number of categories to split conditionals across
    num_categories = 5
    conditionals_per_category = n // num_categories
    remainder = n % num_categories
    
    for cat in range(num_categories):
        cat_conditionals = conditionals_per_category + (1 if cat < remainder else 0)
        
        if cat == 0:
            program += f"if category == '{cat}':\n"
        else:
            program += f"elif category == '{cat}':\n"
        
        # Generate conditionals for this category
        for i in range(cat_conditionals):
            threshold = 100 + (i * 100)
            if i == 0:
                program += f"    if capital_gain < {threshold}: t ~= atomic(loc={i % 2})\n"
            else:
                program += f"    elif capital_gain < {threshold}: t ~= atomic(loc={i % 2})\n"
        
        # Add else clause for last conditional in category
        program += f"    else: t ~= atomic(loc={cat_conditionals % 2})\n"
    
    # Add final else clause
    program += "else:\n"
    program += "    if capital_gain < 4668.5: t ~= atomic(loc=1)\n"
    program += "    else: t ~= atomic(loc=0)\n"
    
    return program


def generate_bn2_sppl(n):
    """
    Generate a Bayesian network program with integrity constraint (BN2) with n conditionals.
    
    Args:
        n: Number of conditionals in the decision model
    
    Returns:
        String containing the complete program
    """
    program = """# Population model
sex ~= choice({'female': 0.3307, 'male': 0.6693})

if (sex == 'female'):
    capital_gain ~= norm(loc=5000, scale=1000000)
    if capital_gain < 7000:
        age ~= norm(loc=40, scale=10)
        education_num ~= norm(loc=10.0827, scale=6.5096)
        category ~= choice({'0': 0.2, '1': 0.2, '2': 0.2, '3': 0.2, '4': 0.2})
    else:
        age ~= norm(loc=45, scale=10)
        education_num ~= norm(loc=10.1041, scale=6.1522)
        category ~= choice({'0': 0.2, '1': 0.2, '2': 0.2, '3': 0.2, '4': 0.2})
else:
    capital_gain ~= norm(loc=6000, scale=1000000)
    if capital_gain < 5000:
        age ~= norm(loc=35, scale=10)
        education_num ~= norm(loc=10.0817, scale=6.4841)
        category ~= choice({'0': 0.2, '1': 0.2, '2': 0.2, '3': 0.2, '4': 0.2})
    else:
        age ~= norm(loc=42, scale=10)
        education_num ~= norm(loc=10.0974, scale=7.1793)
        category ~= choice({'0': 0.2, '1': 0.2, '2': 0.2, '3': 0.2, '4': 0.2})

condition(age > 18)

# Decision model
"""
    
    # Number of categories to split conditionals across
    num_categories = 5
    conditionals_per_category = n // num_categories
    remainder = n % num_categories
    
    for cat in range(num_categories):
        cat_conditionals = conditionals_per_category + (1 if cat < remainder else 0)
        
        if cat == 0:
            program += f"if category == '{cat}':\n"
        else:
            program += f"elif category == '{cat}':\n"
        
        # Generate conditionals for this category
        for i in range(cat_conditionals):
            threshold = 100 + (i * 100)
            if i == 0:
                program += f"    if capital_gain < {threshold}: t ~= atomic(loc={i % 2})\n"
            else:
                program += f"    elif capital_gain < {threshold}: t ~= atomic(loc={i % 2})\n"
        
        # Add else clause for last conditional in category
        program += f"    else: t ~= atomic(loc={cat_conditionals % 2})\n"
    
    # Add final else clause
    program += "else:\n"
    program += "    if capital_gain < 4668.5: t ~= atomic(loc=1)\n"
    program += "    else: t ~= atomic(loc=0)\n"
    
    return program

# Example usage
if __name__ == "__main__":
    # Generate programs with different numbers of conditionals
    print("=" * 80)
    print("IND Program with 1000 conditionals:")
    print("=" * 80)
    print(generate_ind_slice(1000))
