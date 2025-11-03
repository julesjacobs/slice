def generate_ind_program(n):
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


def generate_bn1_program(n):
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


def generate_bn2_program(n):
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


def generate_ind_slice(n):
    """
    Generate an independent variables Slice program with n conditionals.
    
    Args:
        n: Number of conditionals in the decision model
    
    Returns:
        String containing the complete Slice program
    """
    program = """let age = gaussian(38.5816, 186.0614) in
let education = discrete(
  0.1644, 0.2239, 0.0361, 0.3225,
  0.0177, 0.0328, 0.0424, 0.0158,
  0.0198, 0.0133, 0.0530, 0.0051,
  0.0287, 0.0127, 0.0102, 0.0016) in
let category = discrete(
  0.0481, 0.1557, 0.4051,
  0.2551, 0.0301, 0.1059) in
let sex = discrete(0.3307, 0.6693) in
let capital_gain = gaussian(1077.6488, 54542539.1784) in
let _1 = observe(sex <#2 1#2) in
let _2 = observe(age > 18.0) in
let t =
"""
    
    # Number of categories to split conditionals across
    num_categories = 5
    conditionals_per_category = n // num_categories
    remainder = n % num_categories
    
    for cat in range(num_categories):
        cat_conditionals = conditionals_per_category + (1 if cat < remainder else 0)
        
        if cat == 0:
            program += f"  if category ==#6 {cat}#6 then\n"
        else:
            program += f"  else if category ==#6 {cat}#6 then\n"
        
        # Generate conditionals for this category
        for i in range(cat_conditionals):
            threshold = 100 + (i * 100)
            loc_value = "1.0" if (i % 2 == 0) else "0.0"
            
            if i == 0:
                program += f"    if capital_gain < {threshold}.0 then {loc_value}\n"
            else:
                program += f"    else if capital_gain < {threshold}.0 then {loc_value}\n"
        
        # Add else clause for last conditional in category
        else_loc = "1.0" if (cat_conditionals % 2 == 0) else "0.0"
        program += f"    else {else_loc}\n"
    
    # Add final else clause
    program += "  else\n"
    program += "    if capital_gain < 4668.5 then 1.0 else 0.0\n"
    program += "in\n"
    program += "t < 0.5\n"
    
    return program


def generate_bn1_slice(n):
    """
    Generate a Bayesian network Slice program (BN1) with n conditionals.
    
    Args:
        n: Number of conditionals in the decision model
    
    Returns:
        String containing the complete Slice program
    """
    program = """let sex = discrete(0.3307, 0.6693) in
let capital_gain =
  if sex <#2 1#2 then
    gaussian(5000.0, 1000000.0)
  else
    gaussian(6000.0, 1000000.0) in
let age =
  if sex <#2 1#2 then
    if capital_gain < 7000.0 then gaussian(40.0, 10.0)
    else gaussian(45.0, 10.0)
  else
    if capital_gain < 5000.0 then gaussian(35.0, 10.0)
    else gaussian(42.0, 10.0) in
let category =
  if sex <#2 1#2 then
    if capital_gain < 7000.0 then
      discrete(0.2, 0.2, 0.2, 0.2, 0.2)
    else
      discrete(0.2, 0.2, 0.2, 0.2, 0.2)
  else
    if capital_gain < 5000.0 then
      discrete(0.2, 0.2, 0.2, 0.2, 0.2)
    else
      discrete(0.2, 0.2, 0.2, 0.2, 0.2) in
let _1 = observe(sex <#2 1#2) in
let _2 = observe(age > 18.0) in
let t =
"""
    
    # Number of categories to split conditionals across
    num_categories = 5
    conditionals_per_category = n // num_categories
    remainder = n % num_categories
    
    for cat in range(num_categories):
        cat_conditionals = conditionals_per_category + (1 if cat < remainder else 0)
        
        if cat == 0:
            program += f"  if category ==#5 {cat}#5 then\n"
        else:
            program += f"  else if category ==#5 {cat}#5 then\n"
        
        # Generate conditionals for this category
        for i in range(cat_conditionals):
            threshold = 100 + (i * 100)
            loc_value = "1.0" if (i % 2 == 0) else "0.0"
            
            if i == 0:
                program += f"    if capital_gain < {threshold}.0 then {loc_value}\n"
            else:
                program += f"    else if capital_gain < {threshold}.0 then {loc_value}\n"
        
        # Add else clause for last conditional in category
        else_loc = "1.0" if (cat_conditionals % 2 == 0) else "0.0"
        program += f"    else {else_loc}\n"
    
    # Add final else clause
    program += "  else\n"
    program += "    if capital_gain < 4668.5 then 1.0 else 0.0\n"
    program += "in\n"
    program += "t < 0.5\n"
    
    return program


def generate_bn2_slice(n):
    """
    Generate a Bayesian network Slice program with integrity constraint (BN2) with n conditionals.
    
    Args:
        n: Number of conditionals in the decision model
    
    Returns:
        String containing the complete Slice program
    """
    program = """let sex = discrete(0.3307, 0.6693) in
let capital_gain =
  if sex <#2 1#2 then
    gaussian(5000.0, 1000000.0)
  else
    gaussian(6000.0, 1000000.0) in
let age =
  if sex <#2 1#2 then
    if capital_gain < 7000.0 then gaussian(40.0, 10.0)
    else gaussian(45.0, 10.0)
  else
    if capital_gain < 5000.0 then gaussian(35.0, 10.0)
    else gaussian(42.0, 10.0) in
let education_num =
  if sex <#2 1#2 then
    if capital_gain < 7000.0 then gaussian(10.0827, 6.5096)
    else gaussian(10.1041, 6.1522)
  else
    if capital_gain < 5000.0 then gaussian(10.0817, 6.4841)
    else gaussian(10.0974, 7.1793) in
let category =
  if sex <#2 1#2 then
    if capital_gain < 7000.0 then
      discrete(0.2, 0.2, 0.2, 0.2, 0.2)
    else
      discrete(0.2, 0.2, 0.2, 0.2, 0.2)
  else
    if capital_gain < 5000.0 then
      discrete(0.2, 0.2, 0.2, 0.2, 0.2)
    else
      discrete(0.2, 0.2, 0.2, 0.2, 0.2) in
let _1 = observe(sex <#2 1#2) in
let _2 = observe(age > 18.0) in
let t =
"""
    
    # Number of categories to split conditionals across
    num_categories = 5
    conditionals_per_category = n // num_categories
    remainder = n % num_categories
    
    for cat in range(num_categories):
        cat_conditionals = conditionals_per_category + (1 if cat < remainder else 0)
        
        if cat == 0:
            program += f"  if category ==#5 {cat}#5 then\n"
        else:
            program += f"  else if category ==#5 {cat}#5 then\n"
        
        # Generate conditionals for this category
        for i in range(cat_conditionals):
            threshold = 100 + (i * 100)
            loc_value = "1.0" if (i % 2 == 0) else "0.0"
            
            if i == 0:
                program += f"    if capital_gain < {threshold}.0 then {loc_value}\n"
            else:
                program += f"    else if capital_gain < {threshold}.0 then {loc_value}\n"
        
        # Add else clause for last conditional in category
        else_loc = "1.0" if (cat_conditionals % 2 == 0) else "0.0"
        program += f"    else {else_loc}\n"
    
    # Add final else clause
    program += "  else\n"
    program += "    if capital_gain < 4668.5 then 1.0 else 0.0\n"
    program += "in\n"
    program += "t < 0.5\n"
    
    return program


# Example usage
if __name__ == "__main__":
    # Generate programs with different numbers of conditionals
    print("=" * 80)
    print("IND Program with 1000 conditionals:")
    print("=" * 80)
    print(generate_ind_slice(1000))