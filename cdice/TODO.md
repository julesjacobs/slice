# More flexible language and analysis

Currently, we analyze comparisons x < k where k is a constant. The programmer needs to syntactically write the tests in that form.

Furthermore, no arithmetic operations are supported at all.

We can address this by analyzing two properties:
1. Which values can an expression take on: either a finite set of constants, or `any`, which stands for the entire real line. This forms a dataflow lattice.
2. Which constants an expression is compared to. This is either a finite list of bounds of the form `<k` and `<=k` (sorted), or again `any`, indicating that the expression may end up being compared to any value.

This framework would support more flexible comparisons e1 < e2 and e1 <= e2, as well as a limited form of arithmetic. For example:

    let a = if flip() then 0 else 1 in
    let a' = if flip() then 1 else 2 in
    let b = uniform(0,3) in
    let c = (b < a + a') in ...

This would be fine because `a + a'` takes on only finitely many different values. So in a comparison e1 < e2 or e1 <= e2, either e1 or e2 must take on finitely many different values, and the other must be comparable to that set of values.
