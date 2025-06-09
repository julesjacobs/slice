# Invalid Parameter Tests

This directory contains test cases for invalid distribution parameters to verify that the parameter validation system works correctly.

## Purpose

These tests verify that:
1. Invalid distribution parameters are caught during discretization
2. Runtime errors are properly inserted into the discretized program
3. Error messages are consistent and informative
4. The program fails fast when invalid parameters are encountered

## Test Files

- `exponential_negative_lambda.slice` - Tests exponential distribution with negative λ parameter
- `gaussian_negative_sigma.slice` - Tests Gaussian distribution with negative σ parameter  
- `uniform_reversed_bounds.slice` - Tests uniform distribution with low > high bounds
- `laplace_negative_scale.slice` - Tests Laplace distribution with negative scale parameter
- `beta_negative_parameters.slice` - Tests Beta distribution with negative α parameter
- `gamma_zero_scale.slice` - Tests Gamma distribution with zero scale parameter

## How to Run

To test any of these files and see the runtime error insertion:

```bash
dune exec bin/main.exe -- --print-all examples/invalid_tests/<test_file>.slice
```

## Expected Behavior

For each test file, you should see:
1. **Discretized Program (Pretty):** Shows `RUNTIME_ERROR("...")`  inserted in place of the distribution
2. **Runtime Error:** The program fails immediately when the runtime error is encountered

## Example Output

```
Discretized Program (Pretty):
let x = RUNTIME_ERROR("Exponential lambda must be positive") in
x >#2 0#2

*** Error processing file: Runtime Error during Discretized run after 0 runs: Exponential lambda must be positive. Aborting run. *** 