# TODO: 
- Set up proper dockerfile and image: set up opam and rust paths, install sppl in dockerfile
- See if you want to represent the #k as k = num of bits (involves a translation from the user)

Suggestion: use program size (e.g., number of < tests) as the x-axis measure. We may also disregard this example and focus on the linearly sized programs.
- ie in dice, they grow the program by adding one additional layer to the chain of flips that depends on the previous. With this growing pattern, the number of terms that a path enumeration must explore grows exponentially in the number
of layers, so clearly exhaustive enumeration does not scale

Later:
- Take care of higher-order functions (not supported in dice but supported in cdice)
