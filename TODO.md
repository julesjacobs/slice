- Change 'not' in pretty print to '!' to be compatible with dice
- Add 0. to fill the empty spaces for mismatching discrete distributions
- Change discrete(0, 1) and discrete(1, 0) distributions to discrete(0., 1.) and discrete(1., 0.) respectively
- Take max size of all int sizes -- Actually let's use the n#k types directly: those already have the right sizes without using a global max, which is suboptimal
- Install SPPL, create benchmark script

Later:
- Take care of higher-order functions (not supported in dice but supported in cdice)
