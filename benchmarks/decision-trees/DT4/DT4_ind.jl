using Alea, Distributions

DFiP = DistFix{30, 2}
inf = 33554432.0
inf_neg = -33554432.0

code = @alea begin
    # Population model
    age = bitblast(DFiP, Normal(38.5816, 186.0614), 256, inf_neg, inf)
    sex = flip(0.3307)
    capital_gain = bitblast(DFiP, Normal(1077.6488, 54542539.1784), 256, inf_neg, inf)

    observe(sex)
    observe(age > DFiP(18.0))

    # Decision model
    if capital_gain >= DFiP(7073.5)
        if age < DFiP(20.0)
            t = DFiP(1.0)
        else
            t = DFiP(0.0)
        end
    else
        t = DFiP(1.0)
    end

    result = t < DFiP(0.5)
    result
end

@show pr(code)