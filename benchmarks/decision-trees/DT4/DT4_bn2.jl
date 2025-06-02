using Alea, Distributions

DFiP = DistFix{30, 2}
inf = 33554432.0
inf_neg = -33554432.0

code = @alea begin
    sex = flip(0.3307)
    if sex 
        capital_gain = bitblast(DFiP, Normal(568.4105, 24248365.5428), 256, inf_neg, inf)
        if capital_gain < DFiP(7298.0000)
            age = bitblast(DFiP, Normal(38.4208, 184.9151), 256, inf_neg, inf)
            education_num = bitblast(DFiP, Normal(10.0827, 6.5096), 256, inf_neg, inf)
        else
            age = bitblast(DFiP, Normal(38.8125, 193.4918), 256, inf_neg, inf)
            education_num = bitblast(DFiP, Normal(10.1041, 6.1522), 256, inf_neg, inf)
        end
    else
        capital_gain = bitblast(DFiP, Normal(1329.3700, 69327473.1006), 256, inf_neg, inf)
        if capital_gain < DFiP(5178.0000)
            age = bitblast(DFiP, Normal(38.6361, 187.2435), 256, inf_neg, inf)
            education_num = bitblast(DFiP, Normal(10.0817, 6.4841), 256, inf_neg, inf)
        else
            age = bitblast(DFiP, Normal(38.2668, 187.2747), 256, inf_neg, inf)
            education_num = bitblast(DFiP, Normal(10.0974, 7.1793), 256, inf_neg, inf)
        end
    end

    observe(sex)
    observe(age > DFiP(18.0))

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