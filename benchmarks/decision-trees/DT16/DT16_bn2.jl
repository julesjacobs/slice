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
            relationship = discrete(DistUInt{6}, [0.0491, 0.1556, 0.4012, 0.2589, 0.0294, 0.1058])
        else
            age = bitblast(DFiP, Normal(38.8125, 193.4918), 256, inf_neg, inf)
            education_num = bitblast(DFiP, Normal(10.1041, 6.1522), 256, inf_neg, inf)
            relationship = discrete(DistUInt{6}, [0.0416, 0.1667, 0.4583, 0.2292, 0.0166, 0.0876])
        end
    else
        capital_gain = bitblast(DFiP, Normal(1329.3700, 69327473.1006), 256, inf_neg, inf)
        if capital_gain < DFiP(5178.0000)
            age = bitblast(DFiP, Normal(38.6361, 187.2435), 256, inf_neg, inf)
            education_num = bitblast(DFiP, Normal(10.0817, 6.4841), 256, inf_neg, inf)
            relationship = discrete(DistUInt{6}, [0.0497, 0.1545, 0.4021, 0.2590, 0.0294, 0.1053])
        else
            age = bitblast(DFiP, Normal(38.2668, 187.2747), 256, inf_neg, inf)
            education_num = bitblast(DFiP, Normal(10.0974, 7.1793), 256, inf_neg, inf)
            relationship = discrete(DistUInt{6}, [0.0417, 0.1624, 0.3976, 0.2606, 0.0356, 0.1021])
        end
    end

    observe(sex)
    observe(age > DFiP(18.0))

    if prob_equals(relationship, DistUInt{6}(0))
        if capital_gain < DFiP(5095.5)
            t = DFiP(1.0)
        else
            t = DFiP(0.0)
        end
    elseif prob_equals(relationship, DistUInt{6}(1))
        if capital_gain < DFiP(4718.5)
            t = DFiP(1.0)
        else
            t = DFiP(0.0)
        end
    elseif prob_equals(relationship, DistUInt{6}(2))
        if capital_gain < DFiP(5095.5)
            t = DFiP(1.0)
        else
            t = DFiP(0.0)
        end
    elseif prob_equals(relationship, DistUInt{6}(3))
        if capital_gain < DFiP(8296.0)
            t = DFiP(1.0)
        else
            t = DFiP(0.0)
        end
    elseif prob_equals(relationship, DistUInt{6}(4))
        t = DFiP(1.0)
    else
        if capital_gain < DFiP(4668.5)
            t = DFiP(1.0)
        else
            t = DFiP(0.0)
        end
    end

    result = t < DFiP(0.5)
    result
end

@show pr(code)