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
            education = discrete(DistUInt{16}, [0.1638, 0.2308, 0.0354, 0.3230, 0.0173, 0.0321, 0.0412, 0.0156, 0.0200, 0.0112, 0.0528, 0.0050, 0.0290, 0.0119, 0.0092, 0.0017])
            relationship = discrete(DistUInt{6}, [0.0491, 0.1556, 0.4012, 0.2589, 0.0294, 0.1058])
        else
            age = bitblast(DFiP, Normal(38.8125, 193.4918), 256, inf_neg, inf)
            education = discrete(DistUInt{16}, [0.1916, 0.2000, 0.0500, 0.3542, 0.0208, 0.0125, 0.0375, 0.0125, 0.0292, 0.0042, 0.0541, 0.0000, 0.0250, 0.0042, 0.0042, 0.0000])
            relationship = discrete(DistUInt{6}, [0.0416, 0.1667, 0.4583, 0.2292, 0.0166, 0.0876])
        end
    else
        capital_gain = bitblast(DFiP, Normal(1329.3700, 69327473.1006), 256, inf_neg, inf)
        if capital_gain < DFiP(5178.0000)
            age = bitblast(DFiP, Normal(38.6361, 187.2435), 256, inf_neg, inf)
            education = discrete(DistUInt{16}, [0.1670, 0.2239, 0.0358, 0.3267, 0.0159, 0.0320, 0.0426, 0.0155, 0.0198, 0.0121, 0.0518, 0.0047, 0.0287, 0.0125, 0.0096, 0.0014])
            relationship = discrete(DistUInt{6}, [0.0497, 0.1545, 0.4021, 0.2590, 0.0294, 0.1053])
        else
            age = bitblast(DFiP, Normal(38.2668, 187.2747), 256, inf_neg, inf)
            education = discrete(DistUInt{16}, [0.1569, 0.2205, 0.0417, 0.3071, 0.0255, 0.0302, 0.0409, 0.0155, 0.0178, 0.0147, 0.0619, 0.0062, 0.0317, 0.0139, 0.0139, 0.0016])
            relationship = discrete(DistUInt{6}, [0.0417, 0.1624, 0.3976, 0.2606, 0.0356, 0.1021])
        end
    end

    observe(sex)
    observe(age > DFiP(18.0))

    if prob_equals(relationship, DistUInt{6}(0))
        if prob_equals(education, DistUInt{16}(0))
            t = DFiP(0.0)
        elseif prob_equals(education, DistUInt{16}(1))
            t = DFiP(1.0)
        elseif prob_equals(education, DistUInt{16}(2))
            t = DFiP(1.0)
        elseif prob_equals(education, DistUInt{16}(3))
            t = DFiP(1.0)
        elseif prob_equals(education, DistUInt{16}(4))
            t = DFiP(0.0)
        elseif prob_equals(education, DistUInt{16}(5))
            t = DFiP(0.0)
        elseif prob_equals(education, DistUInt{16}(6))
            t = DFiP(0.0)
        elseif prob_equals(education, DistUInt{16}(7))
            t = DFiP(1.0)
        elseif prob_equals(education, DistUInt{16}(8))
            t = DFiP(1.0)
        elseif prob_equals(education, DistUInt{16}(9))
            t = DFiP(1.0)
        elseif prob_equals(education, DistUInt{16}(10))
            t = DFiP(0.0)
        elseif prob_equals(education, DistUInt{16}(11))
            t = DFiP(1.0)
        elseif prob_equals(education, DistUInt{16}(12))
            t = DFiP(1.0)
        elseif prob_equals(education, DistUInt{16}(13))
            t = DFiP(0.0)
        elseif prob_equals(education, DistUInt{16}(14))
            t = DFiP(1.0)
        else
            t = DFiP(1.0)
        end
    elseif prob_equals(relationship, DistUInt{6}(1))
        if capital_gain < DFiP(4718.5)
            t = DFiP(1.0)
        else
            t = DFiP(0.0)
        end
    elseif prob_equals(relationship, DistUInt{6}(2))
        if prob_equals(education, DistUInt{16}(0))
            t = DFiP(0.0)
        elseif prob_equals(education, DistUInt{16}(1))
            t = DFiP(1.0)
        elseif prob_equals(education, DistUInt{16}(2))
            t = DFiP(1.0)
        elseif prob_equals(education, DistUInt{16}(3))
            t = DFiP(1.0)
        elseif prob_equals(education, DistUInt{16}(4))
            t = DFiP(0.0)
        elseif prob_equals(education, DistUInt{16}(5))
            t = DFiP(1.0)
        elseif prob_equals(education, DistUInt{16}(6))
            t = DFiP(1.0)
        elseif prob_equals(education, DistUInt{16}(7))
            t = DFiP(1.0)
        elseif prob_equals(education, DistUInt{16}(8))
            t = DFiP(1.0)
        elseif prob_equals(education, DistUInt{16}(9))
            t = DFiP(1.0)
        elseif prob_equals(education, DistUInt{16}(10))
            t = DFiP(0.0)
        elseif prob_equals(education, DistUInt{16}(11))
            t = DFiP(1.0)
        elseif prob_equals(education, DistUInt{16}(12))
            t = DFiP(1.0)
        elseif prob_equals(education, DistUInt{16}(13))
            t = DFiP(0.0)
        elseif prob_equals(education, DistUInt{16}(14))
            t = DFiP(1.0)
        else
            t = DFiP(1.0)
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