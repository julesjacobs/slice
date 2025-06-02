using Alea, Distributions

DFiP = DistFix{30, 2}
inf = 33554432.0
inf_neg = -33554432.0

code = @alea begin
    age = bitblast(DFiP, Normal(38.5816, 186.0614), 256, inf_neg, inf)
    relationship = discrete(DistUInt{6}, [0.0481, 0.1557, 0.4051, 0.2551, 0.0301, 0.1059])
    sex = flip(0.3307)
    capital_gain = bitblast(DFiP, Normal(1077.6488, 54542539.1784), 256, inf_neg, inf)

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