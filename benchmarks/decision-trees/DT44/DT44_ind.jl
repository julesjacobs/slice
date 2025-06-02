using Alea, Distributions

DFiP = DistFix{30, 2}
inf = 33554432.0
inf_neg = -33554432.0

code = @alea begin
    age = bitblast(DFiP, Normal(38.5816, 186.0614), 256, inf_neg, inf)
    education = discrete(DistUInt{16}, [0.1644, 0.2239, 0.0361, 0.3225, 0.0177, 0.0328, 0.0424, 0.0158, 0.0198, 0.0133, 0.0530, 0.0051, 0.0287, 0.0127, 0.0102, 0.0016])
    relationship = discrete(DistUInt{6}, [0.0481, 0.1557, 0.4051, 0.2551, 0.0301, 0.1059])
    sex = flip(0.3307)
    capital_gain = bitblast(DFiP, Normal(1077.6488, 54542539.1784), 256, inf_neg, inf)

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