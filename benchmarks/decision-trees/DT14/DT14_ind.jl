using Alea, Distributions

DFiP = DistFix{30, 2}
inf = 33554432.0
inf_neg = -33554432.0

code = @alea begin
    # Population model
    age = bitblast(DFiP, Normal(38.5816, 186.0614), 256, inf_neg, inf)
    relationship = discrete(DistUInt{6}, [0.0481, 0.1557, 0.4051, 0.2551, 0.0301, 0.1059])
    sex = flip(0.3307)

    observe(sex)
    observe(age > DFiP(18.0))

    # Decision model
    if prob_equals(relationship, DistUInt{6}(0))
        t = DFiP(1.0)
    elseif prob_equals(relationship, DistUInt{6}(1))
        if age < DFiP(21.5)
            t = DFiP(1.0)
        elseif age < DFiP(47.5)
            t = DFiP(1.0)
        else
            t = DFiP(0.0)
        end
    elseif prob_equals(relationship, DistUInt{6}(2))
        t = DFiP(1.0)
    elseif prob_equals(relationship, DistUInt{6}(3))
        if age < DFiP(50.5)
            t = DFiP(1.0)
        else
            t = DFiP(0.0)
        end
    elseif prob_equals(relationship, DistUInt{6}(4))
        if age < DFiP(49.5)
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