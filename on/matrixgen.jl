# Pawel Zielinski

module matrixgen
using LinearAlgebra
export  blockmat

function matcond(n::Int, c::Float64)
    if n < 2
     error("size n should be > 1")
    end
    if c< 1.0
     error("condition number  c of a matrix  should be >= 1.0")
    end
    (U,S,V)=svd(rand(n,n))
    return U*diagm(0 =>[LinRange(1.0,c,n);])*V'
end

function blockmat(n::Int, l::Int, ck::Float64, outputfile::String)			
    if n < 2
        error("size n should be > 1")
    end
    if n%l!=0 
        error("n is not divisible by l")
    end
                
    nb=div(n,l)
    Ak=Matrix{Float64}(undef, l, l)		
    open(outputfile, "w") do f
        println(f, n," ",l)
        for k in 1:nb
            Ak=matcond(l, ck)
            for i in 1:l, j in 1:l
                println(f,(k-1)*l+i," ",(k-1)*l+j," ", Ak[i,j])
            end
            if k<nb
                for i in 1:l
                    println(f,(k-1)*l+i," ",k*l+i," ",0.3*rand())
                    end
            end
            if k>1
            for i in 1:l
                    println(f,(k-1)*l+i," ",(k-1)*l," ",0.3*rand())
                end
            end 
        end
    end
end

end 