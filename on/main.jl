module main

include("blocksys.jl")
include("matrixgen.jl")

using .blocksys
using .matrixgen
using SparseArrays
using LinearAlgebra

function test_lu_pivot(m::SparseMatrixCSC{Float64,Int64}, b::Vector{Float64})
    L,pivots = lu_pivot(m)
    x = solve_lu_pivot(L,m,b,pivots)
    return x
end

function test_lu(m::SparseMatrixCSC{Float64,Int64}, b::Vector{Float64})
    L = lu_blocksys(m)
    x = solve_lu(L,m,b)
    return x
end

function test_gauss(m::SparseMatrixCSC{Float64,Int64}, b::Vector{Float64})
    x = gauss(m,b)
    return x
end

function test_gauss_pivot(m::SparseMatrixCSC{Float64,Int64}, b::Vector{Float64})
    x = gauss_pivot(m,b)
    return x
end

for n in 500:500:10000
    reps = 100
    err = 0.0
    for r in 1:reps
        blockmat(n, 5, 1.0, "M.txt")
        m = load_matrix_from_file("M.txt")
        b = calc_right_side_vector(m)
        real_x = ones(n)
        x = test_lu_pivot(m,b)
        err += norm(x-real_x) / norm(real_x)
    end
    println(n, "|", err)
end

end