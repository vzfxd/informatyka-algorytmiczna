module blocksys

using SparseArrays

export load_matrix_from_file,load_vector_from_file,calc_right_side_vector,gauss,gauss_pivot,lu_blocksys,lu_pivot,solve_lu,solve_lu_pivot

global _N = 0
global _L = 0

function load_matrix_from_file(filename::String)
    row::Vector{Int64} = Int64[]
    col::Vector{Int64} = Int64[]
    val::Vector{Float64} = Float64[]

    open(filename) do f
        while !eof(f)   
            l = readline(f)
            d = split(l," ")

            if length(d) == 2
                global _N = parse(Int64, d[1])
                global _L = parse(Int64, d[2])
                continue
            end

            push!(row, parse(Int64, d[1]))
            push!(col, parse(Int64, d[2]))
            push!(val, parse(Float64, d[3]))
        end
    end

    return sparse(row,col,val)
end

function load_vector_from_file(filename::String)
    v::Vector{Float64} = Float64[]
    
    open(filename) do f
        readline(f)
        while !eof(f)
            l = readline(f)
            push!(v, parse(Float64,l))
        end
    end
    
    return v
end

function calc_right_side_vector(m::SparseMatrixCSC{Float64,Int64})
    b = zeros(Float64, _N)

    for k in 1:_N
        p = max(1, k-_L)
        q = min(_N, k+_L)

        for j in p:q
            b[k] += m[k, j]
        end
    end
        
    return b
end

function gauss(m::SparseMatrixCSC{Float64,Int64}, b::Vector{Float64})
    for k in 1:_N-1
        for i in k+1:min(_N,_L+k)
            l = m[i, k] / m[k, k]

            for j in k+1:min(_N,_L+k+1)
                m[i, j] -= l * m[k, j]
            end

            b[i] -= l * b[k]
        end
    end

    x::Vector{Float64} = zeros(Float64, _N)
    for k in _N:-1:1
        sum = 0.0
        for j in k+1:min(_N,_L+k+1)
            sum += m[k, j] * x[j]
        end
        x[k] = (b[k] - sum)/m[k,k]
    end

    return x
end

function gauss_pivot(m::SparseMatrixCSC{Float64,Int64}, b::Vector{Float64})
    #pivot[3] = 6 tzn ze w 3 wierszu jest 6 wiersz i na odwrot
    pivots = [i for i in 1:_N]

    for k in 1:_N-1
        diag = m[pivots[k],k]
        row = k
        
        for i in k+1:min(_N, k+_L+1)
            if abs(m[pivots[i],k]) > abs(diag)
                diag = m[pivots[i], k]
                row = i
            end
        end

        pivots[row], pivots[k] = pivots[k], pivots[row]

        for i in k+1:min(_N, k+_L+1)
            l = m[pivots[i], k] / diag

            for j in k+1:min(_N, k+2*_L)
                m[pivots[i], j] -= l * m[pivots[k], j]
            end

            b[pivots[i]] -= l * b[pivots[k]]
        end
    end

    x::Vector{Float64} = zeros(Float64, _N)
    for k in _N:-1:1
        sum = 0.0
        for j in k+1:min(_N, k+2*_L)
            sum += m[pivots[k], j] * x[j]
        end
        x[k] = (b[pivots[k]] - sum) / m[pivots[k], k]
    end

    return x
end

function lu_blocksys(U::SparseMatrixCSC{Float64,Int64})
    L::SparseMatrixCSC = SparseArrays.spzeros(_N, _N)
    for k in 1:_N-1
        for i in k+1:min(_N,k+_L)
            l = U[i, k] / U[k, k]
            L[i, k] = l

            for j in k+1:min(_N, k+_L+1)
                U[i, j] -= l *U[k, j]
            end
        end
    end
    return L
end

function lu_pivot(U::SparseMatrixCSC{Float64,Int64})
    L::SparseMatrixCSC = SparseArrays.spzeros(_N, _N)
    pivots = [i for i in 1:_N]

    for k in 1:_N-1
        diag = U[pivots[k],k]
        row = k
        
        for i in k+1:min(_N, k+_L+1)
            if abs(U[pivots[i],k]) > abs(diag)
                diag = U[pivots[i], k]
                row = i
            end
        end

        pivots[row], pivots[k] = pivots[k], pivots[row]

        for i in k+1:min(_N, k+_L+1)
            l = U[pivots[i], k] / diag
            L[pivots[i], k] = l

            for j in k+1:min(_N, k+2*_L)
                U[pivots[i], j] -= l * U[pivots[k], j]
            end
        end
    end
    return L,pivots
end

function solve_lu_pivot(L::SparseMatrixCSC{Float64,Int64}, U::SparseMatrixCSC{Float64,Int64}, b::Vector{Float64}, pivots::Vector{Int64})
    x::Vector{Float64} = zeros(Float64, _N)
    for k in 1:_N-1
        for i in k+1:min(_N, k+_L+1)
            b[pivots[i]] -= L[pivots[i], k] * b[pivots[k]]
        end
    end

    for k in _N:-1:1
        sum = 0.0
        for j in k+1:min(_N, k+2*_L)
            sum += U[pivots[k], j] * x[j]
        end
        x[k] = (b[pivots[k]] - sum) / U[pivots[k], k]
    end
    return x
end

function solve_lu(L::SparseMatrixCSC{Float64,Int64}, U::SparseMatrixCSC{Float64,Int64}, b::Vector{Float64})
    x::Vector{Float64} = zeros(Float64, _N)
    for k in 1:_N-1
        for i in k+1:min(_N, k+_L+1)
            b[i] -= L[i, k] * b[k]
        end
    end

    for k in _N:-1:1
        sum = 0.0
        for j in k+1:min(_N, k+_L)
            sum += U[k, j] * x[j]
        end
        x[k] = (b[k] - sum) / U[k, k]
    end
    return x
end

end
