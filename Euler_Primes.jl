__precompile__()

module Primes

import Base

const offwheel = Array{Bool}(210)
fill!(offwheel, false)
for p in [2, 3, 5, 7], i in 1:210÷p
    offwheel[i*p] = true
end

const incs = Int[]

function gen_incs()
    c=1
    for i=2:210
        if offwheel[i]
            c+=1
        else
            push!(incs, c)
            c=1
        end
    end
    push!(incs,c)
end

gen_incs()

struct Iter{P <: Integer}
    small_primes :: Array{P}
    sieve :: Dict{P, P}
    offwheel :: Array{Bool}

end

function Iter{P}() where P <: Integer
    first_primes = P[   2,   3,   5,   7,  11,  13,  17,  19,  23,  29,  31
                    ,  37,  41,  43,  47,  53,  59,  61,  67,  71 , 73,  79
                    ,  83,  89,  97, 101, 103, 107, 109, 113, 127, 131, 137
                    , 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193
                    , 197, 199, 211]

    sieve = Dict{P, P}()

    res = Iter{P}(first_primes, sieve, offwheel)
end

const IterState{P} = Tuple{P, Int}

function Base.start(it::Iter{P})::IterState{P} where P <: Integer
    (1, length(incs)-1)
end

function is_wheeled_off(n :: P) :: Bool where P <: Integer
    offwheel[(n-1) % 210 + 1]
end


function Base.next(it::Iter{P}, state::IterState{P}) where P <: Integer
    previous, inc_idx = state

    if isempty(it.small_primes)
        candidate = previous + incs[inc_idx]

        inc_idx = inc_idx == length(incs) ? 1 : inc_idx+1

        period = pop!(it.sieve, candidate, 0)

        while period != 0
            scratched = candidate + period
            while is_wheeled_off(scratched) || haskey(it.sieve, scratched)
                scratched += period
            end
            it.sieve[scratched] = period

            candidate += incs[inc_idx]
            inc_idx = inc_idx == length(incs) ? 1 : inc_idx+1

            period = get(it.sieve, candidate, 0)
        end

        it.sieve[candidate^2] = candidate
        (candidate, (candidate, inc_idx))
    else
        prime = shift!(it.small_primes)

        if prime > 7
            scratched = prime^2
            while scratched < 210 || is_wheeled_off(scratched)
                scratched += prime
            end
            it.sieve[scratched] = prime
        end

        (prime, (prime, 1))
    end
end

Base.done(::Iter{P}, ::IterState{P}) where P <: Integer = false

function test(limit)
    it=Iter{Int}()
    i=1

    for p in it
        # println("$(i): $(p)")
        i += 1
        if i > limit
            return p
        end
    end
end

function iter(T)
    Iter{T}()
end

end # Module
