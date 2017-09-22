#===============================================================================

The palindromic number 595 is interesting because it can be written as the sum
of consecutive squares: 6^2 + 7^2 + 8^2 + 9^2 + 10^2 + 11^2 + 12^2.

There are exactly eleven palindromes below one-thousand that can be written as
consecutive square sums, and the sum of these palindromes is 4164. Note that 1 =
0^2 + 1^2 has not been included as this problem is concerned with the squares of
positive integers.

Find the sum of all the numbers less than 108 that are both palindromic and can
be written as the sum of consecutive squares.

--------------------------------------------------------------------------------

The solution is easy: we generate all i^2 + ... + j^2 below the limit and we
check that the number obtained is smaller than the limit.

Trap: a same number can be the sum of different sums of squares...

===============================================================================#

include("Euler.jl")

function euler(limit)
    i = 1
    ns = Set{Int}()

    while true
        n = i^2
        if limit <= n
            break
        end

        j = i+1
        n += j^2

        while n < limit
            if is_palindromic(n)
                push!(ns, n)
            end
            j += 1
            n += j^2
        end

        i += 1
    end

    sum(ns)
end

println("10^3: $(euler(10^3))")
println("10^8: $(euler(10^8))")
