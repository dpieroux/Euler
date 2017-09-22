#===============================================================================

Let p_n be the nth prime: 2, 3, 5, 7, 11, ..., and let r_n be the remainder when
(p_n−1)^n + (p_n+1)^n is divided by p_n^2.

For example, when n = 3, p3 = 5, and 43 + 63 = 280 ≡ 5 mod 25.

The least value of n for which the remainder first exceeds 10^9 is 7037.

Find the least value of n for which the remainder first exceeds 10^10.

--------------------------------------------------------------------------------

It comes:
    r_n = 2 for n even
    r_n = 2 n p_n % p_n^2 = 2 n p_n as p_n > n

===============================================================================#

include("Euler.jl")

it = Euler.Primes.iter(Int)

st=start(it)
n=0

limit1 = true

while true
    (p, st) = next(it, st)
    n += 1
    # n is odd

    if limit1 && 2*n*p > 1_000_000_000
        limit1 = false
        println("10^9: n=$(n), p=$(p), r=$(2*p*n)")
    end

    if 2*n*p > 10_000_000_000
        println("10^10: n=$(n), p=$(p), r=$(2*p*n)")
        return
    end

    # skep n even
    (_, st) = next(it, st)
    n += 1
end
