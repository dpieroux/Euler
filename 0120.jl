#===============================================================================

Let r be the remainder when (a−1)^n + (a+1)^n is divided by a^2.

For example, if a = 7 and n = 3, then r = 42: 6^3 + 8^3 = 728 ≡ 42 mod 49. And
as n varies, so too will r, but for a = 7 it turns out that r_max = 42.

For 3 ≤ a ≤ 1000, find ∑ r_max.

--------------------------------------------------------------------------------

Let r(a,n) := ((a−1)^n + (a+1)^n) % a^2
It comes: r(a, n) = 0 for a=1 (because of the modulo 1)
                  = 2 for n even
                  = 2an % a^2 = a(2n % a) for n odd

          max_n(2n % a) = a-1 if a is odd
                        = a-2 if a is even

This max_n(r(a, n)) = 0 for a = 1
                    = 2 for a = 2
                    = a(a-1) for 3 <= a and a odd
                    = a(a-2) for 3 <= a and a even

===============================================================================#

rmax(a) = isodd(a) ? a*(a-1) : a*(a-2) # It is assumed that a>=3

function compute(limit)
    result = 0
    for a in 3:limit
        result += rmax(a)
    end
    result;
end

println("Euler 120: $(compute(1000))")
