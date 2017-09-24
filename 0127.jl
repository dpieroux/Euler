#===============================================================================

The radical of n, rad(n), is the product of distinct prime factors of n. For
example, 504 = 2^3 × 3^2 × 7, so rad(504) = 2 × 3 × 7 = 42.

We shall define the triplet of positive integers (a, b, c) to be an abc-hit if:

    gcd(a, b) = gcd(a, c) = gcd(b, c) = 1
    a < b
    a + b = c
    rad(abc) < c

For example, (5, 27, 32) is an abc-hit, because:

    gcd(5, 27) = gcd(5, 32) = gcd(27, 32) = 1
    5 < 27
    5 + 27 = 32
    rad(4320) = 30 < 32

It turns out that abc-hits are quite rare and there are only thirty-one abc-hits
for c < 1000, with ∑c = 12523.

Find ∑c for c < 120000.

--------------------------------------------------------------------------------

Analyse
-------

1) a < b; a + b = c < bound
    a)  ⇒ 2a < a + b = c < bound - 1 ⇒ 1 ≤ a < bound ÷ 2
    b)  ⇒ b < bound - a

2) if a and b are coprime, then c = a+b is coprime with a and with b
    => gcd(a, b) = 1 ⇒ gcd(a, c) = gcd(b, c) = 1

3) gcd(a, b) == 1 ⇔ gcd(rad(a), rad(b)) == 1

4) if a and b are coprime, the rad(ab) = rad(a) rad(b)

5) rad(n) = 1 if n = 1
          ≥ 2 if n > 1

6) So the conditions can be simplifed to
        gcd(a, b) = 1
        1 ≤ a < bound ÷ 2
        a < b < bound - a
        c = a + b
        rad(a) rad(b) rad(c) < c

7) Precomputing rad(n) for all n in 2..bound-2


case a=1
--------
    gcd(1,b) = 1 is granted
    2 ≤ b ≤ bound-2
    c = 1+b
    rad(b) rad(c) < c ⇒ rad(b) < (1+b) ÷ 2
                      ⇒ rad(b) rad(1+b) < 1+b

    Therefore the constraints are:
        1 < b < bound-1
        c = b+1
        rad(b) rad(b+1) < (b+1)


case a>1
--------
    a < b => a+1 ≤ b,
    2a+1 ≤ a + b = c < bound => a < (bound-1)÷2
                             => a ≤ (bound-1)÷2 -1
    rad(a) ≥ 2, rad(b) ≥ 2, rad(c) ≥ 2
    rad(a) rad(b) rad(a+b) < a+b ⇒ rad(a) < c ÷ 4
                                 ⇒ rad(b) < c ÷ (2 rad(a))

    Therefore the constraints are:
        2   ≤ a ≤ (bound-1)÷2 -1
        a+1 ≤ b ≤ bound - 1 - a
        gcd(a,b) = 1
        c = a + b
        rad(a) < c ÷ 4                  < (bound-1) ÷ 4
        rad(b) < c ÷ (2 rad(a))         < (bound-1) ÷ 4
        rad(c) < c ÷ (rad(a) rad(b))    < (bound-1) ÷ 4


Conclusion
==========

The principle behind the algorithm is to precompute rad(n) for 2 ≤ n ≤ bound-, and to proceed to all possible tests before computing gcd(rad(a), rad(b)).

Note that precomputing gcd(rad(a), rad(b)) for all possible a and b is not efficient.

===============================================================================#

import Primes


function euler(bound::Int)
    #precimpute the radicals
    rad = Array{Int}(bound-1)
    for n in 1:bound-1
        rad[n]=prod(keys(Primes.factor(n)))
    end

    res::Int = 0

    # case a=1
    for b in 2:bound-2
        c=b+1
        if (rad[b]*rad[c] < c) res+=c end
    end

    # case a>1
    for a in 2:(bound-1) ÷ 2 -1
        rad_a = rad[a]
        if (rad_a ≥ (bound-1) ÷ 4) continue end

        for b in a+1:bound-1-a
            rad_b = rad[b]
            c=a+b
            if (rad_a * rad_b * rad[c] ≥ c) continue end
            if (gcd(rad_a, rad_b) == 1) res += c end
        end
    end

    res
end

println("  1_000: $(euler(  1_000))")
@time println("120_000: $(euler(120_000))")
