#===============================================================================

Using all of the digits 1 through 9 and concatenating them freely to form
decimal integers, different sets can be formed. Interestingly with the set
{2,5,47,89,631}, all of the elements belonging to it are prime.

How many distinct sets containing each of the digits one through nine exactly
once contain only prime elements?

--------------------------------------------------------------------------------

The algorithm proposed is based on the association of a number to its digits.

Let n=d1...dn be a number made of n digits. Then

    encode(n) = 0 if n contains 0 or if a digit appears multiple times.
              = (Σ 2^di)/2

The division by 2 results from the fact the 0 is not allowed, so Σ 2^di is
always odd. Thus the range of encode(n) is 1 to 511 (9 bits set to 1).

Each prime up to 987654321 are encoded as explained above, and each code is
associated with the number of primes it corresponds to, excepted for primes
encoded as 0, which are discarded.

A similar code is also associated to a set of primes, by computing it over all
the digits of all the primes belonging to the set.

With this, a prime p can be added to the set S if encode(p) and encode(S) have
no digits in common, i.e. if encode(p) & encode(S) is null. The code of the new
set is encode(p) | encode(S).

Of course, this applies to all primes whose code is the same as encode(p).

Then the solution is built by exploring recursively the tree of the
possibilities.

===============================================================================#

import Primes

function encode(n::Int)
    result::UInt16 = 0

    while (n != 0)
        n, digit = divrem(n, 10)

        bit = 1 << digit

        if (digit == 0 || result & bit != 0)
            return 0
        end

        result |= bit
    end

    result >> 1
end

primes = Primes.primes(1, 987654321);

function compute_codes(primes)
    result = zeros(Int64, 511)

    for p in primes
        code = encode(p)
        if code != 0
            result[code] += 1
        end
    end

    result;
end

const allcodes = compute_codes(primes)

const fullcode = encode(123456789)

function compute_nbr_sets(code_idx, code_of_set, nbr_of_prime_sets)

    if code_of_set == fullcode
        return nbr_of_prime_sets
    end

    if code_idx == 512
        return 0
    end

    result = compute_nbr_sets(code_idx+1, code_of_set, nbr_of_prime_sets);

    nbr_assoc_primes = allcodes[code_idx]

    if nbr_assoc_primes != 0 && code_of_set & code_idx == 0
        result += compute_nbr_sets(code_idx+1,
                                   code_of_set|code_idx,
                                   nbr_assoc_primes * nbr_of_prime_sets)
    end

    result
end

println("Euler 118: ", compute_nbr_sets(1, 0, 1));
