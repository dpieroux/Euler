using Memoize
using Base.Test

@doc """
    ncases_swb(minblocklen::Integer, rowlen::Integer)

Compute the number of cases starting with a block.

# Arguments
* `minblocklen::Integer`: mininal block length
* `rowlen::Integer`: row length
""" ncases_swb

@memoize function ncases_swb(minblocklen::Integer, rowlen::Integer)
    if rowlen < minblocklen
        return 0
    end

    result = 0
    for blocklen in minblocklen:rowlen
        result += 1
        for nspace = 1:(rowlen-blocklen)
            result += ncases_swb(minblocklen, rowlen-blocklen-nspace)
        end
    end

    result
end

function ncases(minblocklen::Integer, rowlen::Integer)
    result = 1 # no block at all
    for nheadspace = 0:rowlen-minblocklen
        result += ncases_swb(minblocklen, rowlen-nheadspace)
    end

    result
end

@test ncases(3, 7) == 17
@test ncases(3, 50) == 16475640049
@test ncases(3, 29) == 673135
@test ncases(3, 30) == 1089155
@test ncases(10, 56) == 880711
@test ncases(10, 57) == 1148904

rowlen = 51
while ncases(50, rowlen) < 1_000_000
    rowlen += 1
end

<<<<<<< HEAD
@printf("Euler 0115: F(50, %d) = %d", rowlen-1, ncases(50, rowlen-1))
@printf("Euler 0115: F(50, %d) = %d", rowlen  , ncases(50, rowlen  ))
=======
@printf("Euler 0115: F(50, %d) = %d\n", rowlen-1, ncases(50, rowlen-1))
@printf("Euler 0115: F(50, %d) = %d\n", rowlen  , ncases(50, rowlen  ))
>>>>>>> f3a61389a2ad1f819e5b26e003c47256e493817e
