#===============================================================================

A row of five black square tiles is to have a number of its tiles replaced with
coloured oblong tiles chosen from red (length two), green (length three), or
blue (length four).

If red tiles are chosen there are exactly seven ways this can be done. If green
tiles are chosen there are three ways. And if blue tiles are chosen there are
two ways.Assuming that colours cannot be mixed there are 7 + 3 + 2 = 12 ways of
replacing the black tiles in a row measuring five units in length.

How many different ways can the black tiles in a row measuring fifty units in
length be replaced if colours cannot be mixed and at least one coloured tile
must be used

===============================================================================#

using Memoize
using Base.Test

@memoize function ncases_swb(blocklen, rowlen)
    # Return the number of cases _s_tarting _w_ith a _b_lock

    if rowlen < blocklen
        return 0
    end

    result = 1
    # case for which there is only one block

    rowlen -= blocklen
    # remove room taken by the first block

    for nspace = 0:rowlen-blocklen
        # nbr spaces allowed between first block and second one

        result += ncases_swb(blocklen, rowlen-nspace)
    end

    result
end

function ncases(blocklen, rowlen)
    result = 0
    for nspace = 0:rowlen-blocklen
        # nbr spaces before first block
        result += ncases_swb(blocklen, rowlen-nspace)
    end

    result
end

function euler(rowlen)
    ncases(2, rowlen) + ncases(3, rowlen) + ncases(4, rowlen)
end

@test ncases(2, 5) == 7
@test ncases(3, 5) == 3
@test ncases(4, 5) == 2
@test euler(5) == 12

@printf("Euler 0116: %d", euler(50))
