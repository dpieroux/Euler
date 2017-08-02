#===============================================================================

Using a combination of black square tiles and oblong tiles chosen from: red
tiles measuring two units, green tiles measuring three units, and blue tiles
measuring four units, it is possible to tile a row measuring five units in
length in exactly fifteen different ways.

How many ways can a row measuring fifty units in length be tiled?

===============================================================================#

using Memoize
using Base.Test

@memoize function ncases_swb(rowlen)
    # Return the number of cases starting with a block

    ncases_swglb(2, rowlen) + ncases_swglb(3, rowlen) + ncases_swglb(4, rowlen)
end

@memoize function ncases_swglb(blocklen, rowlen)
    # Return the number of cases starting with a block of a given length
    if rowlen < blocklen
        return 0
    end

    result = 1
    # case for which there is only the starting block

    rowlen -= blocklen
    # account for the head block

    for nspace = 0:rowlen-2
        # nbr spaces allowed between first block and second one
        result += ncases_swb(rowlen-nspace)
    end

    result
end

function ncases(rowlen)
    result = 1
    # No block at all

    for nspace = 0:rowlen-2
        # nbr spaces before first block
        result += ncases_swb(rowlen-nspace)
    end

    result
end

@test ncases(5) == 15

@printf("Euler 0117: %d", ncases(50))
