#===============================================================================

A bag contains one red disc and one blue disc. In a game of chance a player
takes a disc at random and its colour is noted. After each turn the disc is
returned to the bag, an extra red disc is added, and another disc is taken at
random.

The player pays £1 to play and wins if they have taken more blue discs than red
discs at the end of the game.

If the game is played for four turns, the probability of a player winning is
exactly 11/120, and so the maximum prize fund the banker should allocate for
winning in this game would be £10 before they would expect to incur a loss. Note
that any payout will be a whole number of pounds and also includes the original
£1 paid to play the game, so in the example given the player actually wins £9.

Find the maximum prize fund that should be allocated to a single game in which
fifteen turns are played.

===============================================================================#

using Memoize
using Base.Test

#=------------------------------------------------------------------------------
Let p(b, n) the probability to take b blue disks in n attempts, and pb(n) the
probability to take a blue disk at the nth attempt.

It comes:
    p(b, n) = | pb(n) p(b-1, n-1) + (1-pb(n)) p(b, n-1) if b≤n
              | 0 if b>n

    pb(n) = 1 / (1+n)

In addition p(1, 1) = pb(1) p(0,0) + (1-pb(1)) p(0, -1)
                    = 1/2 p(0, 0)
                    = 1/2
            => p(0, 0) = 1
------------------------------------------------------------------------------=#

@memoize
function p(b::Int, n::Int)::Rational{Int}
    if     n<b  0
    elseif b<0  0
    elseif n==0 1  # In that case, b must also be equal to 0
    else        p(b-1, n-1)//(1+n) + n*p(b, n-1)//(1+n)
    end
end


#=------------------------------------------------------------------------------
The probability to win it then given by
    p(n) = sum(p(b,n) for b ∈ [div(n,2)+1, n])
------------------------------------------------------------------------------=#

p(n::Int)::Rational{Int} = sum([p(b,n) for b in div(n,2)+1:n])

@test p(4) == 11//120

#=------------------------------------------------------------------------------
If p(n) = N//D, then the maximal prize fund is div(D/N), i.e. floor(1//p(n))
------------------------------------------------------------------------------=#

maxfund(n) = let p = p(n); div(p.den, p.num) end

@test maxfund(4) == 10


#=----------------------------------------------------------------------------=#
println("Euler 121: $(maxfund(15))")
