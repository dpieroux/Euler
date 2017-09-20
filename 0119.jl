#===============================================================================

The number 512 is interesting because it is equal to the sum of its digits
raised to some power: 5 + 1 + 2 = 8, and 8^3 = 512. Another example of a number
with this property is 614656 = 28^4.

We shall define an to be the nth term of this sequence and insist that a number
must contain at least two digits to have a sum.

You are given that a2 = 512 and a10 = 614656.

Find a30.

--------------------------------------------------------------------------------

The solution is to build iteratively a table whose row and column indices
correspond respectively to the exponents and digit sums. The table is
semi-infinite in each direction (i.e. indices starts at 1 but are unlimited
otherwise).

At iteration n, all numbers up to 2^n (at least) are processed. Those that
fulfil the condition (col = digit_sum(col^row)) are inserted in order in a
queue.

The elements of the queue are returned in order. Once the queue is empty or if
the first element is greater than 2^n, then a new iteration is restarted.

===============================================================================#

import Base
using Base.Test

struct Item
    value :: Int64
    dsum  :: Int
    exp   :: Int
end

Item(dsum::Int, exp::Int) = Item(dsum^exp, dsum, exp)

index(item::Item) = item.dsum + item.exp

isvalid(item::Item) = sum(digits(item.value)) == item.dsum && item.value > 9

Base.isless(lhs::Item, rhs::Item) =  lhs.value < rhs.value

function insertSorted!(list, item)
    index = searchsortedfirst(list, item)
    insert!(list, index, item)
end

struct NumberIter end

mutable struct NumberIterState
    target :: Int
    borders :: Array{Item}
    queue :: Array{Item}
end

Base.start(::NumberIter) = NumberIterState(1, Item[Item(1, 1)], Item[])


function Base.next(::NumberIter, state)
    function populate()
        state.target *= 2

        # Extend existing rows
        for e in 2:length(state.borders) # e is the exponent
            item = state.borders[e]
            while item.value < state.target
                item = Item(item.dsum+1, e)
                if isvalid(item)
                    insertSorted!(state.queue, item)
                end
            end
            state.borders[e] = item
        end

        # Add a border
        push!(state.borders, Item(2, length(state.borders)+1))
    end

    while true
        if isempty(state.queue)
            populate()
        else
            item = shift!(state.queue)

            if item.value <= state.target
                return (item, state)
            else
                unshift!(state.queue, item)
                populate()
            end
        end
    end
end

Base.done(::NumberIter, state) = false

function Euler(limit)
    result = Int64[]
    for item in NumberIter()
        push!(result, item.value)
        if length(result) == limit
            return result
        end
    end
end

euler = Euler(30)

@test euler[2] == 512
@test euler[10] == 614656

println("Euler 0119: $(euler[30])")
