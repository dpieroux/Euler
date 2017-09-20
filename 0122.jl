#===============================================================================

The most naive way of computing n15 requires fourteen multiplications:

n × n × ... × n = n15

But using a "binary" method you can compute it in six multiplications:

n × n = n^2
n^2 × n^2 = n^4
n^4 × n^4 = n^8
n^8 × n^4 = n^12
n^12 × n^2 = n^14
n^14 × n = n^15

However it is yet possible to compute it in only five multiplications:

n × n = n^2
n^2 × n = n^3
n^3 × n^3 = n^6
n^6 × n^6 = n^12
n^12 × n^3 = n^15

We shall define m(k) to be the minimum number of multiplications to compute n^k;
for example m(15) = 5.

For 1 ≤ k ≤ 200, find ∑ m(k).

--------------------------------------------------------------------------------

To start with, this problem can be reformulated by asking how many additions are
necessary to reach a number starting from 1 and using only additions.

The general approach is bottom up. At every iteration we build a new set of sets
of numbers. An inner set contains all the numbers that can be together obtained
by 'iter' calculations.

An inner set is generated from a previous inner set by taking a copy of the
later, then by selecting one of its elements, by adding it to the initial
elements and adding the numbers so obtained to the new set. This must be done
for all elements of the initial set.

At the next iteration, it is only necessary to consider the new elements that
have been added when selecting the element to add to the other.

Because a same inner set can be obtained from different paths, each new inner
set is associated with the numbers selected to produce it.

Each time a new number is generated, the corresponding iteration is recorded
with it.

===============================================================================#

using Base.Test

function compute_nbr_steps(limit)
    nbr_steps = Array{Int}(limit)
    fill!(nbr_steps, -1)
    sets = Dict{Set{Int}, Set{Int}}()
    # Key is all numbers in the set, Value is the set of the numbers added
    # during the previous iteration.

    const empty_value = Set{Int}()
    # empty_value is used as an indicator

    completion = 1
    iter = 0
    nbr_steps[1] = 0
    sets[Set{Int}(1)] = Set{Int}(1)

    while completion<limit
        iter += 1
        updated_sets = typeof(sets)()

        for (set, added_numbers) in sets, l in set, m in added_numbers
            n=l+m
            if n ≤ limit && n ∉ set
                updated_set = copy(set)
                push!(updated_set, n)

                nums = get(updated_sets, updated_set, empty_value)

                if nums == empty_value
                    nums = Set{Int}(n)
                else
                    push!(nums, n)
                end

                updated_sets[updated_set] = nums

                if nbr_steps[n]<0
                    nbr_steps[n] = iter
                    completion += 1
                    println("\t completion = $(completion)")
                    if completion == limit
                        return nbr_steps
                    end
                end
            end
        end
        sets = updated_sets
    end
end

result = compute_nbr_steps(200)

@test result[15] == 5
@test length(result) == 200

print("Euler 122: $(sum(result))")
