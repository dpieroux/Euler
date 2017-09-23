#===============================================================================

The minimum number of cubes to cover every visible face on a cuboid measuring 3 x 2 x 1 is twenty-two.

If we then add a second layer to this solid it would require forty-six cubes to
cover every visible face, the third layer would require seventy-eight cubes, and
the fourth layer would require one-hundred and eighteen cubes to cover every
visible face.

However, the first layer on a cuboid measuring 5 x 1 x 1 also requires
twenty-two cubes; similarly the first layer on cuboids measuring 5 x 3 x 1, 7 x
2 x 1, and 11 x 1 x 1 all contain forty-six cubes.

We shall define C(n) to represent the number of cuboids that contain n cubes in
one of its layers. So C(22) = 2, C(46) = 4, C(78) = 5, and C(118) = 8.

It turns out that 154 is the least value of n for which C(n) = 10.

Find the least value of n for which C(n) = 1000.

--------------------------------------------------------------------------------

Let's first compute the number of cube a layer is composed of given an initial
rectangular parallelepiped of sides a, b and c along 1x, 1y and 1z.

Let chose a slice of size 1 along the plane 1xy, and consider the successive
layers. The figure below represent the initial cut with 0 (here for a=3 and
b=2), and three successive layers grown around it:

        3333
       322223
      32111123
     3210000123
     3210000123
      32111123
       322223
        3333

If P=2*(a+b) is the perimeter, then we see that the number of cubes is:
    Layer 1: P
    Layer 2: P+4
    Layer 3: P+8
    => For the layer n, the number of cubes is P+4(n-1). This is valid for all
slices of length 1, so it comes that the number of cubes in the layer N for all
slices is c(P+4(n-1)).

Doing so, we have covered the faces that are parallel to 1xz and 1yz, but we
still have to cover the two faces parallel to 1xy. Looking again that the
figure, it comes that the number of Cube to cover that face is
    Layer 1: A = ab, the initial area of that face
    Layer 2: Layer 1 + size(border Layer 1) = A + P
    Layer 3: Layer 2 + size(border Layer 2) = A + P + P + 4
    Layer 4: Layer 3 + size(border Layer 3) = A + P + P + 4 + P + 8

=> Layer n = A + (n-1) P + 4 (1+n-2)(n-2)/2
           = A + (n-1) (P + 2 (n-2))

Of course there is a front face and a back face.

So, all put together, it comes that the number of cubes composing the layer n
is N(n) = c(P+4(n-1)) + 2ab + 2(n-1) (P + 2 (n-2)). Replacing P by 2(a+b) and
simplifying, it comes:

    N(n) = 2(ab+bc+ac) + 4(n-1)(a+b+c+n-2)

The formula is obviously symmetric in a, b and c.

===============================================================================#


# Let check it the formula for N(n)

include("Euler.jl")
using Base.Test

n_cubes(n, a, b, c) = 2*(a*b+b*c+a*c) + 4*(n-1)*(a+b+c+n-2)

@test n_cubes(1, 1, 2, 3) ==  22
@test n_cubes(2, 1, 2, 3) ==  46
@test n_cubes(3, 1, 2, 3) ==  78
@test n_cubes(4, 1, 2, 3) == 118

function euler(threshold::Int)

    for half_nc in Euler.naturals(3)
        # nc is the number of cubes in a layer; half_nc is just the half of it

        # Assuming 1 ≤ a ≤ b ≤ c, we have to determine the number of solutions
        # of the equation nc == n_cubes(n, a , b, c).

        nbr_sol = 0

        for n in Euler.naturals(1)
            3 + 2*(n^2-1) ≤ half_nc || break

            for a in Euler.naturals(1)
                3a^2 + 2*(n-1)*(3a+n-2) ≤ half_nc || break

                for b in Euler.naturals(a)
                    b*(2a+b) + 2(n-1)*(a + 2b + n - 2) ≤ half_nc || break

                    num_c = half_nc - a*b -2*(n-1)*(a+b+n-2)
                    den_c = a + b + 2(n-1)

                    if rem(num_c, den_c) == 0
                        nbr_sol += 1
                    end
                end
            end
        end

        if nbr_sol == threshold
            return 2*half_nc
        end
    end
end

@test euler(10) == 154

@time println("Euler 126: $(euler(1000))")
