using Memoize

# Return the number of possibilities having at least 1 block
@memoize function solve(size)
  result = 0
  for head = 0:size-3
    for len = 3:size-head
      result += 1 + solve(size-head-len-1)
    end
  end

  return result
end

# Return the number of possibilities having at least 1 block + the case of no
# no block at all
function euler0114(size)
  1+solve(size)
end

println(string("7:  ", euler0114(7)))
println(string("50: ", euler0114(50)))
