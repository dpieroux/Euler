{-------------------------------------------------------------------------------

Consider the infinite polynomial series AF(x) = xF1 + x²F₂ + x³F₃ + ..., where
F_k is the kth term in the Fibonacci sequence: 1, 1, 2, 3, 5, 8, ... ; that is,
F_k = F_k−1 + F_k−2, F_1 = 1 and F_2 = 1.

For this problem we shall be interested in values of x for which AF(x) is a
positive integer.

Surprisingly AF(1/2) = (1/2).1 + (1/2)².1 + (1/2)³.2 + (1/2)⁴.3 + (1/2)⁵.5 + ...
                     = 1/2 + 1/4 + 2/8 + 3/16 + 5/32 + ...
                     = 2

The corresponding values of x for the first five natural numbers are shown
below.
                x     AF(x)
            √2−1        1
            1/2         2
            (√13−2)/3   3
            (√89−5)/8   4
            (√34−3)/5   5

We shall call AF(x) a golden nugget if x is rational, because they become
increasingly rarer; for example, the 10th golden nugget is 74049690.

Find the 15th golden nugget.

--------------------------------------------------------------------------------

AF(x) is the generating function of the Fibonacci sequence. Using the Fibonacci
recurrence relation it is trivial to show that AF(x) = x/(1-x-x²).

Posing AF(x) = n and solving for x, it comes
    x = [√(5n²+2n+1) - (n+1)]/2n

The other root is negative, so it is discarded.

x is rational iif N = 5n²+2n+1 = (2n)² + (n+1)² is a perfect square, i.e. if 2n
and n+1 are the legs of a Pythogorean triple (PT) and N the head.

Let search for a matching PT of the form k(a, b, c), with (a, b, c) a primitive
PT (PPT) (a < b < c) and k the multiplying factor. Because 2n>n+1 for n>1, we
associate n+1 with ka and 2n with kb:

    ka = n+1 and kb = 2n => 2 (ka-1) = kb => k (2a-b) = 2

They are only two possibilities:
    1) k=2 and 2a-b=1; in that case n = 2(a-1) = b;
    2) k=1 and 2a-b=2; in that case n = a-1 = b/2.

So, the algorithm enumerates the PPTs and retains those for which 2b = a+1 or
a+2.

genPrimTriples returns the PPT with increasing c. However, we have also to mix
the double of some PPTs.


-------------------------------------------------------------------------------}

import Tools.Pythagoras (genPrimTriples)

type T3 = (Int, Int, Int)

data PTriple = Primitive T3  -- a primitive triple (k=1)
             | Valid     Int -- valid candidate to be inserted


iter :: [T3]  -- not yet processed candidate primitive triples
     -> [Int] -- queued solutions corresponding to twice the hypotenuse of a
              -- primitive triple
     -> [Int] -- sorted solutions

iter ps qs =
    case val of
        Valid n -> n : iter ps' qs'
        Primitive (a, b, c) ->
            case 2*a-b of
                1         -> iter ps' (qs' ++ [b])
                2         -> a-1 : iter ps' qs'
                otherwise -> iter ps' qs'
  where
    (val, ps', qs') = mix ps qs

mix :: [T3] -> [Int] -> (PTriple, [T3], [Int])
mix ps@(p@(_, _, c):ps') qs
  | null qs || c < q = (Primitive p, ps', qs)
  | otherwise        = (Valid q, ps, tail qs)
  where
    q = head qs

printUpto :: Int -> Int -> [Int] -> IO()
printUpto i bound (n:ns)
    | i > bound = return ()
    | otherwise = do
        putStrLn ((show i) ++ ": " ++ (show n))
        printUpto (i+1) bound ns

main = do
    printUpto 1 15 $ iter (genPrimTriples ()) []
