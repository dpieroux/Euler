--------------------------------------------------------------------------------
-- |
-- Module      : Pythagoras
-- Description : Pythagorean triples
-- Copyright   : (c) Didier Pieroux, 2017
-- License     : MIT
-- Maintainer  : Didier Pieroux
-- Portability : portable
--
-- This module generates the Pythagorean triples. See [1].
--
-- [1] <https://en.wikipedia.org/wiki/Pythagorean_triple>
--------------------------------------------------------------------------------

module Tools.Pythagoras
    ( genPrimTriples
     , genTriples)
where

import Data.List.Ordered

--------------------------------------------------------------------------------
-- Primitive triples
--------------------------------------------------------------------------------

-- |
-- Generates all primitive Pythagorean triples.
--
-- The triples (a, b, c) returned satisfy a² + b² = c² and a<b. They are sorted
-- by increasing c, and by increasing a for equal c.

genPrimTriples :: Integral int => () -> [(int, int, int)]
genPrimTriples _ = genPrimTriples' [mkPrimitive 2 1]


--
-- Representation of a primitive Pythagorean triple.
--
-- Such a value contains the corresponding (m, n) pair and the triple (a, b, c)
-- itself.
--
data Primitive a = Primitive { mn_  :: (a, a)
                             , abc_ :: (a, a, a)
                             } deriving Eq

instance Ord a => Ord (Primitive a) where
    compare t1 t2 = cmpTriples (abc_ t1) (abc_ t2)


--
-- Build a Primitive given m and n.
--
-- Note: during the build, all values making the primitive are evaluated,
-- excepted the hypotenuse. However, the latter is evaluated by the algorithm.
-- So in practice no value is left unevaluated.
--
mkPrimitive :: Integral int => int -> int -> Primitive int
mkPrimitive m n = Primitive (m, n)
                            (if a < b then (a, b, hyp) else (b, a, hyp))
  where {m²=m*m; n²=n*n; a = m²-n²; b = 2*m*n; hyp=m²+n²}


--
-- Generate the primitive triples in the required order.
--
-- See Pythagoras.txt for a description of the algorithm. The parameter is the
-- list of primitive triples already generated but not yet consumed.
--
genPrimTriples' :: Integral int => [Primitive int] -> [(int, int, int)]
genPrimTriples' ((Primitive (m, n) abc):ps) = abc : (genPrimTriples' ps')
  where
    ps' = if n>2 then downwards else downRightwards

    downwards = if n'<m then insertSet (mkPrimitive m n') ps else ps
      where n' = until ((1==) . gcd m) (+2) (n+2)

    downRightwards = insertSet (mkPrimitive (m+1) (3-n)) downwards


--------------------------------------------------------------------------------
-- All triples
--------------------------------------------------------------------------------

-- | Generates a comprehensive list of Pythagorean triples.
--
-- The triples (a, b, c) returned satisfy a² + b² = c² and a<b. They are sorted
-- by increasing c, and then by increasing a.

genTriples :: Integral int => () -> [(int, int, int)]
genTriples () = let (p:ps) = genPrimTriples () in genTriples' [mkTriple p] ps


-- Representation of a Pythagorean triple.
--
-- Such a value contains also the corresponding primitive triple.
--
data Triple a = Triple { primitive_ :: (a, a, a)
                       , triple_    :: (a, a, a)
                       } deriving Eq

instance Ord a => Ord (Triple a) where
    compare t1 t2 = cmpTriples (triple_ t1) (triple_ t2)


-- Build a Triple from a Primitive
--
-- The multiplicative factor is 1, i.e. the triple is equal to its primitive.
--
mkTriple :: (int, int, int) -> Triple int
mkTriple primitive = Triple primitive primitive

-- True if a triple is primitive.
isPrimitive :: Eq a => Triple a -> Bool
isPrimitive (Triple (a, _, _) (a', _, _)) = (a == a')

-- Return the
nextTriple :: Num n => Triple n -> Triple n
nextTriple (Triple p@(a, b, c) (a', b', c'))
  = let b'' = b + b' in seq b'' (Triple p (a+a', b'', c+c'))

-- Generate the triples in the required order.
--
-- See Pythagoras.txt for a description of the algorithm. The parameters are the
-- list of triples already generated but not yet consumed and the list of
-- primitive triples not yet used.
--
genTriples' :: Integral int => [Triple int]
                            -> [(int, int, int)]
                            -> [(int, int, int)]
genTriples' (item@(Triple _ triple):items) prims =
    triple_ item : if isPrimitive item
                       then (genTriples' downRightwards (tail prims))
                       else (genTriples' downwards prims)
  where
    downwards = insertSet (nextTriple item) items
    downRightwards = insertSet (mkTriple $ head prims) downwards


--------------------------------------------------------------------------------
-- Tools
--------------------------------------------------------------------------------

-- Compare two triples by the hypotenuse first and the by the small side.
cmpTriples :: Ord a => (a, a, a) -> (a, a, a) -> Ordering
cmpTriples (sml, _, hyp) (sml', _, hyp')
  = case compare hyp hyp' of LT -> LT
                             EQ -> compare sml sml'
                             GT -> GT
