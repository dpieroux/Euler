{-------------------------------------------------------------------------------

Investigating multiple reflections of a laser beam Problem 144

In laser physics, a "white cell" is a mirror system that acts as a delay line
for the laser beam. The beam enters the cell, bounces around on the mirrors, and
eventually works its way back out.

The specific white cell we will be considering is an ellipse with the equation
4x² + y² = 100

The section corresponding to −0.01 ≤ x ≤ +0.01 at the top is missing, allowing
the light to enter and exit through the hole.

The light beam in this problem starts at the point (0.0,10.1) just outside the
white cell, and the beam first impacts the mirror at (1.4,-9.6).

Each time the laser beam hits the surface of the ellipse, it follows the usual
law of reflection "angle of incidence equals angle of reflection." That is, both
the incident and reflected beams make the same angle with the normal line at the
point of incidence.

The slope m of the tangent line at any point (x,y) of the given ellipse is: m =
−4x/y

The normal line is perpendicular to this tangent line at the point of incidence.

The animation on the right shows the first 10 reflections of the beam.

How many times does the beam hit the internal surface of the white cell before
exiting?

--------------------------------------------------------------------------------

The solution is found by simply computing the trajectory.

-------------------------------------------------------------------------------}

data Vec a = Vec a a deriving Show
(Vec a b) .+ (Vec c d) = Vec (a+c) (b+d)
(Vec a b) .- (Vec c d) = Vec (a-c) (b-d)
ps (Vec a b) (Vec c d) = (a*c)+(b*d)
scale x (Vec a b) = Vec (x*a) (x*b)

-- Compute the intersection of a ray starting from the ellipse at p and having
-- direction d.
findInteresction :: Fractional a => Vec a -> Vec a -> Vec a
findInteresction p@(Vec x y) d@(Vec dx dy)
  = let λ = - (8*x*dx+2*y*dy) / (4*dx*dx+dy*dy) in  p .+ scale λ d

-- Compute the reflected direction of a ray at a point p on the ellipse arriving
-- with direction d.
genReflectedDir :: Fractional a => Vec a -> Vec a -> Vec a
genReflectedDir p@(Vec x y) inDir@(Vec dx dy)
  = let {t = Vec (-y) (4*x); n = Vec (-4*x) (-y)}
    in  scale (ps inDir t) t .- scale (ps inDir n) n

-- Given two sequent point on the ellipse, return the next one.
nextPoint :: Fractional a => Vec a -> Vec a -> Vec a
nextPoint p0 p1
  = findInteresction p1 outDir
  where
    inDir = p1 .- p0
    outDir = genReflectedDir p1 inDir

-- Generate the trajectory and return the number of hit of the ellipse.
euler = loop (Vec 0.0 10.1) (Vec 1.4 (-9.6)) 1
  where
    loop :: (Real a, Fractional a) => Vec a -> Vec a -> Int -> Int
    loop p0 p1 acc =
      if (y > 0) && (abs x < 0.01) then acc else loop p1 p2 (acc+1)
      where
        p2@(Vec x y) = nextPoint p0 p1

main = do
    putStrLn $ concat ["Euler 144: ", show $ euler]
