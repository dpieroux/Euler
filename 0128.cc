/*******************************************************************************

A hexagonal tile with number 1 is surrounded by a ring of six hexagonal tiles,
starting at "12 o'clock" and numbering the tiles 2 to 7 in an anti-clockwise
direction.

New rings are added in the same fashion, with the next rings being numbered 8 to
19, 20 to 37, 38 to 61, and so on. The diagram below shows the first three
rings.

                                20
                            21     37
                        22      8      36
                    23      9      19      35
                        10      2      18
                    24      3       7      34
                        11      1      17
                    25      4       6      33
                        12      5      16
                    26      13     15      32
                        27      14     31
                            28     30
                                29


By finding the difference between tile n and each of its six neighbours we shall
define PD(n) to be the number of those differences which are prime.

For example, working clockwise around tile 8 the differences are 12, 29, 11, 6,
1, and 13. So PD(8) = 3.

In the same way, the differences around tile 17 are 1, 17, 16, 1, 11, and 10,
hence PD(17) = 2.

It can be shown that the maximum value of PD(n) is 3.

If all of the tiles for which PD(n) = 3 are listed in ascending order to form a
sequence, the 10th tile would be 271.

Find the 2000th tile in this sequence.

--------------------------------------------------------------------------------

Quick facts:
    1) The nth ring has 6n elements
    
    2) So, the nth ring ends with the number 
            last(n) = 1 + 6 + 12 + ... + 6n
                    = 1 + 6 (1 + 2 + ... + n)
                    = 1 + 6 n(n+1)/2
                    = 1 + 3 n(n+1)
    
    3) Thus the nth ring starts with
            start(n) = 1 + end(n-1)
                     = 1 + 1 + 3n(n-1) 
                     = 2 + 3n(n-1) 


There are five type of tiles:
    - type 1: the origin tile 1; PD(1) = 3 since 3-1=2, 4-1=3, 6-1=5
    - type 2a: the tiles starting a ring: 2, 8, 20, ...
    - type 2b: the tiles ending a ring: 7, 19, 37, ...
    - type 3: the tiles that are a corner of a ring (and which are not of type
      2a).
    - type 4: the tiles that are part of a side of a ring (and which are not of
      type 2b).

None of the type-3 tiles can have their PD equal to 3. Indeed, if tile n has
type-3, then    
    - it has 2 adjacent tiles that differ by one unit: n+1 and n-1     
    - it can be shown that the 4 other adjacent tiles split in 2 odd numbers and
      2 even numbers. Therefore the difference between the tile n and each of
      these 4 neighbours must lead to 2 even numbers, which can't be prime. Thus
      PD(n) <= 2.

None of the type-4 tiles can have their PD equal to 3. Indeed if tile n has
type-4, then
    - it has 2 adjacent tiles that differ by one unit: n+1 and n-1
    - the 4 other adjacent tiles are two pairs of adjacent numbers: (a, a+1) and
      (b, b+1). At most one element of each pair can make a prime difference
      with n. Thus here also PD(n) <= 2.

So, only the tiles of types 2 have to be tested.

*******************************************************************************/

#include <iostream>
#include <cstdint>

#include <flint/ulong_extras.h>

using namespace std;

mp_limb_t count = 1;

void process(mp_limb_t n)
{
    ++count;        
    if (count == 10 || count == 2000)
        cout << count << ": " << n << endl;
}

int main(int argc, char** argv)
{
    mp_limb_t n = 2;
    mp_limb_t a = -1, b = 0, c = 6, d = 18;

    while (true) {

        if (n_is_prime(c-1-b)) 
        {
            if (n_is_prime(c+1-b) && n_is_prime(d-1-b)) process(b+2);
            if (n_is_prime(c-1-a) && n_is_prime(d-1-c)) process(c+1);
        }

        if (count >= 2000) break;

        ++n;

        a = b;
        b = c;
        c = d;
        d = 3*n*(n+1);
    }
}