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