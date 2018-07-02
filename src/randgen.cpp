#include "randgen.h"
#include <climits>
#include <iostream>
#if WITH_R
#include <Rcpp.h>
#endif

using namespace std;

long int RandGen::t = 12345;
//long int RandGen::m = LONG_MAX; // not working in 64bit arch.
long int RandGen::m = 2147483647;
long int RandGen::a = 48271;
long int RandGen::q = 44488;
long int RandGen::r = 3399;

RandGen::RandGen() {
}

void RandGen::Seed(int seed) {
    t = (long int) seed;
}

float RandGen::RandFloat() {
    //cout<< "\nt:" << t;
    //cout<<"\nm:" << m;
    t = a * (t % q) - r * (t / q);
    if (t < 0) {
        t += m;
    }
    //cout<< "\nt:" << t;
    //cout<<"\ndouble t:"<<(double)t <<"\n";
    return (float) t / (float) m;
}

// Random number from min to max-1

int RandGen::RandInt(int min, int max) {

    if (min > max) {
        int temp = min;
        min = max;
        max = temp;
    }
    t = a * (t % q) - r * (t / q);
    if (t < 0) {
        t += m;
    }

    return (int) (min + t % (max - min));
}
