#ifndef __RANDGEN_H
#define __RANDGEN_H

class RandGen {
private:
    static long int t;
    static long int m;
    static long int a;
    static long int q;
    static long int r;
public:
    RandGen();
    void Seed(int);
    float RandFloat();
    int RandInt(int, int);
};

#endif
