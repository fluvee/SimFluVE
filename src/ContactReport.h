#ifndef CONTACTREPORT_H
#define	CONTACTREPORT_H
#include<vector>
class ContactReport {
public:
    std::vector<unsigned> s;
    std::vector<unsigned> d;
    std::vector<unsigned> ii;
    std::vector<unsigned> stratum;
    std::vector<unsigned> x;
    std::vector<unsigned> u;
    std::vector<unsigned> vac;
    std::vector<unsigned> prot;
    std::vector<double> pINF;
    std::vector<double> RANDOM;
    std::vector<unsigned> newInf;
};

#endif	/* CONTACTREPORT_H */