#ifndef SEASONSIMREPORT_H
#define	SEASONSIMREPORT_H
#include<vector>
class SeasonSimReport {
public:
    std::vector<unsigned> s;
    std::vector<unsigned> stratum;
    std::vector<unsigned> v;
    std::vector<double> nn;
    std::vector<double> inc2;
    std::vector<double> rate2;
    std::vector<double> inc3;
    std::vector<double> rate3;
    std::vector<double> incT;
    std::vector<double> rateT;
    std::vector<double> incNFARI;
    std::vector<double> rateNFARI;
};

#endif	/* SEASONSIMREPORT_H */