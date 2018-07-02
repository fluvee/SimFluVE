#ifndef DAILYSIMREPORT_H
#define	DAILYSIMREPORT_H
#include<vector>
class DailySimReport {
public:
    std::vector<unsigned> s;
    std::vector<unsigned> d;
    std::vector<unsigned> day;
    std::vector<unsigned> month;
    std::vector<unsigned> year;
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

#endif	/* DAILYSIMREPORT_H */
