#ifndef DAILYREPORT_H
#define	DAILYREPORT_H
#include<vector>
class DailyReport {
public:
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

#endif	/* DAILYREPORT_H */
