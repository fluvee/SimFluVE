#ifndef PREVELANCEREPORT_H
#define	PREVELANCEREPORT_H
#include<vector>
class PrevelanceReport {
public:
    std::vector<unsigned> s;
    std::vector<unsigned> d;
    std::vector<unsigned> day;
    std::vector<unsigned> month;
    std::vector<unsigned> year;
    std::vector<unsigned> stratum;
    std::vector<double> prevS2;
    std::vector<double> prevS3;
    std::vector<double> pInfX0;
    std::vector<double> pInfX1;
    std::vector<double> pInfV0X0;
    std::vector<double> pInfV0X1;
    std::vector<double> pInfV1X0;
    std::vector<double> pInfV1X1;
};

#endif	/* PREVELANCEREPORT_H */