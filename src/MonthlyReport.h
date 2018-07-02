/* 
 * File:   MonthlyReport.h
 * Author: amit
 *
 * Created on December 22, 2014, 4:35 PM
 */

#ifndef MONTHLYREPORT_H
#define	MONTHLYREPORT_H
#include<vector>
class MonthlyReport {
public:
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

#endif	/* MONTHLYREPORT_H */

