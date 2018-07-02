/* 
 * File:   SeasonReport.h
 * Author: amit
 *
 * Created on December 22, 2014, 4:53 PM
 */

#ifndef SEASONREPORT_H
#define	SEASONREPORT_H
#include<vector>
class SeasonReport {
public:
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



#endif	/* SEASONREPORT_H */

