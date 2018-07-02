#ifndef POPULATIONREPORT_H
#define	POPULATIONREPORT_H
#include<vector>
class PopulationReport {
public:
    std::vector<unsigned> sim;
    std::vector<unsigned> person;
    std::vector<unsigned> str;
    std::vector<unsigned> x;
    std::vector<unsigned> u;
    std::vector<unsigned> initinf;
    std::vector<unsigned> defvac;
    std::vector<unsigned> prot;
    std::vector<unsigned> eventcounter;
    std::vector<unsigned> type;
    std::vector<unsigned> onsetday;
    std::vector<unsigned> vacstatus;
    std::vector<unsigned> visit;
};

#endif	/* POPULATIONREPORT_H */