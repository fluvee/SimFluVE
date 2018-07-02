#ifndef __PERSON_H
#define __PERSON_H

enum SEIR {
    SUSCEPTIBLE = 0, EXPOSED = 1, INFECTIOUSNILL = 2, INFECTIOUSILL = 3, RECOVERED = 4
};

class Person {
public:
    int ID; // Person ID
    int Stratum; // Stratum of the person
    bool Vac; // Vaccination 
    bool VacSuccessful; // Vaccnication successful (used in case of all-or-none vaccine)
    bool InitInf; // Initial infection status
    SEIR State; // Infection/illness state
    bool NewInf; // Newly infected
    int DayInf; // No. of days since infection
    bool X; // Health status
    bool U; // Propensity of seeking medical care
    int dayARI; // No. of days since onset of current episode of ARI
    int influenzaStartDay; // The onset day of influenza
    int ariStartDay; // The onset day of ari;
    bool visit; // Visit's clinic if true
};

#endif
