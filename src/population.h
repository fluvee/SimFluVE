#ifndef __POPULATION_H
#define __POPULATION_H

#include <string>
#include <vector>
#include "person.h"
#include "params.h"
#include "randgen.h"

#if WITH_R
#include "DetailReport.h"
#include "DailySimReport.h"
#include "DailyReport.h"
#include "PrevelanceReport.h"
#include "ContactReport.h"
#include "VaccinationReport.h"
#include "MonthlySimReport.h"
#include "SeasonSimReport.h"
#include "MonthlyReport.h"
#include "SeasonReport.h"
#include "PopulationReport.h"
#endif

using namespace std;

class Population
{
private:
  int PSize;        //Population size
  Person* People;   //People in the simulation
  std::vector<vector<vector<vector<vector<Person*> > > > > stratifiedPeople;
  RandGen Rnd;
  Params SimParams;
  int VacPeople[4];//Number of people vaccinated in stratum k
  int UnVacPeople[4];//Number of people unvaccinated in stratum k
  int h2VMSum[4];//Sum for each month of h2V for each strate
  int h2vMSum[4];//Sum for each month of h2V for each strate
  int h3VMSum[4];//Sum for each month of h2V for each strate
  int h3vMSum[4];//Sum for each month of h2V for each strate
  int h2VSSum[4];//Sum for each season of h2V for each strate
  int h2vSSum[4];//Sum for each season of h2V for each strate
  int h3VSSum[4];//Sum for each season of h2V for each strate
  int h3vSSum[4];//Sum for each season of h2V for each strate
  int hARIvDaily[4];
  int hARIVDaily[4];
  int hARIvMSum[4];
  int hARIVMSum[4];
  int hARIvSSum[4];
  int hARIVSSum[4];

public:
  void Init(Params &);
  void Reset();
  void testStratifiedPeople();
  void Vaccinate(int, int, int, int, ofstream &, VaccinationReport*);
#if WITH_R
  void Spread(int,int, int, int, int, int, int, int,ofstream &, ofstream &, ofstream &,ofstream &,ofstream &,ofstream &, ofstream &, ofstream &,ofstream &,ofstream &, DetailReport*, DailySimReport*, DailyReport*, PrevelanceReport*, ContactReport*, MonthlySimReport*, SeasonSimReport*, MonthlyReport*, SeasonReport*, PopulationReport*);
#else  
  void Spread(int,int, int, int, int, int, int, int,ofstream &, ofstream &, ofstream &,ofstream &,ofstream &,ofstream &, ofstream &, ofstream &,ofstream &,ofstream &);
#endif  
  ~Population();
};



#endif
