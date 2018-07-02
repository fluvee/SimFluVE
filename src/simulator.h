#ifndef __SIMULATOR_H
#define __SIMULATOR_H

#include "params.h"
#include "population.h"
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

#if WITH_UI
#include <wx/progdlg.h>
#endif

class Simulator
{
private:
  Params SimPar;    // Simulation parameters
  Population SimPop;// Population in the simulation
  RandGen Rnd;      // Random number generator for the simulation
  string filePostfix;
#if WITH_R
  DetailReport* detReport;
  DailySimReport* daisimReport;
  DailyReport* daiReport;
  PrevelanceReport* preReport;
  ContactReport* conReport;
  VaccinationReport* vacReport;
  MonthlySimReport* monsimReport;
  SeasonSimReport* seasimReport;
  MonthlyReport* monReport;
  SeasonReport* seaReport;
  PopulationReport* populationReport;
#endif
#if WITH_UI
    wxGenericProgressDialog* m_ProgressDialog;
#endif

public:
#if WITH_R
  Simulator(Params &pars, DetailReport*, DailySimReport*, DailyReport*, PrevelanceReport*, ContactReport*, VaccinationReport*, MonthlySimReport*, SeasonSimReport*, MonthlyReport*,SeasonReport*,PopulationReport*,string);
#endif
  Simulator(string fname);
  Simulator(string fname, string filePostfix);
#if WITH_UI
  Simulator(Params, wxGenericProgressDialog*);
#endif
  void Start();
  string spaceToUnderscore(string s);
  Params getSimPar();
  //Reset();
}; // Simulator.h

#endif
