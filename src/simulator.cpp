#include "simulator.h"
#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <vector>

#if WITH_R
#include <Rcpp.h>
#endif

using std::vector;
using namespace std;


#if WITH_R
Simulator::Simulator(Params &pars, DetailReport* det, DailySimReport* ds, DailyReport* dai, PrevelanceReport* pre, ContactReport* con, VaccinationReport* vac, MonthlySimReport* ms, SeasonSimReport* ss, MonthlyReport *m,SeasonReport *s, PopulationReport *p, string post)
{
    SimPar = pars;
    SimPar.Print(Rcpp::Rcout);
    Rnd.Seed(SimPar.RandomSeed);
    SimPop.Init(SimPar);
    detReport = det;
    daisimReport = ds;
    daiReport = dai;
    preReport= pre;
    conReport = con;
    vacReport = vac;
    monsimReport = ms;
    seasimReport = ss;
    monReport = m;
    seaReport = s;
    populationReport = p;
    filePostfix = post;
}
#endif

Simulator::Simulator(string filename) {
    // Read in Parameters of the simulation
    SimPar.Load(filename);
    SimPar.Print(std::cout);

    // Initialize random number generator
    Rnd.Seed(SimPar.RandomSeed);

    // Initialize population
    SimPop.Init(SimPar);
}

Simulator::Simulator(string filename, string postfix) {
    filePostfix = postfix;
    // Read in Parameters of the simulation
    SimPar.Load(filename);
    SimPar.Print(std::cout);

    // Initialize random number generator
    Rnd.Seed(SimPar.RandomSeed);

    // Initialize population
    SimPop.Init(SimPar);
}

#if WITH_UI
Simulator::Simulator(Params pars,wxGenericProgressDialog* ProgressDialog)
{
    SimPar = pars;
    SimPar.Print(std::cout);
    Rnd.Seed(SimPar.RandomSeed);
    SimPop.Init(SimPar);
    m_ProgressDialog = ProgressDialog;
}
#endif

string Simulator::spaceToUnderscore(string s) {
  stringstream ss;
  for (auto &c : s) {
    if (c == ' ') {
      ss << '_';
    }
    else ss << c;
  }
  return ss.str();
}
Params Simulator::getSimPar(){
    return SimPar;
}
void Simulator::Start() {
    ofstream inputs_report, creport, dreport, ireport, preport, vreport, mreport, sreport, mreports, yreport, dreports, popreport;
    string prefix = spaceToUnderscore(SimPar.SimTitle);
    string postfix = spaceToUnderscore(filePostfix);
    if (SimPar.addTimeStampToFileNames) postfix = "_" + postfix + SimPar.timeStamp;
    this->SimPar.InputsReportFile = prefix + "_Inputs_and_Calculated_Parameters" + postfix + ".csv";
    this->SimPar.CReportFile = prefix + "_Contact_Report" + postfix + ".csv";
    this->SimPar.DReportPersonFile = prefix + "_Detailed" + postfix + ".csv";
    this->SimPar.IReportDailyFile = prefix + "_Incidence_Daily_each_Sim" + postfix + ".csv";
    this->SimPar.DReportSumFile = prefix + "_Incidence_Daily_overall" + postfix + ".csv";
    this->SimPar.PReportFile = prefix + "_Prevelance" + postfix + ".csv";
    this->SimPar.VReportFile = prefix + "_Vaccination" + postfix + ".csv";
    this->SimPar.MReportSimFile = prefix + "_Incidence_Monthly_each_Sim" + postfix + ".csv";
    this->SimPar.SReportSimFile = prefix + "_Incidence_Season_each_Sim" + postfix + ".csv";
    this->SimPar.MReportSumFile = prefix + "_Incidence_Monthly_overall" + postfix + ".csv";
    this->SimPar.YReportSumFile = prefix + "_Incidence_Season_overall" + postfix + ".csv";
    this->SimPar.PopReportFile = prefix + "_Outcomes_File" + postfix + ".csv";

    if (this->SimPar.InputsReport) {
        inputs_report.open(this->SimPar.outputFilesPath + this->SimPar.InputsReportFile);
        SimPar.Print(inputs_report, ",");
        inputs_report.close();
    }
    if (this->SimPar.CReport) {
        creport.open(this->SimPar.outputFilesPath + this->SimPar.CReportFile);
    }
    if (this->SimPar.DReportPerson) {
        dreport.open(this->SimPar.outputFilesPath + this->SimPar.DReportPersonFile);
    }
    if (this->SimPar.IReportDaily) {
        ireport.open(this->SimPar.outputFilesPath + this->SimPar.IReportDailyFile);
    }
    if (this->SimPar.DReportSum) {
        dreports.open(this->SimPar.outputFilesPath + this->SimPar.DReportSumFile);
    }
    if (this->SimPar.PReport) {
        preport.open(this->SimPar.outputFilesPath + this->SimPar.PReportFile);
    }
    if (this->SimPar.VReport) {
        vreport.open(this->SimPar.outputFilesPath + this->SimPar.VReportFile);
    }
    if (this->SimPar.MReportSim) {
        mreport.open(this->SimPar.outputFilesPath + this->SimPar.MReportSimFile);
    }
    if (this->SimPar.SReportSim) {
        sreport.open(this->SimPar.outputFilesPath + this->SimPar.SReportSimFile);
    }
    if (this->SimPar.MReportSum) {
        mreports.open(this->SimPar.outputFilesPath + this->SimPar.MReportSumFile);
    }
    if (this->SimPar.YReportSum) {
        yreport.open(this->SimPar.outputFilesPath + this->SimPar.YReportSumFile);
    }
    if (this->SimPar.PopReport) {
        popreport.open(this->SimPar.outputFilesPath + this->SimPar.PopReportFile);
    }
    dreport << "S,D,Day,Month,Year,I,Stratum,X,U,InitInf,Vac,Protected,State,DayInf,DNFARI,Visit" << endl;
    ireport << "S,D,Day,Month,Year,Stratum,V,N,Inc2,Rate2,Inc3,Rate3,IncT,RateT,IncNFARI,RateNFARI" << endl;
    dreports << "D,Day,Month,Year,Stratum,V,N,Inc2,Rate2,Inc3,Rate3,IncT,RateT,IncNFARI,RateNFARI" << endl;
    if (this->SimPar.allOrNone) {
        preport << "S,D,Day,Month,Year,Stratum,Prevalence(S2),Prevalence(S3),P(INF|UNPROTECTED;X=0),P(INF|UNPROTECTED;X=1)" << endl;
    }
    else {
        preport << "S,D,Day,Month,Year,Stratum,Prevalence(S2),Prevalence(S3),P(INF|V=0;X=0),P(INF|V=0;X=1),P(INF|V=1;X=0),P(INF|V=1;X=1)" << endl;
    }
    creport << "S,D,I,Stratum,X,U,Vac,Protected,P(INF),RANDOM,NewInf" << endl;
    vreport << "S,M,Month,Year,K,X,U,Number of Newly Vaccinated, Newly Vaccinated" << endl;
    mreport << "S,Month,Year,Stratum,V,N,Inc2,Rate2,Inc3,Rate3,IncT,RateT,IncNFARI,RateNFARI" << endl;
    sreport << "S,Stratum,V,N,Inc2,Rate2,Inc3,Rate3,IncT,RateT,IncNFARI,RateNFARI" << endl;
    mreports << "Month,Year,Stratum,V,N,Inc2,Rate2,Inc3,Rate3,IncT,RateT,IncNFARI,RateNFARI" << endl;
    yreport << "Stratum,V,N,Inc2,Rate2,Inc3,Rate3,IncT,RateT,IncNFARI,RateNFARI" << endl;
    popreport << "Sim,Person,STR,X,U,INITINF,DEFVAC,Protected,EventCounter,Type,OnsetDay,VacStatus,Visit" << endl;

#if WITH_UI
    if(m_ProgressDialog != NULL)   m_ProgressDialog->Update(0,"Simulation is running!");
#endif

    for (int S = 1; S <= this->SimPar.NSim; S++) {
#if WITH_UI
        if(m_ProgressDialog != NULL){
            if(m_ProgressDialog->WasCancelled()) break;
        }
#endif
        int POPD = 1; //popluation day
#if WITH_R
		Rcpp::Rcout << "Simulation " << S << endl;
        this->SimPop.Reset();
        Rcpp::Rcout << "\tReset Simulation" << endl;
#else
        cout << "Simulation " << S << endl;
        this->SimPop.Reset();
        cout << "\tReset Simulation" << endl;
#endif
        //SimPop.testStratifiedPeople();
        // Start simulation
        int D = 1;
        int NMonths = this->SimPar.NMonths;
        int Month = this->SimPar.FMonth;
        int Year = this->SimPar.FYear;

        // Loop through months
        for (int M = 1; M <= this->SimPar.NMonths; M++) {
#if WITH_R
            Rcpp::Rcout << "\tMonth " << M << endl;
            // Incrementally vaccinate population
            this->SimPop.Vaccinate(S, M, Month, Year, vreport, vacReport);
            // Rcpp::Rcout << "\t\tVaccinated" << endl;
#else
            cout << "\tMonth " << M << endl;
            // Incrementally vaccinate population
            this->SimPop.Vaccinate(S, M, Month, Year, vreport);
            // cout << "\t\tVaccinated" << endl;
#endif
            // Determine the number of days in the month
            int NDays;
            switch (Month) {
                case 1:
                case 3:
                case 5:
                case 7:
                case 8:
                case 10:
                case 12:
                    NDays = 31;
                    break;
                case 4:
                case 6:
                case 9:
                case 11:
                    NDays = 30;
                    break;
                default:
                    if (Year % 400 == 0) {
                        NDays = 29;
                    } else if (Year % 100 == 0) {
                        NDays = 28;
                    } else if (Year % 4 == 0) {
                        NDays = 29;
                    } else {
                        NDays = 28;
                    }
                    break;
            }

            // Spread the infection
            for (int Day = 1; Day <= NDays; Day++) {
#if WITH_R
                //Rcpp::Rcout << "\ta: " << M << "\t" << Day << endl;
                this->SimPop.Spread(D, Day, M, Month, Year, S, NDays, NMonths, dreport, ireport, preport, creport, mreport, sreport, dreports, mreports, yreport, popreport, detReport, daisimReport, daiReport, preReport, conReport, monsimReport, seasimReport, monReport,seaReport,populationReport);
                //Rcpp::Rcout << "\tb: " << M << "\t" << Day << endl;
#else
                //cout << "\ta: " << M << "\t" << Day << endl;
                this->SimPop.Spread(D, Day, M, Month, Year, S, NDays, NMonths, dreport, ireport, preport, creport, mreport, sreport, dreports, mreports, yreport, popreport);
                //cout << "\tb: " << M << "\t" << Day << endl;
#endif
                D++;
            }
            Month++;
            if (Month > 12) {
                Month = 1;
                Year++;
            }
#if WITH_UI
            if (S != this->SimPar.NSim){
                stringstream text;
                text << "Simulation " << S <<" is running!";
                if(m_ProgressDialog != NULL)  m_ProgressDialog->Update(S,text.str());
            }else{
                if(m_ProgressDialog != NULL){
                    m_ProgressDialog->Update(S,"Simulations are completed!");
                }
            }
#endif
        }

    }
    creport.close();
    dreport.close();
    dreports.close();
    ireport.close();
    preport.close();
    vreport.close();
    mreport.close();
    sreport.close();
    mreports.close();
    yreport.close();
    popreport.close();
}

