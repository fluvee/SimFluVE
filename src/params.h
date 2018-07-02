#ifndef __PARAMS_H
#define __PARAMS_H

#include <string>

using namespace std;

class Params  //Stores the parameters of the simulation
{
public:
  //overall simulation variables
  string SimTitle;    //Title of simulation
  string inputFile;   //Input filename
  int NSim;           //Number of simulation runs
  int FYear;          //First year at the start of a simulation
  int FMonth;         //First calendar month at the start of a simulation
  int NMonths;        //Number of months in a simulation

  int NStrata;        //Number of strata in simulation
  int RandomSeed;     //Seed for the random number generator

  //output files
  bool InputsReport;      //Print out inputs
  bool CReport;       //Print out contact report
  bool DReportPerson; //Print out daily report by person
  bool IReportDaily;
  bool PReport;
  bool DReportSummary;//Print out overall daily report
  bool VReport;       //Print out vaccination report
  bool DReportSum;
  bool MReportSim;    //Print out monthly report by simulation
  bool MReportSum;//Print out overall monthly report
  bool SReportSim;    //Print out season report by simulation
  bool YReportSum;//Print out overall season report
  bool PopReport;//Print out population report
  bool addTimeStampToFileNames;
  string timeStamp;

  string outputFilesPath;   // Path to where outputs showd be saved
  string InputsReportFile;      //Print out inputs
  string CReportFile;       //Print out contact report
  string DReportPersonFile; //Print out daily report by person
  string IReportDailyFile;
  string PReportFile;
  string DReportSummaryFile;//Print out overall daily report
  string VReportFile;       //Print out vaccination report
  string DReportSumFile;
  string MReportSimFile;    //Print out monthly report by simulation
  string MReportSumFile;//Print out overall monthly report
  string SReportSimFile;    //Print out season report by simulation
  string YReportSumFile;//Print out overall season report
  string PopReportFile;//Print out population report

  bool allOrNone;

  //Population numbers
  int* N;             //Number of people in each stratum
  float* probX1;      //Probability of X=1
  float* probU1_X1;   //Probability of U=1, given X=1
  float* probU1_X0;   //Probability of U=1, give X=0
  float* vacMultiplierX0;   //Vaccine multiplier for X=0
  float* vacMultiplierU0;   //Vaccine multiplier for U=0
  float* transProbMultiplierX0; //Transmission probability multiplier for X=0
  float* vacEfficacyMultiplierX0;   //Vaccine Efficacy multiplier for X=0
  int PopSize;        //Total number of people in population
  int NInit;          //Number of people infected initially

  //Times required for a person to switch between states
  int* Latent;        //Length of the latent period in days
  int* Infectious;    //Length of the infectious period in days
  float* Probinfill;  //Probability that an infectious person becomes ill

  //Non-influenza ARI parameters
  float** probARI;     //Daily probability of onset of a non-influenza ARI episode
  float* ariMultiplier_Vac;   //Multiplier to probARI for vaccinated or protected humans
  float* ariMultiplier_X0;    //Multiplier to probARI for X=0
  int* LARI;          //Length of a non-influenza ARI

  //Clinic visit related parameters
  float* probVisit_FARI;    //Probability of clinic visit for symptomatic influenza (FARI)
  float* probVisit_NFARI;   //Probability of clinic visit for Non-influenza ARI (NFARI)
  float* visitFARIMultiplier_Vac;   //Multiplier to FARI clinic visit probability for vaccinated or protected humans
  float* visitNFARIMultiplier_Vac;   //Multiplier to NFARI clinic visit probability for vaccinated or protected humans
  float* visitFARIMultiplier_X0;        //Multiplier to FARI clinic visit probability for X=0
  float* visitNFARIMultiplier_X0;        //Multiplier to NFARI clinic visit probability for X=0
  float* visitFARIMultiplier_U0;        //Multiplier to FARI clinic visit probability for U=0
  float* visitNFARIMultiplier_U0;        //Multiplier to NFARI clinic visit probability for U=0


  //Vaccine parameters
  float**** VacCov;     //Incremental vaccine coverage, X, U, Strata, Month

  //float** E;          //Vaccine effectiveness against illness


  //Transmission parameters
  int* C;             //Number of contacts made by each person in stratum
  //int** U;            //Number of illness cases
  float** Rho;        //Fraction of contacts between each strata
  float* Rinfxill;    //Relative infectiousness of a person who is infectious but not ill
  float*** Pai;        //Transmission probability for each X, strata during each month
  float*** Phi;        //Per-contact vaccine efficacy against transmission for each X, strata and month

  //Member functions
  void Load(string);     //Constructor for initializing parameters
  void Print(ostream&, string sep=" ");
  string getValue(string,int);       //Getting a value from a comma separated string
  void cleanUp();
};

#endif
