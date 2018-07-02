#include "simulator.h"
#include <iostream>
#include <chrono>
#include <cstdlib>
#include <Rcpp.h>
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

using namespace Rcpp;


// [[Rcpp::export]]
List simflu6(List para, bool verbose= true, String postfix="") {

    DetailReport* detRep = new DetailReport;
    DailySimReport* dsRep = new DailySimReport;
    DailyReport* daiRep = new DailyReport;
    PrevelanceReport* preRep = new PrevelanceReport;
    ContactReport* conRep = new ContactReport;
    VaccinationReport* vacRep = new VaccinationReport;
    MonthlySimReport* msRep = new MonthlySimReport;
    SeasonSimReport* ssRep = new SeasonSimReport;
    MonthlyReport* mRep = new MonthlyReport;
    SeasonReport* sRep = new SeasonReport;
    PopulationReport* popRep = new PopulationReport;
    Params p;
    /////////////////////////////////////////////////////////////////////////////////////

    // Read overall simulation properties
    p.SimTitle = as<std::string>(para["title"]);
    p.NSim = para["sim"];
    p.RandomSeed = para["rseed"];
    p.FYear = para["begin_year"];
    p.FMonth = para["begin_month"];
    p.NMonths = para["months"];
    p.NStrata = para["strata"];
    p.N = (int*)malloc(p.NStrata * sizeof(int));
    p.PopSize = 0;

    p.allOrNone = para["all_or_no_vaccine"];
    p.InputsReport = para["inputsfile"];
    p.VReport = para["vaccination_file"];
    p.DReportPerson = para["detailed_file"];
    p.CReport = para["contacts_file"];
    p.PReport = para["prevalance_file"];
    p.IReportDaily = para["daily_each_sim_file"];
    p.MReportSim = para["monthly_each_sim_file"];
    p.SReportSim = para["seasonal_each_sim_file"];
    p.DReportSum = para["daily_overall_file"];
    p.MReportSum = para["monthly_overall_file"];
    p.YReportSum = para["seasonal_overall_file"];
    p.PopReport = para["population_report_file"];
    p.addTimeStampToFileNames = para["timestamp"];



    NumericVector tempV = as<NumericVector>(para["size_strata"]);
    for(int i = 0; i < p.NStrata; i++){
        p.N[i] = tempV[i];
        p.PopSize += p.N[i];
    }

    NumericMatrix tempM = as<NumericMatrix>(para["prob_condition_strata"]);
    p.probX1 = (float*) malloc(p.NStrata * sizeof (float));
    for (int i = 1; i <= p.NStrata; i++) {
        p.probX1[i-1] = tempM(0,i-1);
    }
    p.probU1_X1 = (float*) malloc(p.NStrata * sizeof (float));
    for (int i = 1; i <= p.NStrata; i++) {
        p.probU1_X1[i-1] = tempM(1,i-1);
    }
    p.probU1_X0 = (float*) malloc(p.NStrata * sizeof (float));
    for (int i = 1; i <= p.NStrata; i++) {
        p.probU1_X0[i-1] = tempM(2,i-1);
    }

    p.VacCov = (float****) malloc(2 * sizeof (float*));
    for (int i = 0; i < 2; i++) {
        p.VacCov[i] = (float***) malloc(2 * sizeof (float*));
        for (int j = 0; j < 2; j++) {
            p.VacCov[i][j] = (float**) malloc((p.NMonths + 1) * sizeof (float*));
            for (int k = 0; k <= p.NMonths; k++) {
                p.VacCov[i][j][k] = (float*) malloc(p.NStrata * sizeof (float));
            }
        }
    }

    tempM = as<NumericMatrix>(para["vac_inc_coverage"]);
    for (int i = 0; i <= p.NMonths; i++) {
        for (int j = 1; j <= p.NStrata; j++) {
            p.VacCov[1][1][i][j - 1] = tempM(i,j-1);
        }
    }
    tempM = as<NumericMatrix>(para["vac_multiplyer"]);
    p.vacMultiplierX0 = (float*) malloc(p.NStrata * sizeof (float));
    for (int i = 1; i <= p.NStrata; i++) {
        p.vacMultiplierX0[i-1] = tempM(0,i-1);
    }
    p.vacMultiplierU0 = (float*) malloc(p.NStrata * sizeof (float));
    for (int i = 1; i <= p.NStrata; i++) {
        p.vacMultiplierU0[i-1] = tempM(1,i-1);
    }
    for (int i = 0; i <= p.NMonths; i++) {
        try {
            for (int j = 0; j < p.NStrata; j++) {
                if ((p.VacCov[0][1][i][j] = p.VacCov[1][1][i][j] * p.vacMultiplierX0[j]) > 1.) {
                    stringstream e;
                    e << "VacCov > 1 for M = " << i << ", K = " << j+1 << ", X = 0, U = 1";
                    throw e.str();
                }

                if ((p.VacCov[1][0][i][j] = p.VacCov[1][1][i][j] * p.vacMultiplierU0[j]) > 1.) {
                    stringstream e;
                    e << "VacCov > 1 for M = " << i << ", K = " << j+1 << ", X = 1, U = 0";
                    throw e.str();
                }

                if ((p.VacCov[0][0][i][j] = p.VacCov[1][1][i][j] * p.vacMultiplierX0[j] * p.vacMultiplierU0[j]) > 1.) {
                    stringstream e;
                    e << "VacCov > 1 for M = " << i << ", K = " << j+1 << ", X = 0, U = 0";
                    throw e.str();
                }
            }
        }
        catch (string e) {
            cout << "\n" << e << "\nExiting!" << endl;
            cin.get();
            exit(1);
        }
    }

    /////////////////////////// CHECK THIS
    p.NInit = para["num_infected_persons"];

    tempV = as<NumericVector>(para["contacts_per_day"]);
    p.C = (int*)malloc(p.NStrata * sizeof(int));
    for(int i = 1; i <= p.NStrata; i++) {
        p.C[i-1] = tempV[i-1];
    }

    // Distribution of contacts
    p.Rho = (float**)malloc(p.NStrata * sizeof(float*));
    for(int i = 0; i < p.NStrata; i++) {
        p.Rho[i] = (float*)malloc(p.NStrata * sizeof(float));
    }
    tempM = as<NumericMatrix>(para["dist_contacts"]);
    for(int i = 0; i < p.NStrata; i++) {
        for(int j = 1; j <= p.NStrata; j++) {
            p.Rho[i][j-1] = tempM(i,j-1);
        }
    }
    // Latent period for infection
    tempV = as<NumericVector>(para["latency_days"]);
    p.Latent = (int*)malloc(p.NStrata * sizeof(int));
    for(int i = 1; i <= p.NStrata; i++) {
        p.Latent[i-1] = tempV[i-1];
    }

    // Infectious period
    tempV = as<NumericVector>(para["infectious_days"]);
    p.Infectious = (int*)malloc(p.NStrata * sizeof(int));
    for(int i = 1; i <= p.NStrata; i++) {
        p.Infectious[i-1] = tempV[i-1];
    }

    // Probability that infected person becomes ill
    tempV = as<NumericVector>(para["prob_illness"]);
    p.Probinfill = (float*)malloc(p.NStrata * sizeof(float));
    for(int i = 1; i <= p.NStrata; i++) {
        p.Probinfill[i-1] = tempV[i-1];
    }

    // Relative infectiousness
    tempV = as<NumericVector>(para["relative_infectious_not_ill"]);
    p.Rinfxill = (float*)malloc(p.NStrata * sizeof(float));
    for(int i = 1; i <= p.NStrata; i++) {
        p.Rinfxill[i-1] = tempV[i-1];
    }

    // Transmission probabilities unvac
    p.Pai = (float***) malloc(2 * sizeof (float*));
    for (int i = 0; i < 2; i++) {
        p.Pai[i] = (float**) malloc(p.NMonths * sizeof (float*));
        for (int j = 0; j < p.NMonths; j++) {
            p.Pai[i][j] = (float*) malloc(p.NStrata * sizeof (float));
        }
    }
    tempM = as<NumericMatrix>(para["prob_transmission"]);
    for (int i = 0; i < p.NMonths; i++) {
        for (int j = 1; j <= p.NStrata; j++) {
            p.Pai[1][i][j - 1] = tempM(i,j-1);
        }
    }
    tempV = as<NumericVector>(para["trans_prob_multiplier"]);
    p.transProbMultiplierX0 = (float*) malloc(p.NStrata * sizeof (float));
    for (int i = 1; i <= p.NStrata; i++) {
        p.transProbMultiplierX0[i-1] = tempV[i-1];
    }
    for (int i = 0; i < p.NMonths; i++) {
        for (int j = 0; j < p.NStrata; j++) {
            try {
                if ((p.Pai[0][i][j] = p.Pai[1][i][j] * p.transProbMultiplierX0[j]) > 1.) {
                    stringstream e;
                    e << "Pai > 1 for M = " << i+1 << ", K = " << j+1 << ", X = 0";
                    throw e.str();
                }
            }
            catch (string e) {
                cout << "\n" << e << "\nExiting!" << endl;
                cin.get();
                exit(1);
            }
        }
    }

    // Vaccine efficacy
    p.Phi = (float***) malloc(2 * sizeof (float*));
    for (int i = 0; i < 2; i++) {
        p.Phi[i] = (float**) malloc(p.NMonths * sizeof (float*));
        for (int j = 0; j < p.NMonths; j++) {
            p.Phi[i][j] = (float*) malloc(p.NStrata * sizeof (float));
        }
    }
    tempM = as<NumericMatrix>(para["prob_transmission_vac"]);
    for (int i = 0; i < p.NMonths; i++) {
        for (int j = 1; j <= p.NStrata; j++) {
            p.Phi[1][i][j - 1] = tempM(i,j-1);
       }
    }
    tempV = as<NumericVector>(para["vec_efficacy_multiplier"]);
    p.vacEfficacyMultiplierX0 = (float*) malloc(p.NStrata * sizeof (float));
    for (int i = 1; i <= p.NStrata; i++) {
        p.vacEfficacyMultiplierX0[i-1] = tempV[i-1];
    }
    for (int i = 0; i < p.NMonths; i++) {
        for (int j = 0; j < p.NStrata; j++) {
            try {
                if ((p.Phi[0][i][j] = p.Phi[1][i][j] * p.vacEfficacyMultiplierX0[j]) > 1.) {
                    stringstream e;
                    e << "Phi > 1 for M = " << i + 1 << ", K = " << j+1 << ", X = 0";
                    throw e.str();
                }
            }
            catch (string e) {
                cout << "\n" << e << "\nExiting!" << endl;
                cin.get();
                exit(1);
            }
        }
    }
    // Length of non-influenza ARI episode
    tempV = as<NumericVector>(para["length_ARI"]);
    p.LARI = (int*) malloc(p.NStrata * sizeof (int));
    for (int i = 1; i <= p.NStrata; i++) {
        p.LARI[i - 1] = tempV[i-1];
    }
    // Probability of non-influenza ARI
    p.probARI = (float**) malloc(p.NMonths * sizeof (float*));
    for (int i = 0; i < p.NMonths; i++) {
        p.probARI[i] = (float*) malloc(p.NStrata * sizeof (float));
    };
    tempM = as<NumericMatrix>(para["daily_prob_NFARI"]);
    for (int i = 0; i < p.NMonths; i++) {
        for (int j = 1; j <= p.NStrata; j++) {
            p.probARI[i][j - 1] = tempM(i,j-1);
        }
    }
    // Multiplier to probARI for vaccinated/protected humans
    tempV = as<NumericVector>(para["probARI_vaccined_multiplier"]);
    p.ariMultiplier_Vac = (float*) malloc(p.NStrata * sizeof (float));
    for (int i = 1; i <= p.NStrata; i++) {
        p.ariMultiplier_Vac[i - 1] = tempV[i-1];
    }

    // Multiplier to probARI for X=0
    tempV = as<NumericVector>(para["probARI_X0_multiplier"]);
    p.ariMultiplier_X0 = (float*) malloc(p.NStrata * sizeof (float));
    for (int i = 1; i <= p.NStrata; i++) {
        p.ariMultiplier_X0[i - 1] = tempV[i-1];
    }
    // Probability of clinic visit for symptomatic influenza (FARI)
    tempV = as<NumericVector>(para["prob_visit_FARI"]);
    p.probVisit_FARI = (float*) malloc(p.NStrata * sizeof (float));
    for (int i = 1; i <= p.NStrata; i++) {
        p.probVisit_FARI[i - 1] = tempV[i-1];
    }
    // Multiplier to FARI clinic visit probability for vaccinated or protected humans
    tempM = as<NumericMatrix>(para["visit_FARI_multiplier"]);
    p.visitFARIMultiplier_Vac = (float*) malloc(p.NStrata * sizeof (float));
    for (int i = 1; i <= p.NStrata; i++) {
        p.visitFARIMultiplier_Vac[i - 1] = tempM(0,i-1);
    }
    // Multiplier to FARI clinic visit probability for X=0
    p.visitFARIMultiplier_X0 = (float*) malloc(p.NStrata * sizeof (float));
    for (int i = 1; i <= p.NStrata; i++) {
        p.visitFARIMultiplier_X0[i - 1] = tempM(1,i-1);
    }
    // Multiplier to FARI clinic visit probability for U=0
    p.visitFARIMultiplier_U0 = (float*) malloc(p.NStrata * sizeof (float));
    for (int i = 1; i <= p.NStrata; i++) {
        p.visitFARIMultiplier_U0[i - 1] = tempM(2,i-1);
    }
    // Probability of clinic visit for NFARI
    tempV = as<NumericVector>(para["prob_visit_NFARI"]);
    p.probVisit_NFARI = (float*) malloc(p.NStrata * sizeof (float));
    for (int i = 1; i <= p.NStrata; i++) {
        p.probVisit_NFARI[i - 1] = tempV[i-1];
    }
    // Multiplier to NFARI clinic visit probability for vaccinated or protected humans
    tempM = as<NumericMatrix>(para["visit_NFARI_multiplier"]);
    p.visitNFARIMultiplier_Vac = (float*) malloc(p.NStrata * sizeof (float));
    for (int i = 1; i <= p.NStrata; i++) {
        p.visitNFARIMultiplier_Vac[i - 1] = tempM(0,i-1);
    }
    // Multiplier to clinic visit probability NFARI for X=0
    p.visitNFARIMultiplier_X0 = (float*) malloc(p.NStrata * sizeof (float));
    for (int i = 1; i <= p.NStrata; i++) {
        p.visitNFARIMultiplier_X0[i - 1] = tempM(1,i-1);
    }
    // Multiplier to clinic visit probability NFARI U=1
    p.visitNFARIMultiplier_U0 = (float*) malloc(p.NStrata * sizeof (float));
    for (int i = 1; i <= p.NStrata; i++) {
        p.visitNFARIMultiplier_U0[i - 1] = tempM(2,i-1);
    }


    // if(verbose) {
    //     p.Print();
    // }
    Simulator simulation(p, detRep, dsRep, daiRep, preRep, conRep, vacRep, msRep, ssRep, mRep, sRep, popRep, postfix);
    simulation.Start();

    DataFrame det = DataFrame::create(_["S"] = detRep->s,
                                        _["D"] = detRep->d,
                                        _["Day"] = detRep->day,
                                        _["Month"] = detRep->month,
                                        _["Year"] = detRep->year,
                                        _["I"] = detRep->ii,
                                        _["Stratum"] = detRep->stratum,
                                        _["X"] = detRep->x,
                                        _["U"] = detRep->u,
                                        _["InitInf"] = detRep->initInf,
                                        _["Vac"] = detRep->vac,
                                        _["Protected"] = detRep->prot,
                                        _["State"] = detRep->state,
                                        _["DayInf"] = detRep->dayInf,
                                        _["DNFARI"] = detRep->dNFARI,
                                        _["Visit"] = detRep->visit);
    DataFrame daisim = DataFrame::create(_["S"] = dsRep->s,
                                        _["D"] = dsRep->d,
                                        _["Day"] = dsRep->day,
                                        _["Month"] = dsRep->month,
                                        _["Year"] = dsRep->year,
                                        _["Stratum"] = dsRep->stratum,
                                        _["V"] = dsRep->v,
                                        _["N"] = dsRep->nn,
                                        _["Inc2"] = dsRep->inc2,
                                        _["Rate2"] = dsRep->rate2,
                                        _["Inc3"] = dsRep->inc3,
                                        _["Rate3"] = dsRep->rate3,
                                        _["IncT"] = dsRep->incT,
                                        _["RateT"] = dsRep->rateT,
                                        _["IncNFARI"] = dsRep->incNFARI,
                                        _["RateNFARI"] = dsRep->rateNFARI);
    DataFrame dai = DataFrame::create(_["D"] = daiRep->d,
                                        _["Day"] = daiRep->day,
                                        _["Month"] = daiRep->month,
                                        _["Year"] = daiRep->year,
                                        _["Stratum"] = daiRep->stratum,
                                        _["V"] = daiRep->v,
                                        _["N"] = daiRep->nn,
                                        _["Inc2"] = daiRep->inc2,
                                        _["Rate2"] = daiRep->rate2,
                                        _["Inc3"] = daiRep->inc3,
                                        _["Rate3"] = daiRep->rate3,
                                        _["IncT"] = daiRep->incT,
                                        _["RateT"] = daiRep->rateT,
                                        _["IncNFARI"] = daiRep->incNFARI,
                                        _["RateNFARI"] = daiRep->rateNFARI);
    DataFrame prev;
if (p.allOrNone){
    prev = DataFrame::create(_["S"] = preRep->s,
                                        _["D"] = preRep->d,
                                        _["Day"] = preRep->day,
                                        _["Month"] = preRep->month,
                                        _["Year"] = preRep->year,
                                        _["Stratum"] = preRep->stratum,
                                        _["Prevelance(S2)"] = preRep->prevS2,
                                        _["Prevelance(S3)"] = preRep->prevS3,
                                        _["P(INF|UNPROTECTED;X=0)"] = preRep->pInfX0,
                                        _["P(INF|UNPROTECTED;X=1)"] = preRep->pInfX1);
}else{
    prev = DataFrame::create(_["S"] = preRep->s,
                                        _["D"] = preRep->d,
                                        _["Day"] = preRep->day,
                                        _["Month"] = preRep->month,
                                        _["Year"] = preRep->year,
                                        _["Stratum"] = preRep->stratum,
                                        _["Prevelance(S2)"] = preRep->prevS2,
                                        _["Prevelance(S3)"] = preRep->prevS3,
                                        _["P(INF|V=0;X=0)"] = preRep->pInfV0X0,
                                        _["P(INF|V=0;X=1)"] = preRep->pInfV0X1,
                                        _["P(INF|V=1;X=0)"] = preRep->pInfV1X0,
                                        _["P(INF|V=1;X=1)"] = preRep->pInfV1X1);}
    DataFrame con = DataFrame::create( _["S"] = conRep->s,
                                        _["D"] = conRep->d,
                                        _["I"] = conRep->ii,
                                        _["Stratum"] = conRep->stratum,
                                        _["X"] = conRep->x,
                                        _["U"] = conRep->u,
                                        _["Vac"] = conRep->vac,
                                        _["Protected"] = conRep->prot,
                                        _["P(INF)"] = conRep->pINF,
                                        _["RANDOM"] = conRep->RANDOM,
                                        _["NewInf"] = conRep->newInf);
    DataFrame vac = DataFrame::create( _["S"] = vacRep->s,
                                        _["M"] = vacRep->mm,
                                        _["Month"] = vacRep->month,
                                        _["Year"] = vacRep->year,
                                        _["K"] = vacRep->k,
                                        _["X"] = vacRep->x,
                                        _["U"] = vacRep->u,
                                        _["Number of Newly Vaccinated"] = vacRep->number,
                                        _["Newly of Vaccinated"] = vacRep->newly);
    DataFrame monsim = DataFrame::create( _["S"] = msRep->s,
                                        _["Month"] = msRep->month,
                                        _["Year"] = msRep->year,
                                        _["Stratum"] = msRep->stratum,
                                        _["V"] = msRep->v,
                                        _["N"] = msRep->nn,
                                        _["Inc2"] = msRep->inc2,
                                        _["Rate2"] = msRep->rate2,
                                        _["Inc3"] = msRep->inc3,
                                        _["Rate3"] = msRep->rate3,
                                        _["IncT"] = msRep->incT,
                                        _["RateT"] = msRep->rateT,
                                        _["IncNFARI"] = msRep->incNFARI,
                                        _["RateNFARI"] = msRep->rateNFARI);
    DataFrame seasim = DataFrame::create( _["S"] = ssRep->s,
                                        _["Stratum"] = ssRep->stratum,
                                        _["V"] = ssRep->v,
                                        _["N"] = ssRep->nn,
                                        _["Inc2"] = ssRep->inc2,
                                        _["Rate2"] = ssRep->rate2,
                                        _["Inc3"] = ssRep->inc3,
                                        _["Rate3"] = ssRep->rate3,
                                        _["IncT"] = ssRep->incT,
                                        _["RateT"] = ssRep->rateT,
                                        _["IncNFARI"] = ssRep->incNFARI,
                                        _["RateNFARI"] = ssRep->rateNFARI);
    DataFrame mon = DataFrame::create( _["Month"] = mRep->month,
                                        _["Year"] = mRep->year,
                                        _["Stratum"] = mRep->stratum,
                                        _["V"] = mRep->v,
                                        _["N"] = mRep->nn,
                                        _["Inc2"] = mRep->inc2,
                                        _["Rate2"] = mRep->rate2,
                                        _["Inc3"] = mRep->inc3,
                                        _["Rate3"] = mRep->rate3,
                                        _["IncT"] = mRep->incT,
                                        _["RateT"] = mRep->rateT,
                                        _["IncNFARI"] = mRep->incNFARI,
                                        _["RateNFARI"] = mRep->rateNFARI);
    DataFrame sea = DataFrame::create( _["Stratum"] = sRep->stratum,
                                        _["V"] = sRep->v,
                                        _["N"] = sRep->nn,
                                        _["Inc2"] = sRep->inc2,
                                        _["Rate2"] = sRep->rate2,
                                        _["Inc3"] = sRep->inc3,
                                        _["Rate3"] = sRep->rate3,
                                        _["IncT"] = sRep->incT,
                                        _["RateT"] = sRep->rateT,
                                        _["IncNFARI"] = sRep->incNFARI,
                                        _["RateNFARI"] = sRep->rateNFARI);
    DataFrame pop = DataFrame::create( _["Sim"] = popRep->sim,
                                        _["Person"] = popRep->person,
                                        _["STR"] = popRep->str,
                                        _["X"] = popRep->x,
                                        _["U"] = popRep->u,
                                        _["INITINF"] = popRep->initinf,
                                        _["DEFVAC"] = popRep->defvac,
                                        _["Protected"] = popRep->prot,
                                        _["EventCounter"] = popRep->eventcounter,
                                        _["Type"] = popRep->type,
                                        _["OnsetDay"] = popRep->onsetday,
                                        _["VacStatus"] = popRep->vacstatus,
                                        _["Visit"] = popRep->visit);

    //Rcpp::Rcout << "\nmonthly report length: " << mRep.stratum.size();
    //Rcpp::Rcout << "\nseason report length: " << sRep.stratum.size();
    List z = List::create(Named("DReportPerson") = det,
                            Named("IReportDaily") = daisim,
                            Named("DReportSum") = dai,
                            Named("PReport") = prev,
                            Named("CReport") = con,
                            Named("VReport") = vac,
                            Named("MReportSim") = monsim,
                            Named("SReportSim") = seasim,
                            Named("MReportSum") = mon,
                            Named("YReportSum") = sea,
                            Named("PopReport") = pop);
    // if (p.DReportPerson) z$DReportPerson = det;
    // if (p.IReportDaily) z$IReportDaily = daisim;
    // if (p.DReportSum) z$DReportSum = dai;
    // if (p.PReport) z$PReport = prev;
    // if (p.CReport) z$CReport = con;
    // if (p.VReport) z$VReport = vac;
    // if (p.MReportSim) z$MReportSim = monsim;
    // if (p.SReportSim) z$SReportSim = seasim;
    // if (p.MReportSum) z$MReportSum = mon;
    // if (p.YReportSum) z$YReportSum = sea;
    // if (p.PopReport) z$PopReport = pop;
    //delete simulation;
    //Rcpp::Rcout << "\nSimulation deleted" <<endl;
    delete detRep;
    delete dsRep;
    delete daiRep;
    delete preRep;
    delete conRep;
    delete vacRep;
    delete msRep;
    delete ssRep;
    delete mRep;
    delete sRep;
    delete popRep;
    return z ;
}
