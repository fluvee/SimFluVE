#include "params.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <cstdlib>
#include <ctime>
#include <chrono>
#include <iomanip>
#include <locale>
#if WITH_R
#include <Rcpp.h>
#endif

using namespace std;

//Read in the parameters from the supplied filename

void Params::Load(string filename) {
    this->inputFile = filename;
    ifstream fin(filename.c_str());

    if (!fin.good()) {
#if WITH_R
        Rcpp::Rcout << "\n" << inputFile << " not found.";
        Rcpp::Rcout << "\nExiting..." <<endl;
#else
        cout << "\n" << inputFile << " not found.";
        cout << "\nExiting..." <<endl;
#endif
        exit(0);
        cin.get();
    }
    /*
#if WITH_R
    while (!fin.good()) {
        Rcpp::Rcout << "\n" << inputFile << " not found. Enter input file or type quit to exit! << endl";
        Rcpp::Rcout << ": ";
        cin >> filename;
        if (filename == "quit"){
            Rcpp::Rcout << "\nExiting..." <<endl;
            exit(0);
        }
    }
#else
    while (!fin.good()) {
        cout << "\n" << inputFile << " not found. Enter input file or type quit to exit! << endl";
        cout << ": ";
        cin >> filename;
        if (filename == "quit"){
            cout << "\nExiting..." <<endl;
            exit(0);
        }
    }
#endif
    */
    string chute;

    // Read overall simulation properties
    getline(fin, chute);
#if WITH_R
    Rcpp::Rcout << "\nTest read:" << chute << endl;
#else
    std::cout << "\nTest read:" << chute << endl;
#endif
    this->SimTitle = getValue(chute, 1);
    getline(fin, chute);
    this->NSim = atoi(getValue(chute, 1).c_str());
    getline(fin, chute);
    this->RandomSeed = atoi(getValue(chute, 1).c_str());
    getline(fin, chute);
    chute = getValue(chute, 1);
    this->allOrNone = chute == "yes" ? true : false;

    // Read output files required
    getline(fin, chute);

    getline(fin, chute);
    chute = getValue(chute, 1);
    this->InputsReport = chute == "yes" ? true : false;

    getline(fin, chute);
    chute = getValue(chute, 1);
    this->VReport = chute == "yes" ? true : false;

    getline(fin, chute);
    chute = getValue(chute, 1);
    this->DReportPerson = chute == "yes" ? true : false;

    getline(fin, chute);
    chute = getValue(chute, 1);
    this->CReport = chute == "yes" ? true : false;


    getline(fin, chute);
    chute = getValue(chute, 1);
    this->PReport = chute == "yes" ? true : false;

    getline(fin, chute);
    chute = getValue(chute, 1);
    this->IReportDaily = chute == "yes" ? true : false;

    getline(fin, chute);
    chute = getValue(chute, 1);
    this->MReportSim = chute == "yes" ? true : false;

    getline(fin, chute);
    chute = getValue(chute, 1);
    this->SReportSim = chute == "yes" ? true : false;

    getline(fin, chute);
    chute = getValue(chute, 1);
    this->DReportSum = chute == "yes" ? true : false;

    getline(fin, chute);
    chute = getValue(chute, 1);
    this->MReportSum = chute == "yes" ? true : false;

    getline(fin, chute);
    chute = getValue(chute, 1);
    this->YReportSum = chute == "yes" ? true : false;

    //new file
    getline(fin, chute);
    chute = getValue(chute, 1);
    this->PopReport = chute == "yes" ? true : false;

    getline(fin, chute);
    chute = getValue(chute, 1);
    this->addTimeStampToFileNames = chute == "yes" ? true : false;

    std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
    std::time_t now_c = std::chrono::system_clock::to_time_t(now);
    stringstream ss, ss1;

    //ss << std::put_time(std::localtime(&now_c), "%F_%H:%M:%S_%Z");
    //timeStamp = ss.str();

    std::tm * now1 = std::localtime ( &now_c );
    std::string pattern ("%Y-%m-%d_%H%M%S");
    std::locale loc;

    const std::time_put<char>& tmput = std::use_facet <std::time_put<char> > (loc);
    tmput.put (ss1, ss1, ' ', now1, pattern.data(), pattern.data()+pattern.length());

    timeStamp = ss1.str();


    //Read the time period in each simulation
    //getline(fin,chute);//what's wrong? problem of excel?
    getline(fin, chute);
    this->FYear = atoi(getValue(chute, 1).c_str());
    getline(fin, chute);
    this->FMonth = atoi(getValue(chute, 1).c_str());
    getline(fin, chute);
    this->NMonths = atoi(getValue(chute, 1).c_str());

    // Read the population size
    getline(fin, chute);
    this->NStrata = atoi(getValue(chute, 1).c_str());
    this->N = (int*) malloc(this->NStrata * sizeof (int));
    getline(fin, chute);
    this->PopSize = 0;
    for (int i = 1; i <= this->NStrata; i++) {
        this->N[i - 1] = atoi(getValue(chute, i).c_str());
        this->PopSize += this->N[i - 1];
    }

    // Read the covariate probabilities
    this->probX1 = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->probX1[i-1] = atof(getValue(chute, i).c_str());
    }
    this->probU1_X1 = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->probU1_X1[i-1] = atof(getValue(chute, i).c_str());
    }
    this->probU1_X0 = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->probU1_X0[i-1] = atof(getValue(chute, i).c_str());
    }

    // Vaccination coverage
    this->VacCov = (float****) malloc(2 * sizeof (float*));
    for (int i = 0; i < 2; i++) {
        this->VacCov[i] = (float***) malloc(2 * sizeof (float*));
        for (int j = 0; j < 2; j++) {
            this->VacCov[i][j] = (float**) malloc((this->NMonths + 1) * sizeof (float*));
            for (int k = 0; k <= this->NMonths; k++) {
                this->VacCov[i][j][k] = (float*) malloc(this->NStrata * sizeof (float));
            }
        }
    }

    getline(fin, chute);
    for (int i = 0; i <= this->NMonths; i++) {
        getline(fin, chute);
        for (int j = 1; j <= this->NStrata; j++) {
            this->VacCov[1][1][i][j - 1] = atof(getValue(chute, j).c_str());
        }
    }

    this->vacMultiplierX0 = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->vacMultiplierX0[i-1] = atof(getValue(chute, i).c_str());
    }

    this->vacMultiplierU0 = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->vacMultiplierU0[i-1] = atof(getValue(chute, i).c_str());
    }

    for (int i = 0; i <= this->NMonths; i++) {
        try {
            for (int j = 0; j < this->NStrata; j++) {
                if ((this->VacCov[0][1][i][j] = this->VacCov[1][1][i][j] * vacMultiplierX0[j]) > 1.) {
                    stringstream e;
                    e << "VacCov > 1 for M = " << i << ", K = " << j+1 << ", X = 0, U = 1";
                    throw e.str();
                }

                if ((this->VacCov[1][0][i][j] = this->VacCov[1][1][i][j] * vacMultiplierU0[j]) > 1.) {
                    stringstream e;
                    e << "VacCov > 1 for M = " << i << ", K = " << j+1 << ", X = 1, U = 0";
                    throw e.str();
                }

                if ((this->VacCov[0][0][i][j] = this->VacCov[1][1][i][j] * vacMultiplierX0[j] * vacMultiplierU0[j]) > 1.) {
                    stringstream e;
                    e << "VacCov > 1 for M = " << i << ", K = " << j+1 << ", X = 0, U = 0";
                    throw e.str();
                }
            }
        }
        catch (string e) {
#if WITH_R
            Rcpp::Rcout << "\n" << e << "\nExiting!" << endl;
#else
            cout << "\n" << e << "\nExiting!" << endl;
#endif
            cin.get();
            exit(1);
        }
    }


    // Number of cases of illness
    //this->U = (int**)malloc(this->NMonths * sizeof(int*));
    //for(int i = 0; i < this->NMonths; i++)
    //{
    // this->U[i] = (int*)malloc(this->NStrata * sizeof(int));
    //}
    //getline(fin,chute);
    //for(int i = 0; i < this->NMonths; i++)
    //{
    //   getline(fin,chute);
    // for(int j = 1; j <= this->NStrata; j++)
    //{
    // this->U[i][j-1] = atoi(getValue(chute,j).c_str());
    //}
    //}

    // Vaccination effectiveness
    // this->E = (float**)malloc(this->NMonths * sizeof(float*));
    //for(int i = 0; i < this->NMonths; i++)
    //{
    //this->E[i] = (float*)malloc(this->NStrata * sizeof(float));
    //}
    //getline(fin,chute);
    //for(int i = 0; i < this->NMonths; i++)
    //{
    //getline(fin,chute);
    //for(int j = 1; j <= this->NStrata; j++)
    //{
    //this->E[i][j-1] = atof(getValue(chute,j).c_str());
    //}
    //}

    // Contact parameters
    getline(fin, chute);
    this->NInit = atoi(getValue(chute, 1).c_str());
    this->C = (int*) malloc(this->NStrata * sizeof (int));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->C[i - 1] = atoi(getValue(chute, i).c_str());
    }

    // Distribution of contacts
    this->Rho = (float**) malloc(this->NStrata * sizeof (float*));
    for (int i = 0; i < this->NStrata; i++) {
        this->Rho[i] = (float*) malloc(this->NStrata * sizeof (float));
    }
    getline(fin, chute);
    for (int i = 0; i < this->NStrata; i++) {
        getline(fin, chute);
        for (int j = 1; j <= this->NStrata; j++) {
            this->Rho[i][j - 1] = atof(getValue(chute, j).c_str());
        }
    }

    // Latent period for infection
    this->Latent = (int*) malloc(this->NStrata * sizeof (int));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->Latent[i - 1] = atoi(getValue(chute, i).c_str());
    }

    // Infectious period
    this->Infectious = (int*) malloc(this->NStrata * sizeof (int));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->Infectious[i - 1] = atoi(getValue(chute, i).c_str());
    }

    // Probability that infected person becomes ill
    this->Probinfill = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->Probinfill[i - 1] = atof(getValue(chute, i).c_str());
    }

    // Relative infectiousness
    this->Rinfxill = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->Rinfxill[i - 1] = atof(getValue(chute, i).c_str());
    }

    // Transmission probabilities unprotected X=1
    this->Pai = (float***) malloc(2 * sizeof (float*));
    for (int i = 0; i < 2; i++) {
        this->Pai[i] = (float**) malloc(this->NMonths * sizeof (float*));
        for (int j = 0; j < this->NMonths; j++) {
            this->Pai[i][j] = (float*) malloc(this->NStrata * sizeof (float));
        }
    }

    getline(fin, chute);
    for (int i = 0; i < this->NMonths; i++) {
        getline(fin, chute);
        for (int j = 1; j <= this->NStrata; j++) {
            this->Pai[1][i][j - 1] = atof(getValue(chute, j).c_str());
        }
    }

    this->transProbMultiplierX0 = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->transProbMultiplierX0[i-1] = atof(getValue(chute, i).c_str());
    }

    for (int i = 0; i < this->NMonths; i++) {
        for (int j = 0; j < this->NStrata; j++) {
            try {
                if ((this->Pai[0][i][j] = Pai[1][i][j] * transProbMultiplierX0[j]) > 1.) {
                    stringstream e;
                    e << "Pai > 1 for M = " << i+1 << ", K = " << j+1 << ", X = 0";
                    throw e.str();
                }
            }
            catch (string e) {
#if WITH_R
                Rcpp::Rcout << "\n" << e << "\nExiting!" << endl;
#else                
                cout << "\n" << e << "\nExiting!" << endl;
#endif
                cin.get();
                exit(1);
            }
        }
    }





    // Vaccine efficacy
    this->Phi = (float***) malloc(2 * sizeof (float*));
    for (int i = 0; i < 2; i++) {
        this->Phi[i] = (float**) malloc(this->NMonths * sizeof (float*));
        for (int j = 0; j < this->NMonths; j++) {
            this->Phi[i][j] = (float*) malloc(this->NStrata * sizeof (float));
        }
    }
    getline(fin, chute);


    /*
    if (this->allOrNone) {
        for (int i = 0; i < this->NMonths; i++)
            getline(fin, chute);

        getline(fin, chute);
    }
    */

    for (int i = 0; i < this->NMonths; i++) {
        getline(fin, chute);
        for (int j = 1; j <= this->NStrata; j++) {
            this->Phi[1][i][j - 1] = atof(getValue(chute, j).c_str());
            //if (!this->allOrNone)
            //    this->Phi[i][j - 1] = 1 - this->Phi[i][j - 1] / this->Pai[i][j - 1]; //new phi
        }
    }


    this->vacEfficacyMultiplierX0 = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->vacEfficacyMultiplierX0[i-1] = atof(getValue(chute, i).c_str());
    }

    for (int i = 0; i < this->NMonths; i++) {
        for (int j = 0; j < this->NStrata; j++) {
            try {
                if ((this->Phi[0][i][j] = Phi[1][i][j] * vacEfficacyMultiplierX0[j]) > 1.) {
                    stringstream e;
                    e << "Phi > 1 for M = " << i + 1 << ", K = " << j+1 << ", X = 0";
                    throw e.str();
                }
            }
            catch (string e) {
#if WITH_R
                Rcpp::Rcout << "\n" << e << "\nExiting!" << endl;
#else
                cout << "\n" << e << "\nExiting!" << endl;
#endif
                cin.get();
                exit(1);
            }
        }
    }

    // Length of non-influenza ARI episode
    this->LARI = (int*) malloc(this->NStrata * sizeof (int));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->LARI[i - 1] = atoi(getValue(chute, i).c_str());
    }


    // Probability of non-influenza ARI
    this->probARI = (float**) malloc(this->NMonths * sizeof (float*));
    for (int i = 0; i < this->NMonths; i++) {
        this->probARI[i] = (float*) malloc(this->NStrata * sizeof (float));
    }
    getline(fin, chute);
    for (int i = 0; i < this->NMonths; i++) {
        getline(fin, chute);
        for (int j = 1; j <= this->NStrata; j++) {
            this->probARI[i][j - 1] = atof(getValue(chute, j).c_str());
        }
    }


    // Multiplier to probARI for vaccinated/protected humans
    this->ariMultiplier_Vac = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->ariMultiplier_Vac[i - 1] = atof(getValue(chute, i).c_str());
    }

    // Multiplier to probARI for X=0
    this->ariMultiplier_X0 = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->ariMultiplier_X0[i - 1] = atof(getValue(chute, i).c_str());
    }

    getline(fin, chute);

    // Probability of clinic visit for symptomatic influenza (FARI)
    this->probVisit_FARI = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->probVisit_FARI[i - 1] = atof(getValue(chute, i).c_str());
    }

    // Multiplier to FARI clinic visit probability for vaccinated or protected humans
    this->visitFARIMultiplier_Vac = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->visitFARIMultiplier_Vac[i - 1] = atof(getValue(chute, i).c_str());
    }

    // Multiplier to FARI clinic visit probability for X=0
    this->visitFARIMultiplier_X0 = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->visitFARIMultiplier_X0[i - 1] = atof(getValue(chute, i).c_str());
    }

    // Multiplier to FARI clinic visit probability for U=0
    this->visitFARIMultiplier_U0 = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->visitFARIMultiplier_U0[i - 1] = atof(getValue(chute, i).c_str());
    }

    getline(fin, chute);

    // Probability of clinic visit for Non-influenza ARI (NFARI)
    this->probVisit_NFARI = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->probVisit_NFARI[i - 1] = atof(getValue(chute, i).c_str());
    }

    // Multiplier to NFARI clinic visit probability for vaccinated or protected humans
    this->visitNFARIMultiplier_Vac = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->visitNFARIMultiplier_Vac[i - 1] = atof(getValue(chute, i).c_str());
    }

    // Multiplier to NFARI clinic visit probability for X=0
    this->visitNFARIMultiplier_X0 = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->visitNFARIMultiplier_X0[i - 1] = atof(getValue(chute, i).c_str());
    }

    // Multiplier to NFARI clinic visit probability for U=0
    this->visitNFARIMultiplier_U0 = (float*) malloc(this->NStrata * sizeof (float));
    getline(fin, chute);
    for (int i = 1; i <= this->NStrata; i++) {
        this->visitNFARIMultiplier_U0[i - 1] = atof(getValue(chute, i).c_str());
    }

    fin.close();
}

string Params::getValue(string inp, int pos) {
    stringstream ss(inp);
    string elem;

    for (int i = 0; i <= pos; i++) {
        std::getline(ss, elem, ',');
    }

    return elem;
}


//Print out the read in parameters for checking.

void Params::Print(ostream &out, string sep) {
    out << timeStamp << endl;
    out << "Input File" << sep << inputFile << endl;
    out << "Title" << sep << this->SimTitle << endl;
    out << "Number of simulations" << sep << this->NSim << endl;
    out << "Seed" << sep << this->RandomSeed << endl;
    out << "All-or-none vaccine" << sep << this->allOrNone << endl;
    out << "Output files requested:" << endl;
    out << "Inputs and Calculated Parameters" << sep << this->InputsReport << endl;
    out << "Vaccination" << sep << this->VReport << endl;
    out << "Detailed" << sep << this->DReportPerson << endl;
    out << "Contacts" << sep << this->CReport << endl;
    out << "Prevelance" << sep << this->PReport << endl;
    out << "Incidence-daily-each simulation" << sep << this->IReportDaily << endl;
    out << "Incidence-monthly-each simulation" << sep << this->MReportSim << endl;
    out << "Incidence-season-each simulation" << sep << this->SReportSim << endl;
    out << "Incidence-daily-overall" << sep << this->DReportSum << endl;
    out << "Incidence-monthly-overall" << sep << this->MReportSum << endl;
    out << "Incidence-season-overall" << sep << this->YReportSum << endl;
    out << "Outcomes file" << sep << this->PopReport << endl;
    out << "Add timestamp to output file names" << sep << this->addTimeStampToFileNames << endl;

    out << "Starting year" << sep << this->FYear << endl;
    out << "Starting month" << sep << this->FMonth << endl;
    out << "Number of months" << sep << this->NMonths << endl;
    out << "Number of strata" << sep << this->NStrata << endl;
    out << "Population size per stratum";
    for (int i = 1; i <= this->NStrata; i++) {
        //out << "Stratum " << i << sep << this->N[i - 1] << endl;
        out << sep << this->N[i - 1];
    }
    out << endl;
    out << "Total population" << sep << this->PopSize << endl;

    out << "Probabilities of X=1";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->probX1[i - 1];
    }
    out << endl;
    out << "Probabilities of U=1 given X=1";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->probU1_X1[i - 1];
    }
    out << endl;
    out << "Probabilities of U=1 given X=0";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->probU1_X0[i - 1];
    }
    out << endl;
    out << "Vaccination incremental coverage matrix for X=1 and U=1" << endl;
    for (int i = 0; i <= this->NMonths; i++) {
        out << i;
        for (int j = 1; j <= this->NStrata; j++) {
            out << sep << this->VacCov[1][1][i][j - 1];
        }
        out << endl;
    }
    out << "Vaccination incremental coverage multiplier for X=0";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->vacMultiplierX0[i - 1];
    }
    out << endl;
    out << "Vaccination incremental coverage multiplier for U=0";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->vacMultiplierU0[i - 1];
    }
    out << endl;
    out << "Vaccination incremental coverage matrix for X=0 and U=1" << endl;
    for (int i = 0; i <= this->NMonths; i++) {
        out << i;
        for (int j = 1; j <= this->NStrata; j++) {
            out << sep << this->VacCov[0][1][i][j - 1];
        }
        out << endl;
    }
    out << "Vaccination incremental coverage matrix for X=1 and U=0" << endl;
    for (int i = 0; i <= this->NMonths; i++) {
        out << i;
        for (int j = 1; j <= this->NStrata; j++) {
            out << sep << this->VacCov[1][0][i][j - 1];
        }
        out << endl;
    }
    out << "Vaccination incremental coverage matrix for X=0 and U=0" << endl;
    for (int i = 0; i <= this->NMonths; i++) {
        out << i;
        for (int j = 1; j <= this->NStrata; j++) {
            out << sep << this->VacCov[0][0][i][j - 1];
        }
        out << endl;
    }

    out << "Number of initial infected persons in the population" << sep << NInit;
    out << endl;

    // out << "Number of cases of illnesses:" << endl;
    // for(int i = 0; i < this->NMonths; i++)
    //{
    // for(int j = 1; j <= this->NStrata; j++)
    // {
    //  out << this->U[i][j-1] << " ";
    // }
    // out << endl;
    //}
    //out << endl;

    // out << "Vaccination effectiveness:" << endl;
    //for(int i = 0; i < this->NMonths; i++)
    //{
    //for(int j = 1; j <= this->NStrata; j++)
    //{
    // out << this->E[i][j-1] << " ";
    //}
    // out << endl;
    //}
    //out << endl;

    out << "Number of contacts per day";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->C[i - 1];
    }
    out << endl;

    out << "Distribution of contacts" << endl;
    for (int i = 0; i < this->NStrata; i++) {
        out << i+1;
        for (int j = 1; j <= this->NStrata; j++) {
            out << sep << this->Rho[i][j - 1];
        }
        out << endl;
    }

    out << "Length of latent period (days)";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->Latent[i - 1];
    }
    out << endl;

    out << "Length of infectious period (days)";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->Infectious[i - 1];
    }
    out << endl;

    out << "Probabilities that infected person becomes ill";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->Probinfill[i - 1];
    }
    out << endl;

    out << "Relative infectiousness if not ill";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->Rinfxill[i - 1];
    }
    out << endl;

    if (allOrNone) out << "Transmission probabilities to unprotected for X=1" << endl;
    else out << "Transmission probabilities to unvaccinated for X=1" << endl;
    for (int i = 0; i < this->NMonths; i++) {
        out << i+1;
        for (int j = 1; j <= this->NStrata; j++) {
            out << sep << this->Pai[1][i][j - 1];
        }
        out << endl;
    }

    out << "Transmission probabilities multiplier for unvaccinated with X=0";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->transProbMultiplierX0[i - 1];
    }

    out << endl;
    if (allOrNone) out << "Transmission probabilities for unprotected with X=0" << endl;
    else out << "Transmission probabilities for unvaccinated with X=0" << endl;
    for (int i = 0; i < this->NMonths; i++) {
        out << i+1;
        for (int j = 1; j <= this->NStrata; j++) {
            out << sep << this->Pai[0][i][j - 1];
        }
        out << endl;
    }

    out << "Vaccine efficacy for X=1" << endl;
    for (int i = 0; i < this->NMonths; i++) {
        out << i+1;
        for (int j = 1; j <= this->NStrata; j++) {
            out << sep << this->Phi[1][i][j - 1];
        }
        out << endl;
    }

    out << "Vaccine efficacy multiplier for X=0";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->vacEfficacyMultiplierX0[i - 1];
    }

    out << endl;
    out << "Vaccine efficacy for X=0" << endl;
    for (int i = 0; i < this->NMonths; i++) {
        out << i+1;
        for (int j = 1; j <= this->NStrata; j++) {
            out << sep << this->Phi[0][i][j - 1];
        }
        out << endl;
    }

    if (!allOrNone) {
        out << "Transmission probabilities to vaccinated X=1" << endl;
        for (int i = 0; i < this->NMonths; i++) {
            out << i+1;
            for (int j = 1; j <= this->NStrata; j++) {
                out << sep << this->Pai[1][i][j - 1] * (1 - this->Phi[1][i][j - 1]);
            }
            out << endl;
        }
        out << "Transmission probabilities to vaccinated X=0" << endl;
        for (int i = 0; i < this->NMonths; i++) {
            out << i+1;
            for (int j = 1; j <= this->NStrata; j++) {
                out << sep << this->Pai[0][i][j - 1] * (1 - this->Phi[0][i][j - 1]);
            }
            out << endl;
        }
    }

    out << "Length of NFARI episode (days)";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->LARI[i - 1];
    }
    out << endl;

    if (allOrNone) out << "Daily probabilities of onset of NFARI for unprotected with X=1" << endl;
    else out << "Daily probabilities of onset of NFARI for unvaccinated with X=1" << endl;
    for (int i=0; i < this->NMonths; i++) {
        out << i+1;
        for (int j = 1; j <= this->NStrata; j++) {
            out << sep << this->probARI[i][j - 1];
        }
        out << endl;
    }
    if (allOrNone) out << "Onset probabilities of NFARI multiplier for protected person";
    else out << "Onset probabilities of NFARI multiplier for vaccinated person";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->ariMultiplier_Vac[i - 1];
    }
    out << endl;

    if (allOrNone) out << "Daily probabilities of onset of NFARI for protected with X=1" << endl;
    else out << "Daily probabilities of onset of NFARI for vaccinated with X=1" << endl;
    for (int i=0; i < this->NMonths; i++) {
        out << i+1;
        for (int j = 1; j <= this->NStrata; j++) {
            out << sep << this->probARI[i][j - 1] * this->ariMultiplier_Vac[j - 1];
        }
        out << endl;
    }

    out << "Onset probabilities of NFARI multiplier for X=0";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->ariMultiplier_X0[i - 1];
    }
    out << endl;

    if (allOrNone) out << "Daily probabilities of onset of NFARI for unprotected with X=0" << endl;
    else out << "Daily probabilities of onset of NFARI for unvaccinated with X=0" << endl;
    for (int i=0; i < this->NMonths; i++) {
        out << i+1;
        for (int j = 1; j <= this->NStrata; j++) {
            out << sep << this->probARI[i][j - 1] * this->ariMultiplier_X0[j - 1];
        }
        out << endl;
    }

    if (allOrNone) out << "Daily probabilities of onset of NFARI for protected with X=0" << endl;
    else out << "Daily probabilities of onset of NFARI for vaccinated with X=0" << endl;
    for (int i=0; i < this->NMonths; i++) {
        out << i+1;
        for (int j = 1; j <= this->NStrata; j++) {
            out << sep << this->probARI[i][j - 1] * this->ariMultiplier_X0[j - 1] * this->ariMultiplier_Vac[j - 1];
        }
        out << endl;
    }

    if (allOrNone) out << "Probabilities of visit for a case of FARI for unprotected with X=1 and U=1";
    else out << "Probabilities of visit for a case of FARI for unvaccinated with X=1 and U=1";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->probVisit_FARI[i - 1];
    }
    out << endl;

    if (allOrNone) out << "Multiplier for probabilities of visit for protected FARI case";
    else out << "Multiplier for probabilities of visit for vaccinated FARI case";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->visitFARIMultiplier_Vac[i - 1];
    }
    out << endl;

    if (allOrNone) out << "Multiplier for probabilities of visit for FARI and X=0";
    else out << "Multiplier for probabilities of visit for FARI and X=0";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->visitFARIMultiplier_X0[i - 1];
    }
    out << endl;

    if (allOrNone) out << "Multiplier for probabilities of visit for FARI and U=0";
    else out << "Multiplier for probabilities of visit for FARI and U=0";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->visitFARIMultiplier_U0[i - 1];
    }
    out << endl;

    if (allOrNone) out << "Probabilities of visit for an unprotected case of FARI with X=1 and U=1";
    else out << "Probabilities of visit for an unvaccinated case of FARI with X=1 and U=1";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->probVisit_FARI[i - 1];
    }
    out << endl;

    if (allOrNone) out << "Probabilities of visit for an unprotected case of FARI with X=0 and U=1";
    else out << "Probabilities of visit for an unvaccinated case of FARI with X=0 and U=1";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << (this->probVisit_FARI[i - 1] * this->visitFARIMultiplier_X0[i - 1]);
    }
    out << endl;

    if (allOrNone) out << "Probabilities of visit for an unprotected case of FARI with X=1 and U=0";
    else out << "Probabilities of visit for an unvaccinated case of FARI with X=1 and U=0";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << (this->probVisit_FARI[i - 1] * this->visitFARIMultiplier_U0[i - 1]);
    }
    out << endl;

    if (allOrNone) out << "Probabilities of visit for an unprotected case of FARI with X=0 and U=0";
    else out << "Probabilities of visit for an unvaccinated case of FARI with X=0 and U=0";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << (this->probVisit_FARI[i - 1] * this->visitFARIMultiplier_X0[i - 1] * this->visitFARIMultiplier_U0[i - 1]);
    }
    out << endl;

    if (allOrNone) out << "Probabilities of visit for a protected case of FARI with X=1 and U=1";
    else out << "Probabilities of visit for a vaccinated case of FARI with X=1 and U=1";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << (this->probVisit_FARI[i - 1] * this->visitFARIMultiplier_Vac[i - 1]);
    }
    out << endl;

    if (allOrNone) out << "Probabilities of visit for a protected case of FARI with X=0 and U=1";
    else out << "Probabilities of visit for a vaccinated case of FARI with X=0 and U=1";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << (this->probVisit_FARI[i - 1] * this->visitFARIMultiplier_Vac[i - 1] * this->visitFARIMultiplier_X0[i - 1]);
    }
    out << endl;

    if (allOrNone) out << "Probabilities of visit for a protected case of FARI with X=1 and U=0";
    else out << "Probabilities of visit for a vaccinated case of FARI with X=1 and U=0";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << (this->probVisit_FARI[i - 1] * this->visitFARIMultiplier_Vac[i - 1] * this->visitFARIMultiplier_U0[i - 1]);
    }
    out << endl;

    if (allOrNone) out << "Probabilities of visit for a protected case of FARI with X=0 and U=0";
    else out << "Probabilities of visit for a vaccinated case of FARI with X=0 and U=0";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep
            << (this->probVisit_FARI[i - 1] * this->visitFARIMultiplier_Vac[i - 1] * this->visitFARIMultiplier_X0[i - 1] * this->visitFARIMultiplier_U0[i - 1]);
    }
    out << endl;

    if (allOrNone) out << "Probabilities of visit for a case of NFARI for unprotected with X=1 and U=1";
    else out << "Probabilities of visit for a case of NFARI for unvaccinated with X=1 and U=1";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->probVisit_NFARI[i - 1];
    }
    out << endl;

    if (allOrNone) out << "Multiplier for probabilities of visit for protected NFARI case";
    else out << "Multiplier for probabilities of visit for vaccinated NFARI case";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->visitNFARIMultiplier_Vac[i - 1];
    }
    out << endl;

    if (allOrNone) out << "Multiplier for probabilities of visit for NFARI and X=0";
    else out << "Multiplier for probabilities of visit for NFARI and X=0";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->visitNFARIMultiplier_X0[i - 1];
    }
    out << endl;

    if (allOrNone) out << "Multiplier for probabilities of visit for NFARI and U=0";
    else out << "Multiplier for probabilities of visit for NFARI and U=0";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->visitNFARIMultiplier_U0[i - 1];
    }
    out << endl;

    if (allOrNone) out << "Probabilities of visit for an unprotected case of NFARI with X=1 and U=1";
    else out << "Probabilities of visit for an unvaccinated case of NFARI with X=1 and U=1";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << this->probVisit_NFARI[i - 1];
    }
    out << endl;

    if (allOrNone) out << "Probabilities of visit for an unprotected case of NFARI with X=0 and U=1";
    else out << "Probabilities of visit for an unvaccinated case of NFARI with X=0 and U=1";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << (this->probVisit_NFARI[i - 1] * this->visitNFARIMultiplier_X0[i - 1]);
    }
    out << endl;

    if (allOrNone) out << "Probabilities of visit for an unprotected case of NFARI with X=1 and U=0";
    else out << "Probabilities of visit for an unvaccinated case of NFARI with X=1 and U=0";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << (this->probVisit_NFARI[i - 1] * this->visitNFARIMultiplier_U0[i - 1]);
    }
    out << endl;

    if (allOrNone) out << "Probabilities of visit for an unprotected case of NFARI with X=0 and U=0";
    else out << "Probabilities of visit for an unvaccinated case of NFARI with X=0 and U=0";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << (this->probVisit_NFARI[i - 1] * this->visitNFARIMultiplier_X0[i - 1] * this->visitNFARIMultiplier_U0[i - 1]);
    }
    out << endl;

    if (allOrNone) out << "Probabilities of visit for a protected case of NFARI with X=1 and U=1";
    else out << "Probabilities of visit for a vaccinated case of NFARI with X=1 and U=1";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << (this->probVisit_NFARI[i - 1] * this->visitNFARIMultiplier_Vac[i - 1]);
    }
    out << endl;

    if (allOrNone) out << "Probabilities of visit for a protected case of NFARI with X=0 and U=1";
    else out << "Probabilities of visit for a vaccinated case of NFARI with X=0 and U=1";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << (this->probVisit_NFARI[i - 1] * this->visitNFARIMultiplier_Vac[i - 1] * this->visitNFARIMultiplier_X0[i - 1]);
    }
    out << endl;

    if (allOrNone) out << "Probabilities of visit for a protected case of NFARI with X=1 and U=0";
    else out << "Probabilities of visit for a vaccinated case of NFARI with X=1 and U=0";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep << (this->probVisit_NFARI[i - 1] * this->visitNFARIMultiplier_Vac[i - 1] * this->visitNFARIMultiplier_U0[i - 1]);
    }
    out << endl;

    if (allOrNone) out << "Probabilities of visit for a protected case of NFARI with X=0 and U=0";
    else out << "Probabilities of visit for a vaccinated case of NFARI with X=0 and U=0";
    for (int i = 1; i <= this->NStrata; i++) {
        out << sep
            << (this->probVisit_NFARI[i - 1] * this->visitNFARIMultiplier_Vac[i - 1] * this->visitNFARIMultiplier_X0[i - 1] * this->visitNFARIMultiplier_U0[i - 1]);
    }
    out << endl;

}

void Params::cleanUp() {
  free(N);             //Number of people in each stratum
  free(probX1);      //Probability of X=1
  free(probU1_X1);   //Probability of U=1, given X=1
  free(probU1_X0);   //Probability of U=1, give X=0
  free(vacMultiplierX0);   //Vaccine multiplier for X=0
  free(vacMultiplierU0);   //Vaccine multiplier for U=0
  free(transProbMultiplierX0); //Transmission probability multiplier for X=0
  free(vacEfficacyMultiplierX0);   //Vaccine Efficacy multiplier for X=0
  free(Latent);        //Length of the latent period in days
  free(Infectious);    //Length of the infectious period in days
  free(Probinfill);  //Probability that an infectious person becomes ill
  free(ariMultiplier_Vac);   //Multiplier to probARI for vaccinated or protected humans
  free(ariMultiplier_X0);    //Multiplier to probARI for X=0
  free(LARI);          //Length of a non-influenza ARI
  free(probVisit_FARI);    //Probability of clinic visit for symptomatic influenza (FARI)
  free(probVisit_NFARI);   //Probability of clinic visit for Non-influenza ARI (NFARI)
  free(visitFARIMultiplier_Vac);   //Multiplier to FARI clinic visit probability for vaccinated or protected humans
  free(visitNFARIMultiplier_Vac);   //Multiplier to NFARI clinic visit probability for vaccinated or protected humans
  free(visitFARIMultiplier_X0);        //Multiplier to FARI clinic visit probability for X=0
  free(visitNFARIMultiplier_X0);        //Multiplier to NFARI clinic visit probability for X=0
  free(visitFARIMultiplier_U0);        //Multiplier to FARI clinic visit probability for U=0
  free(visitNFARIMultiplier_U0);        //Multiplier to NFARI clinic visit probability for U=0
  free(C);             //Number of contacts made by each person in stratum
  for (int i=0; i<NMonths; i++) {
    free(probARI[i]);
  }

  for (int i=0; i<2; i++) {
    for (int j=0; j<2; j++) {
      for (int k=0; k<(NMonths+1); k++) {
        free(VacCov[i][j][k]);
      }
      free(VacCov[i][j]);
    }
    free(VacCov[i]);
  }
  free(probARI);     //Daily probability of onset of a non-influenza ARI episode
  free(VacCov);     //Incremental vaccine coverage, X, U, Strata, Month
  for (int i=0; i<NStrata; i++) {
    free(Rho[i]);
  }
  free(Rho);        //Fraction of contacts between each strata
  free(Rinfxill);    //Relative infectiousness of a person who is infectious but not ill
  for (int j=0; j<2; j++) {
    for (int k=0; k<NMonths; k++) {
      free(Pai[j][k]);
      free(Phi[j][k]);
    }
    free(Pai[j]);
    free(Phi[j]);
  }
  free(Pai);        //Transmission probability for each X, strata during each month
  free(Phi);        //Per-contact vaccine efficacy against transmission for each X, strata and month
}
