#include "population.h"
#include <iostream>
#include <fstream>
#include <string>
#include <cstdlib>
//#include <cmath>
#include <vector>

#if WITH_R
#include <Rcpp.h>
#endif

using namespace std;
using std::vector;

void Population::Init(Params & params) {
    this->SimParams = params;

    // Create population
    this->PSize = this->SimParams.PopSize;
    this->People = (Person*) malloc(this->PSize * sizeof (Person));
    int p = 0;
    for (int k = 0; k < this->SimParams.NStrata; k++) {
        for (int i = 0; i < this->SimParams.N[k]; i++, p++) {
            this->People[p].ID = p;
            this->People[p].Stratum = k;
            this->People[p].influenzaStartDay = -1;
        }
    }
}

void Population::Reset() {
    // Reset everything
    stratifiedPeople.clear();
    stratifiedPeople.resize(this->SimParams.NStrata,
            vector<vector<vector<vector<Person*> > > >(2,
            vector<vector<vector<Person*> > > (2,
            vector<vector<Person*> > (2))));
    for (int p = 0; p < this->PSize; p++) {
        this->People[p].DayInf = 0;
        this->People[p].InitInf = false;
        this->People[p].NewInf = false;
        this->People[p].State = SUSCEPTIBLE;
        this->People[p].dayARI = 0;
        this->People[p].Vac = false;
        this->People[p].VacSuccessful = false;
        this->People[p].visit = false;
        if (this->Rnd.RandFloat() <= this->SimParams.probX1[this->People[p].Stratum]) {
            this->People[p].X = true;
            if (this->Rnd.RandFloat() <= this->SimParams.probU1_X1[this->People[p].Stratum])
                this->People[p].U = true;
            else this->People[p].U = false;
        } else {
            this->People[p].X = false;
            if (this->Rnd.RandFloat() <= this->SimParams.probU1_X0[this->People[p].Stratum])
                this->People[p].U = true;
            else this->People[p].U = false;
        }
        stratifiedPeople[this->People[p].Stratum][this->People[p].X][this->People[p].U][0].push_back(&this->People[p]);
    }

    // Randomly initialize infection
    int ninit = this->SimParams.NInit;
    if (ninit > this->PSize) {
        ninit = this->PSize;
    }
    int p;
    for (int n = 0; n < ninit; n++) {
        do {
            p = this->Rnd.RandInt(0, this->PSize);

        } while (this->People[p].InitInf == true);

        int Str = this->People[p].Stratum;
        this->People[p].DayInf = this->SimParams.Latent[Str] + 1;
        this->People[p].InitInf = true;

        if (this->Rnd.RandFloat() <= this->SimParams.Probinfill[Str]) {
            this->People[p].State = INFECTIOUSILL;
        } else {
            this->People[p].State = INFECTIOUSNILL;
        }
    }
}

void Population::testStratifiedPeople() {
    int sum = 0;
#if WITH_R
    Rcpp::Rcout << "\n\nTesting Population\n";
    Rcpp::Rcout << "i\tj\tk\tl\tSt\tX\tU\tvac" << endl;
#else
    cout << "\n\nTesting Population\n";
    cout << "i\tj\tk\tl\tSt\tX\tU\tvac" << endl;
#endif    
	for (int i = 0; i < SimParams.NStrata; i++) {
        for (int j = 0; j < 2; j++) {
            for (int k = 0; k < 2; k++) {
                for (int l = 0; l < 2; l++) {
                    for (int m = 0; m < stratifiedPeople[i][j][k][l].size(); m++) {
                        Person * pr = stratifiedPeople[i][j][k][l][m];
#if WITH_R
                        Rcpp::Rcout << i << "\t" << j << "\t" << k << "\t" << l
#else
                        cout << i << "\t" << j << "\t" << k << "\t" << l
#endif
                                << "\t" << pr->Stratum << "\t" << pr->X << "\t" << pr->U << "\t" << pr->Vac << endl;
                        sum += 1;
                    }
                }
            }
        }
    }
#if WITH_R
    Rcpp::Rcout << "\nTotal Population: " << sum << endl;
#else
    cout << "\nTotal Population: " << sum << endl;
#endif
}

void Population::Vaccinate(int s, int m, int month, int year, ofstream & vreport, VaccinationReport *vacReport) {
    // Randomly vaccinate population
    for (int k = 0; k< this->SimParams.NStrata; k++) {
        for (int i = 0; i < 2; i++) {
            for (int j = 0; j < 2; j++) {
                if (this->SimParams.VReport) {
                    vreport << s << ","
                            << m << ","
                            << month << ","
                            << year << ","
                            << k + 1 << ","
                            << i << ","
                            << j << ",";
#if WITH_R
                    vacReport->s.push_back(s);
                    vacReport->mm.push_back(m);
                    vacReport->month.push_back(month);
                    vacReport->year.push_back(year);
                    vacReport->k.push_back(k+1);
                    vacReport->x.push_back(i);
                    vacReport->u.push_back(j);
#endif                            
                }
                vector<int> newVaccinated;
                int nvac = (int) ((float) this->SimParams.VacCov[i][j][m - 1][k] *
                        (stratifiedPeople[k][i][j][0].size() + stratifiedPeople[k][i][j][1].size()));
                while (nvac > 0) {
                    if (stratifiedPeople[k][i][j][0].size() == 0) {
#if WITH_R
                        Rcpp::Rcout << "\nWarning! Everybody in stratum " << k + 1 << " , with X = " << i << " , U = " << j << " is already vaccinated." << endl;
#else
                        cout << "\nWarning! Everybody in stratum " << k + 1 << " , with X = " << i << " , U = " << j << " is already vaccinated." << endl;
#endif
                    } else {
#if WITH_R
                        //Rcpp::Rcout << "\nBefore total size: " << stratifiedPeople[k][i][j][0].size() + stratifiedPeople[k][i][j][1].size();
                        //Rcpp::Rcout << " 0: " << stratifiedPeople[k][i][j][0].size() << " 1: " << stratifiedPeople[k][i][j][1].size();
#else                      
                        //cout << "\nBefore total size: " << stratifiedPeople[k][i][j][0].size() + stratifiedPeople[k][i][j][1].size();
                        //cout << " 0: " << stratifiedPeople[k][i][j][0].size() << " 1: " << stratifiedPeople[k][i][j][1].size();
#endif
                        int p = this->Rnd.RandInt(0, stratifiedPeople[k][i][j][0].size());
                        Person * pr = stratifiedPeople[k][i][j][0][p];
                        pr->Vac = true;
                        stratifiedPeople[k][i][j][1].push_back(pr);
                        stratifiedPeople[k][i][j][0].erase(stratifiedPeople[k][i][j][0].begin() + p);
                        if (this->SimParams.allOrNone && this->Rnd.RandFloat() <= this->SimParams.Phi[i][m - 1][k]) {
                            pr->VacSuccessful = true;
                        }
                        if (this->SimParams.VReport) {
                            newVaccinated.push_back(pr->ID + 1);
                            //vreport << pr->ID + 1 << " ";
                        }
#if WITH_R
                        //Rcpp::Rcout << "\nAfter total size: " << stratifiedPeople[k][i][j][0].size() + stratifiedPeople[k][i][j][1].size();
                        //Rcpp::Rcout << " 0: " << stratifiedPeople[k][i][j][0].size() << " 1: " << stratifiedPeople[k][i][j][1].size();
#else                  
                        //cout << "\nAfter total size: " << stratifiedPeople[k][i][j][0].size() + stratifiedPeople[k][i][j][1].size();
                        //cout << " 0: " << stratifiedPeople[k][i][j][0].size() << " 1: " << stratifiedPeople[k][i][j][1].size();
#endif
                    }
                    nvac--;
                }
                if (this->SimParams.VReport) {
                    vreport << newVaccinated.size() << ",";
#if WITH_R
                    vacReport->number.push_back(newVaccinated.size());
#endif
                    for (auto nVac : newVaccinated) {
                        vreport << nVac << " ";
#if WITH_R
                        vacReport->newly.push_back(nVac);
#endif                        
                    }
                    vreport << endl;
                }
            }
        }
    }
}

//vector for daily simall
vector<int> dailyk;
vector<vector<int > > dailyh; //8 variables (previously 6, added variables for ari)
vector<vector<vector<int > > >dailyd; //day
vector<vector<int > > shk; //seasonly sum
vector<vector<vector<vector<int > > > > dailym; //month
vector<vector<vector<vector<vector<int > > > > >dailyy; //year


//vector for montlysum and yearly sum
vector<vector<int > > var;
vector<vector<vector<int > > > monthly;
vector<vector<vector<int > > > seasonly;

//global vector for population
int dvac;
//vector<vector<int >> pop_sim;
vector<vector<vector<int > > > pop;

#if WITH_R
void Population::Spread(int d, int day, int m, int month, int year, int s, int NDays, int NMonths, ofstream & dreport, ofstream & ireport,
        ofstream & preport, ofstream & creport, ofstream &mreport, ofstream &sreport, ofstream &dreports, ofstream &mreports, ofstream &yreport, ofstream &popreport, 
        DetailReport* detReport, DailySimReport* daisimReport, DailyReport* daiReport, PrevelanceReport* preReport, ContactReport* conReport, MonthlySimReport * monsimReport, SeasonSimReport *seasimReport, MonthlyReport *monReport, SeasonReport *seaReport, PopulationReport *populationReport)
#else
void Population::Spread(int d, int day, int m, int month, int year, int s, int NDays, int NMonths, ofstream & dreport, ofstream & ireport,
        ofstream & preport, ofstream & creport, ofstream &mreport, ofstream &sreport, ofstream &dreports, ofstream &mreports, ofstream &yreport, ofstream &popreport)
#endif
{
    if (year == this->SimParams.FYear && month == this->SimParams.FMonth && day == 1) {
        pop.clear();
        //pop.resize(this->SimParams.PopSize);
        //pop.resize(this->SimParams.NSim, vector<vector<int > >(this->SimParams.PopSize));
        //population file initial
        for (int v = 0; v<this->SimParams.PopSize; v++) {
            vector<vector<int > > popvar;
            vector<int > baseRecord(13);
            baseRecord[0] = s; //sim
            baseRecord[1] = v + 1; //ID
            baseRecord[2] = this->People[v].Stratum + 1; //strata
            baseRecord[3] = this->People[v].X; //X
            baseRecord[4] = this->People[v].U; //U
            baseRecord[5] = this->People[v].InitInf; //InitInf
            baseRecord[6] = 0; //dvac
            if (this->SimParams.allOrNone) {
                baseRecord[7] = 0; //vac successful
            } else baseRecord[7] = 9;
            baseRecord[8] = 999;
            baseRecord[9] = 999;
            baseRecord[10] = 999;
            baseRecord[11] = 0;
            baseRecord[12] = 0;
            popvar.push_back(baseRecord);
            pop.push_back(popvar);
        }
    }

    //initialize of dailyk
    if (s == 1 && year == this->SimParams.FYear && month == this->SimParams.FMonth && day == 1) {
        vector<int> dailyk(this->SimParams.NStrata); //strata
        for (int h = 0; h < 8; h++) {
            //initialize var
            var.push_back(dailyk);
            dailyh.push_back(dailyk);

        }
        for (int d = 0; d < NDays; d++)
            dailyd.push_back(dailyh);

        for (int m = 0; m < 12; m++)
            dailym.push_back(dailyd);
        //year most is 2+NMonths/12
        for (int y = 0; y < 2 + NMonths / 12; y++)
            dailyy.push_back(dailym);
        //initialize of season
        for (int s = 0; s<this->SimParams.NSim; s++)
            seasonly.push_back(dailyh);


    }

    //initialize of var
    if (day == 1) {
        for (int i = 0; i < 8; i++)
            for (int j = 0; j < this->SimParams.NStrata; j++)
                var[i][j] = 0;
    }
    // Transitions at the start of the day
    if (d != 1) {
        float visitP;
        for (int i = 0; i < this->SimParams.PopSize; i++) {
            this->People[i].visit = false;
            if (this->People[i].State == SUSCEPTIBLE && this->People[i].NewInf == true) {
                //cout<< "\n" << this->People[i].ID << " exposed!";
                this->People[i].State = EXPOSED;
                this->People[i].DayInf = 1;
                this->People[i].NewInf = false;
            } else if (this->People[i].State == EXPOSED || this->People[i].State == INFECTIOUSNILL || this->People[i].State == INFECTIOUSILL) {
                this->People[i].DayInf++;
            }
            int Str = this->People[i].Stratum;
            if (this->People[i].State == EXPOSED && this->People[i].DayInf > this->SimParams.Latent[Str]) {
                if (this->Rnd.RandFloat() <= this->SimParams.Probinfill[Str]) {
                    this->People[i].State = INFECTIOUSILL;
                    this->People[i].dayARI = 0;

                    visitP = this->SimParams.probVisit_FARI[Str];
                    if (this->SimParams.allOrNone) {
                        visitP = visitP * (this->People[i].VacSuccessful ? this->SimParams.visitFARIMultiplier_Vac[Str] : 1);
                    } else {
                        visitP = visitP * (this->People[i].Vac ? this->SimParams.visitFARIMultiplier_Vac[Str] : 1);
                    }
                    visitP = visitP * ((this->People[i].X == 0) ? this->SimParams.visitFARIMultiplier_X0[Str] : 1);
                    visitP = visitP * ((this->People[i].U == 0) ? this->SimParams.visitFARIMultiplier_U0[Str] : 1);
                    if (this->Rnd.RandFloat() <= visitP) {
                        this->People[i].visit = true;
                    }
                } else {
                    this->People[i].State = INFECTIOUSNILL;
                }
                this->People[i].influenzaStartDay = d;
            }
            if ((this->People[i].State == INFECTIOUSNILL || this->People[i].State == INFECTIOUSILL) && this->People[i].DayInf > (this->SimParams.Latent[Str] + this->SimParams.Infectious[Str])) {
                this->People[i].State = RECOVERED;
                this->People[i].DayInf = 0;
            }
            if (this->People[i].dayARI > 0) {
                if (this->People[i].dayARI >= this->SimParams.LARI[this->People[i].Stratum]) {
                    this->People[i].dayARI = 0;
                }
                else {
                    this->People[i].dayARI += 1;
                }
            }
            //if (this->People[i].State == INFECTIOUSNILL || this->People[i].influenzaStartDay < d) {
            //    this->People[i].visit = false;
            //}
            //if (this->People[i].dayARI!=0 && this->People[i].ariStartDay < d) {
            //    this->People[i].visit = false;
            //}
        }
    }

    //data for population



    //cout << "\tInitial Transitions done!" << endl;

    // Calculate prevalence of infectious persons



    int* h2v = (int*) malloc(this->SimParams.NStrata * sizeof (int));
    int* h2V = (int*) malloc(this->SimParams.NStrata * sizeof (int));
    int* h3v = (int*) malloc(this->SimParams.NStrata * sizeof (int));
    int* h3V = (int*) malloc(this->SimParams.NStrata * sizeof (int));
    int* Pr2V = (int*) malloc(this->SimParams.NStrata * sizeof (int));
    int* Pr2v = (int*) malloc(this->SimParams.NStrata * sizeof (int));
    int* Pr3V = (int*) malloc(this->SimParams.NStrata * sizeof (int));
    int* Pr3v = (int*) malloc(this->SimParams.NStrata * sizeof (int));
    float* H2 = (float*) malloc(this->SimParams.NStrata * sizeof (float)); // Prevalence of state 2
    float* H3 = (float*) malloc(this->SimParams.NStrata * sizeof (float)); // Prevalence of state 3
    int* hARIv = (int*) malloc(this->SimParams.NStrata * sizeof (int));
    int* hARIV = (int*) malloc(this->SimParams.NStrata * sizeof (int));
    int offset = 0;
    for (int k = 0; k < this->SimParams.NStrata; k++) {
        h2v[k] = 0;
        h2V[k] = 0;
        h3v[k] = 0;
        h3V[k] = 0;
        hARIv[k] = 0;
        hARIV[k] = 0;
        Pr2V[k] = 0;
        Pr2v[k] = 0;
        Pr3V[k] = 0;
        Pr3v[k] = 0;
        for (int i = 0; i < this->SimParams.N[k]; i++) {
            int Str = this->People[i + offset].Stratum;
            if (this->People[i + offset].State == INFECTIOUSNILL && this->People[i + offset].Vac == true) {
                Pr2V[k]++;
                if (this->People[i + offset].DayInf == this->SimParams.Latent[Str] + 1) {
                    h2V[k]++;
                }
            } else if (this->People[i + offset].State == INFECTIOUSNILL && this->People[i + offset].Vac == false) {
                Pr2v[k]++;
                if (this->People[i + offset].DayInf == this->SimParams.Latent[Str] + 1) {
                    h2v[k]++;
                }
            } else if (this->People[i + offset].State == INFECTIOUSILL && this->People[i + offset].Vac == true) {
                Pr3V[k]++;
                if (this->People[i + offset].DayInf == this->SimParams.Latent[Str] + 1) {
                    h3V[k]++;
                }
            } else if (this->People[i + offset].State == INFECTIOUSILL && this->People[i + offset].Vac == false) {
                Pr3v[k]++;
                if (this->People[i + offset].DayInf == this->SimParams.Latent[Str] + 1) {
                    h3v[k]++;
                }
            }

            if (this->People[i + offset].dayARI > 0 && this->People[i + offset].ariStartDay == d - 1) {
                if (this->People[i + offset].Vac == true) {
                    hARIV[k]++;
                } else {
                    hARIv[k]++;
                }
            }

        }
        offset += this->SimParams.N[k];
        H2[k] = (float) (Pr2v[k] + Pr2V[k]) / (float) this->SimParams.N[k];
        H3[k] = (float) (Pr3v[k] + Pr3V[k]) / (float) this->SimParams.N[k];
    }

    // Calculate cross-strata infection probability
    float** CSIPV0H2 = (float**) malloc(2 * sizeof (float*)); //Cross-strata for non-vaccinated person in state 2
    float** CSIPV0H3 = (float**) malloc(2 * sizeof (float*)); //Cross-strata for non-vaccinated person in state 3
    float** CSIPV1H2 = (float**) malloc(2 * sizeof (float*)); //Cross-strata for vaccinated person in state 2
    float** CSIPV1H3 = (float**) malloc(2 * sizeof (float*)); //Cross-strata for vaccinated person in state 3
    for (int indX = 0; indX < 2; indX++) {
        CSIPV0H2[indX] = (float*) malloc(this->SimParams.NStrata * sizeof (float));
        CSIPV0H3[indX] = (float*) malloc(this->SimParams.NStrata * sizeof (float));
        CSIPV1H2[indX] = (float*) malloc(this->SimParams.NStrata * sizeof (float));
        CSIPV1H3[indX] = (float*) malloc(this->SimParams.NStrata * sizeof (float));
    }

    for (int k = 0; k < this->SimParams.NStrata; k++) {
        for (int indX = 0; indX < 2; indX++) {
            CSIPV0H2[indX][k] = this->SimParams.Pai[indX][m - 1][k] * this->SimParams.Rinfxill[k];
            CSIPV0H3[indX][k] = this->SimParams.Pai[indX][m - 1][k];
            CSIPV1H2[indX][k] = this->SimParams.Pai[indX][m - 1][k] * (1.0 - this->SimParams.Phi[indX][m - 1][k]) * this->SimParams.Rinfxill[k];
            CSIPV1H3[indX][k] = this->SimParams.Pai[indX][m - 1][k] * (1.0 - this->SimParams.Phi[indX][m - 1][k]);
        }
    }

    float*** CSIPV0 = (float***) malloc(2 * sizeof (float*)); //Cross-strata for non-vaccinated person
    float*** CSIPV1 = (float***) malloc(2 * sizeof (float*)); //Cross-strata for vaccinated person
    for (int indX = 0; indX < 2; indX++) {
        CSIPV0[indX] = (float**) malloc(this->SimParams.NStrata * sizeof (float*));
        CSIPV1[indX] = (float**) malloc(this->SimParams.NStrata * sizeof (float*));
        for (int i = 0; i < this->SimParams.NStrata; i++) {
            CSIPV0[indX][i] = (float*) malloc(this->SimParams.NStrata * sizeof (float));
            CSIPV1[indX][i] = (float*) malloc(this->SimParams.NStrata * sizeof (float));
        }
    }

    for (int indX = 0; indX < 2; indX++) {
        for (int k = 0; k < this->SimParams.NStrata; k++) {
            for (int l = 0; l < this->SimParams.NStrata; l++) {
                CSIPV0[indX][k][l] = CSIPV0H2[indX][k] * H2[l] + CSIPV0H3[indX][k] * H3[l];
                CSIPV1[indX][k][l] = CSIPV1H2[indX][k] * H2[l] + CSIPV1H3[indX][k] * H3[l];
            }
        }
    }

    // Calculate per-contact infection probability for each strata
    float** PCIPV0 = (float**) malloc(2 * sizeof (float*)); // Per-contact infection probability for non-vaccinated person
    float** PCIPV1 = (float**) malloc(2 * sizeof (float*)); // Per-contact infection probability for vaccinated person
    for (int indX = 0; indX < 2; indX++) {
        PCIPV0[indX] = (float*) malloc(this->SimParams.NStrata * sizeof (float));
        PCIPV1[indX] = (float*) malloc(this->SimParams.NStrata * sizeof (float));
    }

    for (int indX = 0; indX < 2; indX++) {
        for (int k = 0; k < this->SimParams.NStrata; k++) {
            PCIPV0[indX][k] = 0.0;
            PCIPV1[indX][k] = 0.0;
            for (int l = 0; l < this->SimParams.NStrata; l++) {
                //if(s == 1 && d == 1)
                //cout << k << " " << l << " " << CSIPV0[k][l] * this->SimParams.Rho[k][l] << " " << CSIPV1[k][l] * this->SimParams.Rho[k][l] << endl;
                // cout << k << " " << l << " " << this->SimParams.Rho[k][l] << endl;
                PCIPV0[indX][k] += (CSIPV0[indX][k][l] * this->SimParams.Rho[k][l]);
                PCIPV1[indX][k] += (CSIPV1[indX][k][l] * this->SimParams.Rho[k][l]);
            }
        }
    }

    // Calculate overall infection probability for each person
    float** IPV0 = (float**) malloc(2 * sizeof (float*)); // Per-contact infection probability for non-vaccinated person
    float** IPV1 = (float**) malloc(2 * sizeof (float*)); // Per-contact infection probability for vaccinated person
    for (int indX = 0; indX < 2; indX++) {
        IPV0[indX] = (float*) malloc(this->SimParams.NStrata * sizeof (float));
        IPV1[indX] = (float*) malloc(this->SimParams.NStrata * sizeof (float));
    }
    for (int indX = 0; indX < 2; indX++) {
        for (int k = 0; k < this->SimParams.NStrata; k++) {
            float pow0 = 1.0;
            float pow1 = 1.0;
            for (int p = 0; p < this->SimParams.C[k]; p++) {
                pow0 *= (1.0 - PCIPV0[indX][k]);
                pow1 *= (1.0 - PCIPV1[indX][k]);
            }
            IPV0[indX][k] = 1.0 - pow0;
            IPV1[indX][k] = 1.0 - pow1;
        }
    }

    // Calculate infection and ARI for each person
    for (int i = 0; i < this->SimParams.PopSize; i++) {
        float InfP;
        float R;
        float ariP;
        float visitP;
        int Str = this->People[i].Stratum;
        if (this->People[i].State == SUSCEPTIBLE && this->People[i].NewInf == false) {
            if (this->SimParams.allOrNone) {
                InfP = this->People[i].VacSuccessful ? 0 : IPV0[this->People[i].X][Str];
            } else {
                InfP = this->People[i].Vac ? IPV1[this->People[i].X][Str] : IPV0[this->People[i].X][Str];
            }
            R = this->Rnd.RandFloat();
            if (R <= InfP) {
                this->People[i].NewInf = true;
            }
            if (this->SimParams.CReport) {
                creport << s << ","
                << d << ","
                << i + 1 << ","
                << (this->People[i].Stratum + 1) << ","
                << this->People[i].X << ","
                << this->People[i].U << ","
                << this->People[i].Vac << ",";
                if (this->SimParams.allOrNone) {
                    creport << this->People[i].VacSuccessful << ",";
                } else creport << 9 << ",";
                creport << InfP << ","
                << R << ","
                << this->People[i].NewInf << endl;
#if WITH_R
                conReport->s.push_back(s);
                conReport->d.push_back(d);
                conReport->ii.push_back(i+1);
                conReport->stratum.push_back(this->People[i].Stratum + 1);
                conReport->x.push_back(this->People[i].X);
                conReport->u.push_back(this->People[i].U);
                conReport->vac.push_back(this->People[i].Vac);
                if (this->SimParams.allOrNone) {
                    conReport->prot.push_back(this->People[i].VacSuccessful);
                }else conReport->prot.push_back(9);
                conReport->pINF.push_back(InfP);
                conReport->RANDOM.push_back(R);
                conReport->newInf.push_back(this->People[i].NewInf);
#endif
            }
        }
        if (this->People[i].State != INFECTIOUSILL && this->People[i].dayARI == 0) {
            R = this->Rnd.RandFloat();
            ariP = this->SimParams.probARI[m - 1][Str];
            if (this->SimParams.allOrNone) {
                ariP = ariP * (this->People[i].VacSuccessful ? this->SimParams.ariMultiplier_Vac[Str] : 1);
            } else {
                ariP = ariP * (this->People[i].Vac ? this->SimParams.ariMultiplier_Vac[Str] : 1);
            }
            ariP = ariP * ((this->People[i].X == 0) ? this->SimParams.ariMultiplier_X0[Str] : 1);
            if (R <= ariP) {
                this->People[i].dayARI = 1;
                this->People[i].ariStartDay = d;

                visitP = this->SimParams.probVisit_NFARI[Str];
                if (this->SimParams.allOrNone) {
                    visitP = visitP * (this->People[i].VacSuccessful ? this->SimParams.visitNFARIMultiplier_Vac[Str] : 1);
                } else {
                    visitP = visitP * (this->People[i].Vac ? this->SimParams.visitNFARIMultiplier_Vac[Str] : 1);
                }
                visitP = visitP * ((this->People[i].X == 0) ? this->SimParams.visitNFARIMultiplier_X0[Str] : 1);
                visitP = visitP * ((this->People[i].U == 0) ? this->SimParams.visitNFARIMultiplier_U0[Str] : 1);
                if (this->Rnd.RandFloat() <= visitP) {
                    this->People[i].visit = true;
                }
            }
        }
    }

    //incidence report each sim,and all sim
    if (this->SimParams.IReportDaily || this->SimParams.DReportSum) {
        //initialize the dailyk vector only once


        for (int k = 0; k < this->SimParams.NStrata; k++) {
            VacPeople[k] = 0;
            UnVacPeople[k] = 0;
            hARIVDaily[k] = 0;
            hARIvDaily[k] = 0;
            for (int i = 0; i < this->SimParams.PopSize; i++) {
                if (this->People[i].Stratum == k && this->People[i].Vac == true) {
                    VacPeople[k]++;
                }
                if (this->People[i].Stratum == k && this->People[i].Vac == false) {
                    UnVacPeople[k]++;
                }
                if (this->People[i].Stratum == k && this->People[i].dayARI == 1) {
                    if (this->People[i].Vac) {
                        hARIVDaily[k]++;
                    }
                    else hARIvDaily[k]++;
                }
            }

            //if need to output dailysim
            if (this->SimParams.IReportDaily) {
                int v = 0;
                ireport << s << ","
                        << d << ","
                        << day << ","
                        << month << ","
                        << year << ","
                        << k + 1 << ","
                        << v << ","
                        << UnVacPeople[k] << ","
                        << h2v[k] << ","
                        << h2v[k] / 1.0 / UnVacPeople[k] << ","
                        << h3v[k] << ","
                        << h3v[k] / 1.0 / UnVacPeople[k] << ","
                        << h2v[k] + h3v[k] << ","
                        << (h2v[k] + h3v[k]) / 1.0 / UnVacPeople[k] << ","
                        << hARIvDaily[k] << ","
                        << hARIvDaily[k] / 1.0 / UnVacPeople[k] << endl;
#if WITH_R
                daisimReport->s.push_back(s);
                daisimReport->d.push_back(d);
                daisimReport->day.push_back(day);
                daisimReport->month.push_back(month);
                daisimReport->year.push_back(year);
                daisimReport->stratum.push_back(k+1);
                daisimReport->v.push_back(v);
                daisimReport->nn.push_back(UnVacPeople[k]);
                daisimReport->inc2.push_back(h2v[k]);
                daisimReport->rate2.push_back(h2v[k] / 1.0 / UnVacPeople[k]);
                daisimReport->inc3.push_back(h3v[k]);
                daisimReport->rate3.push_back(h3v[k] / 1.0 / UnVacPeople[k]);
                daisimReport->incT.push_back(h2v[k] + h3v[k]);
                daisimReport->rateT.push_back((h2v[k] + h3v[k]) / 1.0 / UnVacPeople[k]);
                daisimReport->incNFARI.push_back(hARIvDaily[k]);
                daisimReport->rateNFARI.push_back(hARIvDaily[k] / 1.0 / UnVacPeople[k]);
#endif
                v = 1;
                ireport << s << ","
                        << d << ","
                        << day << ","
                        << month << ","
                        << year << ","
                        << k + 1 << ","
                        << v << ","
                        << VacPeople[k] << ","
                        << h2V[k] << ","
                        << h2V[k] / 1.0 / VacPeople[k] << ","
                        << h3V[k] << ","
                        << h3V[k] / 1.0 / VacPeople[k] << ","
                        << h2V[k] + h3V[k] << ","
                        << (h2V[k] + h3V[k]) / 1.0 / VacPeople[k] << ","
                        << hARIVDaily[k] << ","
                        << hARIVDaily[k] / 1.0 / VacPeople[k] << endl;
#if WITH_R
                daisimReport->s.push_back(s);
                daisimReport->d.push_back(d);
                daisimReport->day.push_back(day);
                daisimReport->month.push_back(month);
                daisimReport->year.push_back(year);
                daisimReport->stratum.push_back(k+1);
                daisimReport->v.push_back(v);
                daisimReport->nn.push_back(VacPeople[k]);
                daisimReport->inc2.push_back(h2V[k]);
                daisimReport->rate2.push_back(h2V[k] / 1.0 / VacPeople[k]);
                daisimReport->inc3.push_back(h3V[k]);
                daisimReport->rate3.push_back(h3V[k] / 1.0 / VacPeople[k]);
                daisimReport->incT.push_back(h2V[k] + h3V[k]);
                daisimReport->rateT.push_back((h2V[k] + h3V[k]) / 1.0 / VacPeople[k]);
                daisimReport->incNFARI.push_back(hARIVDaily[k]);
                daisimReport->rateNFARI.push_back(hARIVDaily[k] / 1.0 / VacPeople[k]);
#endif
            }

        }


        //save daily data for each sim, if it will be output


        for (int h = 0; h < 8; h++) {
            for (int k = 0; k < this->SimParams.NStrata; k++) {
                if (h == 0) {
                    dailyy[year - this->SimParams.FYear][m - 1][day - 1][h][k] += h2V[k];

                }
                if (h == 1) {
                    dailyy[year - this->SimParams.FYear][m - 1][day - 1][h][k] += h3V[k]; //get data for daily-overall

                }
                if (h == 2) {
                    dailyy[year - this->SimParams.FYear][m - 1][day - 1][h][k] += h2v[k];

                }
                if (h == 3) {
                    dailyy[year - this->SimParams.FYear][m - 1][day - 1][h][k] += h3v[k];


                }
                if (h == 4) {

                    dailyy[year - this->SimParams.FYear][m - 1][day - 1][h][k] += VacPeople[k];


                }
                if (h == 5) {
                    dailyy[year - this->SimParams.FYear][m - 1][day - 1][h][k] += UnVacPeople[k];


                }
                if (h == 6) {
                    dailyy[year - this->SimParams.FYear][m - 1][day - 1][h][k] += hARIV[k];


                }
                if (h == 7) {
                    dailyy[year - this->SimParams.FYear][m - 1][day - 1][h][k] += hARIv[k];


                }

            }

        }


        //output season data overall;
        if (s == this->SimParams.NSim && this->SimParams.DReportSum) {

            for (int k = 0; k < this->SimParams.NStrata; k++) {

                int v = 0;
                dreports << d << ","
                        << day << ","
                        << month << ","
                        << year << ","
                        << k + 1 << ","
                        << v << ","
                        << dailyy[year - this->SimParams.FYear][m - 1][day - 1][5][k] / 1.0 / this->SimParams.NSim << "," //UNVAC
                        << dailyy[year - this->SimParams.FYear][m - 1][day - 1][2][k] / 1.0 / this->SimParams.NSim << ","//h2v
                        << dailyy[year - this->SimParams.FYear][m - 1][day - 1][2][k] / 1.0 / dailyy[year - this->SimParams.FYear][m - 1][day - 1][5][k] << ","//IR2
                        << dailyy[year - this->SimParams.FYear][m - 1][day - 1][3][k] / 1.0 / this->SimParams.NSim << ","//h3v
                        << dailyy[year - this->SimParams.FYear][m - 1][day - 1][3][k] / 1.0 / dailyy[year - this->SimParams.FYear][m - 1][day - 1][5][k] << ","//IR3
                        << (dailyy[year - this->SimParams.FYear][m - 1][day - 1][2][k] + dailyy[year - this->SimParams.FYear][m - 1][day - 1][3][k]) / 1.0 / this->SimParams.NSim << ","//h2v+h3v
                        << (dailyy[year - this->SimParams.FYear][m - 1][day - 1][2][k] + dailyy[year - this->SimParams.FYear][m - 1][day - 1][3][k]) / 1.0 / dailyy[year - this->SimParams.FYear][m - 1][day - 1][5][k] << "," //IR2+IR3
                        << dailyy[year - this->SimParams.FYear][m - 1][day - 1][7][k] / 1.0 / this->SimParams.NSim << ","//hARIv
                        << dailyy[year - this->SimParams.FYear][m - 1][day - 1][7][k] / 1.0 / dailyy[year - this->SimParams.FYear][m - 1][day - 1][5][k] << endl;
#if WITH_R
                daiReport->d.push_back(d);
                daiReport->day.push_back(day);
                daiReport->month.push_back(month);
                daiReport->year.push_back(year);
                daiReport->stratum.push_back(k+1);
                daiReport->v.push_back(v);
                daiReport->nn.push_back(dailyy[year - this->SimParams.FYear][m - 1][day - 1][5][k] / 1.0 / this->SimParams.NSim);
                daiReport->inc2.push_back(dailyy[year - this->SimParams.FYear][m - 1][day - 1][2][k] / 1.0 / this->SimParams.NSim);
                daiReport->rate2.push_back(dailyy[year - this->SimParams.FYear][m - 1][day - 1][2][k] / 1.0 / dailyy[year - this->SimParams.FYear][m - 1][day - 1][5][k]);
                daiReport->inc3.push_back(dailyy[year - this->SimParams.FYear][m - 1][day - 1][3][k] / 1.0 / this->SimParams.NSim);
                daiReport->rate3.push_back(dailyy[year - this->SimParams.FYear][m - 1][day - 1][3][k] / 1.0 / dailyy[year - this->SimParams.FYear][m - 1][day - 1][5][k]);
                daiReport->incT.push_back((dailyy[year - this->SimParams.FYear][m - 1][day - 1][2][k] + dailyy[year - this->SimParams.FYear][m - 1][day - 1][3][k]) / 1.0 / this->SimParams.NSim);
                daiReport->rateT.push_back((dailyy[year - this->SimParams.FYear][m - 1][day - 1][2][k] + dailyy[year - this->SimParams.FYear][m - 1][day - 1][3][k]) / 1.0 / dailyy[year - this->SimParams.FYear][m - 1][day - 1][5][k]);
                daiReport->incNFARI.push_back(dailyy[year - this->SimParams.FYear][m - 1][day - 1][7][k] / 1.0 / this->SimParams.NSim);
                daiReport->rateNFARI.push_back(dailyy[year - this->SimParams.FYear][m - 1][day - 1][7][k] / 1.0 / dailyy[year - this->SimParams.FYear][m - 1][day - 1][5][k]);
#endif
                v = 1;
                dreports
                        << d << ","
                        << day << ","
                        << month << ","
                        << year << ","
                        << k + 1 << ","
                        << v << ","
                        << dailyy[year - this->SimParams.FYear][m - 1][day - 1][4][k] / 1.0 / this->SimParams.NSim << "," //VAC
                        << dailyy[year - this->SimParams.FYear][m - 1][day - 1][0][k] / 1.0 / this->SimParams.NSim << ","//h2V
                        << dailyy[year - this->SimParams.FYear][m - 1][day - 1][0][k] / 1.0 / dailyy[year - this->SimParams.FYear][m - 1][day - 1][4][k] << ","//IR2
                        << dailyy[year - this->SimParams.FYear][m - 1][day - 1][1][k] / 1.0 / this->SimParams.NSim << ","//h3V
                        << dailyy[year - this->SimParams.FYear][m - 1][day - 1][1][k] / 1.0 / dailyy[year - this->SimParams.FYear][m - 1][day - 1][4][k] << ","//IR3
                        << (dailyy[year - this->SimParams.FYear][m - 1][day - 1][0][k] + dailyy[year - this->SimParams.FYear][m - 1][day - 1][1][k]) / 1.0 / this->SimParams.NSim << ","//h2V+h3V
                        << (dailyy[year - this->SimParams.FYear][m - 1][day - 1][0][k] + dailyy[year - this->SimParams.FYear][m - 1][day - 1][1][k]) / 1.0 / dailyy[year - this->SimParams.FYear][m - 1][day - 1][4][k] << "," //IR2+IR3
                        << dailyy[year - this->SimParams.FYear][m - 1][day - 1][6][k] / 1.0 / this->SimParams.NSim << ","//hARIV
                        << dailyy[year - this->SimParams.FYear][m - 1][day - 1][6][k] / 1.0 / dailyy[year - this->SimParams.FYear][m - 1][day - 1][4][k] << endl;
#if WITH_R
                daiReport->d.push_back(d);
                daiReport->day.push_back(day);
                daiReport->month.push_back(month);
                daiReport->year.push_back(year);
                daiReport->stratum.push_back(k+1);
                daiReport->v.push_back(v);
                daiReport->nn.push_back(dailyy[year - this->SimParams.FYear][m - 1][day - 1][4][k] / 1.0 / this->SimParams.NSim);
                daiReport->inc2.push_back(dailyy[year - this->SimParams.FYear][m - 1][day - 1][0][k] / 1.0 / this->SimParams.NSim);
                daiReport->rate2.push_back(dailyy[year - this->SimParams.FYear][m - 1][day - 1][0][k] / 1.0 / dailyy[year - this->SimParams.FYear][m - 1][day - 1][4][k]);
                daiReport->inc3.push_back(dailyy[year - this->SimParams.FYear][m - 1][day - 1][1][k] / 1.0 / this->SimParams.NSim);
                daiReport->rate3.push_back(dailyy[year - this->SimParams.FYear][m - 1][day - 1][1][k] / 1.0 / dailyy[year - this->SimParams.FYear][m - 1][day - 1][4][k]);
                daiReport->incT.push_back((dailyy[year - this->SimParams.FYear][m - 1][day - 1][0][k] + dailyy[year - this->SimParams.FYear][m - 1][day - 1][1][k]) / 1.0 / this->SimParams.NSim);
                daiReport->rateT.push_back((dailyy[year - this->SimParams.FYear][m - 1][day - 1][0][k] + dailyy[year - this->SimParams.FYear][m - 1][day - 1][1][k]) / 1.0 / dailyy[year - this->SimParams.FYear][m - 1][day - 1][4][k]);
                daiReport->incNFARI.push_back(dailyy[year - this->SimParams.FYear][m - 1][day - 1][6][k] / 1.0 / this->SimParams.NSim);
                daiReport->rateNFARI.push_back(dailyy[year - this->SimParams.FYear][m - 1][day - 1][6][k] / 1.0 / dailyy[year - this->SimParams.FYear][m - 1][day - 1][4][k]);
#endif
            }

        }

    }

    //prevalance report

    if (this->SimParams.PReport) {
        for (int k = 0; k < this->SimParams.NStrata; k++) {
            int v = 0;
            preport << s << ","
                    << d << ","
                    << day << ","
                    << month << ","
                    << year << ","
                    << k + 1 << ","
                    << H2[k] << ","
                    << H3[k] << ","
                    << IPV0[0][k] << ","
                    << IPV0[1][k];
            if (!this->SimParams.allOrNone) {
                preport << "," << IPV1[0][k] << ","
                        << IPV1[1][k];
            }
            preport << endl;
#if WITH_R
                preReport->s.push_back(s);
                preReport->d.push_back(d);
                preReport->day.push_back(day);
                preReport->month.push_back(month);
                preReport->year.push_back(year);
                preReport->stratum.push_back(k+1);
                preReport->prevS2.push_back(H2[k]);
                preReport->prevS3.push_back(H3[k]);
                preReport->pInfX0.push_back(IPV0[0][k]);
                preReport->pInfX1.push_back(IPV0[1][k]);
                if (!this->SimParams.allOrNone) {
                    preReport->pInfV0X0.push_back(IPV0[0][k]);
                    preReport->pInfV0X1.push_back(IPV0[1][k]);
                    preReport->pInfV1X0.push_back(IPV1[0][k]);
                    preReport->pInfV1X1.push_back(IPV1[1][k]);             
                }
#endif            
        }
    }


    //cout<<this->SimParams.DReportPerson<<endl;
    if (this->SimParams.DReportPerson) {
        for (int i = 0; i < this->SimParams.PopSize; i++) {
            dreport << s << ","
                    << d << ","
                    << day << ","
                    << month << ","
                    << year << ","
                    << i + 1 << ","
                    << this->People[i].Stratum + 1 << ","
                    << this->People[i].X << ","
                    << this->People[i].U << ","
                    << this->People[i].InitInf << ","
                    << this->People[i].Vac << ",";
            if (this->SimParams.allOrNone) {
                dreport << this->People[i].VacSuccessful << ",";
            } else dreport << 9 << ",";
            dreport << this->People[i].State << ","
                    << this->People[i].DayInf << ","
                    << this->People[i].dayARI << ","
                    << this->People[i].visit << endl;

#if WITH_R
                detReport->s.push_back(s);
                detReport->d.push_back(d);
                detReport->day.push_back(day);
                detReport->month.push_back(month);
                detReport->year.push_back(year);
                detReport->ii.push_back(i+1);
                detReport->stratum.push_back(this->People[i].Stratum + 1 );
                detReport->x.push_back(this->People[i].X);
                detReport->u.push_back(this->People[i].U);
                detReport->initInf.push_back(this->People[i].InitInf);
                detReport->vac.push_back(this->People[i].Vac);
                if (this->SimParams.allOrNone) {
                    detReport->prot.push_back(this->People[i].VacSuccessful);
                }else detReport->prot.push_back(9);
                detReport->state.push_back(this->People[i].State);
                detReport->dayInf.push_back(this->People[i].DayInf);
                detReport->dNFARI.push_back(this->People[i].dayARI);
                detReport->visit.push_back(this->People[i].visit);
#endif
        }

    }

    //monthly incidence
    if (this->SimParams.MReportSim || this->SimParams.MReportSum || this->SimParams.SReportSim || this->SimParams.YReportSum) {

        if (day == 1) {
            for (int k = 0; k < this->SimParams.NStrata; k++) {
                h2VMSum[k] = 0;
                h3VMSum[k] = 0;
                h2vMSum[k] = 0;
                h3vMSum[k] = 0;
                hARIvMSum[k] = 0;
                hARIVMSum[k] = 0;
            }
            for (int k = 0; k < this->SimParams.NStrata; k++) {
                h2VMSum[k] += h2V[k];
                h3VMSum[k] += h3V[k];
                h2vMSum[k] += h2v[k];
                h3vMSum[k] += h3v[k];
                hARIvMSum[k] += hARIv[k];
                hARIVMSum[k] += hARIV[k];
            }
        } else if (day == NDays) {
            for (int k = 0; k < this->SimParams.NStrata; k++) {
                h2VMSum[k] += h2V[k]; //first is I2 vac
                var[0][k] += h2VMSum[k];
                seasonly[s - 1][0][k] += h2VMSum[k];
                //h2Vs[k]+=h2VMSum[k];
                h3VMSum[k] += h3V[k]; //second is I3 vac
                var[1][k] += h3VMSum[k];
                seasonly[s - 1][1][k] += h3VMSum[k];
                //h3Vs[k]+=h2VMSum[k];
                h2vMSum[k] += h2v[k]; //third is I2 unvac
                var[2][k] += h2vMSum[k];
                seasonly[s - 1][2][k] += h2vMSum[k];
                //h2vs[k]+=h2VMSum[k];
                h3vMSum[k] += h3v[k]; //fourth is I3 unvac
                var[3][k] += h3vMSum[k];
                seasonly[s - 1][3][k] += h3vMSum[k];
                //h3vs[k]+=h2VMSum[k];
                VacPeople[k] = 0;
                UnVacPeople[k] = 0;
                for (int i = 0; i < this->SimParams.PopSize; i++) {
                    if (this->People[i].Vac == true && this->People[i].Stratum == k) {
                        VacPeople[k]++;
                    }

                    if (this->People[i].Vac == false && this->People[i].Stratum == k) {
                        UnVacPeople[k]++;

                    }

                }
                //cout<<month<<endl;
                var[4][k] += VacPeople[k]; //fifth is vacN
                seasonly[s - 1][4][k] += VacPeople[k];
                var[5][k] += UnVacPeople[k]; //sixth is unvacN
                seasonly[s - 1][5][k] += UnVacPeople[k];

                hARIVMSum[k] += hARIV[k];
                var[6][k] += hARIVMSum[k];
                seasonly[s - 1][6][k] += hARIVMSum[k];

                hARIvMSum[k] += hARIv[k]; //third is I2 unvac
                var[7][k] += hARIvMSum[k];
                seasonly[s - 1][7][k] += hARIvMSum[k];
                //generate
                if (k == this->SimParams.NStrata - 1 && s == 1) {

                    monthly.push_back(var);

                }
                if (s != 1) {
                    for (int i = 0; i < 8; i++)
                        monthly[m - 1][i][k] += var[i][k];

                }


                //if need to output monthlysim
                if (this->SimParams.MReportSim) {
                    int v = 0;
                    mreport << s << ","
                            << month << ","
                            << year << ","
                            << k + 1 << ","
                            << v << ","
                            << UnVacPeople[k] << ","
                            << h2vMSum[k] << ","
                            << h2vMSum[k] / 1.0 / UnVacPeople[k] << ","
                            << h3vMSum[k] << ","
                            << h3vMSum[k] / 1.0 / UnVacPeople[k] << ","
                            << h2vMSum[k] + h3vMSum[k] << ","
                            << (h2vMSum[k] + h3vMSum[k]) / 1.0 / UnVacPeople[k] << ","
                            << hARIvMSum[k] << ","
                            << hARIvMSum[k] / 1.0 / UnVacPeople[k] << endl;

#if WITH_R          
                monsimReport->s.push_back(s);
                monsimReport->month.push_back(month);
                monsimReport->year.push_back(year);
                monsimReport->stratum.push_back(k + 1);
                monsimReport->v.push_back(v);
                monsimReport->nn.push_back(UnVacPeople[k]);
                monsimReport->inc2.push_back(h2vMSum[k]);
                monsimReport->rate2.push_back(h2vMSum[k] / 1.0 / UnVacPeople[k]);
                monsimReport->inc3.push_back(h3vMSum[k]);
                monsimReport->rate3.push_back(h3vMSum[k] / 1.0 / UnVacPeople[k]);
                monsimReport->incT.push_back(h2vMSum[k] + h3vMSum[k]);
                monsimReport->rateT.push_back((h2vMSum[k] + h3vMSum[k]) / 1.0 / UnVacPeople[k]);
                monsimReport->incNFARI.push_back(hARIvMSum[k]);
                monsimReport->rateNFARI.push_back(hARIvMSum[k] / 1.0 / UnVacPeople[k]);
#endif


                    v = 1;
                    mreport << s << ","
                            << month << ","
                            << year << ","
                            << k + 1 << ","
                            << v << ","
                            << VacPeople[k] << ","
                            << h2VMSum[k] << ","
                            << h2VMSum[k] / 1.0 / VacPeople[k] << ","
                            << h3VMSum[k] << ","
                            << h3VMSum[k] / 1.0 / VacPeople[k] << ","
                            << h3VMSum[k] + h2VMSum[k] << ","
                            << (h3VMSum[k] + h2VMSum[k]) / 1.0 / VacPeople[k] << ","
                            << hARIVMSum[k] << ","
                            << hARIVMSum[k] / 1.0 / VacPeople[k] << endl;
#if WITH_R          
                monsimReport->s.push_back(s);
                monsimReport->month.push_back(month);
                monsimReport->year.push_back(year);
                monsimReport->stratum.push_back(k + 1);
                monsimReport->v.push_back(v);
                monsimReport->nn.push_back(VacPeople[k]);
                monsimReport->inc2.push_back(h2VMSum[k]);
                monsimReport->rate2.push_back(h2VMSum[k] / 1.0 / VacPeople[k]);
                monsimReport->inc3.push_back(h3VMSum[k]);
                monsimReport->rate3.push_back(h3VMSum[k] / 1.0 / VacPeople[k]);
                monsimReport->incT.push_back(h3VMSum[k] + h2VMSum[k]);
                monsimReport->rateT.push_back((h3VMSum[k] + h2VMSum[k]) / 1.0 / VacPeople[k]);
                monsimReport->incNFARI.push_back(hARIVMSum[k]);
                monsimReport->rateNFARI.push_back(hARIVMSum[k] / 1.0 / VacPeople[k]);
#endif
                }
            }

        } else {
            for (int k = 0; k < this->SimParams.NStrata; k++) {
                h2VMSum[k] += h2V[k];
                h3VMSum[k] += h3V[k];
                h2vMSum[k] += h2v[k];
                h3vMSum[k] += h3v[k];
                hARIvMSum[k] += hARIv[k];
                hARIVMSum[k] += hARIV[k];
            }

        }

    }


    //seasonsim
    if (this->SimParams.SReportSim || this->SimParams.YReportSum) {
        if (m == NMonths && day == NDays) {

            for (int k = 0; k < this->SimParams.NStrata; k++) {
                seasonly[s - 1][5][k] = seasonly[s - 1][5][k] / 1.0 / NMonths;
                seasonly[s - 1][4][k] = seasonly[s - 1][4][k] / 1.0 / NMonths;

                int v = 0;
                sreport << s << ","
                        << k + 1 << ","
                        << v << ","
                        << seasonly[s - 1][5][k] << ","
                        << seasonly[s - 1][2][k] << ","
                        << seasonly[s - 1][2][k] / 1.0 / seasonly[s - 1][5][k] << ","
                        << seasonly[s - 1][3][k] << ","
                        << seasonly[s - 1][3][k] / 1.0 / seasonly[s - 1][5][k] << ","
                        << seasonly[s - 1][2][k] + seasonly[s - 1][3][k] << ","
                        << (seasonly[s - 1][2][k] + seasonly[s - 1][3][k]) / 1.0 / seasonly[s - 1][5][k] << ","
                        << seasonly[s - 1][7][k] << ","
                        << seasonly[s - 1][7][k] / 1.0 / seasonly[s - 1][5][k] << endl;
#if WITH_R
                    seasimReport->s.push_back(s);                    
                    seasimReport->stratum.push_back(k + 1);
                    seasimReport->v.push_back(v);
                    seasimReport->nn.push_back(seasonly[s - 1][5][k]);
                    seasimReport->inc2.push_back(seasonly[s - 1][2][k]);
                    seasimReport->rate2.push_back(seasonly[s - 1][2][k] / 1.0 / seasonly[s - 1][5][k]);
                    seasimReport->inc3.push_back(seasonly[s - 1][3][k]);
                    seasimReport->rate3.push_back(seasonly[s - 1][3][k] / 1.0 / seasonly[s - 1][5][k]);
                    seasimReport->incT.push_back(seasonly[s - 1][2][k] + seasonly[s - 1][3][k]);
                    seasimReport->rateT.push_back((seasonly[s - 1][2][k] + seasonly[s - 1][3][k]) / 1.0 / seasonly[s - 1][5][k]);
                    seasimReport->incNFARI.push_back(seasonly[s - 1][7][k]);
                    seasimReport->rateNFARI.push_back(seasonly[s - 1][7][k] / 1.0 / seasonly[s - 1][5][k]);
#endif

                v = 1;
                sreport << s << ","
                        << k + 1 << ","
                        << v << ","
                        << seasonly[s - 1][4][k] << ","
                        << seasonly[s - 1][0][k] << ","
                        << seasonly[s - 1][0][k] / 1.0 / seasonly[s - 1][4][k] << ","
                        << seasonly[s - 1][1][k] << ","
                        << seasonly[s - 1][1][k] / 1.0 / seasonly[s - 1][4][k] << ","
                        << seasonly[s - 1][0][k] + seasonly[s - 1][1][k] << ","
                        << (seasonly[s - 1][0][k] + seasonly[s - 1][1][k]) / 1.0 / seasonly[s - 1][4][k] << ","
                        << seasonly[s - 1][6][k] << ","
                        << seasonly[s - 1][6][k] / 1.0 / seasonly[s - 1][4][k] << endl;
#if WITH_R
                    seasimReport->s.push_back(s);                    
                    seasimReport->stratum.push_back(k + 1);
                    seasimReport->v.push_back(v);
                    seasimReport->nn.push_back(seasonly[s - 1][4][k]);
                    seasimReport->inc2.push_back(seasonly[s - 1][0][k]);
                    seasimReport->rate2.push_back(seasonly[s - 1][0][k] / 1.0 / seasonly[s - 1][4][k]);
                    seasimReport->inc3.push_back(seasonly[s - 1][1][k]);
                    seasimReport->rate3.push_back(seasonly[s - 1][1][k] / 1.0 / seasonly[s - 1][4][k]);
                    seasimReport->incT.push_back(seasonly[s - 1][0][k] + seasonly[s - 1][1][k]);
                    seasimReport->rateT.push_back((seasonly[s - 1][0][k] + seasonly[s - 1][1][k]) / 1.0 / seasonly[s - 1][4][k]);
                    seasimReport->incNFARI.push_back(seasonly[s - 1][6][k]);
                    seasimReport->rateNFARI.push_back(seasonly[s - 1][6][k] / 1.0 / seasonly[s - 1][4][k]);
#endif
            }
        }

    }

    //monthly sum

    if (s == this->SimParams.NSim && day == NDays) {


        int m1 = month + 12 * (year - this->SimParams.FYear) - this->SimParams.FMonth;

        for (int k = 0; k < this->SimParams.NStrata; k++) {


          int v = 0;

#if WITH_R          
                monReport->month.push_back(month);
                monReport->year.push_back(year);
                monReport->stratum.push_back(k + 1);
                monReport->v.push_back(v);
                monReport->nn.push_back(monthly[m1][5][k] / 1.0 / this->SimParams.NSim);
                monReport->inc2.push_back(monthly[m1][2][k] / 1.0 / this->SimParams.NSim);
                monReport->rate2.push_back(monthly[m1][2][k] / 1.0 / monthly[m1][5][k]);
                monReport->inc3.push_back(monthly[m1][3][k] / 1.0 / this->SimParams.NSim);
                monReport->rate3.push_back(monthly[m1][3][k] / 1.0 / monthly[m1][5][k]);
                monReport->incT.push_back(monthly[m1][2][k] / 1.0 / this->SimParams.NSim + monthly[m1][3][k] / 1.0 / this->SimParams.NSim);
                monReport->rateT.push_back((monthly[m1][2][k] + monthly[m1][3][k]) / 1.0 / monthly[m1][5][k]);
                monReport->incNFARI.push_back(monthly[m1][7][k] / 1.0 / this->SimParams.NSim);
                monReport->rateNFARI.push_back(monthly[m1][7][k] / 1.0 / monthly[m1][5][k]);
#endif
        
            if (this->SimParams.MReportSum) {


                mreports << month << ","
                        << year << ","
                        << k + 1 << ","
                        << v << ","
                        << monthly[m1][5][k] / 1.0 / this->SimParams.NSim << ","
                        << monthly[m1][2][k] / 1.0 / this->SimParams.NSim << ","
                        << monthly[m1][2][k] / 1.0 / monthly[m1][5][k] << ","
                        << monthly[m1][3][k] / 1.0 / this->SimParams.NSim << ","
                        << monthly[m1][3][k] / 1.0 / monthly[m1][5][k] << ","
                        << monthly[m1][2][k] / 1.0 / this->SimParams.NSim + monthly[m1][3][k] / 1.0 / this->SimParams.NSim << ","
                        << (monthly[m1][2][k] + monthly[m1][3][k]) / 1.0 / monthly[m1][5][k] << ","
                        << monthly[m1][7][k] / 1.0 / this->SimParams.NSim << ","
                        << monthly[m1][7][k] / 1.0 / monthly[m1][5][k] << endl;
            }

                v = 1;

#if WITH_R
                    monReport->month.push_back(month);
                    monReport->year.push_back(year);
                    monReport->stratum.push_back(k + 1);
                    monReport->v.push_back(v);
                    monReport->nn.push_back(monthly[m1][4][k] / 1.0 / this->SimParams.NSim);
                    monReport->inc2.push_back(monthly[m1][0][k] / 1.0 / this->SimParams.NSim);
                    monReport->rate2.push_back(monthly[m1][0][k] / 1.0 / monthly[m1][4][k]);
                    monReport->inc3.push_back(monthly[m1][1][k] / 1.0 / this->SimParams.NSim);
                    monReport->rate3.push_back(monthly[m1][1][k] / 1.0 / monthly[m1][4][k]);
                    monReport->incT.push_back(monthly[m1][0][k] / 1.0 / this->SimParams.NSim + monthly[m1][1][k] / 1.0 / this->SimParams.NSim);
                    monReport->rateT.push_back((monthly[m1][0][k] + monthly[m1][1][k]) / 1.0 / monthly[m1][4][k]);
                    monReport->incNFARI.push_back(monthly[m1][6][k] / 1.0 / this->SimParams.NSim);
                    monReport->rateNFARI.push_back(monthly[m1][6][k] / 1.0 / monthly[m1][4][k]);
#endif



            if (this->SimParams.MReportSum) {


                mreports << month << ","
                        << year << ","
                        << k + 1 << ","
                        << v << ","
                        << monthly[m1][4][k] / 1.0 / this->SimParams.NSim << ","
                        << monthly[m1][0][k] / 1.0 / this->SimParams.NSim << ","
                        << monthly[m1][0][k] / 1.0 / monthly[m1][4][k] << ","
                        << monthly[m1][1][k] / 1.0 / this->SimParams.NSim << ","
                        << monthly[m1][1][k] / 1.0 / monthly[m1][4][k] << ","
                        << monthly[m1][0][k] / 1.0 / this->SimParams.NSim + monthly[m1][1][k] / 1.0 / this->SimParams.NSim << ","
                        << (monthly[m1][0][k] + monthly[m1][1][k]) / 1.0 / monthly[m1][4][k] << ","
                        << monthly[m1][6][k] / 1.0 / this->SimParams.NSim << ","
                        << monthly[m1][6][k] / 1.0 / monthly[m1][4][k] << endl;
            }
        }
	}

    // season overall

    if (s == this->SimParams.NSim && m == this->SimParams.NMonths && day == NDays) {


        vector<int> k(this->SimParams.NStrata);
        vector < vector<int > > seasonly1;
        for (int h = 0; h < 8; h++)
            seasonly1.push_back(k);


        for (int h = 0; h < 8; h++)
            for (int k = 0; k<this->SimParams.NStrata; k++)
                for (int s = 0; s<this->SimParams.NSim; s++) {
                    seasonly1[h][k] += seasonly[s][h][k];
                }



        for (int k = 0; k < this->SimParams.NStrata; k++) {
            if (this->SimParams.SReportSim != 1) {
                seasonly[s - 1][5][k] = seasonly[s - 1][5][k] / 1.0 / NMonths;
                seasonly[s - 1][4][k] = seasonly[s - 1][4][k] / 1.0 / NMonths;
            }
            int v = 0;

#if WITH_R
                    seaReport->stratum.push_back(k + 1);
                    seaReport->v.push_back(v);
                    seaReport->nn.push_back(seasonly1[5][k] / 1.0 / this->SimParams.NSim);
                    seaReport->inc2.push_back(seasonly1[2][k] / 1.0 / this->SimParams.NSim);
                    seaReport->rate2.push_back(seasonly1[2][k] / 1.0 / seasonly1[5][k]);
                    seaReport->inc3.push_back(seasonly1[3][k] / 1.0 / this->SimParams.NSim);
                    seaReport->rate3.push_back(seasonly1[3][k] / 1.0 / seasonly1[5][k]);
                    seaReport->incT.push_back((seasonly1[2][k] + seasonly1[3][k]) / 1.0 / this->SimParams.NSim);
                    seaReport->rateT.push_back((seasonly1[2][k] + seasonly1[3][k]) / 1.0 / seasonly1[5][k]);
                    seaReport->incNFARI.push_back(seasonly1[7][k] / 1.0 / this->SimParams.NSim);
                    seaReport->rateNFARI.push_back(seasonly1[7][k] / 1.0 / seasonly1[5][k]);
#endif

            if (this->SimParams.YReportSum) {
            yreport
                    << k + 1 << ","
                    << v << ","
                    << seasonly1[5][k] / 1.0 / this->SimParams.NSim << ","
                    << seasonly1[2][k] / 1.0 / this->SimParams.NSim << ","
                    << seasonly1[2][k] / 1.0 / seasonly1[5][k] << ","
                    << seasonly1[3][k] / 1.0 / this->SimParams.NSim << ","
                    << seasonly1[3][k] / 1.0 / seasonly1[5][k] << ","
                    << (seasonly1[2][k] + seasonly1[3][k]) / 1.0 / this->SimParams.NSim << ","
                    << (seasonly1[2][k] + seasonly1[3][k]) / 1.0 / seasonly1[5][k] << ","
                    << seasonly1[7][k] / 1.0 / this->SimParams.NSim << ","
                    << seasonly1[7][k] / 1.0 / seasonly1[5][k] << endl;

            }
            v = 1;

#if WITH_R
                    seaReport->stratum.push_back(k + 1);
                    seaReport->v.push_back(v);
                    seaReport->nn.push_back(seasonly1[4][k] / 1.0 / this->SimParams.NSim);
                    seaReport->inc2.push_back(seasonly1[0][k] / 1.0 / this->SimParams.NSim);
                    seaReport->rate2.push_back(seasonly1[0][k] / 1.0 / seasonly1[4][k]);
                    seaReport->inc3.push_back(seasonly1[1][k] / 1.0 / this->SimParams.NSim);
                    seaReport->rate3.push_back(seasonly1[1][k] / 1.0 / seasonly1[4][k]);
                    seaReport->incT.push_back((seasonly1[0][k] + seasonly1[1][k]) / 1.0 / this->SimParams.NSim);
                    seaReport->rateT.push_back((seasonly1[0][k] + seasonly1[1][k]) / 1.0 / seasonly1[4][k]);
                    seaReport->incNFARI.push_back(seasonly1[6][k] / 1.0 / this->SimParams.NSim);
                    seaReport->rateNFARI.push_back(seasonly1[6][k] / 1.0 / seasonly1[4][k]);
#endif

            if (this->SimParams.YReportSum) {
            yreport
                    << k + 1 << ","
                    << v << ","
                    << seasonly1[4][k] / 1.0 / this->SimParams.NSim << ","
                    << seasonly1[0][k] / 1.0 / this->SimParams.NSim << ","
                    << seasonly1[0][k] / 1.0 / seasonly1[4][k] << ","
                    << seasonly1[1][k] / 1.0 / this->SimParams.NSim << ","
                    << seasonly1[1][k] / 1.0 / seasonly1[4][k] << ","
                    << (seasonly1[0][k] + seasonly1[1][k]) / 1.0 / this->SimParams.NSim << ","
                    << (seasonly1[0][k] + seasonly1[1][k]) / 1.0 / seasonly1[4][k] << ","
                    << seasonly1[6][k] / 1.0 / this->SimParams.NSim << ","
                    << seasonly1[6][k] / 1.0 / seasonly1[4][k] << endl;
            }
        }
    }


    //population files
    if (this->SimParams.PopReport) {
        if (day == 1)
            dvac = d;


        for (int i = 0; i < this->SimParams.PopSize; i++) {

            if (this->People[i].Vac == 1 && pop[i][0][6] == 0) {

                pop[i][0][6] = dvac; //dvac
                if (this->SimParams.allOrNone) {
                    pop[i][0][7] = this->People[i].VacSuccessful;
                }
            }
            if (this->People[i].influenzaStartDay == d) {
                vector< int> record = pop[i][0];
                if (this->People[i].State == INFECTIOUSNILL) {
                    record[8] = 1;
                    record[9] = d;
                    record[10] = this->People[i].Vac;
                    record[11] = this->People[i].visit;
                    pop[i].push_back(record); // only insert record when the state is either INFECTIOUSNILL or INFECTIOUSILL
                } else if (this->People[i].State == INFECTIOUSILL) {
                    record[8] = 2;
                    record[9] = d;
                    record[10] = this->People[i].Vac;
                    record[11] = this->People[i].visit;
                    pop[i].push_back(record);
                }
            }
            if (this->People[i].dayARI > 0 && this->People[i].ariStartDay == d) {
                vector< int> record = pop[i][0];
                record[8] = 3;
                record[9] = d;
                record[10] = this->People[i].Vac;
                record[11] = this->People[i].visit;
                pop[i].push_back(record);
            }
        }

        if (this->SimParams.PopReport && m == NMonths && day == NDays) {
            for (int i = 0; i < this->SimParams.PopSize; i++) {
                int event = 0;
                for (int j = 0; j < pop[i].size(); j++) {
                    if (pop[i][j][6] == 0) {
                        if (j == 0) {
                            pop[i][j][6] = 999;
                        }
                        else {
                            pop[i][j][6] = pop[i][0][6];
                        }
                    }

                    popreport << pop[i][j][0] << ","
                            << pop[i][j][1] << ","
                            << pop[i][j][2] << ","
                            << pop[i][j][3] << ","
                            << pop[i][j][4] << ","
                            << pop[i][j][5] << ","
                            << pop[i][j][6] << ","
                            << pop[i][j][7] << ","
                            << event++ << ","
                            << pop[i][j][8] << ","
                            << pop[i][j][9] << ","
                            << pop[i][j][10] << ","
                            << pop[i][j][11] << endl;
                            //<< pop[i][j][12] << endl;

#if WITH_R
                    populationReport->sim.push_back(pop[i][j][0]);
                    populationReport->person.push_back(pop[i][j][1]);
                    populationReport->str.push_back(pop[i][j][2]);
                    populationReport->x.push_back(pop[i][j][3]);
                    populationReport->u.push_back(pop[i][j][4]);
                    populationReport->initinf.push_back(pop[i][j][5]);
                    populationReport->defvac.push_back(pop[i][j][6]);
                    populationReport->prot.push_back(pop[i][j][7]);
                    populationReport->eventcounter.push_back(event);
                    populationReport->type.push_back(pop[i][j][8]);
                    populationReport->onsetday.push_back(pop[i][j][9]);
                    populationReport->vacstatus.push_back(pop[i][j][10]);
                    populationReport->visit.push_back(pop[i][j][11]);
#endif


                }
            }
        }
    }

    // Free all memory
    free(h2v);
    free(h2V);
    free(h3v);
    free(h3V);
    free(hARIv);
    free(hARIV);
    free(Pr2v);
    free(Pr2V);
    free(Pr3v);
    free(Pr3V);
    free(H2);
    free(H3);
    for (int indX = 0; indX < 2; indX++) {
        free(CSIPV0H2[indX]);
        free(CSIPV0H3[indX]);
        free(CSIPV1H2[indX]);
        free(CSIPV1H3[indX]);
        for (int i = 0; i < this->SimParams.NStrata; i++) {
            free(CSIPV0[indX][i]);
            free(CSIPV1[indX][i]);
        }

        free(CSIPV0[indX]);
        free(CSIPV1[indX]);
        free(PCIPV0[indX]);
        free(PCIPV1[indX]);
        free(IPV0[indX]);
        free(IPV1[indX]);
    }
    free(CSIPV0H2);
    free(CSIPV0H3);
    free(CSIPV1H2);
    free(CSIPV1H3);
    free(CSIPV0);
    free(CSIPV1);
    free(PCIPV0);
    free(PCIPV1);
    free(IPV0);
    free(IPV1);
}

Population::~Population() {
    free(People);
    dailyy.clear();
    dailym.clear();
    dailyk.clear();
    dailyh.clear();
    monthly.clear();
    seasonly.clear();
    pop.clear();
    dvac = 0;
    var.clear();
    dailyd.clear();
    shk.clear();
    this->SimParams.cleanUp();
}
