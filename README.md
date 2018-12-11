# SimFluVE
SimFluVE is an R package for stochastic agent-based simulations for the transmission of influenza in a stratified population. 
SimFluVE performs a set of simulations with fixed values of the input parameters.  Each simulation corresponds to a single outbreak. 

## How to Install
```
devtools::install_github("fluvee/SimFluVE")
```

## Important Functions
### readFLUVEE
```
# Usage
readFLUVEE(filename)
# filename: name of input parameters file
```

### saveFLUVEE
```
# Usage
readFLUVEE(para, filename)
# para: SimFluVE parameters list
# filename: name of input parameters file
```

### simflu6
```
# Usage
readFLUVEE(...)
# size_strata: Number of strata
# num_infected_persons: Number of initially infected persons
# prob_condition_strata: Probability of X=1, Probability of U=1 given X=1, Probability of U=1 given X=0 with strata as columns, (X: Health status; Healthy: 1, Frail: 0, U: Propensity to seek medical care; High: 1, Low: 0)
# vac_inc_coverage: Vaccine incremental coverage (A matrix with months as rows and strata as columns)
```


\item{vac_multiplyer}{Vaccination incremental coverage multiplier for X=0 and U=0}

\item{contacts_per_day}{A vector containing number of contacts per day for each stratum}

\item{dist_contacts}{Distribution matrix of contacts (Rho); square matrix with size size_strata}

\item{latency_days}{A vector containing length of latent period for each stratum}

\item{infectious_days}{A vector containing length of infectious period for each stratum}

\item{prob_illness}{A vector containing probability of illness given infection for each stratum}

\item{relative_infectious_not_ill}{A vector containing relative infectiousness if not ill for each stratum}

\item{prob_transmission}{Transmission probabilities matrix (rows for months, columns for strata)}

\item{trans_prob_multiplier}{Transmission probabilities multipliers for X=0 in unvaccinated or unprotected}

\item{prob_transmission_vac}{Transmission probabilities vac matrix (rows for months, columns for strata)}

\item{vec_efficacy_multiplier}{Vaccine efficacy multipliers for X=0}

\item{length_ARI}{Length of NFARI episode (FARI: Influenza Actute Respiratory Illness, NFARI: Non-Influenza Actute Respiratory Illness)}

\item{daily_prob_NFARI}{Daily probabilities of onset of NFARI for X=1 in unvaccinated or unprotected persons}

\item{probARI_vaccined_multiplier}{NFARI probabilities multipliers for vaccinated or protected persons}

\item{probARI_X0_multiplier}{NFARI probabilities multipliers for vaccinated or protected persons}

\item{prob_visit_FARI}{Probability of visit for a case of FARI: For unvaccinated or unprotected with X=1 and U=1}

\item{visit_FARI_multiplier}{Multiplier for vaccinated, X=0, U=0}

\item{prob_visit_NFARI}{Probability of visit for a case of NFARI: For unvaccinated or unprotected with X=1 and U=1}

\item{visit_NFARI_multiplier}{Multiplier for vaccinated, X=0, U=0}

\item{all_or_no_vaccine}{All-or-none vaccine (Leaky vaccine if 'no')}

\item{sim}{Number of simulations}

\item{rseed}{Seed for random number generation}

\item{begin_year}{Year of beginning of season}

\item{begin_month}{Month of beginning of season}

\item{months}{Number of months in the season}

\item{strata}{Number of strata}

\item{inputsfile}{A boolean for whether to output input and calculated file or not}

\item{vaccination_file}{A boolean for whether to output vaccination file or not}

\item{detailed_file}{A boolean for whether to output detailed report file or not}

\item{contacts_file}{A boolean for whether to output contacts file or not}

\item{prevalance_file}{A boolean for whether to output prevalance file or not}

\item{daily_each_sim_file}{A boolean for whether to output daily report file for each simulation or not}

\item{monthly_each_sim_file}{A boolean for whether to output monthy report file for each simulation or not}

\item{seasonal_each_sim_file}{A boolean for whether to output seasonal report file for each simulation or not}

\item{daily_overall_file}{A boolean for whether to output overall daily report file or not}

\item{monthly_overall_file}{A boolean for whether to output overall monthly report file or not}

\item{seasonal_overall_file}{A boolean for whether to output overall seasonal report file or not}

\item{population_report_file}{A boolean for whether to output population report file or not}

\item{timestamp}{Add timestamp to output file names}

\item{verbose}{A boolean for whether to print information or not}

