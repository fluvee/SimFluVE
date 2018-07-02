saveFLUVEE = function(para, filename)
{
  outputM = matrix("",nrow=55+para$strata+4*para$months,ncol=1+para$strata)
  kk = 1
  outputM[kk,1] <- "Title (alphanumeric)"
  outputM[kk,2] <- para$title; kk = kk + 1
  outputM[kk,1] <- "Number of simulations"
  outputM[kk,2] <- para$sim; kk = kk + 1
  outputM[kk,1] <- "Seed"
  outputM[kk,2] <- para$rseed; kk = kk + 1
  outputM[kk,1] <- "All-or-none vaccine (Leaky vaccine if 'no')"
  outputM[kk,2] <- ifelse(para$all_or_no_vaccine,"yes","no"); kk = kk + 1
  outputM[kk,1] <- "Output files ('yes' or 'no' for each)"; kk = kk + 1
  outputM[kk,1] <- "Input and calculated parameters"
  outputM[kk,2] <- ifelse(para$inputsfile,"yes","no"); kk = kk + 1
  outputM[kk,1] <- "Vaccination *"
  outputM[kk,2] <- ifelse(para$vaccination_file,"yes","no"); kk = kk + 1
  outputM[kk,1] <- "Detailed *"
  outputM[kk,2] <- ifelse(para$detailed_file,"yes","no"); kk = kk + 1
  outputM[kk,1] <- "Contacts *"
  outputM[kk,2] <- ifelse(para$contacts_file,"yes","no"); kk = kk + 1
  outputM[kk,1] <- "Prevalence *"
  outputM[kk,2] <- ifelse(para$prevalance_file,"yes","no"); kk = kk + 1
  outputM[kk,1] <- "Incidence-daily each simulation"
  outputM[kk,2] <- ifelse(para$daily_each_sim_file,"yes","no"); kk = kk + 1
  outputM[kk,1] <- "Incidence-monthly each simulation"
  outputM[kk,2] <- ifelse(para$monthly_each_sim_file,"yes","no"); kk = kk + 1
  outputM[kk,1] <- "Incidence-season each simulation"
  outputM[kk,2] <- ifelse(para$seasonal_each_sim_file,"yes","no"); kk = kk + 1
  outputM[kk,1] <- "Incidence-daily overall"
  outputM[kk,2] <- ifelse(para$daily_overall_file,"yes","no"); kk = kk + 1
  outputM[kk,1] <- "Incidence-monthly overall"
  outputM[kk,2] <- ifelse(para$monthly_overall_file,"yes","no"); kk = kk + 1
  outputM[kk,1] <- "Incidence-season overall"
  outputM[kk,2] <- ifelse(para$seasonal_overall_file,"yes","no"); kk = kk + 1
  outputM[kk,1] <- "Outcomes file"
  outputM[kk,2] <- ifelse(para$population_report_file,"yes","no"); kk = kk + 1
  outputM[kk,1] <- "Add timestamp to output file names"
  outputM[kk,2] <- ifelse(para$timestamp,"yes","no"); kk = kk + 1
  outputM[kk,1] <- "Year of beginning of study"
  outputM[kk,2] <- para$begin_year; kk = kk + 1
  outputM[kk,1] <- "Month of beginning of study"
  outputM[kk,2] <- para$begin_month; kk = kk + 1
  outputM[kk,1] <- "Number of months in the study"
  outputM[kk,2] <- para$months; kk = kk + 1
  outputM[kk,1] <- "Number of strata"
  outputM[kk,2] <- para$strata; kk = kk + 1
  outputM[kk,1] <- "Sizes of strata"
  outputM[kk,2:(1+para$strata)] <- para$size_strata; kk = kk + 1
  outputM[kk,1] <- "Probability of X=1"
  outputM[kk,2:(1+para$strata)] <- para$prob_condition_strata[1,]; kk = kk + 1
  outputM[kk,1] <- "Probability of U=1 given X=1"
  outputM[kk,2:(1+para$strata)] <- para$prob_condition_strata[2,]; kk = kk + 1
  outputM[kk,1] <- "Probability of U=1 given X=0"
  outputM[kk,2:(1+para$strata)] <- para$prob_condition_strata[3,]; kk = kk + 1
  outputM[kk,1] <- "Vaccination incremental coverage for X=1, U=1 (matrix - rows for months, columns for strata) **"; kk = kk + 1
  outputM[kk:(kk+para$months),1] <- 0:para$months
  outputM[kk:(kk+para$months),2:(1+para$strata)] <- para$vac_inc_coverage; kk = kk + para$months + 1
  outputM[kk,1] <- "Vaccination incremental coverage multiplier for X=0"
  outputM[kk,2:(1+para$strata)] <- para$vac_multiplyer[1,]; kk = kk + 1
  outputM[kk,1] <- "Vaccination incremental coverage multiplier for U=0"
  outputM[kk,2:(1+para$strata)] <- para$vac_multiplyer[2,]; kk = kk + 1
  outputM[kk,1] <- "Initial number of infected persons"
  outputM[kk,2] <- para$num_infected_persons; kk = kk + 1
  outputM[kk,1] <- "Number of contacts per day for each person"
  outputM[kk,2:(1+para$strata)] <-  para$contacts_per_day; kk = kk + 1
  outputM[kk,1] <- "Distribution of contacts - K by K matrix (rho)"; kk = kk + 1
  outputM[kk:(kk+para$strata-1),1] <- 1:para$strata
  outputM[kk:(kk+para$strata-1),2:(1+para$strata)] <- para$dist_contacts; kk = kk + para$strata
  outputM[kk,1] <- "Length of latent period (days)"
  outputM[kk,2:(1+para$strata)] <- para$latency_days; kk = kk + 1
  outputM[kk,1] <- "Length of infectious period (days)"
  outputM[kk,2:(1+para$strata)] <- para$infectious_days; kk = kk + 1
  outputM[kk,1] <- "Probability of illness given infection"
  outputM[kk,2:(1+para$strata)] <-  para$prob_illness; kk = kk + 1
  outputM[kk,1] <- "Relative infectiousness if not ill"
  outputM[kk,2:(1+para$strata)] <-  para$relative_infectious_not_ill; kk = kk + 1
  outputM[kk,1] <- "Transmission probabilities to unprotected for X=1 (matrix - rows for months, columns for strata)"; kk = kk + 1
  outputM[kk:(kk+para$months-1),1] <-  1:para$months
  outputM[kk:(kk+para$months-1),2:(1+para$strata)] <- para$prob_transmission; kk = kk + para$months
  outputM[kk,1] <- "Transmission probabilities multipliers for X=0 in unvaccinated or unprotected "
  outputM[kk,2:(1+para$strata)] <-  para$trans_prob_multiplier; kk = kk + 1
  outputM[kk,1] <- "Vaccine efficacy for X=1 (matrix - rows for months, columns for strata) ***"; kk = kk + 1
  outputM[kk:(kk+para$months-1),1] <- 1:para$months
  outputM[kk:(kk+para$months-1),2:(1+para$strata)] <- para$prob_transmission_vac; kk = kk + para$months
  outputM[kk,1] <- "Vaccine efficacy multipliers for X=0"
  outputM[kk,2:(1+para$strata)] <-  para$vec_efficacy_multiplier; kk = kk + 1
  outputM[kk,1] <- "Length of NFARI episode (days)"
  outputM[kk,2:(1+para$strata)] <-  para$length_ARI; kk = kk + 1
  outputM[kk,1] <- "Daily probabilities of onset of NFARI for X=1 in unvaccinated or unprotected persons"; kk = kk + 1
  outputM[kk:(kk+para$months-1),1] <- 1: para$months
  outputM[kk:(kk+para$months-1),2:(1+para$strata)] <- para$daily_prob_NFARI; kk = kk + para$months
  outputM[kk,1] <- "NFARI probabilities multipliers for vaccinated or protected persons"
  outputM[kk,2:(1+para$strata)] <-  para$probARI_vaccined_multiplier; kk = kk + 1
  outputM[kk,1] <- "NFARI probabilities multipliers for X=0"
  outputM[kk,2:(1+para$strata)] <-  para$probARI_X0_multiplier; kk = kk + 1
  outputM[kk,1] <- "Probability of visit for a case of FARI:"; kk = kk + 1
  outputM[kk,1] <- "For unvaccinated or unprotected with X=1 and U=1"
  outputM[kk,2:(1+para$strata)] <-  para$prob_visit_FARI; kk = kk + 1
  outputM[kk:(kk+2),1] <- c("Multiplier for vaccinated", "Multiplier for X=0", "Multiplier for U=0")
  outputM[kk:(kk+2),2:(1+para$strata)] <-  para$visit_FARI_multiplier; kk = kk + 3
  outputM[kk,1] <- "Probability of visit for a case of NFARI:"; kk = kk + 1
  outputM[kk,1] <- "For unvaccinated or unprotected with X=1 and U=1"
  outputM[kk,2:(1+para$strata)] <-  para$prob_visit_NFARI; kk = kk + 1
  outputM[kk:(kk+2),1] <- c("Multiplier for vaccinated", "Multiplier for X=0", "Multiplier for U=0")
  outputM[kk:(kk+2),2:(1+para$strata)] <- para$visit_NFARI_multiplier


  write.table(outputM, filename, row.names = FALSE, col.names = FALSE, sep=",")
}
