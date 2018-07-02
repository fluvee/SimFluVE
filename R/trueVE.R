trueVE = function(filename)
{
  para <- readFLUVEE(filename)
  X1 <- para$prob_condition_strata[1,]
  U1_X1 <- para$prob_condition_strata[2,]
  U1_X0 <- para$prob_condition_strata[3,]
  VAC_X1U1 <- para$vac_inc_coverage[1,]
  VAC_Mul_X0 <- para$vac_multiplyer[1,]
  VAC_Mul_U0 <- para$vac_multiplyer[2,]

  PVAC_X1U1 <- VAC_X1U1
  PVAC_X1U0 <- PVAC_X1U1 * VAC_Mul_U0;
  PVAC_X0U1 <- PVAC_X1U1 * VAC_Mul_X0;
  PVAC_X0U0 <- PVAC_X1U1 * VAC_Mul_X0 * VAC_Mul_U0;

  PX1U1 <- U1_X1 * X1;
  PX0U1 <- U1_X0 * (1 - X1);
  PX1U0 <- (1 - U1_X1) * X1;
  PX0U0 <- (1 - U1_X0) * (1 - X1);

  VacCov_True <- PVAC_X1U1 * PX1U1 + PVAC_X1U0 * PX1U0 + PVAC_X0U1 * PX0U1 + PVAC_X0U0 * PX0U0;

  para_mod <- para
  para_mod$vac_inc_coverage <- matrix(0,nrow = nrow(para$vac_inc_coverage), ncol = ncol(para$vac_inc_coverage))
  para_mod$vac_inc_coverage[1,] <- VacCov_True
  para_mod$vac_multiplyer[1,] <- 1
  para_mod$vac_multiplyer[2,] <- 1

 # newname <- gsub(".csv","_modif.csv",filename, ignore.case=T)
 # saveFLUVEE(para_mod,newname)

  para_mod["population_report_file"] <- TRUE
  simflu6(para_mod)
  TrueVE_outcomes <- read.csv(paste(getwd(),"/",gsub(" ", "_",para_mod$title), "_Outcomes_File_.csv",sep = ""))

  require('plyr')
  summary_True0 <- TrueVE_outcomes[TrueVE_outcomes$EventCounter == 0,c(-6,-8)]
  summary_True_Vac0 <- plyr::count(summary_True0,c("Sim","STR","DEFVAC"))
  summary_True_VAC <- summary_True_Vac0[summary_True_Vac0$DEFVAC == 1,-3]
  summary_True_VAC_new <- plyr::rename(summary_True_VAC, replace = c("freq" = "N_VAC"))
  summary_True_UNVAC <- summary_True_Vac0[summary_True_Vac0$DEFVAC == 999,-3]
  summary_True_UNVAC_new <- plyr::rename(summary_True_UNVAC, replace = c("freq" = "N_UNVAC"))
  summary_True <- merge(summary_True_VAC_new,summary_True_UNVAC_new, by = c("Sim","STR"))
  ARI_True0 <- TrueVE_outcomes[TrueVE_outcomes$EventCounter != 0,]
  ARI_True_0_sort <- ARI_True0[order(ARI_True0$Sim,ARI_True0$Person),]


  require(dplyr)
  ARI_True_0_sort$SI_U_Mark = case_when(
    ARI_True_0_sort$Type == 2 &  ARI_True_0_sort$VacStatus == 0 ~ 1,
    TRUE ~ 0
  )
  ARI_True_0_sort$SI_V_Mark = case_when(
    ARI_True_0_sort$Type == 2 &  ARI_True_0_sort$VacStatus == 1 ~ 1,
    TRUE ~ 0
  )
  ARI_True_0_sort$MAI_U_Mark = case_when(
    ARI_True_0_sort$Type == 2 &  ARI_True_0_sort$VacStatus == 0 & ARI_True_0_sort$Visit == 1 ~ 1,
    TRUE ~ 0
  )
  ARI_True_0_sort$MAI_V_Mark = case_when(
    ARI_True_0_sort$Type == 2 &  ARI_True_0_sort$VacStatus == 1 & ARI_True_0_sort$Visit == 1 ~ 1,
    TRUE ~ 0
  )
  by_sim_person <- ARI_True_0_sort %>% group_by(Sim,Person)
  ARI_True1 <- by_sim_person %>% dplyr::summarise(
    SI_U = sum(SI_U_Mark),
    SI_V = sum(SI_V_Mark),
    MAI_U = sum(MAI_U_Mark),
    MAI_V = sum(MAI_V_Mark)
  )
  ARI_True1_new <- ARI_True1 %>% group_by(Sim,Person) %>% dplyr::filter(row_number() == max(row_number()))
  ARI_True1_sort <- ARI_True1_new[order(ARI_True1_new$Sim,ARI_True1_new$STR),c("Sim","Person","STR","SI_U","SI_V","MAI_U","MAI_V")]

  by_sim_STR <- ARI_True1_sort %>% group_by(Sim,STR)
  ARI_True <- by_sim_STR %>% dplyr::summarise(
    sumSI_U = sum(SI_U),
    sumSI_V = sum(SI_V),
    sumMAI_U = sum(MAI_U),
    sumMAI_V = sum(MAI_V)
  )
  Results_detailed <- merge(summary_True,ARI_True, by = c("Sim","STR"))
  Results_detailed$ARS_V = Results_detailed$sumSI_V/Results_detailed$N_VAC
  Results_detailed$ARS_U = Results_detailed$sumSI_U/Results_detailed$N_UNVAC
  Results_detailed$VE_SI = 1 - Results_detailed$ARS_V/Results_detailed$ARS_U
  Results_detailed$ARM_V = Results_detailed$sumMAI_V/Results_detailed$N_VAC
  Results_detailed$ARM_U = Results_detailed$sumMAI_U/Results_detailed$N_UNVAC
  Results_detailed$VE_MAI = 1 - Results_detailed$ARM_V/Results_detailed$ARM_U

  TrueVE <- list()
  TrueVE$VE_SI = mean(Results_detailed$VE_SI)
  TrueVE$VE_MAI = mean(Results_detailed$VE_MAI)

  return(TrueVE)
}

ASCestimation = function(N = 1000, SENS = 1, SPEC = 1, outcomes)
{
  require(dplyr)
  outcomes$FARI = case_when(
    outcomes$Type == 2 ~ 1,
    TRUE ~ 0
  )
  outcomes$NFARI = case_when(
    outcomes$Type == 3 ~ 1,
    TRUE ~ 0
  )
  outcomes$DFluPos = case_when(
    outcomes$Type == 999 | outcomes$Type == 1 | outcomes$Type == 3 ~ 999,
    outcomes$Type == 2 ~ as.double(outcomes$OnsetDay),
    TRUE ~ 0
  )
  outcomes$DNFluPos = case_when(
    outcomes$Type == 999 | outcomes$Type == 1 | outcomes$Type == 2 ~ 999,
    outcomes$Type == 3 ~ as.double(outcomes$OnsetDay),
    TRUE ~ 0
  )
  ASC_summary <- outcomes[outcomes$EventCounter == 0,]
  ASC_ARI <- outcomes[outcomes$EventCounter != 0,]

  by_sim <- ASC_summary %>% group_by(Sim)
  ASC_sample <- sample_n(by_sim,size=N)
  summary_sample <- merge(ASC_summary,ASC_sample,all.y = TRUE)

  require('plyr')
  summary_sample_vac0 <- plyr::count(summary_sample,c("Sim","STR","DEFVAC"))
  summary_sample_vac1 <- summary_sample_vac0[summary_sample_vac0$DEFVAC == 1,-3]
  summary_sample_vac1_new <- plyr::rename(summary_sample_vac1, replace = c("freq" = "N_VAC"))
  summary_sample_vac2 <- summary_sample_vac0[summary_sample_vac0$DEFVAC == 999,-3]
  summary_sample_vac2_new <- plyr::rename(summary_sample_vac2, replace = c("freq" = "N_UNVAC"))
  summary_sample_vac <- merge(summary_sample_vac1_new,summary_sample_vac2_new, by = c("Sim","STR"))

  ARI_Sample <- merge(ASC_sample[,c("Sim","Person")],ASC_ARI,all.x = TRUE)
  ARI_Sample <- ARI_Sample[!is.na(ARI_Sample$STR),]
  ARI_Sample$X_FARI = runif(nrow(ARI_Sample),0,1)
  ARI_Sample$X_NFARI = runif(nrow(ARI_Sample),0,1)
  ARI_Sample$T = case_when(
    ARI_Sample$FARI == 1 & ARI_Sample$X_FARI <= SENS ~ 1,
    ARI_Sample$NFARI == 1 & ARI_Sample$X_NFARI <= 1-SPEC ~ 1,
    ARI_Sample$FARI == 0 & ARI_Sample$NFARI == 0 ~ 999,
    TRUE ~ 0
  )

  new_outcomes <- plyr::rbind.fill(summary_sample,ARI_Sample)
  new_outcomes[is.na(new_outcomes$T),]$T <- 999

  ARI_Test0 <- ARI_Sample[order(ARI_Sample$Sim,ARI_Sample$Person),]
  ARI_Test0_g <- ARI_Test0 %>% group_by(Sim, Person)
  ARI_Test0_g_pos <- ARI_Test0_g %>% filter(.,T>0) %>% filter(.,EventCounter==min(EventCounter))
  ARI_Test0_g_neg <- ARI_Test0_g %>% filter(.,T==0)
  ARI_Test <- merge(ARI_Test0_g_pos,ARI_Test0_g_neg,all=TRUE)

  ARI_sample_T0 <- plyr::count(ARI_Test,c("Sim","STR","DEFVAC","T"))
  ARI_Sample_T1 <- ARI_sample_T0[ARI_sample_T0$T == 1,-4]
  ARI_Sample_T1_new <- plyr::rename(ARI_Sample_T1, replace = c("freq" = "TPos_Count"))
  ARI_Sample_T2 <- ARI_Sample_T1_new[ARI_Sample_T1_new$DEFVAC == 1,-3]
  ARI_Sample_T2_new <- rename(ARI_Sample_T2, replace = c("TPos_Count" = "VAC_TPos"))
  ARI_Sample_T3 <- ARI_Sample_T1_new[ARI_Sample_T1_new$DEFVAC == 999,-3]
  ARI_Sample_T3_new <- rename(ARI_Sample_T3, replace = c("TPos_Count" = "UNVAC_TPos"))
  ARI_Sample_FARI <- merge(ARI_Sample_T2_new,ARI_Sample_T3_new, by = c("Sim","STR"))

  EstVE <- merge(summary_sample_vac,ARI_Sample_FARI, all.x = TRUE, all = TRUE)
  EstVE[is.na(EstVE)] <- 0
  EstVE$AR_Vac = EstVE$VAC_TPos/EstVE$N_VAC
  EstVE$AR_UNVAC = EstVE$UNVAC_TPos/EstVE$N_UNVAC
  EstVE$estVE = 1 - EstVE$AR_Vac/EstVE$AR_UNVAC
  EstVE_new <- rename(EstVE, replace = c("VAC_TPos" = "Cases_Vac", "UNVAC_TPos" = "Cases_Unvac"))

  return(summary(EstVE_new[,c(-1,-2)]))

}
