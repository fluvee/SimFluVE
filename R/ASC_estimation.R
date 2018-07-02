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