PSCestimation = function(N = 1000, SENS = 1, SPEC = 1, outcomes)
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
  PSC_summary <- outcomes[outcomes$EventCounter == 0,]
  PSC_ARI <- outcomes[outcomes$EventCounter != 0,]
  
  require('dplyr')
  by_sim <- PSC_summary %>% group_by(Sim)
  PSC_sample <- sample_n(by_sim,size=1000)
  summary_sample <- merge(PSC_summary,PSC_sample,all.y = TRUE)
  
  ARI_Sample0 <- merge(PSC_sample[,c("Sim","Person")],PSC_ARI,all.x = TRUE)
  ARI_Sample <- ARI_Sample0[ARI_Sample0$Visit == 1 & ARI_Sample0$Type != 1,]
  ARI_Sample <- na.omit(ARI_Sample)
  ARI_Sample$X_FARI = runif(nrow(ARI_Sample),0,1) 
  ARI_Sample$X_NFARI = runif(nrow(ARI_Sample),0,1)
  ARI_Sample$T = case_when(
    ARI_Sample$FARI == 1 & (!is.na(ARI_Sample$X_FARI)) & ARI_Sample$X_FARI <= SENS ~ 1,
    ARI_Sample$NFARI == 1 & (!is.na(ARI_Sample$X_NFARI)) & ARI_Sample$X_NFARI <= 1-SPEC ~ 1,
    is.na(ARI_Sample$X_FARI) | is.na(ARI_Sample$X_NFARI) ~ 999,
    ARI_Sample$FARI == 0 & ARI_Sample$NFARI == 0 ~ 999,
    TRUE ~ 0
  )
  
  require('plyr')
  new_outcomes <- plyr::rbind.fill(summary_sample,ARI_Sample)
  new_outcomes[is.na(new_outcomes$T),]$T <- 999 
  
  ARI_Test0 <- ARI_Sample[order(ARI_Sample$Sim,ARI_Sample$Person),]
  ARI_Test0_g <- ARI_Test0 %>% group_by(Sim, Person) 
  ARI_Test0_g_pos <- ARI_Test0_g %>% filter(.,T>0) %>% filter(.,EventCounter==min(EventCounter))
  ARI_Test0_g_neg <- ARI_Test0_g %>% filter(.,T==0)
  ARI_Test <- merge(ARI_Test0_g_pos,ARI_Test0_g_neg,all=TRUE)
  
  Case_ID0 <- ARI_Test[ARI_Test$T == 1,c("Sim","Person")]
  Case_ID0$Case <- 1
  Case_ID <- Case_ID0[!duplicated(Case_ID0),]
  
  final_summary <- merge(summary_sample,Case_ID, all.x = TRUE)
  final_summary[is.na(final_summary$Case),]$Case <- 2
  final_summary$Vacc = case_when(
    final_summary$DEFVAC == 1 ~ 1,
    TRUE ~ 2
  )

  
  require("plyr")
  Sample_vac0 <- plyr::count(final_summary,c("Sim","STR","Vacc"))
  Sample_vac1 <- Sample_vac0[Sample_vac0$Vacc == 1,-3]
  Sample_vac1_new <- plyr::rename(Sample_vac1, replace = c("freq" = "N_VACC"))
  Sample_vac2 <- Sample_vac0[Sample_vac0$Vacc == 2,-3]
  Sample_vac2_new <- plyr::rename(Sample_vac2, replace = c("freq" = "N_UNVACC"))
  Sample_vac2_final <- merge(Sample_vac1_new,Sample_vac2_new, by = c("Sim","STR"))
  
  Sample_vac3 <- plyr::count(final_summary[final_summary$Case == 1,],c("Sim","STR","Vacc"))
  Sample_vac4 <- Sample_vac3[Sample_vac3$Vacc == 1,-3]
  Sample_vac4_new <- plyr::rename(Sample_vac4, replace = c("freq" = "Vacc_Case"))
  Sample_vac5 <- Sample_vac3[Sample_vac3$Vacc == 2,-3]
  Sample_vac5_new <- plyr::rename(Sample_vac5, replace = c("freq" = "Unvacc_Case"))
  Sample_vac5_final <- merge(Sample_vac4_new,Sample_vac5_new, by = c("Sim","STR"))
  
  EstVE <- merge(Sample_vac2_final,Sample_vac5_final)
  EstVE[is.na(EstVE)] <- 0
  EstVE$AR_Vac = EstVE$Vacc_Case/EstVE$N_VACC
  EstVE$AR_UNVAC = EstVE$Unvacc_Case/EstVE$N_UNVACC
  EstVE$estVE = 1 - EstVE$AR_Vac/EstVE$AR_UNVAC
  EstVE_new <- rename(EstVE, replace = c("Vacc_Case" = "Cases_Vac", "Unvacc_Case" = "Cases_Unvac"))
  
  return(summary(EstVE[,c(-1,-2)]))
  
}