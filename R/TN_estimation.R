TNestimation = function(SENS = 1, SPEC = 1, outcomes)
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
  TN_summary <- outcomes[outcomes$EventCounter == 0,]
  TN_ARI <- outcomes[outcomes$EventCounter != 0,]
  
  ARI_Visit0 <- TN_ARI[TN_ARI$Visit == 1 & (TN_ARI$FARI == 1 | TN_ARI$NFARI == 1),]
  ARI_Visit1 <- ARI_Visit0[order(ARI_Visit0$Sim,ARI_Visit0$Person),]
  ARI_Visit <- ARI_Visit1 %>% group_by(Sim,Person) %>% dplyr::filter(row_number() == 1)
  
  Sample_ID <- ARI_Visit[,c("Sim","Person")]
  summary_sample <- merge(Sample_ID,TN_summary, all.x = TRUE)
  
  ARI_Sample <- ARI_Visit
  ARI_Sample$X_FARI = runif(nrow(ARI_Sample),0,1) 
  ARI_Sample$X_NFARI = runif(nrow(ARI_Sample),0,1)
  ARI_Sample$T = case_when(
    ARI_Sample$FARI == 1 & ARI_Sample$X_FARI <= SENS ~ 1,
    ARI_Sample$NFARI == 1 & ARI_Sample$X_NFARI <= 1-SPEC ~ 1,
    ARI_Sample$FARI == 0 & ARI_Sample$NFARI == 0 ~ 999,
    TRUE ~ 0
  )
  
  require("plyr")
  new_outcomes <- plyr::rbind.fill(summary_sample,ARI_Sample)
  new_outcomes$T[is.na(new_outcomes$T)] <- 999
  
  ARI_Sample_T0 <- plyr::count(ARI_Sample,c("Sim","STR","T","DEFVAC"))
  ARI_Sample_T01 <- ARI_Sample_T0[ARI_Sample_T0$T == 0 & ARI_Sample_T0$DEFVAC == 1,c(-3,-4)]
  ARI_Sample_T01_new <- plyr::rename(ARI_Sample_T01, replace = c("freq" = "Control_Vac"))
  ARI_Sample_T0999 <- ARI_Sample_T0[ARI_Sample_T0$T == 0 & ARI_Sample_T0$DEFVAC == 999,c(-3,-4)]
  ARI_Sample_T0999_new <- plyr::rename(ARI_Sample_T0999, replace = c("freq" = "Control_Unvac"))
  ARI_Sample_T11 <- ARI_Sample_T0[ARI_Sample_T0$T == 1 & ARI_Sample_T0$DEFVAC == 1,c(-3,-4)]
  ARI_Sample_T11_new <- plyr::rename(ARI_Sample_T11, replace = c("freq" = "Case_Vac"))
  ARI_Sample_T1999 <- ARI_Sample_T0[ARI_Sample_T0$T == 1 & ARI_Sample_T0$DEFVAC == 999,c(-3,-4)]
  ARI_Sample_T1999_new <- plyr::rename(ARI_Sample_T1999, replace = c("freq" = "Case_Unvac"))
  EstVE <- merge(ARI_Sample_T01_new, ARI_Sample_T0999_new, by = c("Sim","STR"))
  EstVE <- merge(EstVE, ARI_Sample_T11_new, by = c("Sim","STR"))
  EstVE <- merge(EstVE, ARI_Sample_T1999_new, by = c("Sim","STR"))
  EstVE$Odds_Case <- EstVE$Case_Vac/EstVE$Case_Unvac
  EstVE$Odds_Control <- EstVE$Control_Vac/EstVE$Control_Unvac
  EstVE$estVE <- 1 - EstVE$Odds_Case/EstVE$Odds_Control
  
  return(summary(EstVE[,c(-1,-2)]))
  
}