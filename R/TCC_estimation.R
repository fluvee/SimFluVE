find.sample <- function (data){
  require(dplyr)
  n <- all_size$final_size[data$Sim[1]]
  return(sample_n(data, n))
}


TCCestimation = function(SENS = 1, SPEC = 1, outcomes)
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
  TCC_summary <- outcomes[outcomes$EventCounter == 0,]
  TCC_ARI <- outcomes[outcomes$EventCounter != 0,]
  
  ARI_Visit0 <- TCC_ARI[TCC_ARI$Visit == 1 & (TCC_ARI$FARI == 1 | TCC_ARI$NFARI == 1),]
  ARI_Visit1 <- ARI_Visit0[order(ARI_Visit0$Sim,ARI_Visit0$Person),]
  ARI_Visit <- ARI_Visit1 %>% group_by(Sim,Person) %>% dplyr::filter(row_number() == 1)
  
  ARI_Test <- ARI_Visit
  ARI_Test$X_FARI = runif(nrow(ARI_Test),0,1)
  ARI_Test$X_NFARI = runif(nrow(ARI_Test),0,1)
  ARI_Test$T = case_when(
    ARI_Test$FARI == 1 & ARI_Test$X_FARI <= SENS ~ 1,
    ARI_Test$NFARI == 1 & ARI_Test$X_NFARI <= 1-SPEC ~ 1,
    ARI_Test$FARI == 0 & ARI_Test$NFARI == 0 ~ 999,
    TRUE ~ 0
  )
  ARI_Case <- ARI_Test[ARI_Test$T == 1,]
  ARI_Case$C <- 1
  ARI_Case_Test <- ARI_Case[,c("Sim","Person","EventCounter","T")]
  Case0 <- ARI_Case[,c("Sim","Person","C")]
  
  require("plyr")
  case_size <- plyr::count(Case0, "Sim")
  case_size <- plyr::rename(case_size, replace = c("freq" = "CaseSize"))
  
  by_sim_person <- outcomes %>% group_by(Sim,Person)
  ARI_Count0 <- by_sim_person %>% dplyr::summarise(
    sumFARI = sum(FARI),
    sumNFARI = sum(NFARI)
  )
  
  ARI_Count1 <- by_sim_person %>% dplyr::filter(row_number() == max(row_number()))
  ARI_Count <- merge(ARI_Count0,ARI_Count1,by=c("Sim","Person"))
  ARI_Count$sumARI = ARI_Count$sumFARI + ARI_Count$sumNFARI
  
  Control0 <- ARI_Count[ARI_Count$sumARI == 0, c("Sim","Person")]
  Control0$C <- 2
  control_size <- plyr::count(Control0,"Sim")
  control_size <- plyr::rename(control_size, replace = c("freq" = "ControlSize"))
  
  all_size <- merge(case_size,control_size, by = "Sim", all = TRUE)
  all_size$final_size <- pmin(all_size$CaseSize, all_size$ControlSize)
  all_size[is.na(all_size$final_size),]$final_size = 0
  
  control_ID <- mapply(find.sample, 
                       split(ungroup(Control0), Control0$Sim), 
                       SIMPLIFY = FALSE) %>% bind_rows
  Case_ID <- mapply(find.sample, 
                    split(ungroup(Case0), Case0$Sim), 
                    SIMPLIFY = FALSE) %>% bind_rows
  
  
  Summary_Case <- merge(Case_ID, TCC_summary, all.x = TRUE, by = c("Sim","Person"))
  Summary_Case$C <- 1
  Summary_Case$T <- 999
  ARI_Case_AllEvents0 <- merge(Case_ID, TCC_ARI, all.x = TRUE, by = c("Sim", "Person"))
  ARI_Case_AllEvents <- merge(ARI_Case_AllEvents0, ARI_Case_Test, all = TRUE, by = c("Sim", "Person", "EventCounter"))
  
  require("plyr")
  Case <- plyr::rbind.fill(Summary_Case, ARI_Case_AllEvents)
  
  Summary_control <- merge(control_ID, TCC_summary, all.x = TRUE, by = c("Sim", "Person"))
  Summary_control$T <- 999
  ARI_control <- merge(control_ID, TCC_ARI, all.x = TRUE, by = c("Sim", "Person"))
  ARI_control$T = as.numeric(NA)
  Control <- plyr::rbind.fill(Summary_control,ARI_control)
  Control <- Control[!is.na(Control$STR),]
  
  new_outcomes <- plyr::rbind.fill(Case,Control)
  new_outcomes_summary <- new_outcomes[new_outcomes$EventCounter == 0,]
  new_outcomes_summary0 <- plyr::count(new_outcomes_summary,c("Sim","STR","C","DEFVAC"))
  new_outcomes_summary11 <- new_outcomes_summary0[new_outcomes_summary0$C == 1 & new_outcomes_summary0$DEFVAC == 1,c(-3,-4)]
  new_outcomes_summary11_new <- plyr::rename(new_outcomes_summary11, replace = c("freq" = "Case_Vac"))
  new_outcomes_summary1999 <- new_outcomes_summary0[new_outcomes_summary0$C == 1 & new_outcomes_summary0$DEFVAC == 999,c(-3,-4)]
  new_outcomes_summary1999_new <- plyr::rename(new_outcomes_summary1999, replace = c("freq" = "Case_Unvac"))
  new_outcomes_summary21 <- new_outcomes_summary0[new_outcomes_summary0$C == 2 & new_outcomes_summary0$DEFVAC == 1,c(-3,-4)]
  new_outcomes_summary21_new <- plyr::rename(new_outcomes_summary21, replace = c("freq" = "Control_Vac"))
  new_outcomes_summary2999 <- new_outcomes_summary0[new_outcomes_summary0$C == 2 & new_outcomes_summary0$DEFVAC == 999,c(-3,-4)]
  new_outcomes_summary2999_new <- plyr::rename(new_outcomes_summary2999, replace = c("freq" = "Control_Unvac"))
  EstVE <- merge(new_outcomes_summary11_new, new_outcomes_summary1999_new, by = c("Sim","STR"))
  EstVE <- merge(EstVE, new_outcomes_summary21_new, by = c("Sim","STR"))
  EstVE <- merge(EstVE, new_outcomes_summary2999_new, by = c("Sim","STR"))
  EstVE$Odds_Case <- EstVE$Case_Vac/EstVE$Case_Unvac
  EstVE$Odds_Control <- EstVE$Control_Vac/EstVE$Control_Unvac
  EstVE$estVE <- 1 - EstVE$Odds_Case/EstVE$Odds_Control
  
  return(summary(EstVE[,c(-1,-2)]))
}