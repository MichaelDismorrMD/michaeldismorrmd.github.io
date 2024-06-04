### Creating function for EuroSCORE 2


calc_euroscore <- function(data.frame) {

   euroscore <- data.frame %>% mutate(est_NYHA = case_when(EURO_NYHA == 2 ~ 0.1070545, 
                                              EURO_NYHA == 3 ~ 0.2958358,
                                              EURO_NYHA == 4 ~ 0.5597929,
                                              EURO_NYHA == 1 ~ 0,
                                              EURO_NYHA == 9 ~ NA_real_),
                                      est_ccs4 = case_when(EURO_CCS4 == 9 ~ NA_real_,
                                                           TRUE ~ 0.2226147 * EURO_CCS4),
                                      est_IDDM = case_when(is.na(DIABETESINSULIN) ~ 0,
                                                           .default = 0.3542749 * DIABETESINSULIN),
                                      est_age = case_when(EURO_AGE <= 60 ~ 0.0285181,
                                                          EURO_AGE > 60 ~ (EURO_AGE - 59) * 0.0285181),
                                      est_female = case_when(EURO_SEX == 2 ~ 0.2196434,
                                                             EURO_SEX == 1 ~ 0),
                                      est_eca = case_when(EURO_EXTRACARDIAC == 9 ~ NA_real_,
                                                          TRUE ~ 0.5360268 * EURO_EXTRACARDIAC),
                                      est_cpd = case_when(EURO_CHRONICPULM == 9 ~ NA_real_,
                                                          TRUE ~ 0.1886564 * EURO_CHRONICPULM),
                                      est_neuro = case_when(EURO_POORMOBILITY == 9 ~ NA_real_,
                                                            TRUE ~ 0.2407181 * EURO_POORMOBILITY),
                                      est_PREVCARDSURG = case_when(EURO_PREVCARDSURG == 9 ~ NA_real_,
                                                               TRUE ~ 1.118599 * EURO_PREVCARDSURG),
                                      est_cc = case_when(DIALYS == 1 ~ 0.6421508,
                                                         EURO_CREATININE_CLEAREANCE <= 50 ~ 0.8592256,
                                                         EURO_CREATININE_CLEAREANCE > 50 & EURO_CREATININE_CLEAREANCE <= 85 ~ 0.303553,
                                                         EURO_CREATININE_CLEAREANCE > 85 ~ 0),
                                      est_AE = case_when(EURO_ENDOCARDITIS == 9 ~ NA_real_,
                                                         TRUE ~ 0.6194522 * EURO_ENDOCARDITIS),
                                      est_critical = case_when(EURO_CRITICAL == 9 ~ NA_real_,
                                                               TRUE ~ 1.086517 * EURO_CRITICAL),
                                      est_LV = case_when(EURO_VK_EJFRACT2 == 0 ~ 0,
                                                         EURO_VK_EJFRACT2 == 1 ~ 0.3150652,
                                                         EURO_VK_EJFRACT2 == 2 ~ 0.8084096,
                                                         EURO_VK_EJFRACT2 == 3 ~ 0.9346919),
                                      est_hjinf = case_when(EURO_HJINF == 9 ~ NA_real_,
                                                            TRUE ~ 0.1528943 * EURO_HJINF),
                                      est_pulmonchoice = case_when(EURO_PULMONCHOICE == 0 ~ 0, 
                                                                   EURO_PULMONCHOICE == 1 ~ 0.1788899,
                                                                   EURO_PULMONCHOICE == 2 ~ 0.3491475), 
                                      est_urgency = case_when(EURO_URGENCY == 1 ~ 0,
                                                              EURO_URGENCY == 2 ~ 0.3174673, 
                                                              EURO_URGENCY == 3 ~ 0.7039121,
                                                              EURO_URGENCY == 4 ~ 1.362947),
                                      est_weightofprocedure = case_when(EURO_WEIGHTOFPROCEDURE == 0 ~ 0,
                                                                        EURO_WEIGHTOFPROCEDURE == 1 ~ 0.0062118,
                                                                        EURO_WEIGHTOFPROCEDURE == 2 ~ 0.5521478,
                                                                        EURO_WEIGHTOFPROCEDURE == 3 ~ 0.9724533),
                                      est_THORACALAORTA = case_when(EURO_THORACALAORTA == 9 ~ NA_real_,
                                                               TRUE ~ 0.6527205 * EURO_THORACALAORTA),
                                      est_sum = est_NYHA + est_ccs4 + est_IDDM + est_age + est_female + est_eca + est_cpd + est_neuro +
                                                est_PREVCARDSURG + est_cc + est_AE + est_critical + est_LV + est_hjinf + est_pulmonchoice +
                                                est_urgency + est_weightofprocedure + est_THORACALAORTA,
                                      euro_new = exp(-5.324537 + est_sum)/(1 + exp(-5.324537 + est_sum)),
                                      euro_new = round(euro_new * 100, 2)) %>%
                                      pull(euro_new)
                                      

                               return(euroscore)
}


#### Trying out EUROSCORE with test data.
test_data <- data.frame(EURO_NYHA = c(2, 9),
                             EURO_CCS4 = c(1, 1),
                             DIABETESINSULIN = c(1, NA), 
                             EURO_AGE = c(70, 9), 
                             EURO_SEX = c(2, 2),  
                             EURO_EXTRACARDIAC = c(1, 1),
                             EURO_CHRONICPULM = c(1, 9),
                             EURO_POORMOBILITY = c(1, 1), 
                             EURO_PREVCARDSURG = c(1, 1), 
                             DIALYS = c(1, 0), 
                             EURO_CREATININE_CLEAREANCE = c(40, 40), 
                             EURO_ENDOCARDITIS = c(1, 9), 
                             EURO_CRITICAL = c(1, 9), 
                             EURO_VK_EJFRACT2 = c(2, 2), 
                             EURO_HJINF = c(1, 1),
                             EURO_PULMONCHOICE = c(1, 1),
                             EURO_URGENCY = c(1, 1),
                             EURO_WEIGHTOFPROCEDURE = c(1, 1),
                             EURO_THORACALAORTA = c(1, 1))

calc_euroscore(test_data)


test_data$euroscore <- calc_euroscore(test_dataframe)


