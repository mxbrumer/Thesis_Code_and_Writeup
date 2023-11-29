#Compute Latent Variables

pacman::p_load(DescTools, naniar, tidyverse, hablar, mice)

##Load Data######################################################################
#Load Data

US_nomiss <- read.csv('US Clean Temporary No Missing.csv', 
               header = T,
               na.strings = c("", "NA"))

DK_nomiss <- read.csv('DK Clean Temporary No Missing.csv', 
               header = T,
               na.strings = c("", "NA"))


##MEIM-R####
#US
US_nomiss$EIE <- rowMeans(US_nomiss[c("MEIM_R1", "MEIM_R4", "MEIM_R5")], 
                          na.rm = T
                          )


US_nomiss$EIC <- rowMeans(US_nomiss[c("MEIM_R2", "MEIM_R3", "MEIM_R6")],
                          na.rm = T
                          )

#DK
DK_nomiss$EIE <- rowMeans(DK_nomiss[c("MEIM_R1", "MEIM_R4", "MEIM_R5")], 
                          na.rm = T
                          )


DK_nomiss$EIC <- rowMeans(DK_nomiss[c("MEIM_R2", "MEIM_R3", "MEIM_R6")],
                          na.rm = T
                          )

##COVID Stress Scale####
#US
US_nomiss$CSS_Danger_Contamination <- rowMeans(US_nomiss[c("CSS_1_1", "CSS_1_2", 
                                                           "CSS_1_3", "CSS_1_4", 
                                                           "CSS_1_5", "CSS_1_6",
                                                           "CSS_1_19", "CSS_1_20", 
                                                           "CSS_1_21", "CSS_1_22", 
                                                           "CSS_1_23", "CSS_1_24")],
                                               na.rm = T
                                               )

US_nomiss$CSS_SEC <- rowMeans(US_nomiss[c("CSS_1_7", "CSS_1_8", "CSS_1_9", 
                                          "CSS_1_10", "CSS_1_11", "CSS_1_12")],
                              na.rm = T
                              )

US_nomiss$Xenophobia <- rowMeans(US_nomiss[c("CSS_1_13", "CSS_1_14", "CSS_1_15", 
                                             "CSS_1_16", "CSS_1_17", "CSS_1_18")],
                                 na.rm = T
                                 )

US_nomiss$TS <- rowMeans(US_nomiss[c("CSS_2_1", "CSS_2_2", "CSS_2_3", "CSS_2_4", 
                                     "CSS_2_5", "CSS_2_6")],
                         na.rm = T
                         )

US_nomiss$Checking <- rowMeans(US_nomiss[c("CSS_3_1", "CSS_3_2", "CSS_3_3", 
                                           "CSS_3_4", "CSS_3_5", "CSS_3_6")],
                               na.rm = T
                               )

#DK
DK_nomiss$CSS_Danger_Contamination <- rowMeans(DK_nomiss[c("CSS_1_1", "CSS_1_2", 
                                                           "CSS_1_3", "CSS_1_4", 
                                                           "CSS_1_5", "CSS_1_6",
                                                           "CSS_1_19", "CSS_1_20", 
                                                           "CSS_1_21", "CSS_1_22", 
                                                           "CSS_1_23", "CSS_1_24")],
                                               na.rm = T
                                               )

DK_nomiss$CSS_SEC <- rowMeans(DK_nomiss[c("CSS_1_7", "CSS_1_8", "CSS_1_9", 
                                          "CSS_1_10", "CSS_1_11", "CSS_1_12")],
                              na.rm = T
                              )

DK_nomiss$Xenophobia <- rowMeans(DK_nomiss[c("CSS_1_13", "CSS_1_14", "CSS_1_15", 
                                             "CSS_1_16", "CSS_1_17", "CSS_1_18")],
                                 na.rm = T
                                 )

DK_nomiss$TS <- rowMeans(DK_nomiss[c("CSS_2_1", "CSS_2_2", "CSS_2_3", "CSS_2_4", 
                                     "CSS_2_5", "CSS_2_6")],
                         na.rm = T
                         )

DK_nomiss$Checking <- rowMeans(DK_nomiss[c("CSS_3_1", "CSS_3_2", "CSS_3_3", 
                                           "CSS_3_4", "CSS_3_5", "CSS_3_6")],
                               na.rm = T
                               )

##Rosenberg Self-Esteem####
#US
US_nomiss$RSES_Mean <- rowMeans(US_nomiss[c("RSES_1", "RSES_2", "RSES_3", 
                                            "RSES_4", "RSES_5","RSES_6", 
                                            "RSES_7", "RSES_8", "RSES_9", 
                                            "RSES_10")],
                                na.rm = T
                                )

#DK
DK_nomiss$RSES_Mean <- rowMeans(DK_nomiss[c("RSES_1", "RSES_2", "RSES_3", 
                                            "RSES_4", "RSES_5","RSES_6", 
                                            "RSES_7", "RSES_8", "RSES_9", 
                                            "RSES_10")],
                                na.rm = T
                                )

##General Belongingness Scale####
#US
US_nomiss$GBS_Mean <- rowMeans(US_nomiss[c("GBS_1", "GBS_2", "GBS_3", "GBS_4", 
                                           "GBS_5", "GBS_6", "GBS_7", "GBS_8", 
                                           "GBS_9", "GBS_10", "GBS_11", 
                                           "GBS_12")],
                               na.rm = T
                               )

#DK
DK_nomiss$GBS_Mean <- rowMeans(DK_nomiss[c("GBS_1", "GBS_2", "GBS_3", "GBS_4", 
                                           "GBS_5", "GBS_6", "GBS_7", "GBS_8", 
                                           "GBS_9", "GBS_10", "GBS_11", 
                                           "GBS_12")],
                               na.rm = T
                               )

##Revised Autonomy Scale####
#US
US_nomiss$RAS_Mean <- rowMeans(US_nomiss[c("RAS_1", "RAS_2", "RAS_3", "RAS_4")],
                               na.rm = T
                               )

#DK
DK_nomiss$RAS_Mean <- rowMeans(DK_nomiss[c("RAS_1", "RAS_2", "RAS_3", "RAS_4")],
                               na.rm = T
                               )

##Fulfillment Scale####
#US
US_nomiss$FS_Mean <- rowMeans(US_nomiss[c("FS_1", "FS_2", "FS_3", "FS_4", "FS_5")],
                              na.rm = T
                              )

#DK
DK_nomiss$FS_Mean <- rowMeans(DK_nomiss[c("FS_1", "FS_2", "FS_3", "FS_4", "FS_5")],
                              na.rm = T
                              )

##Reduced Multidimensional Scale of Perceived Social Support####
#US
US_nomiss$RMSPSS_Mean <- rowMeans(US_nomiss[c("RMSPSS_1", "RMSPSS_2", 
                                              "RMSPSS_3", "RMSPSS_4", 
                                              "RMSPSS_5", "RMSPSS_6")],
                                  na.rm = T
                                  )

#DK
DK_nomiss$RMSPSS_Mean <- rowMeans(DK_nomiss[c("RMSPSS_1", "RMSPSS_2", 
                                              "RMSPSS_3", "RMSPSS_4", 
                                              "RMSPSS_5", "RMSPSS_6")],
                                  na.rm = T
                                  )

##Psychological Wellbeing Scale####
#US
US_nomiss$PWS_Autonomy <- rowMeans(US_nomiss[c("PWS_15", "PWS_17", "PWS_18")],
                                   na.rm = T
                                   )

US_nomiss$PWS_Enviromental <- rowMeans(US_nomiss[c("PWS_4", "PWS_8", "PWS_9")], 
                                       na.rm = T
                                       )

US_nomiss$PWS_Growth <- rowMeans(US_nomiss[c("PWS_11", "PWS_12", "PWS_14")],
                                 na.rm = T
                                 )

US_nomiss$PWS_Relation <- rowMeans(US_nomiss[c("PWS_6", "PWS_13", "PWS_16")],
                                   na.rm = T
                                   )

US_nomiss$PWS_Purpose <- rowMeans(US_nomiss[c("PWS_3", "PWS_7", "PWS_10")],
                                  na.rm = T
                                  )

US_nomiss$PWS_Self <- rowMeans(US_nomiss[c("PWS_1", "PWS_2", "PWS_5")],
                               na.rm = T
                               )

#DK
DK_nomiss$PWS_Autonomy <- rowMeans(DK_nomiss[c("PWS_15", "PWS_17", "PWS_18")],
                                   na.rm = T
                                   )

DK_nomiss$PWS_Enviromental <- rowMeans(DK_nomiss[c("PWS_4", "PWS_8", "PWS_9")], 
                                       na.rm = T
                                       )

DK_nomiss$PWS_Growth <- rowMeans(DK_nomiss[c("PWS_11", "PWS_12", "PWS_14")],
                                 na.rm = T
                                 )

DK_nomiss$PWS_Relation <- rowMeans(DK_nomiss[c("PWS_6", "PWS_13", "PWS_16")],
                                   na.rm = T
                                   )

DK_nomiss$PWS_Purpose <- rowMeans(DK_nomiss[c("PWS_3", "PWS_7", "PWS_10")],
                                  na.rm = T
                                  )

DK_nomiss$PWS_Self <- rowMeans(DK_nomiss[c("PWS_1", "PWS_2", "PWS_5")],
                               na.rm = T
                               )

##Social and Personal Identities Scale####
#US
US_nomiss$Social_Id <- rowMeans(US_nomiss[c("SIPIS_1", "SIPIS_3", "SIPIS_5", 
                                            "SIPIS_7", "SIPIS_9", "SIPIS_11", 
                                            "SIPIS_13", "SIPIS_15")],
                                na.rm = T
                                )

US_nomiss$Personal_Id <- rowMeans(US_nomiss[c("SIPIS_2", "SIPIS_4", "SIPIS_6", 
                                              "SIPIS_8", "SIPIS_10", "SIPIS_12", 
                                              "SIPIS_14", "SIPIS_16")],
                                  na.rm = T
                                  )

#DK
DK_nomiss$Social_Id <- rowMeans(DK_nomiss[c("SIPIS_1", "SIPIS_3", "SIPIS_5", 
                                            "SIPIS_7", "SIPIS_9", "SIPIS_11", 
                                            "SIPIS_13", "SIPIS_15")],
                                na.rm = T
                                )

DK_nomiss$Personal_Id <- rowMeans(DK_nomiss[c("SIPIS_2", "SIPIS_4", "SIPIS_6", 
                                              "SIPIS_8", "SIPIS_10", "SIPIS_12", 
                                              "SIPIS_14", "SIPIS_16")],
                                  na.rm = T
                                  )

#Create CSV Files####

write.csv(US_nomiss, "US Clean.csv")
write.csv(DK_nomiss, "DK Clean.csv")
