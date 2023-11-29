#Recode and save a CSV file.

#Load Packages##################################################################

pacman::p_load(tidyverse, hablar)

#Load Data and Clean############################################################
#Load Data
DF <- read.csv('Raw Data.csv', 
               header = T,
               na.strings = c("", "NA"))

#Remove unneeded variables/col (i.e. language, start time, and informed consent)
DF <- DF[c(8,11,14:145,148:171)]
#Remove top two cases/rows that contained text information
DF <- DF[-c(1:2),]
#Remove non-students
DF <- subset(DF, School == "Aarhus University" | 
                 School == "California State University, Sacramento")
#remove casses with missing data. One participant had a valid missing case and 
#was coded as 99
DF <- subset(DF, SIPIS_16 > 0)

#Combine US and Danish MEIM_R###################################################

DF <- DF %>%
  mutate(MEIM_R_O = ifelse(School == "Aarhus University",
                           paste(MEIM_R_D_O), 
                           ifelse(School == "California State University, Sacramento",
                                  paste(MEIM_R__US_O),
                                  NA
                                  )
                           ),
        MEIM_R1 = ifelse(School == "Aarhus University",
                          paste(MEIM_R_D_Q_1), 
                          ifelse(School == "California State University, Sacramento",
                                 paste(MEIM_R_US_Q_1),
                                 NA
                                 )
                          ),
         MEIM_R2 = ifelse(School == "Aarhus University",
                          paste(MEIM_R_D_Q_2), 
                          ifelse(School == "California State University, Sacramento",
                                 paste(MEIM_R_US_Q_2),
                                 NA
                                 )
                          ),
         MEIM_R3 = ifelse(School == "Aarhus University",
                          paste(MEIM_R_D_Q_3), 
                          ifelse(School == "California State University, Sacramento",
                                 paste(MEIM_R_US_Q_3),
                                 NA
                                 )
                          ),
         MEIM_R4 = ifelse(School == "Aarhus University",
                          paste(MEIM_R_D_Q_4), 
                          ifelse(School == "California State University, Sacramento",
                                 paste(MEIM_R_US_Q_4),
                                 NA
                                 )
                          ),
         MEIM_R5 = ifelse(School == "Aarhus University",
                          paste(MEIM_R_D_Q_5), 
                          ifelse(School == "California State University, Sacramento",
                                 paste(MEIM_R_US_Q_5),
                                 NA
                                 )
                          ),
         MEIM_R6 = ifelse(School == "Aarhus University",
                          paste(MEIM_R_D_Q_6), 
                          ifelse(School == "California State University, Sacramento",
                                 paste(MEIM_R_US_Q_6),
                                 NA
                                 )
                          )
         )

#Rename the COVID Stress Scale

#Recode variables into numerals#################################################

#MEIM_R
DF <- DF %>% 
  mutate_at(c("MEIM_R1", "MEIM_R2", "MEIM_R3", "MEIM_R4", "MEIM_R5", "MEIM_R6"),
            funs(
              recode(
                .,
                `1 - Strongly disagree`  = 1,
                `2 - Disagree` = 2,
                `3 - Neutral` = 3,
                `4 - Agree` = 4,
                `5 - Strongly agree` = 5,
                .default = NaN
                )
              )
            )

#Covid Stress Scale
DF <- DF %>% 
  mutate_at(c("CSS_1_1", "CSS_1_2", "CSS_1_3", "CSS_1_4", "CSS_1_5", "CSS_1_6",
              "CSS_1_7", "CSS_1_8", "CSS_1_9", "CSS_1_10", "CSS_1_11", 
              "CSS_1_12", "CSS_1_13", "CSS_1_14", "CSS_1_15", "CSS_1_16",
              "CSS_1_17", "CSS_1_18", "CSS_1_19", "CSS_1_20", "CSS_1_21",
              "CSS_1_22", "CSS_1_23", "CSS_1_24"),
            funs(
              recode(
                .,
                `0 - Not at all` = 0,
                `1 - Slightly` = 1,
                `2 - Moderately` = 2,
                `3 - Very` = 3,
                `4 - Extremely` = 4
                )
              )
            )


            
DF <- DF %>%
  mutate_at(c("CSS_2_1", "CSS_2_2", "CSS_2_3", "CSS_2_4", "CSS_2_5", "CSS_2_6",
              "CSS_3_1", "CSS_3_2", "CSS_3_3", "CSS_3_4", "CSS_3_5", "CSS_3_6"),
            funs(
              recode(
                .,
                `0 - Never` = 0,
                `1 - Rarely` = 1,
                `2 - Sometimes` = 2,
                `3 - Often` = 3,
                `4 - Almost always` = 4
                )
              )
            )

DF <- DF %>%
  mutate_at(c("RSES_1", "RSES_2", "RSES_3", "RSES_4", "RSES_5", "RSES_6",
              "RSES_7", "RSES_8", "RSES_9", "RSES_10"),
            funs(
              recode(
                .,
                `1 - Strongly disagree` = 1,
                `2 - Disagree` = 2,
                `3 - Agree` = 3,
                `4 - Strongly agree` = 4
                )
              )
            )

DF <- DF %>%
  mutate_at(c("GBS_1", "GBS_2", "GBS_3", "GBS_4", "GBS_5", "GBS_6", "GBS_7",
              "GBS_8", "GBS_9", "GBS_10", "GBS_11", "GBS_12"),
            funs(
              recode(
                .,
                `1 - Strongly disagree` = 1,
                `2` = 2,
                `3` = 3,
                `4` = 4,
                `5` = 5,
                `6` = 6,
                `7 - Strongly agree` = 7
                )
              )
            )

DF <- DF %>%
  mutate_at(c("RAS_1", "RAS_2", "RAS_3", "RAS_4"),
            funs(
              recode(
                .,
                `1 - Strongly disagree` = 1,
                `2 - Disagree` = 2,
                `3 - Agree` = 3,
                `4 - Strongly agree` = 4
                )
              )
            )

DF <- DF %>%
  mutate_at(c("FS_1", "FS_2", "FS_3", "FS_4", "FS_5"),
            funs(
              recode(
                .,
                '1 - Not at all true' = 1,
                '2' = 2,
                '3' = 3,
                `4` = 4,
                `5 - Very true` = 5
                )
              )
            )

DF <- DF %>%
  mutate_at(c("LSM"),
         funs(
           recode(
             .,
             `0 Worst possible life overall` = 0,
             `1` = 1,
             `2` = 2,
             `3` = 3,
             `4` = 4,
             `5` = 5,
             `6` = 6,
             `7` = 7,
             `8` = 8,
             `9` = 9,
             `10 best possible life overall` = 10
             )
           )
         )

DF <- DF %>%
  mutate_at(c("RMSPSS_1", "RMSPSS_2", "RMSPSS_3", "RMSPSS_4", "RMSPSS_5",
            "RMSPSS_6"),
            funs(
              recode(
                .,
                `1 - Very strongly disagree` = 1,
                `2` = 2,
                `3` = 3,
                `4` = 4,
                `5` = 5,
                `6` = 6,
                `7 - Very strongly agree` = 7
                )
              )
            )

DF <- DF %>%
  mutate_at(c("PWS_1", "PWS_2", "PWS_3", "PWS_4", "PWS_5", "PWS_6", "PWS_7",
              "PWS_8", "PWS_9", "PWS_10", "PWS_11", "PWS_12", "PWS_13", 
              "PWS_14", "PWS_15", "PWS_16", "PWS_17", "PWS_18"),
            funs(
              recode(
                .,
                `1 - Strongly disagree` = 1,
                `2 - Somewhat disagree` = 2,
                `3 - A little disagree` = 3,
                `4 - Neither agree nor disagree` = 4,
                `5 - A little agree` = 5,
                `6 - Somewhat agree` = 6,
                `7 - Strongly Agree` = 7
                )
              )
            )

DF <- DF %>%
  mutate_at(c("SIPIS_1", "SIPIS_2", "SIPIS_3", "SIPIS_4", "SIPIS_5", "SIPIS_6",
              "SIPIS_7", "SIPIS_8", "SIPIS_9", "SIPIS_10", "SIPIS_11",
              "SIPIS_12", "SIPIS_13", "SIPIS_14", "SIPIS_15", "SIPIS_16"),
            funs(
              recode(
                .,
                `1 - Not applicable to who I am` = 1,
                `2` = 2,
                `3` = 3,
                `4` = 4,
                `5` = 5,
                `6` = 6,
                `7` = 7,
                `8` = 8,
                `9 - Extremely important to who I am` = 9
                )
              )
            )

#Danish Semester
DF$School_D <- recode(DF$Semester__D,
       `1` = "Freshman",
       `2` = "Freshman",
       `3` = "Sophomore",
       `4` = "Sophomore",
       `5` = "Junior",
       `6` = "Junior",
       `7` = "Senior",
       `8` = "Senior",
       `9` = "Super Senior or Graduate Student",
       `10` = "Super Senior or Graduate Stuent",
       `11` = "Graduate Student",
       `12` = "Graduate Student",
       `13` = "Graduate Student",
       `14` = "Graduate Student",
       `15` = "Graduate Student",
       `16` = "Graduate Student",
       `Graduate Student` = "Graduate Student")

#Danish family income in USD rounded to the nearest hundred
DF$Income_D_USD <- recode(DF$Income_D,
                          `Less than 99.999kr per year` = "Less than $16,300 per year",
                          `100.000kr - 149.999kr per year` = "$16,301 - $24,500 per year",
                          `150.000kr - 199.999kr per year` = "$24,501 - $32,600 per year",
                          `200.000kr - 249.999kr per year` = "$32,601 - $40,800 per year",
                          `250.000kr - 299.999kr per year` = "$40,801 - $49,000 per year",
                          `300.000kr - 349.999kr per year` = "$49,001 - $57,100 per year",
                          `350.000kr - 399.999kr per year` = "$57,101 - $65,300 per year",
                          `400.000kr - 449.999kr per year` = "$65,301 - $73,400 per year",
                          `450.000kr - 499.999kr per year` = "$73,401 - $81,600 per year",
                          `500.000kr - 549.999kr per year` = "$81,601 - $89,800 per year",
                          `550.000kr - 599.99kr per year` = "$89,801 - $97,900 per year",
                          `600.000kr - 649.999kr per year` = "$97,901 - $106,100 per year",
                          `650.000kr - 699.99kr per year` = "$106,101 - $114,200 per year",
                          `700.000kr - 749.999kr per year` = "$114,201 - $122,400 per year",
                          `749.999kr - 799.999kr per year` = "$122,401 - $130,500 per year",
                          `800.000kr or more per year` = "$130,501 or more per year",
                          `Do not know` = "Do not know"
                          )


#Recode negatively scored items#################################################

DF <- DF %>%
  mutate_at(c("RSES_3", "RSES_5", "RSES_8", "RSES_9", "RSES_10"),
            funs(
              recode(
                .,
                `1` = 4,
                `2` = 3,
                `3` = 2,
                `4` = 1
                )
              )
            )

DF <- DF %>%
  mutate_at(c("GBS_3", "GBS_4", "GBS_6", "GBS_7", "GBS_9", "GBS_12"),
            funs(
              recode(
                .,
                `1` = 7,
                `2` = 6,
                `3` = 5,
                `4` = 4,
                `5` = 3,
                `6` = 2,
                `7` = 1
                )
              )
            )

DF <- DF %>%
  mutate_at(c("PWS_1", "PWS_2", "PWS_3", "PWS_8", "PWS_9", "PWS_11", "PWS_12",
              "PWS_13", "PWS_17", "PWS_18"),
            funs(
              recode(
                .,
                `1` = 7,
                `2` = 6,
                `3` = 5,
                `4` = 4,
                `5` = 3,
                `6` = 2,
                `7` = 1
                )
              )
            )


#Print Clean Data###############################################################
#Write files for US and Denmark#################################################
US <- subset(DF, School == "California State University, Sacramento")
US <- US[c(1,2,10:134,159:165)]
write.csv(US, "US Clean Temporary.csv")

DK <- subset(DF, School == "Aarhus University")
DK <- DK[c(1:2,10:117,142:167)]
write.csv(DK, "DK Clean Temporary.csv")

USDK <- DF[c(1:2,10:134,142:167)]
write.csv(USDK, "Total Clean Temporary.csv")

