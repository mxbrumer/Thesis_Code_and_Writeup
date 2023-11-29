#Sstructural Equation Models####################################################

#Load Packages##################################################################

pacman::p_load(lavaan, blavaan, semPlot)

#Load Data######################################################################

DF <- read.csv('Clean Data Temporary.csv', 
               header = T,
               na.strings = c("", "NA"))

#H1: fear of COVID-19 Predicting Lower Fundumental 4############################

H1 <- '
#Measurement Model

##CSS
Danger_Contamination =~ CSS_1_1 + CSS_1_2 + CSS_1_3 + CSS_1_4 + CSS_1_5 + 
                          CSS_1_6 + CSS_1_19 + CSS_1_20 + CSS_1_21 + CSS_1_22 + 
                          CSS_1_23
Socio Economic Consequences =~ CSS_1_7 + CSS_1_8 + CSS_1_9 + CSS_1_10 + 
                               CSS_1_11+ CSS_1_12
Xenophobia_Latent =~ CSS_1_13 + CSS_1_14 + CSS_1_15 + CSS_1_16 + CSS_1_17
Traumatic Stress =~ CSS_2_1 + CSS_2_2 + CSS_2_3 + CSS_2_4 + CSS_2_5 + CSS_2_6
Checking Behavior =~ CSS_3_1 + CSS_3_2 + CSS_3_3 + CSS_3_4 + CSS_3_5 + CSS_3_6

##fundumental 4
Belongingness =~ GBS_1 + GBS_2 + GBS_3 + GBS_4 + GBS_5 + GBS_6 + GBS_7 + GBS_8 +
                 GBS_9 + GBS_10 + GBS_11 + GBS_12
Self Esteem =~ RSES_1 + RSES_2 + RSES_3 + RSES_4 + RSES_5 + RSES_6 + RSES_7 + 
               RSES_8 + RSES_9 + RSES_10
Control =~ RAS_1 + RAS_2 + RAS_3 + RAS_4
Meaningful Existence =~ FS_1 + FS_2 + FS_3 + FS_4 + FS_5

#Regressions
Danger_Contamination ~ Belongingness + Self Esteem + Control + Meaningful Existence
Socio Economic Consequences ~ Belongingness + Self Esteem + Control + 
                              Meaningful Existence
Xenophobia ~ Belongingness + Self Esteem + Control + Meaningful Existence
Traumatic Stress ~ Belongingness + Self Esteem + Control + Meaningful Existence
Checking Behavior ~ Belongingness + Self Esteem + Control + Meaningful Existence
'

H1_fit <- sem(H1, DK, likelihood = "wishart")
summary (H1_fit, fit.measures = T)

semPaths(H1_fit,what="paths",whatLabels="par")
