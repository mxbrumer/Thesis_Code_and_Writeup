#Confirmatory Factor Analysis###################################################

#Load Packages##################################################################

pacman::p_load(lavaan, blavaan, semPlot)

#Load Data######################################################################

USDK <- read.csv('Total With Latent Temporary.csv', 
               header = T,
               na.strings = c("", "NA"))

#Psychological Wellbeing CFA####################################################

##6 factor solution####

PWS6Fac <- '
Autonomy =~ PWS_15 + PWS_17 + PWS_18
Enviromental Mastery =~ PWS_4 + PWS_8 + PWS_9
Personal Growth =~ PWS_11 + PWS_12 + PWS_14
Relations with Others =~ PWS_6 + PWS_13 + PWS_16
Purpose in Life =~ PWS_3 + PWS_7 + PWS_10
Self Acceptance =~ PWS_1 + PWS_2 + PWS_5
'

PWS6Fac_fit <- cfa(PWS6Fac, DK)
summary(PWS6Fac_fit, fit.measures = T)

##1 factor solution####

PWS1Factor <- '
Wellbeing =~ PWS_1 + PWS_2 + PWS_3 + PWS_4 + PWS_5 + PWS_6 + PWS_7 + PWS_8 + 
PWS_9 + PWS_10 + PWS_11 + PWS_12 + PWS_13 + PWS_14 + PWS_15 + PWS_16 + PWS_17 + 
PWS_18
'

PWS1Factor_fit <- cfa(PWS1Factor, USDK)
summary(PWS1Factor_fit, fit.measures = T)
