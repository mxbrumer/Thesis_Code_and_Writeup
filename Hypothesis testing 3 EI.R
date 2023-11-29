#Hypothesis 3b (EI)####

##Packages####

pacman::p_load(lavaan)
pacman::p_load(blavaan)

library(lavaan)
library(blavaan)
##Load Data####

US <- read.csv('US Clean.csv', 
               header = T,
               na.strings = c("", "NA"))

DK <- read.csv('DK Clean.csv', 
               header = T,
               na.strings = c("", "NA"))

##SEMs####

###Full################################

H3_full <- '
#Measurement Model

##CSS (IV)
CSS =~ CSS_Danger_Contamination + CSS_SEC + Xenophobia + TS + Checking

##(mediators)
EXPLORE =~ MEIM_R1 + MEIM_R4 + MEIM_R5

COMMIT =~ MEIM_R2 + MEIM_R3 + MEIM_R6

##(DVs)
Belongingness =~ prior("normal(.70,.10)")*GBS_1 + prior("normal(.67,.10)")*GBS_2 + 
                 prior("normal(.78,.10)")*GBS_3 + prior("normal(.66,.10)")*GBS_4 + 
                 prior("normal(.65,.10)")*GBS_5 + prior("normal(.77,.10)")*GBS_6 + 
                 prior("normal(.82,.10)")*GBS_7 + prior("normal(.67,.10)")*GBS_8 + 
                 prior("normal(.79,.10)")*GBS_9 + prior("normal(.70,.10)")*GBS_10 +
                 prior("normal(.78,.10)")*GBS_11 + prior("normal(.66,.10)")*GBS_12
                 
Self Esteem =~ prior("normal(.76,.10)")*RSES_1 + prior("normal(.74,.10)")*RSES_2 + 
               prior("normal(.77,.10)")*RSES_3 + prior("normal(.66,.10)")*RSES_4 + 
               prior("normal(.70,.10)")*RSES_5 + prior("normal(.85,.10)")*RSES_6 + 
               prior("normal(.80,.10)")*RSES_7 + prior("normal(.73,.10)")*RSES_8 + 
               prior("normal(.68,.10)")*RSES_9 + prior("normal(.76,.10)")*RSES_10
               
Control =~ RAS_1 + RAS_2 + RAS_3 + RAS_4

Meaningful Existence =~ FS_1 + FS_2 + FS_3 + FS_4 + FS_5

Life_sat =~ LSM


#Regressions
##predict DVs
Belongingness ~ C1*CSS + B1*EXPLORE + D1*COMMIT
Self Esteem ~ C2*CSS+ B2*EXPLORE + D2*COMMIT
Control ~ C3*CSS+ B3*EXPLORE + D3*COMMIT
Meaningful Existence ~ C4*CSS + B4*EXPLORE + D4*COMMIT
Life_sat ~ C5*CSS + B5*EXPLORE + D5*COMMIT

##Predict Med
EXPLORE ~ AB*CSS
COMMIT ~ AD*CSS

#Effects
##Total
TotBelong := C1 + (AB*B1) + (AD*D1)
TotSE := C2 + (AB*B2) + (AD*D2)
TotCon := C3 + (AB*B3) + (AD*D3)
TotME := C4 + (AB*B4) + (AD*D4)
TotLS := C5 + (AB*B5) + (AD*D5)

##Indirect through SID
EXPLOREBel := (AB*B1)
EXPLORESE := (AB*B2)
EXPLORECon := (AB*B3)
EXPLOREME := (AB*B4)
EXPLORELS := (AB*B5)

##Indirect through PID
COMMITBel := (AD*D1)
COMMITSE := (AD*D2)
COMMITCon := (AD*D3)
COMMITME := (AD*D4)
COMMITLS := (AD*D5)
'

###No Med##################

H3_No_Med <- '
#Measurement Model

##CSS (IV)
CSS =~ CSS_Danger_Contamination + CSS_SEC + Xenophobia + TS + Checking

##(DVs)
###fundumental 4
Belongingness =~ prior("normal(.70,.10)")*GBS_1 + prior("normal(.67,.10)")*GBS_2 + 
                 prior("normal(.78,.10)")*GBS_3 + prior("normal(.66,.10)")*GBS_4 + 
                 prior("normal(.65,.10)")*GBS_5 + prior("normal(.77,.10)")*GBS_6 + 
                 prior("normal(.82,.10)")*GBS_7 + prior("normal(.67,.10)")*GBS_8 + 
                 prior("normal(.79,.10)")*GBS_9 + prior("normal(.70,.10)")*GBS_10 +
                 prior("normal(.78,.10)")*GBS_11 + prior("normal(.66,.10)")*GBS_12
                 
Self Esteem =~ prior("normal(.76,.10)")*RSES_1 + prior("normal(.74,.10)")*RSES_2 + 
               prior("normal(.77,.10)")*RSES_3 + prior("normal(.66,.10)")*RSES_4 + 
               prior("normal(.70,.10)")*RSES_5 + prior("normal(.85,.10)")*RSES_6 + 
               prior("normal(.80,.10)")*RSES_7 + prior("normal(.73,.10)")*RSES_8 + 
               prior("normal(.68,.10)")*RSES_9 + prior("normal(.76,.10)")*RSES_10
               
Control =~ RAS_1 + RAS_2 + RAS_3 + RAS_4

Meaningful Existence =~ FS_1 + FS_2 + FS_3 + FS_4 + FS_5

###LMS
Life_sat =~ LSM


#Regressions
##predict DVs
Belongingness ~ ca*CSS
Self Esteem ~ cb*CSS
Control ~ cc*CSS
Meaningful Existence ~ cd*CSS
Life_sat ~ ce*CSS
'

###No DV############################

H3_No_DV <- '
#Measurement Model

##CSS (IV)
CSS =~ CSS_Danger_Contamination + CSS_SEC + Xenophobia + TS + Checking

##Social Support (mediator)

RMSPSS =~ RMSPSS_1 + RMSPSS_2 + RMSPSS_3 + RMSPSS_4 + RMSPSS_5 + RMSPSS_6

#Regressions

##Predict Med
RMSPSS ~ a*CSS
'

##US Model####
H3_fit_US <- bsem(H3_full, 
                  data = US, 
                  n.chains = 3, 
                  burnin = 1000, 
                  sample = 10000
                  )

summary(H3_fit_US, fit.measures = T)

blavFitIndices(H3_fit_US, 
               thin = 1L,
               pD = "loo",
               rescale = "devM",
               fit.measures = "all", 
               baseline.model = NULL
)

###No Med####
H2_Fit_US_No_Med <- bsem(H2_No_Med, 
                         data = US, 
                         n.chains = 3, 
                         burnin = 1000, 
                         sample = 10000
)

summary (H2_Fit_US_No_Med, fit.measures = T)


###No DV####
H2_Fit_US_No_DV <- bsem(H2_No_DV, 
                        data = US, 
                        n.chains = 3, 
                        burnin = 1000, 
                        sample = 10000
)

summary (H2_Fit_US_No_DV, fit.measures = T)


standardizedPosterior(H2_fit_US)

#Play around with this to get the mean and SD
summary(standardizedPosterior(H2_fit_US))


##DK Model####
H3_fit_DK <- bsem(H3_full, 
                  data = DK, 
                  n.chains = 3, 
                  burnin = 1000, 
                  sample = 10000 )

summary (H3_fit_DK, fit.measures = T)

blavFitIndices(H3_fit_DK, 
               thin = 1L,
               pD = "loo",
               rescale = "devM",
               fit.measures = "all", 
               baseline.model = NULL
)

###No Med####
H2_Fit_DK_No_Med <- bsem(H2_No_Med, 
                         data = DK, 
                         n.chains = 3, 
                         burnin = 1000, 
                         sample = 10000
)

summary (H2_Fit_DK_No_Med, fit.measures = T)


###No DV####
H2_Fit_DK_No_DV <- bsem(H2_No_DV, 
                        data = DK, 
                        n.chains = 3, 
                        burnin = 1000, 
                        sample = 10000
)

summary (H2_Fit_DK_No_DV, fit.measures = T)



stdpost_DK <- standardizedPosterior(H1_DK_fit)
apply(stdpost_DK, 2, quantile, c(.025,.975))

#Play around with this to get the mean and SD
summary(standardizedPosterior(H1_DK_fit), "mean")