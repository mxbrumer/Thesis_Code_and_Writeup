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

US<- subset(US, 
             Ethnicity_US == "Latinx" |
             Ethnicity_US == "Asian American/Pacific Islander"|
             Ethnicity_US == "Multiethnic" |
             Ethnicity_US == "Black/African American" |
             Ethnicity_US == "Native American/Alaskan Native"
             )

##SEMs####

###Full################################

H3_EI <- '
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
Belongingness ~ CSS + EXPLORE + COMMIT
Self Esteem ~ CSS+ EXPLORE + COMMIT
Control ~ CSS+ EXPLORE + COMMIT
Meaningful Existence ~ CSS + EXPLORE + COMMIT
Life_sat ~ CSS + EXPLORE + COMMIT

##Predict Med
EXPLORE ~ CSS
COMMIT ~ CSS


'

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

##US Model####
H3_EI_fit_US <- bsem(H3_EI, 
                  data = US, 
                  n.chains = 3, 
                  burnin = 1000, 
                  sample = 10000
                  )

summary(H3_EI_fit_US, fit.measures = T)

blavFitIndices(H3_EI_fit_US, 
               thin = 1L,
               pD = "loo",
               rescale = "devM",
               fit.measures = "all", 
               baseline.model = NULL
               )
