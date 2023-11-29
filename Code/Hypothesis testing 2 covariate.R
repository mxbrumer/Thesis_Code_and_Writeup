#Hypothesis 2####

##Packages####

pacman::p_load(lavaan)
pacman::p_load(blavaan)

##Load Data####

US <- read.csv('US Clean.csv', 
               header = T,
               na.strings = c("", "NA"))

DK <- read.csv('DK Clean.csv', 
               header = T,
               na.strings = c("", "NA"))

##SEMs####

###Full################################

H2_cov <- '
#Measurement Model

##CSS (IV)
CSS =~ CSS_Danger_Contamination + CSS_SEC + Xenophobia + TS + Checking

##Social Support (mediator)

RMSPSS =~ RMSPSS_1 + RMSPSS_2 + RMSPSS_3 + RMSPSS_4 + RMSPSS_5 + RMSPSS_6

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
Belongingness ~ CSS + RMSPSS
Self Esteem ~ CSS + RMSPSS
Control ~ CSS +RMSPSS
Meaningful Existence ~ CSS + RMSPSS
Life_sat ~ CSS + RMSPSS

##covariate
CSS ~ RMSPSS
'

##US Model####
H2_cov_fit_US <- bsem(H2_cov, 
                  data = US, 
                  n.chains = 3, 
                  burnin = 1000, 
                  sample = 10000
                  )

summary(H2_cov_fit_US, fit.measures = T)

blavFitIndices(H2_cov_fit_US, 
               thin = 1L,
               pD = "loo",
               rescale = "devM",
               fit.measures = "all", 
               baseline.model = NULL
               )

##DK Model####
H2_cov_fit_DK <- bsem(H2_cov, 
                  data = DK, 
                  n.chains = 3, 
                  burnin = 1000, 
                  sample = 10000 )

summary (H2_cov_fit_DK, fit.measures = T)

blavFitIndices(H2_cov_fit_DK, 
               thin = 1L,
               pD = "loo",
               rescale = "devM",
               fit.measures = "all", 
               baseline.model = NULL
               )

