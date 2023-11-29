#Hypothesis 1####

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

USDK <- read.csv('Total Clean Temporary No Missing.csv', 
               header = T,
               na.strings = c("", "NA"))

##SEM####

H1 <- '
#Measurement Model

##CSS
CSS =~ CSS_Danger_Contamination + CSS_SEC + Xenophobia + TS + Checking

##fundumental 4
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

#Regressions
Belongingness ~ CSS
Self Esteem ~ CSS
Control ~ CSS 
Meaningful Existence ~ CSS
'

##US Model####
H1_fit_US <- bsem(H1, data = US, n.chains = 3, burnin = 1000, sample =10000)
summary (H1_fit_US, fit.measures = T)

blavFitIndices(H1_fit_US, thin = 1L, pD = c("loo","waic","dic"),
               rescale = c("devM","ppmc","mcmc"),
               fit.measures = "all", baseline.model = NULL)

blavFitIndices(H1_fit_US, 
               thin = 1L,
               pD = "loo",
               rescale = "devM",
               fit.measures = "all", 
               baseline.model = NULL
               )


standardizedPosterior(H1_fit_US)

#Play around with this to get the mean and SD
summary(standardizedPosterior(H1_fit_US))


##DK Model####
H1_fit_DK <- bsem(H1, data = DK, n.chains = 3, burnin = 1000, sample = 10000)

summary (H1_fit_DK, fit.measures = T)

blavFitIndices(H1_fit_DK, 
               thin = 1L,
               pD = "loo",
               rescale = "devM",
               fit.measures = "all", 
               baseline.model = NULL
               )




stdpost_DK <- standardizedPosterior(H1_fit_DK)
apply(stdpost_DK, 2, quantile, c(.025,.975))

#Play around with this to get the mean and SD
summary(standardizedPosterior(H1_fit_DK), "mean")

standardizedposterior(H1_fit_DK) %>%
  summary()
?summary
