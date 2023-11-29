H3_full <- '
#Measurement Model

##CSS (IV)
CSS =~ CSS_Danger_Contamination + CSS_SEC + Xenophobia + TS + Checking

##(mediators)

Explore =~ MEIM_R1 + MEIM_R4 + MEIM_R5

Commit =~ MEIM_R2 + MEIM_R3 + MEIM_R6

SID =~ SIPIS_1 + SIPIS_3 + SIPIS_5 + SIPIS_7 + SIPIS_9 + SIPIS_11 + SIPIS_13 + SIPIS_15

PID =~ SIPIS_2 + SIPIS_4 + SIPIS_6 + SIPIS_8 + SIPIS_10 + SIPIS_12 + SIPIS_14 + SIPIS_16

##(DVs)
                 
Self Esteem =~ prior("normal(.76,.10)")*RSES_1 + prior("normal(.74,.10)")*RSES_2 + 
               prior("normal(.77,.10)")*RSES_3 + prior("normal(.66,.10)")*RSES_4 + 
               prior("normal(.70,.10)")*RSES_5 + prior("normal(.85,.10)")*RSES_6 + 
               prior("normal(.80,.10)")*RSES_7 + prior("normal(.73,.10)")*RSES_8 + 
               prior("normal(.68,.10)")*RSES_9 + prior("normal(.76,.10)")*RSES_10
               

Life_sat =~ LSM


#Regressions
##predict DVs
Self Esteem ~ CSS + EIE + EIC + SID + PID
Life_sat ~ CSS + EIE + EIC + SID + PID

##Predict Med
EIE ~ CSS
EIC ~ CSS
SID ~ CSS
PID ~ CSS
'



H3_full <- '
#Measurement Model

##CSS (IV)
CSS =~ CSS_Danger_Contamination + CSS_SEC + Xenophobia + TS + Checking

##(mediators)

SID =~ SIPIS_1 + SIPIS_3 + SIPIS_5 + SIPIS_7 + SIPIS_9 + SIPIS_11 + SIPIS_13 + SIPIS_15

PID =~ SIPIS_2 + SIPIS_4 + SIPIS_6 + SIPIS_8 + SIPIS_10 + SIPIS_12 + SIPIS_14 + SIPIS_16

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
Belongingness ~ CSS + SID + PID
Self Esteem ~ CSS + SID + PID
Control ~ CSS + SID + PID
Meaningful Existence ~ CSS + SID + PID
Life_sat ~ CSS + SID + PID

##Predict Med
SID ~ CSS
PID ~ CSS
'
