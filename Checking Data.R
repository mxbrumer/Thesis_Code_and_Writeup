#Checking Data##################################################################

#Load Packages##################################################################

pacman::p_load(ggplot2)

#Histograms#####################################################################

##MEIM_R####
###Individual items####
hist(as.numeric(USDK[c(161:166)]))

###latent scores####

####Exploration####
ggplot(data = US) +
  geom_histogram(mapping = aes(x = EIE)) +
  labs(title = "Ethnic Identity Exploration - United States",
       x = "Response",
       y = "Frequency") +
  xlim(.9,5.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = EIE))+
  labs(title = "Ethnic Identity Exploration - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(.9,5.1)

####Commitment####
ggplot(data = US) +
  geom_histogram(mapping = aes(x = EIC))+
  labs(title = "Ethnic Identity Commitment - United States",
       x = "Response",
       y = "Frequency") +
  xlim(.9,5.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = EIC))+
  labs(title = "Ethnic Identity Commitment - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(.9,5.1)
       
       
##COVID Stress Scale####

###Individual items####
hist(DF[c(10:45)])

###latent scores####

####Danger & Contamination####
ggplot(data = US) +
  geom_histogram(mapping = aes(x = CSS_Danger_Contamination),
                 binwidth = .05) +
  labs(title = "COVID Stress: Danger & Contamination - United States",
       x = "Response",
       y = "Frequency") +
  xlim(-.1,4.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = CSS_Danger_Contamination),
                 binwidth = .05)+
  labs(title = "COVID Stress: Danger & Contamination - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(-.1,4.1)

####Socio-Economic Consequences####

ggplot(data = US) +
  geom_histogram(mapping = aes(x = CSS_SEC),
                 binwidth = .05) +
  labs(title = "COVID Stress: Socio-Economic Consequences - United States",
       x = "Response",
       y = "Frequency") +
  xlim(-.1,4.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = CSS_SEC),
                 binwidth = .05)+
  labs(title = "COVID Stress: Socio-Economic Consequences - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(-.1,4.1)

####Xenophobia####

ggplot(data = US) +
  geom_histogram(mapping = aes(x = Xenophobia),
                 binwidth = .05) +
  labs(title = "COVID Stress: Xenophobia - United States",
       x = "Response",
       y = "Frequency") +
  xlim(-.1,4.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = Xenophobia),
                 binwidth = .05)+
  labs(title = "COVID Stress: Xenophobia - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(-.1,4.1)

####Traumatic Stress

ggplot(data = US) +
  geom_histogram(mapping = aes(x = TS),
                 binwidth = .05) +
  labs(title = "COVID Stress: Traumatic Stress - United States",
       x = "Response",
       y = "Frequency") +
  xlim(-.1,4.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = TS),
                 binwidth = .05)+
  labs(title = "COVID Stress: Traumatic Stress - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(-.1,4.1)

####Checking Behaviors####

ggplot(data = US) +
  geom_histogram(mapping = aes(x = Checking),
                 binwidth = .05)+
  labs(title = "COVID Stress: Checking Behaviors - United States",
       x = "Response",
       y = "Frequency") +
  xlim(-.1,4.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = Checking),
                 binwidth = .05)+
  labs(title = "COVID Stress: Checking Behaviors - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(-.1,4.1)

##Rosenberg Self-Esteem Scale####

###Individual items####

###Latent Score####

ggplot(data = US) +
  geom_histogram(mapping = aes(x = RSES_Mean),
                 binwidth = .05)+
  labs(title = "Rosenberg Self-Esteem Scale - United States",
       x = "Response",
       y = "Frequency") +
  xlim(.9,4.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = RSES_Mean),
                 binwidth = .05)+
  labs(title = "Rosenberg Self-Esteem Scale - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(.9,4.1)

##General Belongingness Scale####

###Individual items####

###Latent Score####

ggplot(data = US) +
  geom_histogram(mapping = aes(x = GBS_Mean),
                 binwidth = .05)+
  labs(title = "General Belongingness Scale - United States",
       x = "Response",
       y = "Frequency") +
  xlim(.9,4.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = GBS_Mean),
                 binwidth = .05)+
  labs(title = "General Belongingness Scale - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(.9,4.1)

##Revised Autonomy Scale####

###Individual items####

###Latent Score####

ggplot(data = US) +
  geom_histogram(mapping = aes(x = RAS_Mean),
                 binwidth = .05)+
  labs(title = "Revised Autonomy Scale - United States",
       x = "Response",
       y = "Frequency") +
  xlim(.9,4.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = RAS_Mean),
                 binwidth = .05)+
  labs(title = "Revised Autonomy Scale - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(.9,4.1)

##Fulfilment Scale####

###Individual items####

###Latent Scores####

ggplot(data = US) +
  geom_histogram(mapping = aes(x = FS_Mean),
                 binwidth = .05)+
  labs(title = "Fulfilment Scale - United States",
       x = "Response",
       y = "Frequency") +
  xlim(.9,5.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = FS_Mean),
                 binwidth = .05)+
  labs(title = "Fulfilment Scale - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(.9,5.1)


##Life Satisfaction####

ggplot(data = US) +
  geom_histogram(mapping = aes(x = LSM))+
  labs(title = "Life Satisfaction - United States",
       x = "Response",
       y = "Frequency") +
  xlim(-.1,10.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = LSM))+
  labs(title = "Life Satisfaction - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(-.1,10.1)

##Reduced Multidimensional Scale of Perceived Social Support####

###Individual items####

###Latent Score####

ggplot(data = US) +
  geom_histogram(mapping = aes(x = RMSPSS_Mean),
                 binwidth = .05)+
  labs(title = "Scale of Perceived Social Support - United States",
       x = "Response",
       y = "Frequency") +
  xlim(.9,7.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = RMSPSS_Mean),
                 binwidth = .05)+
  labs(title = "Scale of Perceived Social Support - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(.9,7.1)

##Psychological Wellbeing Scale####

###Individual items####

###Latent Scores####

####Autonomy####

ggplot(data = US) +
  geom_histogram(mapping = aes(x = PWS_Autonomy),
                 binwidth = .05)+
  labs(title = "PWS: Autonomy - United States",
       x = "Response",
       y = "Frequency") +
  xlim(.9,7.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = PWS_Autonomy),
                 binwidth = .05)+
  labs(title = "PWS: Autonomy - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(.9,7.1)

####Environmental Mastery####

ggplot(data = US) +
  geom_histogram(mapping = aes(x = PWS_Enviromental),
                 binwidth = .05)+
  labs(title = "PWS: Environmental Mastery - United States",
       x = "Response",
       y = "Frequency") +
  xlim(.9,7.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = PWS_Enviromental),
                 binwidth = .05)+
  labs(title = "PWS: Envioronmental Mastery - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(.9,7.1)

####Positive Relations####

ggplot(data = US) +
  geom_histogram(mapping = aes(x = PWS_Relation),
                 binwidth = .05)+
  labs(title = "PWS: Positive Relations - United States",
       x = "Response",
       y = "Frequency") +
  xlim(.9,7.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = PWS_Relation),
                 binwidth = .05)+
  labs(title = "PWS: Positive Relations - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(.9,7.1)

####Purpose in Life####

ggplot(data = US) +
  geom_histogram(mapping = aes(x = PWS_Purpose),
                 binwidth = .05)+
  labs(title = "PWS: Purpose in Life - United States",
       x = "Response",
       y = "Frequency") +
  xlim(.9,7.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = PWS_Purpose),
                 binwidth = .05)+
  labs(title = "PWS: Purpose in Life - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(.9,7.1)

####Personal Growth####

ggplot(data = US) +
  geom_histogram(mapping = aes(x = PWS_Growth),
                 binwidth = .05)+
  labs(title = "PWS: Personal Growth - United States",
       x = "Response",
       y = "Frequency") +
  xlim(.9,7.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = PWS_Growth),
                 binwidth = .05)+
  labs(title = "PWS: Personal Growth - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(.9,7.1)

####Self-Acceptance####

ggplot(data = US) +
  geom_histogram(mapping = aes(x = PWS_Self),
                 binwidth = .05)+
  labs(title = "PWS: Self-Acceptance - United States",
       x = "Response",
       y = "Frequency") +
  xlim(.9,7.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = PWS_Self),
                 binwidth = .05)+
  labs(title = "PWS: Self-Acceptance - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(.9,7.1)

##Social and Personal Identities Scale####

###Individual items####

###Latent Score####

####Social Identity####

ggplot(data = US) +
  geom_histogram(mapping = aes(x = Social_Id),
                 binwidth = .05)+
  labs(title = "Social Identity - United States",
       x = "Response",
       y = "Frequency") +
  xlim(.9,9.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = Social_Id),
                 binwidth = .05)+
  labs(title = "Social Identity - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(.9,9.1)

####Personal Identity####

ggplot(data = US) +
  geom_histogram(mapping = aes(x = Personal_Id),
                 binwidth = .05)+
  labs(title = "Personal Identity - United States",
       x = "Response",
       y = "Frequency") +
  xlim(.9,9.1)

ggplot(data = DK) +
  geom_histogram(mapping = aes(x = Personal_Id),
                 binwidth = .05)+
  labs(title = "Personal Identity - Denmark",
       x = "Response",
       y = "Frequency") +
  xlim(.9,9.1)
