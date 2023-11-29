#Assumption Tests####

pacman::p_load(DescTools, naniar, tidyverse, hablar, mice)

#Load Data######################################################################
#Load Data
USDK <- read.csv('Total Clean Temporary.csv', 
               header = T,
               na.strings = c("", "NA"))

US <- read.csv('US Clean Temporary.csv', 
                 header = T,
                 na.strings = c("", "NA"))

DK <- read.csv('DK Clean Temporary.csv', 
                 header = T,
                 na.strings = c("", "NA"))


##Missing Values################################################################
#DANISH PARTICIPANTS 18 AND 29 WERE DELETED FOR MISSING DATA
###Little's MCAR Test####

#1. Reduce dataset to non-demographic variables
US_MCAR <- US[c(4:111,130:135)]
mcar_test(US_MCAR)

DK_MCAR <- DK[c(4:111, 130:135)]
mcar_test(DK_MCAR)

#BOTH DATASETS ARE MCAR

#2. Imputing missing data with the MICE algorithm

USDK_nomiss_model <- mice(USDK)

summary(USDK_nomiss_model)

USDK_nomiss <- complete(USDK_nomiss_model,1)

#3. Split data set into US and Denmark.

US_nomiss <- subset(USDK_nomiss, School == "California State University, Sacramento")
DK_nomiss <- subset(USDK_nomiss, School == "Aarhus University")

#4. Write CSV Files

write.csv(USDK_nomiss, "Total Clean Temporary No Missing.csv")
write.csv(US_nomiss, "US Clean Temporary No Missing.csv")
write.csv(DK_nomiss, "DK Clean Temporary No Missing.csv")


##Check for Univariate Outliers#################################################

USDK_nomiss <- read.csv('Total Clean Temporary No Missing.csv', 
                 header = T,
                 na.strings = c("", "NA"))

US_nomiss <- read.csv('US Clean Temporary No Missing.csv', 
               header = T,
               na.strings = c("", "NA"))

DK_nomiss <- read.csv('DK Clean Temporary No Missing.csv', 
               header = T,
               na.strings = c("", "NA"))


#US
Desc(US_nomiss[c(4:25)])
Desc(US_nomiss[c(26:50)])
Desc(US_nomiss[c(51:75)])
Desc(US_nomiss[c(76:100)])
Desc(US_nomiss[c(101:112,147:152)])

#DK
Desc(DK_nomiss[c(4:25)])
Desc(DK_nomiss[c(26:50)])
Desc(DK_nomiss[c(51:75)])
Desc(DK_nomiss[c(76:100)])
Desc(DK_nomiss[c(101:111,129,147:152)])


#Explore the worst....
Desc(US_nomiss[c(20,21,30)])
Desc(DK_nomiss[c(11:16,19:21,28:30,32,34,36,39,40)])
