#Hypothesis Testing Preliminary####

##Packages####

pacman::p_load(apaTables, hablar)

##Load Data####

US <- read.csv('US Clean.csv', 
               header = T,
               na.strings = c("", "NA"))

DK <- read.csv('DK Clean.csv', 
               header = T,
               na.strings = c("", "NA"))

##Correlation Tables####

apa.cor.table(US[c(151:166,173,174)])
