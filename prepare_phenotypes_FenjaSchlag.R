#-------------------Phenotype preparation script-------------------####
# Run this script to format your phenotype  input in order to make use of the GWAS pipeline
# Name: FenjaSchlag
# Date: 04-04-2021
#------------------------------------------------------------------####

#-------------------Load libraries---------------------------------####
library(dplyr)
library(tidyr)
#-------------------Define input data------------------------------####
phenotypeDirectory <- "C:/Users/Fenja/Documents/Work/EAGLE/" #"/path/to/phenotype/data"
phenotypeData <- "social.pheno.phe"  #"phenotype_data_file.txt"
separator <- "\t" #separator in phenotype file 
na_string <- "NA" #NA string in phenotype file
cohort =  "COHORT1" #"cohortname" # paste cohort name

#This ID should be the same identifier as it is used in the genotype files
subject_ID<-"IID"
#Fill in the column name that indicates the family ID, if not available fill in NA
FID<-"fam_id"
#Fill in the column name that indicates the within family personal ID, if not available fill in NA
PID<-"iid"

#Fill in column names of the phenotypes of interest
phenotype_columns <-c("pheno1", "pheno2", "pheno3", "pheno4", "pheno5", "pheno6", "pheno7", "pheno8", "pheno9", "pheno10", "pheno11", "pheno12", "pheno13", "pheno14", "pheno15")

#Fill in which instrument has been used for each corresponding elemnet in phenotype_columns:
#   Choose between: 
#   "RR"     --> Rutter Parent scale 
#   "SDQ"    --> Strengths and Difficulties Qestionnaire
#   "CBCL"   --> Child Behavior Checklist
#   "MPNI"   --> Multidimensional Peer Nomination Invetory 
phenotype_instrument <-c("RR",rep("SDQ",14))

#Fill in which trait has been assessed for each corresponding element in phenotype_columns:
#   Choose between: 
#   "PB"  --> Prosocial Behavior
#   "PP"  --> Peer problems
#   "SP"  --> Social problems
#   "PDP" --> Pervasive developmental problems
#   "A"   --> Adjustment scale
phenotype_trait<- c(rep("PB",8),rep("PP",7))

#Fill in which kind of rater conducted the scoring for each corresponding element in phenotype_columns:
#   Choose between:
#   "M" --> mother rated
#   "F" --> father rated
#   "P" --> parent rated
#   "T" --> teacher rated
#   "S" --> self report
phenotype_rater<- c(rep("M",6),rep("T",2),rep("M",5),rep("T",2))

#Fill in column names that contain the age information for each corresponding element in phenotype_columns:
age_columns <- c("age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9", "age10", "age11", "age12", "age13", "age14", "age15")

#Fill in column name that contains sex information
sex <-  "kz021" #"sex" 

#Specify how males and females are coded
males <-1  #"M"
females <-2 #"F"

#Fill in column names of your principal components that you would like to include in your analysis
pc<-c("pc1", "pc2")

#-------------------------Load  data-------------------------------####
pheno <- read.table(paste(phenotypeDirectory,phenotypeData, sep=""), sep=separator,na.strings = na_string, header=TRUE)
#Identify age groups
age<-paste(round(sapply(subset(pheno,select = age_columns),mean, na.rm=T),digits = 0),"Y",sep="")
#Create phenotype and age descriptions: cohort-name_instrument_trait_rater_age
phenotype_descriptions<-paste(cohort,phenotype_instrument,phenotype_trait,phenotype_rater,age,sep="_")
age_descriptions<-paste(phenotype_descriptions,"age",sep = ".")

#-------------------Data fame ormatting----------------------------####
FID <-data.frame(ifelse(is.na(FID),rep(NA,nrow(pheno)),subset(pheno,select = FID)))
if (is.na(FID)==TRUE){
  FID<-rep(NA,nrow(pheno))}
PID<-data.frame(ifelse(is.na(PID),rep(NA,nrow(pheno)),subset(pheno,select = PID)))
if (is.na(PID)==TRUE){
  PID<-rep(NA,nrow(pheno))}

pheno_OUT<-data.frame(FID,PID,subset(pheno, select = c(subject_ID,sex,phenotype_columns,age_columns,pc)))
colnames(pheno_OUT)<-c("FID","PID","IID","Sex",phenotype_descriptions,age_descriptions,paste("pc",seq(1,length(pc)),sep=""))


# Then run dplyr/tidyr to reformat table to columns with: IID, Instrument, Trait, Age, Rater, Sex
new_PH<-  pheno_OUT %>%
  select(-c(FID,PID)) %>% 
  gather(variable,score,-c(IID,Sex,pc1,pc2),na.rm = FALSE) %>%
  separate(variable,into=c("cohort","instr","trait","rater","age"),remove = FALSE) %>%
  group_by(instr,trait) %>%
  #caluclate mean score per group with summarze function
  #assign per individual score a z score 
  head(new_PH)
