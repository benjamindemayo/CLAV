#######################################################################
############# CLAV iPAD Data Analysis #################################
#######################################################################

#######################################################################
######################### PRELIMINARIES ###############################
#######################################################################
#clear all previous variables
rm(list=ls())

#load libraries for data manipulation and graphing
library(ggplot2)
library(plyr)
library(directlabels)

#some useful functions for computing measurement error
sem <- function(x) {sd(x)/sqrt(length(x))}
ci95 <- function(x) {3.96*sem(x)}

#######################################################################
#################### LOAD IN THE DATA FROM .CSV #######################
#######################################################################
#reads in dataframe object from python output .csv
df.001 <- read.csv("~/Documents/Research/CLAV_iPad/ResultsCSVs/CLAV_iPad_001_Exp1results.csv")
df.002 <- read.csv("~/Documents/Research/CLAV_iPad/ResultsCSVs/CLAV_iPad_002_Exp1results.csv")
df.003 <- read.csv("~/Documents/Research/CLAV_iPad/ResultsCSVs/CLAV_iPad_003_Exp1results.csv")
df.004 <- read.csv("~/Documents/Research/CLAV_iPad/ResultsCSVs/CLAV_iPad_004_Exp1results.csv")
df.005 <- read.csv("~/Documents/Research/CLAV_iPad/ResultsCSVs/CLAV_iPad_005_Exp1results.csv")
df.006 <- read.csv("~/Documents/Research/CLAV_iPad/ResultsCSVs/CLAV_iPad_006_Exp1results.csv")

df <- rbind(df.001,df.002,df.003,df.004,df.005,df.006)
nrow(df)
long <- df[df$RT>=0 & df$RT<=20,]
nrow(long)
df <- df[df$RT>=0 & df$RT<=20,] #remove trials with negative values
head(df)

#######################################################################
#################### CALCULATE MEAN RT ################################
#######################################################################
df_mens_voice <- df[df$voice_type=="mens",]
df_womens_voice <- df[df$voice_type=="womens",]
df_mens_voice_comp <- df[df$voice_type=="mens" & df$has_comp=="competitor",]
df_mens_voice_no_comp <- df[df$voice_type=="mens" & df$has_comp=="no_competitor",]
df_womens_voice_comp <- df[df$voice_type=="womens" & df$has_comp=="competitor",]
df_womens_voice_no_comp <- df[df$voice_type=="womens" & df$has_comp=="no_competitor",]
mean_RT_mens <- mean(df_mens_voice$RT)
mean_RT_mens_comp <- mean(df_mens_voice_comp$RT)
mean_RT_mens_no_comp <- mean(df_mens_voice_no_comp$RT)
mean_RT_womens <- mean(df_womens_voice$RT)
mean_RT_womens_comp <- mean(df_womens_voice_comp$RT)
mean_RT_womens_no_comp <- mean(df_womens_voice_no_comp$RT)
#############################################################
#############CALCULATE MEAN PERCENT CORRECT##################
#############################################################
################The data subsets for subject1################
df1_men <- df[df$subjectID == 1 & df$voice_type=="mens",]
percent_df1_men <- nrow(df1_men[df1_men$clicked == df1_men$correct,])/nrow(df1_men)
df1_women <- df[df$subjectID == 1 & df$voice_type=="womens",]
percent_df1_women <- nrow(df1_women[df1_women$clicked == df1_women$correct,])/nrow(df1_women)
df1_men_comp <- df[df$subjectID == 1 & df$voice_type=="mens" & df$has_comp=="competitor",]
percent_df1_men_comp <- nrow(df1_men_comp[df1_men_comp$clicked == df1_men_comp$correct,])/nrow(df1_men_comp)
df1_women_comp <- df[df$subjectID == 1 & df$voice_type=="womens" & df$has_comp=="competitor",]
percent_df1_women_comp <- nrow(df1_women_comp[df1_women_comp$clicked == df1_women_comp$correct,])/nrow(df1_women_comp)
df1_men_no_comp <- df[df$subjectID == 1 & df$voice_type=="mens" & df$has_comp=="no_competitor",]
percent_df1_men_no_comp <- nrow(df1_men_no_comp[df1_men_no_comp$clicked == df1_men_no_comp$correct,])/nrow(df1_men_no_comp)
df1_women_no_comp <- df[df$subjectID == 1 & df$voice_type=="womens" & df$has_comp=="no_competitor",]
percent_df1_women_no_comp <- nrow(df1_women_no_comp[df1_women_no_comp$clicked == df1_women_no_comp$correct,])/nrow(df1_women_no_comp)
################The data subsets for subject2################
df2_men <- df[df$subjectID == 2 & df$voice_type=="mens",]
percent_df2_men <- nrow(df2_men[df2_men$clicked == df2_men$correct,])/nrow(df2_men)
df2_women <- df[df$subjectID == 2 & df$voice_type=="womens",]
percent_df2_women <- nrow(df2_women[df2_women$clicked == df2_women$correct,])/nrow(df2_women)
df2_men_comp <- df[df$subjectID == 2 & df$voice_type=="mens" & df$has_comp=="competitor",]
percent_df2_men_comp <- nrow(df2_men_comp[df2_men_comp$clicked == df2_men_comp$correct,])/nrow(df2_men_comp)
df2_women_comp <- df[df$subjectID == 2 & df$voice_type=="womens" & df$has_comp=="competitor",]
percent_df2_women_comp <- nrow(df2_women_comp[df2_women_comp$clicked == df2_women_comp$correct,])/nrow(df2_women_comp)
df2_men_no_comp <- df[df$subjectID == 2 & df$voice_type=="mens" & df$has_comp=="no_competitor",]
percent_df2_men_no_comp <- nrow(df2_men_no_comp[df2_men_no_comp$clicked == df2_men_no_comp$correct,])/nrow(df2_men_no_comp)
df2_women_no_comp <- df[df$subjectID == 2 & df$voice_type=="womens" & df$has_comp=="no_competitor",]
percent_df2_women_no_comp <- nrow(df2_women_no_comp[df2_women_no_comp$clicked == df2_women_no_comp$correct,])/nrow(df2_women_no_comp)
################The data subsets for subject3################
df3_men <- df[df$subjectID == 3 & df$voice_type=="mens",]
percent_df3_men <- nrow(df3_men[df3_men$clicked == df3_men$correct,])/nrow(df3_men)
df3_women <- df[df$subjectID == 3 & df$voice_type=="womens",]
percent_df3_women <- nrow(df3_women[df3_women$clicked == df3_women$correct,])/nrow(df3_women)
df3_men_comp <- df[df$subjectID == 3 & df$voice_type=="mens" & df$has_comp=="competitor",]
percent_df3_men_comp <- nrow(df3_men_comp[df3_men_comp$clicked == df3_men_comp$correct,])/nrow(df3_men_comp)
df3_women_comp <- df[df$subjectID == 3 & df$voice_type=="womens" & df$has_comp=="competitor",]
percent_df3_women_comp <- nrow(df3_women_comp[df3_women_comp$clicked == df3_women_comp$correct,])/nrow(df3_women_comp)
df3_men_no_comp <- df[df$subjectID == 3 & df$voice_type=="mens" & df$has_comp=="no_competitor",]
percent_df3_men_no_comp <- nrow(df3_men_no_comp[df3_men_no_comp$clicked == df3_men_no_comp$correct,])/nrow(df3_men_no_comp)
df3_women_no_comp <- df[df$subjectID == 3 & df$voice_type=="womens" & df$has_comp=="no_competitor",]
percent_df3_women_no_comp <- nrow(df3_women_no_comp[df3_women_no_comp$clicked == df3_women_no_comp$correct,])/nrow(df3_women_no_comp)
################The data subsets for subject4################
df4_men <- df[df$subjectID == 4 & df$voice_type=="mens",]
percent_df4_men <- nrow(df4_men[df4_men$clicked == df4_men$correct,])/nrow(df4_men)
df4_women <- df[df$subjectID == 4 & df$voice_type=="womens",]
percent_df4_women <- nrow(df4_women[df4_women$clicked == df4_women$correct,])/nrow(df4_women)
df4_men_comp <- df[df$subjectID == 4 & df$voice_type=="mens" & df$has_comp=="competitor",]
percent_df4_men_comp <- nrow(df4_men_comp[df4_men_comp$clicked == df4_men_comp$correct,])/nrow(df4_men_comp)
df4_women_comp <- df[df$subjectID == 4 & df$voice_type=="womens" & df$has_comp=="competitor",]
percent_df4_women_comp <- nrow(df4_women_comp[df4_women_comp$clicked == df4_women_comp$correct,])/nrow(df4_women_comp)
df4_men_no_comp <- df[df$subjectID == 4 & df$voice_type=="mens" & df$has_comp=="no_competitor",]
percent_df4_men_no_comp <- nrow(df4_men_no_comp[df4_men_no_comp$clicked == df4_men_no_comp$correct,])/nrow(df4_men_no_comp)
df4_women_no_comp <- df[df$subjectID == 4 & df$voice_type=="womens" & df$has_comp=="no_competitor",]
percent_df4_women_no_comp <- nrow(df4_women_no_comp[df4_women_no_comp$clicked == df4_women_no_comp$correct,])/nrow(df4_women_no_comp)
###############The data subsets for subject5################
df5_men <- df[df$subjectID == 5 & df$voice_type=="mens",]
percent_df5_men <- nrow(df5_men[df5_men$clicked == df5_men$correct,])/nrow(df5_men)
df5_women <- df[df$subjectID == 5 & df$voice_type=="womens",]
percent_df5_women <- nrow(df5_women[df5_women$clicked == df5_women$correct,])/nrow(df5_women)
df5_men_comp <- df[df$subjectID == 5 & df$voice_type=="mens" & df$has_comp=="competitor",]
percent_df5_men_comp <- nrow(df5_men_comp[df5_men_comp$clicked == df5_men_comp$correct,])/nrow(df5_men_comp)
df5_women_comp <- df[df$subjectID == 5 & df$voice_type=="womens" & df$has_comp=="competitor",]
percent_df5_women_comp <- nrow(df5_women_comp[df5_women_comp$clicked == df5_women_comp$correct,])/nrow(df5_women_comp)
df5_men_no_comp <- df[df$subjectID == 5 & df$voice_type=="mens" & df$has_comp=="no_competitor",]
percent_df5_men_no_comp <- nrow(df5_men_no_comp[df5_men_no_comp$clicked == df5_men_no_comp$correct,])/nrow(df5_men_no_comp)
df5_women_no_comp <- df[df$subjectID == 5 & df$voice_type=="womens" & df$has_comp=="no_competitor",]
percent_df5_women_no_comp <- nrow(df5_women_no_comp[df5_women_no_comp$clicked == df5_women_no_comp$correct,])/nrow(df5_women_no_comp)
################The data subsets for subject6################
df6_men <- df[df$subjectID == 6 & df$voice_type=="mens",]
percent_df6_men <- nrow(df6_men[df6_men$clicked == df6_men$correct,])/nrow(df6_men)
df6_women <- df[df$subjectID == 6 & df$voice_type=="womens",]
percent_df6_women <- nrow(df6_women[df6_women$clicked == df6_women$correct,])/nrow(df6_women)
df6_men_comp <- df[df$subjectID == 6 & df$voice_type=="mens" & df$has_comp=="competitor",]
percent_df6_men_comp <- nrow(df6_men_comp[df6_men_comp$clicked == df6_men_comp$correct,])/nrow(df6_men_comp)
df6_women_comp <- df[df$subjectID == 6 & df$voice_type=="womens" & df$has_comp=="competitor",]
percent_df6_women_comp <- nrow(df6_women_comp[df6_women_comp$clicked == df6_women_comp$correct,])/nrow(df6_women_comp)
df6_men_no_comp <- df[df$subjectID == 6 & df$voice_type=="mens" & df$has_comp=="no_competitor",]
percent_df6_men_no_comp <- nrow(df6_men_no_comp[df6_men_no_comp$clicked == df6_men_no_comp$correct,])/nrow(df6_men_no_comp)
df6_women_no_comp <- df[df$subjectID == 6 & df$voice_type=="womens" & df$has_comp=="no_competitor",]
percent_df6_women_no_comp <- nrow(df6_women_no_comp[df6_women_no_comp$clicked == df6_women_no_comp$correct,])/nrow(df6_women_no_comp)

#########################Calculate the percentages correct############################
######################mens#############################
threes_percent_correct_mens <- mean(percent_df2_men)
fours_percent_correct_mens <- mean(percent_df1_men,percent_df3_men,percent_df6_men)
fives_percent_correct_mens <- mean(percent_df4_men,percent_df5_men)
percent_correct_mens <- c(percent_df1_men, percent_df2_men, percent_df3_men, percent_df4_men, percent_df5_men, percent_df6_men)
mean_percent_correct_mens <- mean(percent_correct_mens)
######################mens_comp#############################
threes_percent_correct_mens_comp <- mean(percent_df2_men_comp)
fours_percent_correct_mens_comp <- mean(percent_df1_men_comp,percent_df3_men_comp,percent_df6_men_comp)
fives_percent_correct_mens_comp <- mean(percent_df4_men_comp,percent_df5_men_comp)
percent_correct_mens_comp <- c(percent_df1_men_comp, percent_df2_men_comp, percent_df3_men_comp, percent_df4_men_comp, percent_df5_men_comp, percent_df6_men_comp)
mean_percent_correct_mens_comp <- mean(percent_correct_mens_comp)
#####################mens_no_comp###########################
threes_percent_correct_mens_no_comp <- mean(percent_df2_men_no_comp)
fours_percent_correct_mens_no_comp <- mean(percent_df1_men_no_comp,percent_df3_men_no_comp,percent_df6_men_no_comp)
fives_percent_correct_mens_no_comp <- mean(percent_df4_men_no_comp,percent_df5_men_no_comp)
percent_correct_mens_no_comp <- c(percent_df1_men_no_comp, percent_df2_men_no_comp, percent_df3_men_no_comp, percent_df4_men_no_comp, percent_df5_men_no_comp, percent_df6_men_no_comp)
mean_percent_correct_mens_no_comp <- mean(percent_correct_mens_no_comp)
######################womens#############################
threes_percent_correct_womens <- mean(percent_df2_women)
fours_percent_correct_womens <- mean(percent_df1_women,percent_df3_women,percent_df6_women)
fives_percent_correct_womens <- mean(percent_df4_women,percent_df5_women)
percent_correct_womens <- c(percent_df1_women, percent_df2_women, percent_df3_women, percent_df4_women, percent_df5_women, percent_df6_women)
mean_percent_correct_womens <- mean(percent_correct_womens)
#####################womens_comp############################
threes_percent_correct_womens_comp <- mean(percent_df2_women_comp)
fours_percent_correct_womens_comp <- mean(percent_df1_women_comp,percent_df3_women_comp,percent_df6_women_comp)
fives_percent_correct_womens_comp <- mean(percent_df4_women_comp,percent_df5_women_comp)
percent_correct_womens_comp <- c(percent_df1_women_comp, percent_df2_women_comp, percent_df3_women_comp, percent_df4_women_comp, percent_df5_women_comp, percent_df6_women_comp)
mean_percent_correct_womens_comp <- mean(percent_correct_womens_comp)
######################womens_no_comp#############################
threes_percent_correct_womens_no_comp <- mean(percent_df2_women_no_comp)
fours_percent_correct_womens_no_comp <- mean(percent_df1_women_no_comp,percent_df3_women_no_comp,percent_df6_women_no_comp)
fives_percent_correct_womens_no_comp <- mean(percent_df4_women_no_comp,percent_df5_women_no_comp)
percent_correct_womens_no_comp <- c(percent_df1_women_no_comp, percent_df2_women_no_comp, percent_df3_women_no_comp, percent_df4_women_no_comp, percent_df5_women_no_comp, percent_df6_women_no_comp)
mean_percent_correct_womens_no_comp <- mean(percent_correct_womens_no_comp)
#############################################################
############OUTPUT MEAN RT AND PERCENT CORRECT###############
#############################################################
mean_RT_mens
mean_RT_mens_comp
mean_RT_mens_no_comp
mean_RT_womens
mean_RT_womens_comp
mean_RT_womens_no_comp

threes_percent_correct_mens
threes_percent_correct_mens_comp
threes_percent_correct_mens_no_comp
threes_percent_correct_womens
threes_percent_correct_womens_comp
threes_percent_correct_womens_no_comp

fours_percent_correct_mens
fours_percent_correct_mens_comp
fours_percent_correct_mens_no_comp
fours_percent_correct_womens
fours_percent_correct_womens_comp
fours_percent_correct_womens_no_comp

fives_percent_correct_mens
fives_percent_correct_mens_comp
fives_percent_correct_mens_no_comp
fives_percent_correct_womens
fives_percent_correct_womens_comp
fives_percent_correct_womens_no_comp

mean_percent_correct_mens
mean_percent_correct_mens_comp
mean_percent_correct_mens_no_comp
mean_percent_correct_womens
mean_percent_correct_womens_comp
mean_percent_correct_womens_no_comp
