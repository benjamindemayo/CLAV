#######################################################################
##################### CLAV iPAD Data Analysis #########################
#######################################################################

#######################################################################
######################### PRELIMINARIES ###############################
#######################################################################
#clear all previous variables
rm(list=ls())

#load libraries for data manipulation and graphing
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)

#some useful functions for computing measurement error
sem <- function(x) {sd(x)/sqrt(length(x))}
ci95 <- function(x) {3.96*sem(x)}

#######################################################################
#################### LOAD IN THE DATA FROM .CSV #######################
#######################################################################
d <- data.frame() #initializes df var
files <- dir("~/Documents/Research/CLAV_iPad/ResultsCSVs/",pattern="*.csv")

for (f in files) { #iterates through folder
  df <- read.csv(paste("~/Documents/Research/CLAV_iPad/ResultsCSVs/",
                       f,sep=""))
  d <- rbind(d, df) #constructs master dataframe
}
str(d)

#######################################################################
#################### CLEAN THE DF #####################################
#######################################################################
##clip RTs outside 2 SDs##
str(d) #look at a quick overview of d data
qplot(RT, data=d) #look at a preliminary plot to get an idea of RT spread
m.rt <- exp(mean(log(d$RT),na.rm=TRUE)) #take log average of RT: average natural logs of RTs, then take the exponential of that
sd.rt <- exp(sd(log(d$RT),na.rm=TRUE)) #geometric standard deviation: exponentiated value of the standard deviation of the log-transformed values
qplot(RT, data=d) + 
  geom_vline(xintercept=m.rt + 2*sd.rt, col="red", lty=2) + #lets us view how much is spread 2 SDs above mean
  geom_vline(xintercept=m.rt - 2*sd.rt, col="red", lty=2) + #lets us view how much is spread 2 SDs below mean
  xlab("RT spread") +
  ylab("Frequency") +
  ggtitle("RT Histogram of CLAV Data (marking +/- 2 SDs)")

with(d, mean(RT > m.rt - 2*sd.rt & RT < m.rt + 2*sd.rt)) #calculate the mean to check what it is in console
df <- subset(d, RT > m.rt - 2*sd.rt & RT < m.rt + 2*sd.rt) #get the subset of the df between 2 SDs of mean (dat's legit, man)
df <- subset(d, RT > 0) #gets rid of negative RTs
##change up some minor aesthetic details##
#df <- subset(df,select=-c(X)) #remove the mysterious X column
df$subjectID <- factor(df$subjectID) #convert subjectID to a factor
df$is.correct <- df$clicked==df$correct #add a correct column as a logical of whether 'clicked' matched 'correct'

##merge in demographics##
str(df) #quick overview of the df before the merge
demos <- read.csv("~/Documents/Research/CLAV_iPad/clav_demographics.csv")
str(demos) #get a quick look at the demos file to see if something's amiss
df <- merge(df, demos, by.x="subjectID", by.y="subjectID") #by.x says in the df, merge by subjectID and by.y says in the demos df merge by subjectID 
df$age <- factor(df$age)
str(df) #check this to make sure it merged correctly!

#df <- df[df$tar_name != "Mug",]
#df <- df[df$tar_name != "Pajamas",]
#df <- df[df$tar_name != "Bicycle",]
####################################################################################
#################### ANALYSIS: Aggregating Mean RT #################################
####################################################################################

####################################################################################
##Notes on aggregate(), since you always forget:####################################
##aggregate plays nicely with ggplot aggregate(output DV var ~ predictors in order##
##you want the columns to cycle thru, dataframe, statistical function) #############
##gives you a new df with the summary statistic you want (here it's the mean) ######
##I want to know what the mean is for each voice type, but I want to know for ######
##each trial type as well. Returns a dataframe consisting of the aggregated stats ##
##It gives you a summary stat (here means) for each combination of the predictor ###
##variables I give; it's basically creating a pivot table, right-most predictor var#
##is in blocks, left-most predictor var is cycling thru the fastest ################
####################################################################################

plot.style <- theme(panel.grid.major=element_blank(), legend.position="right", plot.title=element_text(face="bold", size=14), axis.title=element_text(size=12))
PD <- position_dodge(.9)

####Plot Reaction Time by Age
quartz()
rts <- aggregate(RT ~ has_comp + age, df, mean) #aggregate mean RTs by voice, trial type
rts.SEM <- aggregate(RT ~ has_comp + age, df, sem)
rts$SEM <- rts.SEM[,3]
qplot(age, RT, fill=has_comp, #plot bar graph with aggregated means dataframe
      geom="bar",
      position="dodge",
      stat="identity",
      data=rts) +
      geom_errorbar(aes(ymin=RT-SEM, ymax=RT+SEM), colour="black", width=.2, position=PD) +
      xlab("Subject Age") +
      ylab("RT (mean)") +
      ggtitle("Reaction Time by Trial Type and Age") +
      scale_fill_discrete(name="Trial\nType",labels=c("Competitor","No Competitor")) +
      plot.style

####Plot Proportion Correct by Age
quartz()
correct <- aggregate(is.correct ~ has_comp + age, df, mean)
correct.sd <- aggregate(is.correct ~ has_comp + age, df, sd)
correct.SEM <- aggregate(is.correct ~ has_comp + age, df, sem)
correct$SEM <- correct.SEM[,3]
qplot(age, is.correct, fill=has_comp, #plot bar graph with aggregated corrects dataframe
      geom="bar",
      position="dodge",
      stat="identity",
      data=correct) +
  geom_errorbar(aes(ymin=is.correct-SEM, ymax=is.correct+SEM), colour="black", width=.2, position=PD) +
  geom_hline(yintercept=.50,linetype="dashed") +
  xlab("Subject Age") +
  ylab("Proportion Correct (mean)") +
  ggtitle("Proportion Correct by Trial Type and Age") +
  scale_fill_discrete(name="Trial\nType",labels=c("Competitor","No Competitor")) +
  plot.style +
  geom_text(aes(1.9,0.99,label="***"),colour="black",size=5) + 
  geom_text(aes(2.9,0.99,label="**"),colour="black",size=5)

####Plot Regression Line by Agenum (note that this doesn't account for random effects!)
correctnum <- aggregate(is.correct ~ subjectID + agenum + gender + has_comp, df, mean)
correctnum <- correctnum[correctnum$has_comp=="competitor",]
correctnummale <- correctnum[correctnum$gender=="Male",]
correctnumfemale <- correctnum[correctnum$gender=="Female",]
ggplot(correctnum, aes(x=subjectID, y=is.correct, color=gender, group=1)) +
  ggtitle("Relationship of Score and Age\n") +
  xlab("Age of Child") + ylab("Child Score") +
  geom_point(shape=16, 
             alpha = 0.5) +
  geom_smooth(method=lm, color = 'red')

ggplot(correctnummale, aes(x=subjectID, y=is.correct, color=gender, group=1)) +
  ggtitle("Relationship of Score and Age for Males\n") +
  xlab("Age of Child") + ylab("Child Score") +
  geom_point(shape=16, 
             alpha = 0.5) +
  geom_smooth(method=lm, color = 'red')

ggplot(correctnumfemale, aes(x=subjectID, y=is.correct, color=gender, group=1)) +
  ggtitle("Relationship of Score and Age for Females\n") +
  xlab("Age of Child") + ylab("Child Score") +
  geom_point(shape=16, 
             alpha = 0.5) +
  geom_smooth(method=lm, color = 'red')

###############################
######Mixed Effect Modeling####
###############################

###Mixed Effect Correct Model without Interaction
stereotype.mixed.model <- glmer(is.correct ~ age + voice_type + has_comp + (1|subjectID) + 
                           (1|tar_name), data=df, family=binomial, na.action=na.omit) 
summary(stereotype.mixed.model)
###Mixed Effect Correct Model with Gender, no interaction
stereotype.mixed.gender.model <- glmer(is.correct ~ age + gender + voice_type + has_comp + (1|subjectID) + 
                                  (1|tar_name), data=df, family=binomial, na.action=na.omit) 
summary(stereotype.mixed.gender.model)
###Compare the age and age + gender models
anova(stereotype.mixed.model,stereotype.mixed.gender.model)

###Mixed Effect Correct Model with Gender, with interaction
stereotype.mixed.gender.int.model <- glmer(is.correct ~ age * gender + voice_type + has_comp + (1|subjectID) + 
                                         (1|tar_name), data=df, family=binomial, na.action=na.omit) 
summary(stereotype.mixed.gender.int.model)

###Compare the age + gender and age * gender models
anova(stereotype.mixed.gender.model,stereotype.mixed.gender.int.model) #interaction terms dont give a significantly better model

###Mixed Effect RT Model without Interaction
stereotypeRT.mixed.model <- lmer(RT ~ age + gender + has_comp + (1|subjectID) + 
                                  (1|tar_name), data=df, na.action=na.omit) 
summary(stereotypeRT.mixed.model)

###Check whether fours and fives are significantly different; contrasts change to make 4s default
contrasts(df$age) <- cbind(c(1,0,0),c(0,0,1))
stereotype.mixed.model2 <- glmer(is.correct ~ age + voice_type + has_comp + (1|subjectID) + 
                                  (1|tar_name), data=df, family=binomial, na.action=na.omit) 
summary(stereotype.mixed.model2)

contrasts(df$age) <- cbind(c(0,0,1),c(0,1,0)) #change the contrasts back
#################################################################
######Plotting Logistic Regression Modeling With Mixed Effects###
#################################################################




#########################################################
###Statistical power analysis and effect size############
##################################################################
#####Cohen's d is a descriptive statistic used to estimate########
#the sample size necessary to see an effect with substantial######
#statistical power. d is (m1 - m2)/sd for your sample population##
#This is a good thing to calculate after you have pilot data, as##
#it gives you an idea of how many subjects you will need in total#
#to get a main effect with substantial size.
#
#Effect size is a measure of the strength of a phenomenon; it
#conveys the estimated magnitude of a relationship without making
#any statement about whether the apparent relationship in the data
#reflects a true relationship in the population.
#
#A lower Cohen's d indicates the necessity of larger sample sizes,
#and can be determined together with the additional parameters of 
#desired significance level (p > .05, duh) and statistical power
#
mean_correct <- aggregate(is.correct ~ has_comp, df, mean) #calculate mean correct by condition
sd_correct <- aggregate(is.correct ~ has_comp, df, sd) #calculate sd correct by condition

###########################################################
#divide (no_competitor correct minus competitor correct)###
#from mean_correct by sd(df$is.correct) to get Cohen's d.##
###########################################################
mean_correct #check to see which mean is higher (no_competitor)
m1_minus_m2 <- mean_correct$is.correct[2] - mean_correct$is.correct[1] #difference in means for competitor effect
cohens_d <- m1_minus_m2 / sd(df$is.correct) #calculate cohen's d
cohens_d #for critical as opposed to noncritical
num3_4 <- correct$is.correct[3] - correct$is.correct[1]
cohens_d2 <- num3_4 / sd(df$is.correct)
cohens_d2 #4s as opposed to 3s
num4_5 <- correct$is.correct[5] - correct$is.correct[3]
cohens_d3 <- num4_5 / sd(df$is.correct) 
cohens_d3 #5s as opposed to 4s
num3_5 <- correct$is.correct[5] - correct$is.correct[1]
cohens_d4 <- num3_5 / sd(df$is.correct)
cohens_d4 #5s as opposed to 3s
###################################################################
#Now for the a priori power analysis. Statistical power is defined#
#as the conditional probability that a test correctly rejects the #
#null hypothesis when the null hypothesis is false. ###############
#
#Power = P(reject null hypothesis | null is false) OR
#Power = P(accept alternative hypothesis | alternative is true)
#
#As statistical power increases, the probability of a type 2
#error decreases. (since power is 1 - Beta, if power is larger,
#Beta must be getting smaller)
#
#Type 1: Rejecting the null hypothesis when it was actually true (false positive)
#Type 2: Not rejecting a false null hypothesis (false negative)
#
#Beta is the false negative rate and power is equal to 1 - Beta
#
#Factors influencing power:
#1. Statistical significance criterion used in your test: alpha, usually .05, .01, or .001
#2. The magnitude of the effect of interest in the population: quantified as effect size
#3. The sample size used to detect the effect: effects harder to detect in smaller samples
#
#Using G*Power program for the CLAV iPad study experiment 1,
#we need 48 total! N=48 24 3 to 3+ and 24 4 to 4+
#12 per gender per bin
#
#As estimated, we would need 28 kids to see the power we want reliably; independent of those 6
#Effect size (now) is means/SD (m1 - m2 / sd). Power with 24 kids around 90%.
#
#We have 8 lists, used equally means 6 kids per list, 3 girls per list, 3 boys per list
#12 girl 3s
#12 boy 3s
#12 girl 4s
#12 boy 4s


      
