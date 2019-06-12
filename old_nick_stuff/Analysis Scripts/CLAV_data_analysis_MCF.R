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

d <- data.frame() #initializes df var
files <- dir("~/Documents/Research/CLAV_iPad/ResultsCSVs/",pattern="*.csv")

for (f in files) { #iterates through folder
  df <- read.csv(paste("~/Documents/Research/CLAV_iPad/ResultsCSVs/",
                       f,sep=""))
  d <- rbind(d, df) #constructs master dataframe
}

##clip RTs outside 2 SDs
qplot(RT, data=d)
m.rt <- exp(mean(log(d$RT),na.rm=TRUE)) #take log average of RT: average natural logs of RTs, then take the exponential of that
sd.rt <- exp(sd(log(d$RT),na.rm=TRUE)) #geometric standard deviation: exponentiated value of the standard deviation of the log-transformed values
qplot(RT, data=d) + 
  geom_vline(xintercept=m.rt + 2*sd.rt, col="red", lty=2) +
  geom_vline(xintercept=m.rt - 2*sd.rt, col="red", lty=2)

with(d, mean(RT > m.rt - 2*sd.rt & RT < m.rt + 2*sd.rt)) #claculate the mean to check what it is
df <- subset(d, RT > m.rt - 2*sd.rt & RT < m.rt + 2*sd.rt) #get the subset between 2 SDs

#######################################################################
#################### CLEAN THE DF #####################################
#######################################################################
df$is.correct <- df$clicked==df$correct #add a correct column as a logical of whether 'clicked' matched 'correct'
df <- subset(df,select=-c(X)) #remove the mysterious X column
df$subjectID <- factor(df$subjectID) #convert subjectID to a factor

##merge in demographics##
str(df)
demos <- read.csv("~/Documents/Research/CLAV_iPad/clav_demographics.csv")
str(demos)
df <- merge(df, demos, by.x="subjectID", by.y="subjectID") #by.x says in the df, merge by subjectID and by.y says in the demos df merge by subjectID 
str(df)

#######################################################################
#################### CALCULATE MEANS ##################################
#######################################################################
##aggregate plays nicely with ggplot aggregate(output DV var ~ predictors in order you want the columns, dataframe, statistical function)
rts <- aggregate(RT ~ voice_type + has_comp, df, mean) #gives you a new df with the summary statistic you want (here it's the mean)
#I want to know what the mean is for each voice type, but I want to know for each trial type as well.
#It gives you means for each combination of the predictor variables I give
#it's basically creating a pivot table, right-most is in blocks, left-most is cycling thru the fastest
qplot(has_comp, RT, fill=voice_type, 
      geom="bar",
      position="dodge",
      stat="identity",
      data=rts)

accs <- aggregate(is.correct ~ voice_type + has_comp, df, mean)
qplot(has_comp, is.correct, fill=voice_type, 
      geom="bar",
      position="dodge",
      stat="identity",
  data=accs)

######################################################
########Aggregate by individual and then plot#########
######################################################
#as a bar plot#
rts.subj <- aggregate(RT ~ voice_type + has_comp + subjectID, df, mean)
qplot(has_comp, RT, fill=voice_type, facets=.~subjectID, geom="bar",
      stat="identity",
      position="dodge",
      data=rts.subj)
#as a scatterplot#
rts.subj <- aggregate(RT ~ voice_type + has_comp + subjectID, df, mean)
qplot(has_comp, RT, colour=voice_type, facets=.~subjectID, geom="point",
      stat="identity",
      data=rts.subj)
#as a line plot#
rts.subj <- aggregate(RT ~ voice_type + has_comp + subjectID, df, mean)
qplot(has_comp, RT, colour=voice_type, facets=.~subjectID, geom="point",
      stat="identity",
      data=rts.subj) + geom_line(aes(group=voice_type))

#aggregate by subjectID and then plot#
rts.subj <- aggregate(RT ~ voice_type + has_comp + Name + upper_left, df, mean)
qplot(has_comp, RT, fill=voice_type, facets= . ~ Name, geom="bar",
      stat="identity",
      position="dodge",
      data=rts.subj) + xlab("Trial Type")
      + ylab("Reaction Time")
      + ggtitle("RTs by SubjectID")
      + plot.style
