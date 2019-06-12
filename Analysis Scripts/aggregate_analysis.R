### Practicum Psych 254 - Variable checking and aggregation
## Goals of this assignment
## 1. Review the distribution and types of your inputs to R 
## 2. Explore questions about data using aggregate (pivot table replacement)

### BASIC SETUP AND READING IN DATA ###
# remove all the variables from our workspace
rm(list=ls())  ## I usually start scripts this way

# set the working directory using setwd(dir)
#setwd("~/Projects/R/psych254/aggregate example/")

# read in the data from a CSV file using read.csv
data <- read.csv("~/Documents/Research/CLAV_iPad/scales_data.csv")

### PART 1 ###
# from here, you'll need to fill in the blanks
# look at the top part of the dataset using head
head(data)

# check on the levels of individual variables 
# and that factors have been read in correctly
# you can do this using "summary(variable)" or "head(variable)"
str(data)
head(data$age)
head(data$trial)
summary(data)

# check distribution of ages in the study using hist.
# take a look at the help using ?hist and see if you can get the bins to line up 
# at year or half-year boundaries. Which is most informative?
hist(data$age) #normal histogram of the data
hist(data$age,breaks=c(2:5)) #bins to line up at a year boundary
hist(data$age,breaks=c(seq(2,5,.5)) #half-year much more informative! seq() helps everything line up right

# find out how many participants there are total
length(unique(data$subid))

### LEARNING TO USE AGGREGATE ###
# aggregate is a function that works like a pivot table: 
# you give it a formula specifying the arrangement of DV and IV, and the 
# function and data to use, and it outputs the resulting data frame, for example:
aggregate(Sepal.Length ~ Species, iris, mean)

# use aggregate to find the mean number of correct answers by age group
# a formula can be of the form y ~ x1 + x2 + x3 etc. 
aggregate(correct ~ age.group + condition, data, mean)

# try aggregating a different variable: find out how many subjects in each condition
# use this function below and aggregate subids
n.unique <- function(x) {length(unique(x))}
aggregate(subid ~ age.group + condition, data, n.unique)

# now aggregate correct across item ("trial") alone
aggregate(correct ~ trial, data, mean)

# and across items and condition
aggregate(correct ~ trial + condition, data, mean)

# and items, age groups, and conditions. do you see any "item effects"
# try also reordering the factors on the right side of your equation!
aggregate(correct ~ trial + age.group + condition, data, mean)
aggregate(correct ~ trial + condition + age.group, data, mean)
aggregate(correct ~ condition + trial + age.group, data, mean)

## CHALLENGE PROBLEM ###
# get the standard error of the mean ACROSS SUBJECTS using the SEM function
# do this by age and condition, but not item
sem <- function (x) {sd(x) / sqrt(length(x))}

# first aggregate the mean for each subject (assign this to a new data frame)
subject.means <- aggregate(correct ~ age.group + condition + subid, data, mean)

# THEN aggregate sem across subjects 
aggregate(correct ~ subid, subject.means, sem(subject.means))
