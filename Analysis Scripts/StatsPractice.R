rm(list=ls())
library(ggplot2)
######################################################
###########DF and Table Basics########################
######################################################
df_death <- read.csv("~/Documents/College/Senior Year/Psych 252/earlydeaths.csv")
head(df_death)
summary(df_death)
is.data.frame(df_death)
death_table <- table(df_death$time, df_death$cause) #creates a contingency table
death_table
summary(death_table) #conducts chi-squared test on contingency table

##########################################################
############A couple more basic DF operations#############
##########################################################
d2 <- read.csv("~/Documents/College/Senior Year/Psych 252/fieldsimul1.csv")
head(d2) #gets the first 6 rows of the df
summary(d2) #gets overarching summary info
str(d2)
d2[5,] #look at the 5th participant
d2$agecat <- factor(d2$agecat) #treat a categorical variable as factor

##########################################################
############Using with to plot df data easily#############
##########################################################
with(d2, plot(age ~ agecat)) #with tells R not to need d2$ notation before each var
#same as above --> with (d2, plot(agecat, age))

##########################################################
############How to Create a New Categorical Var###########
##########################################################
d2$agecat0 <- findInterval(d2$age, c(29, 39, 49, 65)) #create a  new categorical var (factor)
d2$agecat0 <- factor(d2$agecat0) #by specifying breaks at specific intervals

##########################################################
############Make a histogram of the data##################
##########################################################
hist(d2$age, col="orange",main="Distribution of Age") #plot age in orange bars as a histogram

##########################################################
###########Make a scatterplot with a lowess line##########
##########################################################
#that is, a non-parametric best-filling curve
plot(d2$age, d2$optmism, main = "Optimism vs Age") #gets a scatterplot of the data
lines(lowess(d2$age, d2$optmism), lty = 2, col = "red") #add in a line to get a sense of the general data trend

##########################################################
###########Basic Linear Regression########################
##########################################################
rs2 <- lm(d2$optmism ~ d2$age) #fits linear model for the data using formula Y ~ X (DV ~ IV)
summary(rs2) #summarizes the data

plot(d2$optmism ~ d2$age, main="Optimism vs Age", xlab = "Age", ylab = "Optimism") #plots scatterplot
abline(rs2, col = "red") #plots regression line fit by least squares regression on the plot
plot(rs2) #gives 4 useful plots of the residuals from the lm() output
#that way you can check assumptions of normality etc are met or to identify outliers

######################################################
###########READ IN DATA FROM A URL####################
######################################################
#that is, from an online repository; header=TRUE makes the first row the header row
flu.table <- read.table("http://www-stat.stanford.edu/~jtaylo/courses/stats191/data/flu.table",header=TRUE) 
str(flu.table) #describes the dataframe
plot(flu.table, cex.labels = 2, pch = 23, bg="orange", cex = 2) #plots the data
plot(flu.table$Health.Aware, flu.table$Shot, main = "Prob(Shot) vs Awareness", xlab="Health Awareness", ylab="Shot Probability") #scatterplot of the two vars
lines(lowess(flu.table$Health.Aware, flu.table$Shot), lty=2, col="red") #overlays non parametric best-fitting curve

######Or With Another Data Set#######
rtw.table <- read.csv("http://www.ilr.cornell.edu/~hadi/RABE4/Data4/P005.txt",sep="\t") #tells it to expect tab delimiters
str(rtw.table)
#plot boxplot of cost of living by whether or not there is right-to-work law
rtw.table$RTWL = as.logical(rtw.table$RTWL) #converts RTWL 0s and 1s to logicals
boxplot(rtw.table$COL ~ rtw.table$RTWL, col="orange",pch=23,bg="red",main="Cost of Living as a Function of Right to Work Laws")

##########################################################
##############Attaching Dataframes########################
##########################################################
#be ye careful that names in the dfs don't overlap! Thar be danger there
attach(rtw.table) #no longer need to prefix with rtw.table
plot(URate, COL, pch=23, bg="red", main="Cost of living vs. Unemployment Rate") #x-axis, y-axis, shape of points, color, title
lines(lowess(URate, COL), lty=2, col = "red") #nonparametric best fitting line, x by y, pattern, color
detach(rtw.table) #absolutely critical to detach if you've attached

##########################################################
##############Simple and Multiple Regression##############
##########################################################
attach(rtw.table)
#only one single independent or 'predictor' var, which is on the right of ~, so it's simple
rs3 = lm(COL ~ URate) #dependent by independent, Y ~ X, measure ~ manipulate
summary(rs3) #look under Coefficients to see p is greater than .05 so there's not a significant
#relationship between unemployment rate and cost of living; gives you F values F(1,36)=2.803, p=0.1028
plot(rs3) #plots residuals, giving you outliers, how normal the data is, etc.

###############USING rs3 to check the Error Variance########################
#Residuals vs Fitted: Residuals let you know the quality of a regression. Residual is an observed value of y y' minus the predicted value of y
#A residuals plot with an increasing trend suggests that the error variance increases with the independent var
#A residuals plot distribution with a decreasing trend suggests the error variance decreases with the independent var
#Either of these indicates that the assumption of constant variance is not likely to be true and your regression is not good
#A horizontal band pattern suggests that the variance of the residuals is constant.

###############USING residuals to check the normality of variance########################
#A histogram of the residual can check whether the variance is normally distributed

###############Linear Regression Basics#########################
#In linear regression, data are modeled using linear predictor functions, and unknown model parameters are estimated from the data. These models are linear models.
#It's all about trying to fit a predictive model to an observed data set of y and x values.
#Linear regression models are usually fitted using the ordinary least squares (OLS) approach
#y = XB+E where y is the dependent or response var, and x is the explanatory, dependent, or covariate var,
#and Beta is a parameter vector of effects or regression coefficients; all about statistical
#estimation and inference focuses on Beta
#E is the error term (disturbance term, or noise) and is all other factors which influence
#the depdendent var y other than the regressors x. The relationship between the error term and the
#regressors, for example whether they are correlated, is a crucial step in formulating a linear
#regression model, as it determines the method to use for estimation.

###############Assumptions of OLS Method of Linear Regression###############
#Weak Exogeneity: Assume the predictor vars x can be treated as fixed values, rather than random vars; assumed to be error free
#Linearity: Mean of the response var is a linear combination of the parameters (regression coefficients) and predictor vars
#Constant Variance: different response variables have the same variance in their errors, regardless of the values of the predictor vars

#OLS minimizes the sum of squared residuals, and leads to a closed-form expression for the estimated value of unknown param Beta
#if more than one independent or 'predictor' var x, which is on the right of ~, you have a vector of predictor vars X, so it's multiple regression

###################Using rs3 to find outliers##############################
#Residuals versus Leverage plot  and scale location (square of standardized residuals by fitted values)
#give you outliers... To get rid of 6 and 34 in this case (the outliers), give R
#a negative vector with -c(6,34) and grab URate[-c(6,34)] to get everything in URate but the outliers
plot(URate[-c(6,34)], COL[-c(6,34)], pch=23, bg="red", main="Cost of living vs. Unemployment rate (no outliers")
lines(lowess(URate[-c(6,34)],COL[-c(6,34)]),lty=2,col="red")
#take out the outliers with
rs4 <- lm(COL[-c(6,34)] ~ URate[-c(6,34)])
summary(rs4) #and now you get no difference in lowess curve but effect of unemployment rate on cost of living is now marginally significant

################Multivariate Analysis#######################
rs5 <- lm(COL ~ RTWL+ URate + Income + PD) #provide multiple IVs w/ format lm(Y ~ x1 + x2 + x3...) telling us unique variance in Y explained by each IV when controlling for the other vars
#this asks if right to work law affects cost of living when controlling for the other vars, such as income
summary(rs5)
detach(rtw.table) #absolutely critical to detach if you've attached

########################################################################
######################Reshaping Dataframes##############################
########################################################################
d <- read.csv("~/Documents/College/Senior Year/Psych 252/kv0.csv")
head(d) #the data is in short-form, where each subject is its own row, and within-subject observations get their own columns
str(d)
d1 <- melt(d, id.vars = c("subidr","attnr"), measure.vars = c("num1","num2","num3"))
colnames(d1) <- c("id","attn","num","score")
head(d1) #melt converts the df to long form; id.vars are vars we want the same for each subject and measure.vars are those that are repeated measures on each subject
#you want your id.vars to be the between-subjects manipulations and the measure.vars to be the within-subjects manipulations
#note it loops through each subject by the measure.vars, first in a block of one of the id.vars and then in the block of the other id.vars var
#the df now has 3 rows for each subject
d1$num <- as.numeric(d1$num) #we want to treat num as a quantitative var
d1$id <- factor(d1$id) #we want the subject id to be a factor
d1$cond.id <- as.numeric(d1$id)
d1$cond.id[d1$attn=="focused"] = d1$cond.id[d1$attn=="focused"] - 10

########################################################################
##############Plotting with ggplot2 and reshape2########################
########################################################################
#for qplot(), since we're looking at relationship between the number var and the subjects' scores,
#the first two params are num and score. We look at each subject's data separately (id 1-10) for each
#attn condition) using the facet option. With facets, facets = y ~ x means you want the 'score v num' plots
# to be arrayed vertically (y-axis) by levels of Y, and horizontally (x-axis) by levels of X. This is analogous
#to plot() plots where the item to the left of the ~ is on the y axis and the item on the right is on the x axis.
#You can also do facets = Y ~ ., which will array frames vertically by Y-levels, and in each frame, plot 'score v num'
#at each level of X. Similarly, facets = . ~ X will array frames horizontally by X-levels and, in each frame,
#plot 'score v num'at each level of Y.
qp1 <- qplot(num, score, facets = attn ~ cond.id, data = d1) + geom_smooth(method="lm", se=F) + theme_bw()
#x-axis, y-axis, facets, data
qp2 <- qplot(num, score, geom = "line", colour = id, facets = . ~ attn, data=d1) + theme_bw() #the option geom="line" joins the points by lines and suppresses the points
#this breaks down by subject within each level of attn
qp3 <- qplot(num, score, facets = cond.id ~ attn, data = d1) + geom_smooth(method - lm, se=FALSE) + theme_bw()

##########################################################################
#############Practicing with qplot########################################
##########################################################################
head(mpg) #mpg dataframe
qplot(displ,hwy,data=mpg, color=drv) #displ on x-axis, hwy on y-axis, mpg df, color by a diff var
#geoms or lowess lines are smoothers; a statistic is just a summary of the data
qplot(displ,hwy,data=mpg, geom=c("point","smooth")) #smooths the data so you can see the overall trend in the dataset with 95% CI for the line
qplot(hwy,data=mpg,fill=drv) #You get a histogram if you just supply one arg to qplot
#facets create separate plots of subsets of your data (like panels in lattice)
qplot(displ,hwy,data=mpg,facets=.~drv) #var on left and right separated by the ~. var on the right of ~ determines columns, left is rows
qplot(displ,hwy,data=mpg,facets=drv~.,binwidth=2) #puts them in rows instead

##########################################################################
#############Density Smooth with qplot####################################
##########################################################################
qplot(hwy,data=mpg,geom="density",color=drv)
qplot(displ,hwy,data=mpg,color=drv,geom=c("point","smooth"),method="lm") #have smoothing line with regression and CI
qplot(displ,hwy,data=mpg,facets=.~drv,geom=c("point","smooth"),method="lm") #separate into separate windows this time
str(mpg)

##########################################################################
###########Building a ggplot from the ground up###########################
##########################################################################
qplot(displ,hwy,data=mpg,facets=.~drv,geom=c("point","smooth"),method="lm") #is the same as
q <- ggplot(mpg, aes(displ,hwy))
summary(q) #can give you useful insight into how ggplot looks at the data
p <- q + geom_point() #now that you add in how you want the data visualized, it can be plotted
r <- q + geom_point() + geom_smooth() #defaults to lowess smoother
r <- q + geom_point() + geom_smooth(method="lm") #now uses regression line instead of lowess smoother
r <- q + geom_point() + facet_grid(. ~ drv) + geom_smooth(method="lm") #facet_grid(rows ~ cols)
#labels: xlab(), ylab(), labs(), ggtitle(); use theme() for global changes
r <- q + geom_point(aes(color=drv),size=4) + geom_smooth(method="lm") #modifiers in aes() for geom_point changes aesthetics; color assigned to data var, size of point, some transparency
q <- q + geom_point(aes(color=drv),size=4) + geom_smooth(method="lm")

##########################################################################
##############T-Tests and the t statistic ################################
##########################################################################
d0 <- read.csv("http://www.ilr.cornell.edu/~hadi/RABE4/Data4/P005.txt",sep="\t")
str(d0)
boxplot(COL ~ RTWL, col = "yellow", pch = 23, xlab = "RTWL",
        ylab = "COL", main = "COL VS RTWL", data = d0) #plot to see relationship between RTWL and COL
#######convert to short-form for t-test########
COL0 = d0$COL[d0$RTWL == 0] #cities without Right to Work Law
COL1 = d0$COL[d0$RTWL == 1] #cities with Right to Work Law
str(COL0) #take a look at the variables, real fast
str(COL1) #look at em both!
############################################################
######Run a t-test testing H0 that the means are equal######
############################################################
res1 = t.test(COL0, COL1, var.equal = TRUE) #t-test assuming variances are equal
res1 #look at the t-test data
c(d0 = sd(COL0), sd1 = sd(COL1)) #check SDs to get an idea if the variances are the same
var.test(COL0, COL1) #directly test whether the variances are the same with ANOVA
####if the data is paired, add paired=TRUE to the t.test params####
rtwl = rep(c(0,1), each=14) #rep() is for repeat; gives you 14 0s and 14 1s
###########################################
######Additive and Interactive Models######
###########################################
plot(COL ~ Income, data = d0) #plot income and COL
abline(lm(COL ~ Income, data=d0), col = "red") #plot linear line of best fit

#############################################################################################
#Say we're interested in how the effect of RTWL on COL is related to income level.
#In an additive model, (e.g., COL ~ RTWL + INCOME), the effect of RTWL on COL is
#assumed to be teh same at all levels of Income. Here, if income level were
#tightly correlated with whether or not cities have a RTWL, the unique variance explained
#by RTWL and Income level might be small.
#
#In an interactive model, however, (e.g., COL ~ RTWL * Income) the effect of RTWL on COL
#is NOT assumed to be the same at all levels of Income. Here, the RTWL could be positively
#correlated with COL at low incomes, but negatively correlated at high incomes. Just sayin'.
#############################################################################################
d0$Incomecat = findInterval(d0$Income, 4000) #splits Income data into <4000 and >=4000
d0$Incomecat = factor(d0$Incomecat, labels = c("low", "high")) #makes it a factor, and adds appropriate labels
table(d0$Incomecat, d0$RTWL) #show how many cities are in each group (shows only 2 cities have RTWL and high income, so there's probz an interaction there)

#######as an additive model#######
res3 = lm(COL ~ RTWL + Incomecat, data = d0)
summary(res3)
#######as an interactive model#######
res4 = lm(COL ~ RTWL * Incomecat, data=d0)
summary(res4) #shows that the interaction is SUPER significant
with(d0, interaction.plot(RTWL, Incomecat, COL, fun=mean, #shows that when income is high, effect of RTWL on COL is small
                          xlab = "RTWL", ylab = "COL", lwd=2)) #but when income is low, COL is lower if there's a RTWL

#source("mc.plots1.r") #use source() to run an r script externally

##########################################################################
#############More Practice with all of this###############################
##########################################################################
d2 <- read.csv("~/Documents/College/Senior Year/Psych 252/exer.csv")
str(d2)
summary(d2)
#######Wewant to convert id, diet, and exertype to factors with informative levels#####
d2$diet <- factor(d2$diet, labels=c("lo.fat","non.lo.fat"))
d2$id <- factor(d2$id)
d2$exertype <- factor(d2$exertype, labels = c("rest","walk","run"))
summary(d2) #double check the formatting worked out ok

####Plot with qplot()
qp3 = qplot(time, pulse, facets = diet ~ exertype, colour = id, geom = "line", data = d2)
    + theme_bw() + plot.style
print(qp3)

####Plot as barplots; use aggregate() to get means and standard errors from the data
ms <- aggregate(pulse ~ time + diet + exertype, d2, mean) #gets mean pulses across subjects for each of the six groups at all three points
ms
se.mean <- function(x) { #gives us a standard error of the mean function
  sd(x) / sqrt(length(x))
}
ms$err <- aggregate(pulse ~ time + diet + exertype, d2, se.mean)$pulse #gives us a column with standard error of mean
ms
qp4 <- qplot(time, pulse, facets = . ~ diet, ymin = pulse - err, ymax = pulse +
               err, geom = c("pointrange","line"), colour = exertype, data = ms) +
               theme_bw()
qp4

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