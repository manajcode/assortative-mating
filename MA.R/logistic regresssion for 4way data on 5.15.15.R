#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@ logistic regression in R @@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# originally I ran the logistic regression in JMP to assess 
# how time of data and male holding number affected assortative mating.
# several kinds of data, independent variables:
#   am or pm: a varaible. character data, essentially binary.
#   male holding number: a variable. numerical data, essentially binary.
#   assortative mating: data recorded. numeric, binary data.

# Dependent variables:
#   frequency of assortative mating: the ratio of assort/total matings. numeric continuous data.


#step 1: upload data in r:
setwd("C:/Program Files/RStudio/Man R-directory")

all.data = read.csv("logsitic regression data for R analysis on 5.15.15.csv")

#step 2: use all.data and modify it to include frequencies, proportion of assort
# amd disassort, and wing clip variable 
summary(all.data)  for all columns
str(all.data)
all.data$a.freq <- (all.data$assort/all.data$N)
all.data$d.freq <- ((all.data$N-all.data$assort)/all.data$N)
all.data$disassort <- (all.data$N-all.data$assort)

all.data$wingclipeffect <- c("no")
all.data[16:19,11] <- "yes"

# here turn on library plyr and merge data based on the mating pattern and the 
# number of assortative or disassortative matings.
library(plyr)
library(reshape)
library(reshape2)

all.data <- all.data[,-(8:9)]

melt.data.frame(all.data, id.var = c("date","strain","male.hold",
                                     "population","time.day","N","wingclipeffect"))
all.data <- melt.data.frame(all.data, id.var = c("date","strain","male.hold",
                                                 "population","time.day","N","wingclipeffect"))
all.data$freq <- (all.data$value/all.data$N)
mode(all.data$freq)

# order the data by genotype, mating pattern, then date
ndx = order(all.data$strain,all.data$variable,all.data$date)
freq.data = all.data[ndx,]

# now split the data frame into 2 based on genotype.
all.ral <- as.data.frame(freq.data[1:16,])
can.s <- as.data.frame(freq.data[17:42,])


# step 3: run regressions. 
allral.regression <- glm(variable~value + time.day + population, data =all.ral, family=binomial(logit))
summary(allral.regression)
plot(allral.regression)

cans.regression <- glm(variable~value + male.hold + time.day + population + wingclipeffect, data =can.s, family=binomial(logit))
summary(cans.regression)
plot(cans.regression)

#outputs below:
#ALL RAL
#Call:
#glm(formula = variable ~ value + time.day + population, family = binomial, 
#      data = all.ral)

#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.24842  -1.18649   0.01321   1.16840   1.25309  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)  0.31219    2.13145   0.146    0.884
#value       -0.01068    0.04228  -0.252    0.801
#time.daypm  -0.18945    1.43750  -0.132    0.895
#population  -0.05070    1.24342  -0.041    0.967

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 22.181  on 15  degrees of freedom
#Residual deviance: 22.117  on 12  degrees of freedom
#AIC: 30.117

#Number of Fisher Scoring iterations: 3



#CANTON_S
#Call:
#  glm(formula = variable ~ value + male.hold + time.day + population + 
#        wingclipeffect, family = binomial(logit), data = can.s)

#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.44404  -1.18260   0.07539   1.10163   1.45620  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)        0.27017    1.95291   0.138    0.890
#value             -0.03253    0.03597  -0.904    0.366
#male.hold          0.03647    0.10640   0.343    0.732
#time.daypm        -0.11653    0.87693  -0.133    0.894
#population        -0.27518    0.96509  -0.285    0.776
#wingclipeffectyes -0.08955    1.07648  -0.083    0.934

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 36.044  on 25  degrees of freedom
#Residual deviance: 35.181  on 20  degrees of freedom
#AIC: 47.181

#Number of Fisher Scoring iterations: 4


#step 4: confidence intervals for coefficients for each independent variable.
confint(allral.regression)
confint(cans.regression)

#step 5: Anova to see if terms interact
anova(cans.regression, test="Chisq")
anova(allral.regression, test="Chisq")