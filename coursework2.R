# Coursework 2 - Applied Statistics (UEA)

# To explore which patient demographic characteristics and organisational factors 
# influence the length of stay (LOS) among medical admissions, we record data for 
# several hospitals. The data obtained are as follows (SPSS data set length.sav 
# must be read in R):

# VARIABLE    DESCRIPTION
# id          id
# age         Patients’ average age
# risk        Risk of infection in the hospital
# rcr         Ratio of the number of infected to the number of uninfected (×100)
# xray        Ratio of the number of xrays to the number of patients (×100)
# beds        Average number of beds
# aff         University Affiliation (0: No, 1: Yes)
# avdaily     Average number of admissions per day
# nurses      Average number of nurses
# fac         Percentage of surgeries
# slength     LOS


# Read in the dataset length.sav
library(foreign)
dataset<-read.spss(file.choose())
attach(dataset)

# Import functions for backward elimination/forward selection (model_selection.txt)
source(file.choose())

# 1. Determine the best regression model with the backward elimination and with the 
#    forward selection. Explain what happens in every step. Compare the models and 
#    select the best regression model. 
x<-data.frame(AGE,RISK,RCR,XRAY,BEDS,AFF,AVDAILY,NURSES,FAC)

# Implement the functions (the threshold for the following is set to 0.1)
#    Forward selection
forward<-stepfor(SLENGTH,x,alfa=0.1)

#    Backward elimination
backward<-stepback(SLENGTH,x,alfa=0.1)

# Print summary for both to determine best
summary(forward)
summary(backward)

# Go through the steps manually
#    Forward selection
forward <- lm(SLENGTH ~ BEDS)
summary(forward)

forward <- lm(SLENGTH ~ BEDS + AVDAILY)
summary(forward)

forward <- lm(SLENGTH ~ BEDS + AVDAILY + AFF)
summary(forward)

forward <- lm(SLENGTH ~ BEDS + AVDAILY + AFF + AGE)
summary(forward)

forward <- lm(SLENGTH ~ BEDS + AVDAILY + AFF + AGE + RISK)
summary(forward)

forward <- lm(SLENGTH ~ BEDS + AVDAILY + AFF + AGE + RISK + NURSES)
summary(forward)

#     RCR p-value > than threshold
forward <- lm(SLENGTH ~ BEDS + AVDAILY + AFF + AGE + RISK + NURSES + RCR)
summary(forward)

#     XRAY p-value > than threshold
forward <- lm(SLENGTH ~ BEDS + AVDAILY + AFF + AGE + RISK + NURSES + XRAY)
summary(forward)

#     FAC p-value > than threshold
forward <- lm(SLENGTH ~ BEDS + AVDAILY + AFF + AGE + RISK + NURSES + FAC)
summary(forward)

#    Backward elimination
backward <- lm(SLENGTH ~ AGE + RISK + RCR + XRAY + BEDS + AFF + AVDAILY + NURSES + FAC)
summary(backward)
anova(backward)

# XRAY removed as it has the highest p-value greater than threshold of 0.1
backward <- lm(SLENGTH ~ AGE + RISK + RCR + BEDS + AFF + AVDAILY + NURSES + FAC)
summary(backward)
anova(backward)

# RCR removed as it has the highest p-value greater than threshold of 0.1
backward <- lm(SLENGTH ~ AGE + RISK + BEDS + AFF + AVDAILY + NURSES + FAC)
summary(backward)
anova(backward)

# FAC removed as it has the highest p-value greater than threshold of 0.1
backward <- lm(SLENGTH ~ AGE + RISK + BEDS + AFF + AVDAILY + NURSES)
summary(backward)
anova(backward)

# Backward Elimintation finished as other variables have p-values < threshold

# As found previously, using forward selection and backward elimination gives the
# same result so just assign either model to best
best<-forward

# 2. Check the adequacy of this model.
yhat<-fitted(best)
r<-residuals(best)
rstud<-rstandard(best)
rjack<-rstudent(best)
h<-hatvalues(best)
d<-cooks.distance(best)

par(mfrow=c(1,2))
qqnorm(rjack)
qqline(rjack)
hist(rjack,xlab="Jackknife residuals",main="Jackknife residuals")

graphics.off()

# checking for constant error variance
plot(yhat,rjack,xlab="Predicted values", ylab="Jackknife residuals")
abline(h=0)
abline(h=2,lty=2)
abline(h=-2,lty=2)

shapiro.test(rjack)

# 3. If the assumptions are violated transform the dependent variable
#    to correct the model and determine the best regression model.

# Square-root transformations
tforward <- stepfor(sqrt(SLENGTH),x,alfa=0.1)
tbackward <- stepback(sqrt(SLENGTH),x,alfa=0.1)
summary(tforward)
summary(tbackward)
# Both are identical
tbest <- tforward

yhat<-fitted(tbest)
r<-residuals(tbest)
rstud<-rstandard(tbest)
rjack<-rstudent(tbest)
h<-hatvalues(tbest)
d<-cooks.distance(tbest)

par(mfrow=c(1,2))
qqnorm(rjack)
qqline(rjack)
hist(rjack,xlab="Jackknife residuals",main="Jackknife residuals")

graphics.off()

# checking for constant error variance
plot(yhat,rjack,xlab="Predicted values",ylab="Jackknife residuals",main="Constance error variance")
abline(h=0)
abline(h=2,lty=2)
abline(h=-2,lty=2)

identify(yhat,rjack,ID)
shapiro.test(rjack)
# Accept null hypothesis as p-value = 0.3031 > significance level threshold 0.1

# 4. Report any outlier or influential value in this model.

# Check leverage values
# value shouldn't be larger than 2(k+1)/n = 2(6+1)/150 = 0.09333333
max(h)
# gives 0.2531936 so there seems to be problems with values
# identify the problem values
h[h > 0.09333333]
# IDs = 6,14,24,44,67,75,76,89,90,94,111,114

# Check Cook's distance; shouldn't be greater than 1
max(d)
# value not greater than one

tbest2<-lm(sqrt(SLENGTH) ~ AGE + RISK + BEDS + AFF + AVDAILY + NURSES, subset=h<0.09333333)
summary(tbest)
summary(tbest2)

# 5. In this model decide whether the addition of the interaction term between age and aff
#    contributes substantially to the prediction of the length of stay. Use all the taught 
#    tests and methods.
interaction_model<-lm(sqrt(SLENGTH) ~ AGE + RISK + BEDS + AFF + AVDAILY + NURSES + AGE * AFF, 
                      subset=h<0.09333333)
summary(interaction_model)
anova(interaction_model)

# 6. Adding the above interaction term, which are the predicted regression models for
#    (a) University Affiliation         slength = (11.081 - 4.930) + (-0.075 + 0.126) age
#    (b) Non-University Affiliation     slength =  11.081 - 0.075 age


# 7. Interpret the regression coefficients in the best regression model with the 
#    interaction term. 

# For hospitals without University Affiliation, increasing the age of a patient by a year decreases the
# length of stay on average 0.075.
# For hospitals with University Affiliation, increasing the age of a patient by a year increases
# the length of stay on average (-0.075 + 0.126 = 0.051).


# 8. Interpret the multiple R^2 in the best regression model with the interaction term.

# R^2 = 0.7913. This means that approximately 79% of the variability in the length of stay was explained
# by this regression model