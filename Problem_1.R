## Homework 4, Problem 1

library(dplyr)

## Read the data
houseWork <-
  read.csv('https://s3.eu-central-1.amazonaws.com/econometrics2018/data/houseWork.csv')
str(houseWork)

## a)
summary(houseWork)
# 4252 females and 6764 males are observed in the data set

## b)
hoursf <- houseWork[houseWork$sex == "f", "hours"]
hoursm <- houseWork[houseWork$sex == "m", "hours"]
muf <- mean(hoursf)
mum <- mean(hoursm)

## c)

## Easier:

houseWork <- within(houseWork, {
    female <- sex == 'f'
})

male <- ifelse (houseWork$sex == "m", TRUE, FALSE)
female <- ifelse (houseWork$sex == "f", TRUE, FALSE)
houseWork <- within (houseWork,
                     male <- male)
houseWork <- within (houseWork,
                     female <- female)

## d)

model1 <- lm(hours ~ female, data = houseWork)
summary(model1)

## e)
## /score -1
#The intercept shows us the situation where female=TRUE=0, which means that
# it shows us the estimated average hours of housework done by males per week. (32.81)
# the coefficient beta1 = -14.46, shows that if we add another woman, the average
# working hours per week will decrease.

## f)
## H0 states that women spend on average less time with housework than men.

#H0 is beta1 >=0 and H1 beta1 <0. The null hypothesis states that the hours woman work
# play a role in the average hours worked and the alternative hypothesis states the opposite.
# Looking at the results from model1 we can reject the null hypothesis, as the models
# shows that there is a negative relationship between the two variables.

## g)
## Wrong test /score -2
populationMean <- mean(houseWork$hours)
testStatistic <- sqrt(11016) * (muf - populationMean) / 0.3186
pt(testStatistic , df = 11014)

## h)
# We can reject the null hypothesis since the probability of making a mistake
# by rejecting it is almost 0.

## i)
## /score -2
#The test assumes that the population follows a t-distribution, but
# our expectations are that the population follows a normal distribution.

## j)
## Coefficient for male cannot be estimated due to multicollinearity.

model2 <- lm(hours ~ female + male, data = houseWork)
summary(model2)

# the beta2 coefficient shows NA because when male is true female is false and
# this is expressed in the intercept coefficient
