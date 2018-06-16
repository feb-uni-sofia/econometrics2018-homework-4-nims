## Homework 4, Problem 2
crime <-
  read.delim(
    'https://s3.eu-central-1.amazonaws.com/econometrics2018/data/crime.csv',
    stringsAsFactors = FALSE
  )
str(crime)

## a)

fit <- lm(C ~ HS, data = crime)
summary(fit)
#the intercept shows that the estimated crime rate in a country,
# where no one has finished high school is around minus 50.
# the beta1 coefficient = 1.48, shows that an increase of people who has graduated
# high school with 1 percent is observed with an estimated increase of crime rate of 1.48 (cases per 1000 persons)
## b)
crime2 <- crime[,-1]
pairs( ~ ., data = crime2)

## c)

fit2 <- lm(C ~ HS + U, data = crime)
summary(fit2)

## /score -0.5 answer not specific enough

#We can explain the change in the sign is due to some kind of a relationship
# between the two variables HS and U (what kind of relationship???). There is multicolinearity, meaning one feature
# variable can be explained by another. Because of this the estimates for the change
                                        # in the explained variable may not be accurate.


## d)
## /score -1 not specific enough!
# Even though the model shows that there is some kind of a negative relationship
# between the two variabes we cannot conclude that there is causation.
# (няма причинно-следствена връзка). Also there are other variables that
#may influence the crime rate but are not included in our model.

## e)

fit3 <- lm(C ~ HS + U + I, data = crime)
summary(fit3)

#We cannot reject the null hypothesis stating that beta1 and beta3 are zero with a
#5% significance level since the p-value is more than 0.05
# (0.4 for beta1 and 0.69 for beta3)

## /score -5

