library(lme4)
library(mosaic)

bloodpressure = read.csv("../data/bloodpressure.csv")
head(bloodpressure)

# Naive approach: treat every observation as independent
mu_hat = mean(systolic ~ factor(treatment), data=bloodpressure)
sigma_hat = sd(systolic ~ factor(treatment), data=bloodpressure)
sample_size = tally( ~ factor(treatment), data=bloodpressure)

# naive standard errors
sigma_hat/sqrt(sample_size)

# Notice the structure of the data
xtabs( ~ factor(subject) + factor(treatment), data=bloodpressure)

# Naive strategy: add dummy variables for each subject
# Notice how drastically wrong the answer is for this "fixed effects" model
lm3 = lm(systolic ~ treatment + factor(subject), data=bloodpressure)
summary(lm3)

# How do we know this is badly wrong?
# Just average the person-level data and treat
# treat each average as a single data point
person_mean = mean(systolic ~ factor(subject), data=bloodpressure)
treatment_ind = c(rep(1,10), rep(2,10))

# Mean and standard deviation for each group
mean(person_mean ~ factor(treatment_ind))
sd(person_mean ~ factor(treatment_ind))

# not a crazy answer and easy to phrase as a linear model
lm1 = lm(person_mean ~ factor(treatment_ind))
summary(lm1)

# But this doesn't account for varying sample sizes
xtabs( ~ factor(subject), data=bloodpressure)

# we need a model that accounts for the fact that within-person correlation
# of measurements exceeds between-person correlation
