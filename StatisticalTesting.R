# Comparing groups using statistical tests.
seg.df <- read.csv("http://goo.gl/qw303p")


# Chi-square test: chisq.test()
# Test whether frequencies in cells are significantly different from expectations based on total counts.
# Ex. from seg.df, equal # of respondents in each segment given marginal count of N = 300 observations.

tmp.tab <- table(rep(c(1:4), times = c(25,25,25,20)))
as.data.frame(tmp.tab)

chisq.test(tmp.tab)   # p = 0.852, FAIL TO REJECT NULL, difference between values in cells is insignificant!

# Change observation of Variable 4 from '20' to '10':
tmp.tab <- table(rep(c(1:4), times = c(25,25,25,10)))
chisq.test(tmp.tab)   # p = 0.04724, REJECT NULL, difference is significant

# NOT get similar results if smaller sample! 
tmp.tab <- tmp.tab / 5
chisq.test(tmp.tab)   # p = 0.6621, FAIL TO REJECT NULL, difference is insignificant!!!
# Difference shown to be insignificant even though proportion of people in Var - 4 is the same!
# Significance testing also dependent on sample size as well... !


# Segment sizes significantly different from eachother? 
# (Assumption: N=300 customers are random sample of larger population)
chisq.test(table(seg.df$Segment))    # p = 0.0006035, REJECT NULL, difference is significant.


# Subscription independent from home ownership? 
table(seg.df$subscribe, seg.df$ownHome)
chisq.test(table(seg.df$subscribe, seg.df$ownHome))   # p = 0.9187, FAIL TO REJECT
# Difference is insignificant.

library(tidyverse)
seg.df %>% count(subscribe)

subhome <- seg.df %>%
  group_by(ownHome, subscribe) %>%
  summarise (n = n())

table(seg.df$subscribe, seg.df$ownHome) %>% chisq.test(correct = FALSE) 
# p = 0.7854, FAIL TO REJECT, use (correct = FALSE) if not use Yates' correction default.

# Factors unrelated (no relationship), home ownership and subscription are independent from each other.

# Calculate using simulation method.
table(seg.df$subscribe, seg.df$ownHome) %>% chisq.test(sim = TRUE, B = 10000) # based on B = 10,000 replicated tables
# p = 0.8596, FAIL TO REJECT. Differences = insignificant. Factors are independent. 


# Test observed proportions: binom.test()
# observe 12 groups of Liverpool fans and 8 groups of Everton fans.
# observed 60% LFC fans significantly different from equal representation of 50%-50%?
# Likelihood of randomly observing 12/20 cases (60%), if true likelihood = 50%. 

binom.test(12, 20, p = 0.5)
# p = 0.5034
# CI: 0.36 - 0.80
# FAIL TO REJECT NULL. Difference is insignificant.
# Observation of 60% LFC fans in sample is NOT representative of entire population. 
# Entire population observation is 50%-50%. 

# Odds of observe 8~12 LFC fans out of 20 if true rate = 50%?
sum(dbinom(8:12, 20, 0.5))
# 0.736
# From observing 20 total sample fans and true split is 50%, 73.6% chance to observe between
# 8~12 LFC fans. (27.3% chance of observing fewer than 8 OR more than 12)


# IF observe 120/200 fans = LFC? (same proportion but larger sample size of total pop.)
binom.test(120, 200, p = 0.5)
# p = 0.005685
# CI: 0.52 - 0.668
# REJECT NULL. Difference is significant.
# Observation of 60% LFC fans in sample IS representative of entire population.

sum(dbinom(80:120, 200, 0.5))

# Classical binomial test is EXACT and may be overly conservative for CIs.
# Usage Agresti-Coull method from binom package:
install.packages("binom")
library(binom)
binom.confint(12, 20, method = "agresti-coull")
# CI: 0.38-0.78, compared to 0.36-0.8 in classical method. 


# Testing group means: t.test()
# Necessity to check for normality of distributions before testing. 
hist(seg.df$income)
seg.df %>% hist(income[ownHome == "ownYes"])

as.data.frame(seg.df$income) %>% hist(isencome)

income <- data.frame(seg.df$income)

hist(income)

lapply(income, hist, breaks = 10)
lapply(income[ownHome == "ownYes"], hist, breaks = 10)
lapply(seg.df$income[ownHome == "ownYes"], hist, breaks = 10)
seg.df$ownHome

with(seg.df, hist(income[ownHome == "ownYes"]))


seg.df %>% select(income, ownHome) %>% 
  filter(ownHome == "ownYes") %>% ggplot(aes(income)) + geom_histogram(bins = 40)

seg.df %>% t.test(income ~ ownHome)
seg.df %>% do(t.test(income ~ ownHome))

t.test(income ~ ownHome, data = seg.df)
# p = 0.001195, REJECT NULL, significant difference in income between HomeYes and HomeNo
# CI: -12080.155 | -3007.193      NOT include ZERO. With 95% confidence, Mean in Group ownNo
# will be -12080 to -3007 different than Mean in Group ownYes.

t.test(income ~ ownHome, data = subset(seg.df, Segment == "Travelers"))
# p = 0.7916, FAIL REJECT NULL, insignificant difference in income between HomeYes and HomeNo (for Travelers segment)
# CI: -8508.993 | 11107.604       INCLUDES ZERO. 


# ANOVA for testing multiple group means:
seg.aov.own <- aov(income ~ ownHome, data = seg.df)
anova(seg.aov.own)
# p = 0.001118, REJECT NULL, significant difference in income between HomeYes and HomeNo.

seg.aov.own <- aov(income ~ Segment, data = seg.df)
anova(seg.aov.own)
# p = 2.2e^-16, REJECT NULL, significant difference in income between Segments.

anova(aov(income ~ Segment + ownHome, data = seg.df))
# Segment: p = 2e^-16, REJECT NULL, Segment is significant predictor of income variance
# ownHome: p = 0.5772, FAIL REJECT NULL, ownHome is NOT significant predictor
# Segment and HomeOwnership NOT independent, variance effect captured by Segment by itself. 
# HomeOwnership predict variance in income for  SOME segments? Interaction effects.
anova(aov(income ~ Segment * ownHome, data = seg.df))
# Segment * ownHome: p = 0.7613, FAIL REJECT NULL
# Segment by itself is still best predictor of income variance. 

# Model comparison in ANOVA. 
anova(aov(income ~ Segment, data = seg.df),
      aov(income ~ Segment + ownHome, data = seg.df))
# p = 0.5772, FAIL REJECT NULL
# Model with Segment AND ownHome is NOT signficantly different from Model 1 (only Segment).
# Model comparison only work with NESTED models.


# Visualization of Group CIs:
install.packages("multcomp")
library(multcomp)
seg.aov <- aov(income ~ - 1 + Segment, data = seg.df) # -1 to remove intercept in default aov() formula.
glht(seg.aov)
# Output: Mean values for each Segment.

plot(glht(seg.aov))   # dot = Means, line = CIs


seg.aov.step <- step(aov(income ~ ., data = seg.df))
seg.aov.step
anova(aov(income ~ ., data = seg.df), test = "f")
anova(seg.aov.step)



# Bayesian ANOVA analysis:



