# Create amusement park data set: 
# weekend: respondent visited on weekend
# num.child: # of children along on trip
# distance: distance traveled to park
# overall: expressed satisfaction overall     * 100-point scale
# rides: satisfaction with rides
# games: satisfaction with games
# wait: satisfaction with wait times
# clean: satisfaction with cleanliness

#####




#####
library(corrplot)
library(gpairs)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(scales)
library(dplyr)
library(broom)


sat.df <- read.csv("~/R materials/MarketResearchAnalysis/rintro-chapter7.csv")
summary(sat.df)

# Check for normality in variables, check joint relationships among variables:


gpairs(sat.df)
# See that 'distance' is skewed.
# Transform using common log transformation!
hist(sat.df$distance)
sat.df$logdist <- log(sat.df$distance)
gpairs(sat.df) # or
hist(sat.df$logdist)

# Most rest of variables are normal distribution.
# Joint relationships: ~elliptical shape of scatterplots, lower right variables = positive correlation.
# Concerns: satisfaction survey variables = usually ^correlation with each other. 
# Respondents might form overall halo rating = rate individual elements along overall feelign.
# When in effect = difficult to assess each individual element as individual effect on statistical models.

# Investigate correlations of variables:
corrplot.mixed(cor(sat.df[ , c(2, 4:9)]), upper = "ellipse") # select (2, 4:9) to exclude categorical 'weekend' and raw 'distance' variables
# NO extreme correlations found. x < 0.8~0.9

# Satisfaction drivers analysis: discover relationships between customer satisfaction with specific features of g/s + overall experience

# Ex. What extent is rides satisfaction related to overall experience? 
plot(overall ~ rides, data = sat.df, 
     xlab = "Satisfaction with Rides", ylab = "OVerall Satisfaction")

sat.df %>% ggplot(aes(rides, overall)) + geom_point()

# Present higher satisfaction with rides ~tendency to higher overall satisfaction!

# Linear models: 
# Overall satisfaction to satisfaction with rides:
lm(overall ~ rides, data = sat.df)
sat.df %>% do(tidy(lm(overall ~ rides, .)))   # only makes sense to do this if you are subsetting, create graphic, etc. before/after
# (Intercept)        rides  
#   -94.962          1.703  

m1 <- lm(overall ~ rides, data = sat.df)
str(m1)

plot(overall ~ rides, data = sat.df)
abline(m1, col = 'blue')

ggplot(aes(rides, overall), data = m1) + geom_point() + 
  geom_abline(aes(intercept = -95, slope = 1.7, color = I("blue")), size = 1)

summary(m1)
# Estimate: 1.7033, Error = 0.1055
# p-value: 2.2e^-16, significant.
# CI: 1.96 +- 1.7033 * 0.1055 = (1.495, 1.910)
confint(m1)
# 95% confident that coefficient estimate for 'ride' is between (1.495, 1.910)
# Residuals: vary largely from -33.597 to +34.699. Median: 0.425 (~0). Model is unbiased.
# Residual standard error: 12.88 on 498 df. 
# R-squared: 0.3434, 34% of variation in overall satisfaction is from ride satisfaction.
# F-statistic: 260.4 (df: 1,498), REJECT NULL. M1 > M0.

# Assumptions of GLM:
par(mfrow = c(2,2))
plot(m1)
# Residual vs. Fitted plot: no pattern between fitted (overall satisfaction) and residals (overall satisfaction), residuals are independent (from random error)
# Scale-Location plot: fitted vs. sqrt(residual), no clear pattern, IF pattern = nonlinear relationship? (heteroskedasticity). Outliers highlighted: rows 103, 149
# QQ plot: residuals follow normal distribution? Yes. 
# Residual vs. Leverage plot: Leverage = how much influence individual point has on model coefficient.
# HIGH residual + HIGH leverage = value has different pattern and undue influence on model coefficients. OUTLIER.

sat.df[c(57, 129, 295), ]
# Row: 57, 129, 295...  Row 129 = input error???

# OVERALL: model of overall satisfaction ~ rides is reasonable and statistcally sound.

# Fit Multiple Linear Model
m2 <- lm(overall ~ rides + games + wait + clean, data = sat.df)

m2 <- sat.df %>% do(tidy(lm(overall ~ rides + games + wait + clean, .)))
m2
summary(m2)

library(coefplot)
coefplot(m2, intercept = F, outerCI = 1.96, lwdOuter = 2.5, decreasing = F,  # does NOT in descending order of est. coefficient!!
         ylab = "Rating of Feature",
         xlab = "Association with OVerall Satisfaction")

ov <- names(sort(coef(m2), decreasing = T))
dwplot(m2, order_vars = ov) + theme_bw() + geom_vline(xintercept = 0, lty = 2)   # CORRECT ORDER

# Comparison M1 and M2:

summary(m1)$r.squared         # 0.3433799
summary(m2)$r.squared         # 0.558621            m2 explains more. ^# of predictors = ^ R^(2)
# Usage adjusted-r.squared:
summary(m1)$adj.r.squared     # 0.3420614
summary(m2)$adj.r.squared     # 0.5550543           m2 explains more of the variance in overall satisfaction

summary(m1)
summary(m2)
# Coefficient for 'rides' from 1.7 >>> 0.52 as independent effect of 'rides' taken away by correlated variables of 'games', 'clean', 'wait'


plot(sat.df$overall, fitted(m1), col = "red",
                xlim = c(0, 100), ylim = c(0, 100),
                xlab = "Actual overall satisfcaction", ylab = "Fitted Overal satisfaction")
points(sat.df$overall, fitted(m2), col = "blue")
legend("topleft", legend = c("model 1", "model 2"), 
       col = c("red", "blue"), pch = 1)
abline(0 , 1)


sat.df %>% ggplot(aes(x = overall)) +
                  geom_point(aes(y = fitted(m1), color = "m1")) + 
                  geom_point(aes(y = fitted(m2), color = "m2")) +
                  labs(x = "Actual Overall Satisfaction", y = "Fitted Overall Satisfaction") +
                  scale_y_continuous(limits = c(0, 100), breaks = pretty_breaks()) +
                  scale_x_continuous(limits = c(0, 100), breaks = pretty_breaks()) +
                  scale_color_manual("", values = c(m1 = "#008B00", m2 = "#CD4F39")) +
                  geom_abline(intercept = 0, slope = 1)
                  

m1 <- do(tidy(data.frame(m1)))

m1.df <- augment(m1)
m2.df <- augment(m2)

ggplot(sat.df, aes(x = overall)) +
  geom_point(aes(y = fitted(m1), color = I("red"))) + 
  geom_point(aes(y = fitted(m2), color = I("darkgreen"))) +
  scale_y_continuous(limits = c(0, 100), breaks = pretty_breaks()) +
  scale_x_continuous(limits = c(0, 100), breaks = pretty_breaks())



# ANOVA for testing nested models
anova(m1, m2)
# p < 2.2e^-16, REJECT NULL, m2 significantly explains more variation/improves fit than m1. 



# Predictions with models:
# Overall rating for customer rated aspectas as 100 pts/each:
coef(m2)%*%c(1, 100, 100, 100, 100)
# 90.58612

predict(m2, sat.df[1:10, ])    # predictions for first 10 customers (rows) in data set...
# OR
fitted(m2)[1:10]

# Standardizing predictors:
# Not all variables in data share same scale. ex. rides = 1-10 scale while cleanliness = 1-5 scale.
# Conversion to units of SD, subtract variable mean from each observation and divide by SD
(sat.df$rides - mean(sat.df$rides)) / sd(sat.df$rides)
scale(sat.df$rides)

# drop distance, only use logdist
sat.std <- sat.df[ , -3]
# standardize each column:
sat.std[ , 3:8] <- scale(sat.std[ , 3:8])
head(sat.std)
summary(sat.std)   # should have mean of 0...

# Using factors as predictors:
# Continue to improve upon m2:
# Satisfaction different for weekend, long-distance, more children customers.
m3 <- sat.std %>% do(fit = lm(.$overall ~ .$rides + .$games + .$wait + .$clean + .$weekend + .$logdist + .$num.child))
str(m3)

m3 %>% tidy(fit)     # logdist = 0.064, num.child = 0.2271, weekendYES = -0.04588
m3 %>% augment(fit)
m3 %>% glance(fit)   # r^squared: 0.6786, logdist, num.child = SIGNIFICANT (travel faurther, more children = higher overall satisfaction ratings)
# for weekendYES (1 = WEEKEND, 0 = NOWEEKEND), on avg. those on Weekend rate overall sat. -0.046 std. units lower than those NOT weekend.

m3 %>% augment(fit) %>% head()

summary(aov(overall ~ weekend, data = sat.std))
summary(lm(overall ~ weekend, data = sat.std))

# Convert nu.child to factor (NOT assume satisfaction UP/DOWN linearly as fct(# of children), same effect for each child...
sat.std$num.child.factor <- factor(sat.std$num.child)
sat.std <- sat.std %>% mutate(num.child.factor = factor(num.child))

m4 <- sat.std %>% do(fit = lm(overall ~ rides + games + wait + clean + weekend + logdist + num.child.factor, data = .))
# adjusted r^squared: 0.77

m4 %>% tidy(fit) # each num.child has own coefficient. comparison of coefficient with BASE LEVEL (parties with zero children.)
                 # parties with 1 child rate overall sat. on avg. 1.016 sd units higher than parties with ZERO chidlren.
                 # increase in overall sat. = SAME (+ ~1.00 sd units) regardless of # children in party. 
                 # Increase of overall sat. same for one child compared to BASE as for five children compared to BASE.
                 # NOT necessary to estimate a different increase for each incremental number of children.
                  
# Create new factor variable 'has.child' set TRUE vs. FALSE 
sat.std <- sat.std %>% mutate(has.child = factor(num.child > 0))

m5 <- sat.std %>% do(fit = lm(overall ~ rides + games + wait + clean + weekend + logdist + has.child, data = .))
m5 %>% tidy(fit)   # adjusted r^squared: 0.7713

# negligible change in model fit from simplifying num.child factor. 






