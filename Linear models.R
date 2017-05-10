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
install.packages("gpairs")
install.packages("corrplot")
library(corrplot)
library(gpairs)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(scales)


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
# Residual vs. Fitted plot: residuals are independent
# Scale-Location plot: sqrt(residual), no clear pattern, if pattern = nonlinear relationship? (heteroskedasticity)
# QQ plot: residuals follow normal distribution? Yes. 
# Residual vs. Leverage plot: Leverage = how much influence individual point has on model coefficient.


