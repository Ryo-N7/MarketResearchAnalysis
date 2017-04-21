# Multiple (continuous) variable relationships: 
# Dataset for 1000 customers for online + physical retailer
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)
library(grid)

set.seed(21821)
ncust <- 1000
cust.df <- data.frame(cust.id = as.factor(c(1:ncust)))
# By declaring 'ncust' variable with # of customers, allows for ease of create new dataset 
# with different number of customers by changing only 1 line and re-run rest!

# Customer's age:
cust.df$age <- rnorm(n = ncust, mean =35, sd = 5)
hist(cust.df$age)
# Customer's credit score:
# Score related to customer age (older assumed higher credit scores, on avg.)
cust.df$credit.score <- rnorm(n = ncust, mean = 3*cust.df$age + 620, sd = 50)
hist(cust.df$credit.score)

# Customer's email on file? Y/N:
cust.df$email <- factor(sample(c("Yes", "No"), size = ncust, replace = TRUE, prob = c(0.8, 0.2)))

# Customer physical distance from store:
# Use exponential of normal distribution for positive values, many distances close, few 
# distances that are far. 
cust.df$distance.to.store <- exp(rnorm(n = ncust, mean = 2, sd = 1.2))
# OR use rlnorm() function for this lognormal distribution:
cust.df$distance.to.store <- rlnorm(n = ncust, meanlog = 2, sdlog = 1.2)

summary(cust.df)

# In-store and Online store sales data: 
# Visits, transactions, spending.
# Use negative binomial distribution, rnbinom(),
# For discrete variables, counts of events over time. Positive values, long right-tail, 
# Most customers = few visits, few customers = many visits
cust.df$online.visits <- rnbinom(ncust, size = 0.3, # size = degree of dispersion(variation)
                                 mu = 15 + ifelse(cust.df$email == "yes", 15, 0) -
                                   0.7 * (cust.df$age - median(cust.df$age)))
# avg. of 15 visits for customers with email in system (ifelse). 
# younger age = ^visits
cust.df %>% 
  filter(online.visits < 150) %>% 
  ggplot(aes(x = online.visits)) + geom_histogram(binwidth = 5) +
  scale_x_continuous(breaks = pretty_breaks(n = 20)) +
  scale_y_continuous(breaks = pretty_breaks(n = 15)) +
  theme_excel()

# Assumption: Each online visit = 30% chance of ordering, rbinom()
# Assumption: $ amount spent = lognormal distribution:
cust.df$online.trans <- rbinom(ncust, size = cust.df$online.visits, prob = 0.3)
cust.df$online.spend <- rlnorm(ncust, meanlog = 3.5, sdlog = 0.4) * cust.df$online.trans
# random value for amount/transaction multiplied by # online transactions for TOTAL SPEND.

cust.df$store.trans <- rnbinom(ncust, size = 5, mu = 3 / sqrt(cust.df$distance.to.store))
cust.df$store.spend <- rlnorm(ncust, meanlog = 3.5, sdlog = 0.4) * cust.df$store.trans

summary(cust.df)

# Customer satisfaction survey data: 
# Overall satisfaction = Service + Selection satisfaction criterion. 
# Assumption: customer responses based on unobserved "overall" satisfaction levels along
# with specific levels of service + selection. 
sat.overall <- rnorm(ncust, mean = 3.1, sd = 0.7)
summary(sat.overall)

?floor()
# Use floor() function to convert continuous random variables from rnorm() to discrete &
# ordinal scale used in survey responses ("Very Unsatisfied", "Satisfied", etc.).
sat.service <- floor(sat.overall + rnorm(ncust, mean = 0.5, sd = 0.4))
sat.selection <- floor(sat.overall + rnorm(ncust, mean = -0.2, sd = 0.6))
summary(cbind(sat.service, sat.selection))
# sat.service    sat.selection   
# Min.   :1.000   Min.   :-1.000  
# 1st Qu.:3.000   1st Qu.: 2.000  
# Median :3.000   Median : 2.000  
# Mean   :3.118   Mean   : 2.393  
# 3rd Qu.:4.000   3rd Qu.: 3.000  
# Max.   :6.000   Max.   : 6.000

# Satisfaction items typically on 5-point scale. Replace values > 5 with 5 and < 1 with 1.

sat.service[sat.service > 5] <- 5
sat.service[sat.service < 1] <- 1
sat.selection[sat.selection > 5] <- 5
sat.selection[sat.selection < 1] <- 1

summary(cbind(sat.service, sat.selection))
# sat.service    sat.selection  
# Min.   :1.000   Min.   :1.000  
# 1st Qu.:3.000   1st Qu.:2.000  
# Median :3.000   Median :2.000  
# Mean   :3.115   Mean   :2.422  
# 3rd Qu.:4.000   3rd Qu.:3.000  
# Max.   :5.000   Max.   :5.000

# Some customers NOT respond to surveys. Eliminate certain # of answers for subset of those
# customers who did not respond. Create 'no.response' variable with T/F values. 
# Assign those with TRUE in no.response with NA values in survey response 
no.response <- as.logical(rbinom(ncust, size = 1, prob = cust.df$age / 100))
sat.service[no.response] <- NA
sat.selection[no.response] <- NA

summary(cbind(sat.service, sat.selection))

cust.df$sat.service <- sat.service
cust.df$sat.selection <- sat.selection

rm(ncust, sat.overall, sat.service, sat.selection, no.response)


# Scatterplots
str(cust.df)

cust.df %>% 
  ggplot(aes(x = age, y = credit.score)) + 
  geom_point(color = I("blue"), shape = 1) + scale_shape(solid = FALSE) +
  geom_hline(yintercept = mean(cust.df$credit.score), color = "dark blue", linetype = 2, size = 0.75) +
  geom_vline(xintercept = mean(cust.df$age), color = "darkblue", linetype = 2, size = 0.75) +
  scale_y_continuous(limits = c(500, 900), breaks = pretty_breaks(n = 10)) + 
  scale_x_continuous(limits = c(15, 55), breaks = pretty_breaks(n = 10)) +
  xlab("Age") + ylab("Credit Score") +
  ggtitle("Active Customers for SuperMarketStore (2015)") +
  theme(plot.title = element_text(hjust = 0.5))

par(mfrow = c(1,2))

hist1 <- cust.df %>% 
  ggplot(aes(x = store.spend)) + geom_histogram()

hist2 <- cust.df %>% 
  ggplot(aes(x = online.spend)) + geom_histogram()
library(grid)
pushViewport(viewport(layout = grid.layout(1,2)))
print(hist1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(hist2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
graphics.off()

ggplot(cust.df, aes(store.spend, online.spend)) + geom_point()


# Log scales:
# Hard to distinguish due to skew of non-spenders (store and online)

my.col <- c(I("black"), I("green3"))
my.pch <- c(1, 19)
as.numeric(cust.df$email)
# my.col[as.numeric(cust.df$email)]
my.col[cust.df$email]

# Fix using log scale!
# use ~spend + 1 to avoid log(0) = NaN error. 
log.store.spend <- log(cust.df$store.spend + 1)
log.online.spend <- log(cust.df$online.spend + 1)

ggplot(cust.df, aes(x = log.store.spend, y = log.online.spend)) + 
  geom_point(aes(colour = email)) + scale_color_manual(values = c("black", "green3"))
 
?scale_fill_discrete

?geom_point()

plot(cust.df$store.spend + 1, cust.df$online.spend + 1, 
     cex = 0.7, log = "xy",
     col = my.col[cust.df$email], pch = my.pch[cust.df$email])


# Correlations:
# Covariance using cov():
cust.df %>% 
  summarize(cov(age, credit.score))

cov(cust.df$age, cust.df$credit.score)
# covariance interpretation dependent on scales of variables
cor.test(cust.df$age, cust.df$credit.score)

cust.df %>% 
  summarize(cor(age, credit.score)) # 0.25, ~medium size effect (Cohen's)
# Statistically significant??
cust.df %>% 
  summarize(cor.test(age, credit.score)$p.value) # 4.441e^-16 (SIGNIFICANT)


# Correlation matrix: pass multiple variables into the cor() function.
cor(cust.df[ , c(2, 3, 5:12)])
cor(cust.df[ , c(2, 3, 5:12)], use = "complete.obs") # instruct R use only cases without NA values...

# Use Correlation matrices charts:
install.packages("corrplot")
install.packages("gplots")
library(corrplot)
library(gplots)

corrplot.mixed(corr = cor(cust.df[ , c(2, 3, 5:12)], use = "complete.obs"), 
               upper = "ellipse", t1.pos = "n",
               col = colorpanel(50, "red", "grey60", "blue4"))

# Correlation 'r' coefficient measures LINEAR associations. Misleading interpretations for NON-linear .
# Many marketing data relationships = nonlinear, necessary to transform variables for proper correlation measurements.
# Ex. 
set.seed(49931)
x <- runif(1000, min = -10, max = 10)
x
plot(x, x^2)  # Show perfect nonlinear relationship.
cor(x, x^2)   # -0.00367~      ???? relationship not accurately shown by correlation measure...

# Correlation ~0 despite perfect nonlinear relationship!  

# Correlation: Distance to store  &  store spend??
cor(cust.df$distance.to.store, cust.df$store.spend)         # -0.2414949, modest negative correlation.

# IF transform using INVERSE (1/distance) or INVERSE SQRT (1/sqrt(distance)), LARGER association! 
cor(1/cust.df$distance.to.store, cust.df$store.spend)       # +0.4295245
cor(1/sqrt(cust.df$distance.to.store), cust.df$store.spend) # +0.4750991

# Interpretations:
# Inverse sqrt: customer live 1 mile away = spend MORE than customer live 5 miles away, however
# customer living 20 miles away will only buy slightly more than customer living 30 miles away...

par(mfrow = c(1,2))
plot(cust.df$distance.to.store, cust.df$store.spend)
plot(1/sqrt(cust.df$distance.to.store), cust.df$store.spend)
# Graphs show the association between variables to be clearer with the inverse transformed data.
graphics.off()

# Typical marketing data transformations:

# Unit sales, revenue, household income, price    ||||||   log(x)
# Distance                                        ||||||   1/x, 1/x^2, log(x)
# Market/preference share based on utility value  ||||||   e^x / (1 + e^x)
# Right-tail distributions                        ||||||   sqrt(x), log(x)     Warning: log(x =< 0)
# Left-tail distributions                         ||||||   x^2

# General purpose transofrmation function: 

#
#

# powerTransform() function:
library(car)
powerTransform(cust.df$distance.to.store)   
# lambda to make distance.to.store similar to Normal Distribution = -0.003696!

lambda <- coef(powerTransform(1/ cust.df$distance.to.store))
bcPower(cust.df$distance.to.store, lambda)

# Show difference in distributions between untransformed and untransformed:
par(mfrow = c(1,2))
hist(cust.df$distance.to.store, xlab = "Distance to Nearest Store", ylab = "Count of Customers",
     main = "Original Distribution")
hist(bcPower(cust.df$distance.to.store, lambda), xlab = "Box-Cox Transform of Distance",
     ylab = "Count of Customers", main = "Transformed Distribution")
graphics.off()

par(mfrow = c(1,2))

cust.df %>% 
  ggplot(aes(distance.to.store)) + geom_histogram(binwidth = 5) + 
  xlab("Distance to Nearest Store") + 
  ylab("Count of Customers")
#
cust.df %>% 
  ggplot(aes(bcPower(cust.df$distance.to.store, lambda))) + geom_histogram(binwidth = 0.1) + 
  xlab("Box-Cox Transform of Distance") + ylab("Count of Customers")

# IF attempt transform variable already NORMAL distribution, powerTransform() will report lambda = ~1.
# Ex. Variable - Age:
powerTransform(cust.df$age) # 1.036

# Compute correlations using transformed variable: 
l.dist  <- coef(powerTransform(cust.df$distance.to.store)) 
l.spend <- coef(powerTransform(cust.df$store.spend + 1))

cor(bcPower(cust.df$distance.to.store, l.dist),
    bcPower(cust.df$store.spend + 1, l.spend))
# Correlation: -0.4683126, strong negative correlation! 




# Exploring associations in survey responses 
plot(cust.df$sat.service, cust.df$sat.selection)

# NOT informative, same responses on top of eachother. Can NOT judge strength of association.
# Use jitter() to move points slightly away from eachother to reveal occurence of each combination of (x,y) values
plot(jitter(cust.df$sat.selection), jitter(cust.df$sat.service))
# (2,3) and (3,3) were most common responses. Present positive relationship between satisfaction variables! 


# Correlations for ordinal responses:
# Limited range of rating scales impact ability for computing accurate correlation metrics. 
# Use polychoric correlation coefficient! 
# Respondents have continuous values for ratings however limited to selection of discrete values, choice of 
# points on discrete scale that is closest to their (unobserved) latent continuous values.
# Polychoric attempts to recover correlations between the unobserved continuous values. 

resp <- !is.na(cust.df$sat.service)
cor(cust.df$sat.service[resp], cust.df$sat.selection[resp])
# Pearson coefficient: 0.6013005, strong positive correlation. 

library(psych)
polychoric(cbind(cust.df$sat.service[resp],
                 cust.df$sat.selection[resp]))
# Polychoric correlations 
# C1   C2  
# R1 1.00     
# R2 0.63 1.00
# with tau of 
#         1      2    3     4 
# [1,] -2.11  -0.71  0.45  1.6      sat.service
# [2,] -0.93   0.16  1.11  2.1      sat.selection

# Polychoric coefficient: 0.63, strong positive correlation!
# "with tau of" describes how estimated latent values are mapped to the discrete item values. 



