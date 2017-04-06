# Multiple (continuous) variable relationships: 
# Dataset for 1000 customers for online + physical retailer
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(scales)

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
# Hard to distinguish due to skew of non-spenders (store and online)

my.col <- c(I("black"), I("green3"))
my.pch <- c(1, 19)
as.numeric(cust.df$email)
# my.col[as.numeric(cust.df$email)]
my.col[cust.df$email]

log.store.spend <- log(cust.df$store.spend + 1)
log.online.spend <- log(cust.df$online.spend + 1)

ggplot(cust.df, aes(x = log.store.spend, y = log.online.spend)) + 
  geom_point(aes(colour = email)) + scale_color_manual(values = c("black", "green3"))
# Fix using log scale! 
?scale_fill_discrete

?geom_point()

plot(cust.df$store.spend + 1, cust.df$online.spend + 1, 
     cex = 0.7, log = "xy",
     col = my.col[cust.df$email], pch = my.pch[cust.df$email])


# Correlations:








