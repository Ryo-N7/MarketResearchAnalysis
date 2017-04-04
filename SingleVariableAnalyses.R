library(tidyverse)

# Simulating data for simpel data analyses
# Dataset: total sales by week for 2 products at chain stores (T = 2 years)
k.stores <- 20
k.weeks <- 104 # 104 weeks = 2 years

# Create data frame of (initially) missing values to hold all data
store.df <- data.frame(matrix(NA, ncol = 10, nrow = k.stores * k.weeks))
names(store.df) <- c("storeNum", "Year", "Week", "p1sales", "p2sales", "p1price", "p2price", 
                     "p1prom", "p2prom", "country")
# Check dimensions:
dim(store.df) # 2080, 10  >>> 20*104

# Vectors for store number and country for each observation: 
store.num <- 101:(100 + k.stores)
(store.cty <- c(rep("US", 3), rep("DE", 5), rep("GB", 3), rep("BR", 2), rep("JP", 4), 
                rep("AU", 1), rep("CN", 2)))

length(store.cty)  # Check length: 20 for 20 stores

?rep()
# Replace currently NA values in data frame with created store.num and store.cty vectors:
store.df$storeNum <- rep(store.num, each = k.weeks)
store.df$country <- rep(store.cty, each = k.weeks)

# Cleanup stor.num and store.cty vectors, not needed anymore:
rm(store.cty)
rm(store.num)

# Create vectors for week and year columns:
(store.df$Week <- rep(1:52, times = k.stores))
(store.df$Year <- rep(rep(1:2, each = k.weeks/2), times = k.stores))
str(store.df)

# Change variable type for $country and $storeNum to factor: 
# (store.Num is a lable, NOT integer/number)
store.df$storeNum <- store.df$storeNum %>% factor()
store.df$country <- store.df$country %>% factor()
# Check changes
store.df %>% str()
store.df %>% head()
store.df %>% head(120)
store.df %>% tail()
store.df %>% tail(120)

# Input sales, price, prom data with PRNG, set.seed(): 
set.seed(98250)
# Each row: one week of one year for one store
?rbinom
store.df$p1prom <- rbinom(n = nrow(store.df), size = 1, p = 0.1)  # 10$ chance of promotion
store.df$p2prom <- rbinom(n = nrow(store.df), size = 1, p = 0.15) # 15% chance of promotion
head(store.df)

# Each product 5 distinct prices, price range from $2.19 ~ $3.19, use sample():
store.df$p1price <- sample(x = c(2.19, 2.29, 2.49, 2.79, 2.99), size = nrow(store.df), replace = TRUE)
store.df$p2price <- sample(x = c(2.29, 2.49, 2.59, 2.99, 3.19), size = nrow(store.df), replace = TRUE)

# Simulate sales figures for each week, use poisson distribution.
# Sales as function of relative prices of P1 and P2 with their promotional status
# Default sales in absence of promotion:
tmp.sales1 <- rpois(nrow(store.df), lambda = 120)
tmp.sales2 <- rpois(nrow(store.df), lambda = 100)

# Scale unit count of item sales depending on relative prices, log function:
tmp.sales1 <- tmp.sales1 * log(store.df$p2price) / log(store.df$p1price)
tmp.sales2 <- tmp.sales2 * log(store.df$p1price) / log(store.df$p2price)
# ASSUMPTION: sales variance due to inverse ratio of prices
# i.e. sales of P1 go up to degree log(price) of P1 is LOWER than log(price) of P2.

# ASSUMPTION: sales = +30% (P1) and +40% (P2) when each product is promoted. 
# Promotional status vector (0 or 1) * 0.3 or 0.4
?floor()
store.df$p1sales <- floor(tmp.sales1 * (1 + store.df$p1prom * 0.3))
store.df$p2sales <- floor(tmp.sales2 * (1 + store.df$p2prom * 0.4))

head(store.df)
library(car)
some(store.df)


# Discrete variables:
# Use table() to summarize certain set of data:
table(store.df$p1price)
p1.table <- table(store.df$p1price)
str(p1.table)
plot(p1.table)

# Cross-tabs when include 2nd variable in table():
table(store.df$p1price, store.df$p1prom)
# At each price level, P1 seems to have been discounted ~10% of the time. (as specified when 
# creating our data set).
# Calculate fraction of promotions at each price level: 
p1.table2 <- table(store.df$p1price, store.df$p1prom)
p1.table2[ , 2] / (p1.table2[ , 1] + p1.table2[ , 2])
# all fractions ~10%

# Continuous variables, use distribution functions: 
min(store.df$p1sales)
max(store.df$p1sales)
mean(store.df$p1prom)
median(store.df$p2sales)
var(store.df$p1sales)
sd(store.df$p1sales)
IQR(store.df$p1sales)
store.df$p1sales %>% mad()
store.df$p1sales %>% quantile(probs = c(0.25, 0.5, 0.75))
# Customize quantiles by 0:10 / 10 or seq(from = 0, to = 1, by = 0.1)

# For skewed and assymetric distributions (unit sales, household income, etc.) usage of median 
# and interquartile range are more useful for summarizing data. 
# Create neat summarizations using a data frame: 
mysummary.df <- data.frame(matrix(NA, nrow = 2, ncol = 2))
names(mysummary.df) <- c("Median Sales", "IQR")
rownames(mysummary.df) <- c("Product 1", "Product 2")
mysummary.df["Product 1", "Median Sales"] <- median(store.df$p1sales)
mysummary.df["Product 2", "Median Sales"] <- median(store.df$p2sales)
mysummary.df["Product 1", "IQR"] <- IQR(store.df$p1sales)
mysummary.df["Product 2", "IQR"] <- IQR(store.df$p2sales)
mysummary.df

summary(store.df)
summary(store.df$Year)
summary(store.df, digits = 2)

install.packages("psych")
library(psych)
# Use describe():
# useful for viewing skew, kurtosis, trimmed mean. 
# Survey data with discrete values (1-7 Likert scales...)
# * denotes that summary statistics not valid for these variables. 
describe(store.df)
describe(store.df[ , c(4:9)])   # Only show relevant columns! 

# Using apply():
apply(store.df[ , 4:9], MARGIN = 2, FUN = mean)
apply(store.df[ , 4:9], MARGIN = 2, FUN = sd)
apply(store.df[ , 4:9], MARGIN = 2, FUN = sum)

mysummary2.df <- data.frame(matrix(NA, nrow = 2, ncol = 2))
names(mysummary2.df) <- c("Median Sales", "IQR")
rownames(mysummary2.df) <- names(store.df)[4:5]
mysummary2.df[ , "Median Sales"] <- apply(store.df[ , 4:5], 2, median)
mysummary2.df[ , "IQR"] <- apply(store.df[ , 4:5], 2, IQR)
mysummary2.df
# With apply(), takes 5 lines compared to 7 lines of code to create custom summary data frame!

# Visualization:
library(ggplot2)
library(ggthemes)
library(tidyverse)

ggplot(store.df, aes(x = p1sales)) + geom_histogram(binwidth = 10, fill = "red") + 
  ggtitle("Weekly Sales Frequencies", subtitle = "Product 1, All Stores") +
  theme_economist_white() +
  xlab("Product 1 Sales (Units)") +
  ylab("Frequency") 

ggplot(store.df, aes(x = p2sales, y = p2sales)) + geom_boxplot() +
  ggtitle("Weekly Sales", subtitle = "Product 2, All Stores") +
  xlab("Product 2") + ylab("Weekly Sales") +
  coord_flip() +
  theme(axis.text.y = element_blank())

# Weekly Sales of Product 2 by Store

store.df %>% 
  as.numeric(unlist(store.num)) %>% 
  ggplot(aes(x = p2sales, y = storeNum)) + geom_boxplot() +
  ggtitle("Weekly Sales of Product 2 by Store") +
  ylab("Store") + xlab("Weekly unit sales")

str(store.df)
ls()
exists(store.num)

as.numeric(store.df$storeNum)
ggplot(store.df, aes(y = p2sales, x = storeNum)) + geom_boxplot() +
  ggtitle("Weekly Sales of Product 2 by Store") +
  xlab("Store") + ylab("Weekly unit sales") +
  coord_flip()

store.df %>% 
  ggplot(aes(x = storeNum, y = p2sales)) + geom_boxplot() +
  ggtitle("Weekly Sales of Product 2 by Store") +
  xlab("Store") + ylab("Weekly unit sales") +
  coord_flip()

boxplot(p2sales ~ p2prom, data = store.df, horizontal = TRUE, yaxt = "n", 
        ylab = "P2 promoted in store?", xlab = "Weekly sales",
        main = "Weekly sales of P2 with and without promotion")
axis(side = 2, at = c(1,2), labels = c("No", "YES"))
# MORE sales when P2 promotion discount! 

store.df %>% 
  ggplot(aes(x = p2prom, y = p2sales, group = p2prom)) + geom_boxplot() +
  ggtitle("Weekly sales of P2 with and without promotion") +
  xlab("P2 promoted in store?") + ylab("Weekly sales") +
  coord_flip() +
  scale_x_continuous(breaks = 0:1, labels = c("NO","YES"))

ggplot(store.df, aes(x = p2prom, y = p2sales, group = p2prom)) + geom_boxplot() +
  ggtitle("Weekly sales of P2 with and without promotion") +
  xlab("P2 promoted in store?") + ylab("Weekly sales") +
  coord_flip() +
  scale_x_continuous(breaks = 0:1, labels = c("0.0" = "NO","1.0" = "YES"))

# QQ plot for normal distributions: 

qqnorm(store.df$p1sales)
qqline(store.df$p1sales)
# Upward curve from normality line at the ends, high positive skew 

# log transform data:
qqnorm(log(store.df$p1sales))
qqline(log(store.df$p1sales))
# Better! Closer to normal distribution

# Empirical Cumulative Distribution Function (ECDF) graphs: 
ggplot(store.df, aes( x = p2sales)) + stat_ecdf()

# Investigate 90 Percentile:
quantile(store.df$p1sales, probs = c(0.9))    # 170 units 

library(scales)

ggplot(store.df, aes(x = p1sales)) + stat_ecdf(geom = "step") + 
  scale_y_continuous(breaks = pretty_breaks(n = 10), labels = percent) +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  theme_economist() +
  ylab("Cumulative Proportion") + xlab("P2 Weekly Sales \n (Units)") +
  geom_hline(yintercept = 1.0, size = 1) +
  geom_hline(yintercept = 0.9, color = "grey", size = 1.5) +
  geom_vline(xintercept = 170, color = "grey", size = 1.5) +
  annotate("text", x = 230, y = 0.6, label = "90% of weeks sold =< 170 units")

# Aggregate by country:
library(tidyverse)

store.df %>% 
  select(p1sales, storeNum) %>% 
  filter(country)

p1_mean_sales <- (store.df %>% 
  group_by(country) %>% 
  summarize(mean_sales = mean(p1sales)) %>% 
  arrange(desc(mean_sales)))

p1_total_sales <- (store.df %>% 
                    group_by(country) %>% 
                    summarize(total_sales = sum(p1sales)) %>% 
                    arrange(desc(total_sales)))
p1_total_sales

p1sales.sum <- aggregate(store.df$p1sales,
                         by = list(country = store.df$country), sum)

# World Map visualization:
install.packages(c("rworldmap", "RColorBrewer"))
library(rworldmap)
library(RColorBrewer)

p1_sales_map <- joinCountryData2Map(p1sales.sum, joinCode = "ISO2", 
                                    nameJoinColumn = "country")

mapCountryData(p1_sales_map, nameColumnToPlot = "x", 
               mapTitle = "Total P1 Sales by Country", 
               colourPalette = brewer.pal(7, "Greens"),
               catMethod = "fixedWidth", addLegend = FALSE)

