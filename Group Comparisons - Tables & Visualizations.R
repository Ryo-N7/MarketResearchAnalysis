# Consumer segmentation:
# Simulating consumer segment data

# Subscription-based service, n = 300
# Variables: age, gender, income, # children, OWN home vs. RENT home, subscribed vs. NOT subscribed
# Consumer segments: suburban mix, urban hip, travelers, moving up. 

# Data:
# 1. Define structure: demographic variables, segment names, segment sizes
# 2. Parameters: distributions of demographic variables (mean, variance, SD, etc.)
# 3. Code = reiterate over segments + variables to draw random values according to definitions + parameters
rm(list = ls())

segVars <-c("age", "gender", "income", "kids", "ownHome", "subscribe")
segVarType <- c("norm", "binom", "norm", "pois", "binom", "binom")   # normal (continuous), binomial (Y/N), poisson (counts)
segNames <- c("Suburb mix", "Urban hip", "Travelers", "Moving up")
segSize <- c(100, 50, 80, 70)

# Define values of data:
# Specify distributional parameters: Ex. mean values of each variable within each segment
segMeans <- matrix(c(
  40, 0.5, 55000, 2, 0.5, 0.1,
  24, 0.7, 21000, 1, 0.2, 0.2,
  58, 0.3, 52000, 2, 0.3, 0.2,
  36, 0.3, 52000, 2, 0.3, 0.2), ncol = length(segVars), byrow = TRUE)
# for Normal variable types, necessity to specify variance of distribution as well:
segSDs <- matrix(c(
  5, NA, 12000, NA, NA, NA, 
  2, NA, 5000, NA, NA, NA, 
  8, NA, 21000, NA, NA, NA, 
  3, NA, 10000, NA, NA, NA), ncol = length(segVars), byrow = TRUE)
# Ex. Travelers segment: mean = 58, SD = 8, ~50% = male, avg. income = 64000, SD income = 21000. 
# Separate data definition and procedural code = ^ease change definitions. 

# Loops: 
# Usage for() loops to iterate over variables and segments as structure of random draws = similar
# Ex. 
for(i in 1:10) {print(i)}

i.seq <- rep(sqrt(seq(from = 2.1, to = 6.2, by = 1.7)), 3)
i.seq
for (i in i.seq) {print(i)}

for (i in c("Hello ", "world, ", "welcome to R!")) {cat(i)}

# Most common index variable: "i" (inner loops = "j")
# AVOID for() loops in R for indexing on 1:length(someVariable)
# Usage seq_along(someVariable) instead!
for (i in 1:length(i.seq)) {cat("Entry", i , "=", i.seq[i], "\n")}   
# >>>>> instead of
for (i in 1:length(i.seq)) {cat("Entry", i, "-", i.seq[i], "\n")}
# as, IF i.seq = NULL, then ERROR! 




