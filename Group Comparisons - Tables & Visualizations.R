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
  4, NA, 10000, NA, NA, NA), ncol = length(segVars), byrow = TRUE)
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




# if() statements:
# Ex. 
if(condition1) {
  statements
} else if (condition2) {
  statements
} else {
  statements
}

# Condition: evaluates to TRUE or FALSE
# Conditional evaluation on EVERY element of vector, use ifelse()
# Ex. 
x <- 1:5
ifelse(x > 1, "hi", "bye")



# Segment data generation: 

# Set up data frame "seg.df" and pseudorandom number sequence
# For each SEGMENT i in "segNames" {
#   Set up a temporary data frame "this.seg" for this SEGMENT's data
#   For each VARIABLE j in "segVars" {
#     Use nested if() on "segVarType[j]" to determine data type for VARIABLE
#     Use segMeans[i, j] and sedSDs[i, j] to
#     ... Draw random data for VARIABLE (within SEGMENT) with
#     ... "segSize[i]" observations
#   }
#   Add this SEGMENT's data ("this.seg") to the overall data ("seg.df")
# }

seg.df <- NULL

set.seed(02554)

# iterate over segments and create data for each 
for (i in seq_along(segNames)) {
  cat(i, segNames[i], "\n")
  
  # Create empty matrix to hold particular segment's data
  this.seg <- data.frame(matrix(NA, nrow = segSize[i], ncol = length(segVars)))
  
  # Within segment, iterate over variables and draw appropriate random data
  for (j in seq_along(segVars)) { # and iterate over each variable
    if (segVarType[j] == "norm") {# draw random normals
      this.seg[, j] <- rnorm(segSize[i], mean = segMeans[i, j], sd = segSDs[i, j])
    } else if (segVarType[j] == "pois") { # draw counts
      this.seg[, j] <- rpois(segSize[i], lambda = segMeans[i, j])
    } else if (segVarType[j] == "binom") {# draw binomials
      this.seg[, j] <- rbinom(segSize[i], size = 1, prob = segMeans[i, j])
    } else {
      stop("Bad segment data type: ", segVarType[j])
    }}
seg.df <- rbind(seg.df, this.seg)          # add segment to total dataset
  }


# Explanation: 
# according to data type (norm, pois, binom), use appropriate RNG function (rnorm, rpois, rbinom).
# all values for variable in segment drawn from single command, length specified by segSize[i]
# Predefine data frame "this.seg", preallocating allows for smoother memory processing + RNG draws into segment at once, error checking
# Filling objects with NA = error check for missing values when we expect data filled in
# Stop() command executes in case data type NOT match expected. Quick error string shown. 

names(seg.df) <- segVars
seg.df$Segment <- factor(rep(segNames, times = segSize))
seg.df$ownHome <- factor(seg.df$ownHome, labels = c("ownNo", "ownYes"))
seg.df$gender <- factor(seg.df$gender, labels = c("Female", "Male"))
seg.df$subscribe <- factor(seg.df$subscribe, labels = c("subNo", "subYes"))

summary(seg.df)



# Data variance by different segments of consumers. 
# Tailor offerings for certain segments, engage in different ways to reach targetted segment audience. 
library(tidyverse)

# Data frame indexing:
# Find rows that match criterion(s), then take summary statistic for matching observations.
mean(seg.df$income[seg.df$Segment == "Moving up"])   # mean income for "Moving up" segment: 50249.33

seg.df %>% filter(Segment == "Moving up") %>% 
  summarize(mean_income = mean(income, na.rm = TRUE))   # 50249.33


mean(seg.df$income[seg.df$Segment == "Moving up" & 
                   seg.df$subscribe == "subNo"])        
# mean income for "Moving up" segment AND subscribe = NO: 50902.45

seg.df %>% filter(Segment == "Moving up", subscribe == "subNo") %>% 
  summarize(mean_income = mean(income, na.rm = TRUE))   # 50902.45

# Use by(data, indices, function) function:
by(seg.df$income, seg.df$Segment, mean)
# seg.df$Segment: Moving up
# [1] 50249.33
# --------------------------------------------------------------------------------- 
# seg.df$Segment: Suburb mix
# [1] 55033.82
# --------------------------------------------------------------------------------- 
# seg.df$Segment: Travelers
# [1] 50213.94
# --------------------------------------------------------------------------------- 
# seg.df$Segment: Urban hip
# [1] 21681.93

# for multiple factors, supply them in list(): 
by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)
# : Moving up
# : subNo
# [1] 50902.45
# --------------------------------------------------------------------------------- 
# : Suburb mix
# : subNo
# [1] 54942.69
# etc. etc....................


aggregate(seg.df$income, list(seg.df$Segment), mean)   # same as above, tidier. result is data frame!
# Group.1        x
# 1  Moving up 50249.33
# 2 Suburb mix 55033.82
# 3  Travelers 50213.94
# 4  Urban hip 21681.93


seg.df %>% group_by(Segment) %>% 
  summarize(mean_income = mean(income, na.rm = TRUE)) 


