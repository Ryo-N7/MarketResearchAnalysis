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
}

# add segment to total dataset
seg.df <- rbind(seg.df, this.seg)

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











