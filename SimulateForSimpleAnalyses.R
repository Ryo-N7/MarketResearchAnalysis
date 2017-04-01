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

