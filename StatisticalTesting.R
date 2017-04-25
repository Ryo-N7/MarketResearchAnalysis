# Comparing groups using statistical tests.
seg.df <- read.csv("http://goo.gl/qw303p")


# Chi-square test: chisq.test()
# Test whether frequencies in cells are significantly different from expectations based on total counts.
# Ex. from seg.df, equal # of respondents in each segment given marginal count of N = 300 observations.

tmp.tab <- table(rep(c(1:4), times = c(25,25,25,20)))
chisq.test(tmp.tab)
as.data.frame(tmp.tab)









