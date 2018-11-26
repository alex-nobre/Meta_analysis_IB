# Combine multiple outcomes from single study

# For now, only two outcomes (until we know how to code the formula with more than two)

# Number of outcomnes
# n <- 2

# effect sizes for each outcome
es1 <- 1
es2 <- 1

# variance for each outcome
var1 <- 0.5
var2 <- 0.5

# correlation between outcomes
r <- 0.5

# Combined effect size
mean_es <- (1/2) * (es1 + es2)

# Combined variance
mean_var <- (1/2)**2 * (var1 + var2 + 2*r*sqrt(var1)*sqrt(var2))




