
#==============================================================================#
# Combines multiple measurements in a single study in one measure
# Mean
# Standard deviation
#==============================================================================#

# Beanland and Pammer (2010) - eyes-fixating

# Experiment 1A
# Combine means:

m1_1 <- 4.5
m1_2 <- 5.4

# Combine sds
sd1_1 <- 3.6
sd1_2 <- 2.5

combined_mean_1 <- mean(c(m1_1, m1_2))

combined_sd_1 <- sqrt(sd1_1^2 + sd1_2^2)

# Beanland and Pammer (2010) - eye-moving

# Experiment 1A
# Combine means:

m1_1 <- 2.2
m1_2 <- 3.3

# Combine sds
sd1_1 <- 2.9
sd1_2 <- 2.6

combined_mean_1 <- mean(c(m1_1, m1_2))

combined_sd_1 <- sqrt(sd1_1^2 + sd1_2^2)

#==============================================================================================#
# Experiment 2 - slow
# Combine means:

m1_1 <- 0.1
m1_2 <- -0.2

# Combine sds
sd1_1 <- 0.7
sd1_2 <- 0.6

combined_mean_1 <- mean(c(m1_1, m1_2))

combined_sd_1 <- sqrt(sd1_1^2 + sd1_2^2)



# Experiment 2 - fast

# Combine means:

m1_1 <- 0.1
m1_2 <- -0.3

# Combine sds
sd1_1 <- 0.6
sd1_2 <- 0.7

combined_mean_1 <- mean(c(m1_1, m1_2))

combined_sd_1 <- sqrt(sd1_1^2 + sd1_2^2)

#n1 <- round(56 * 25/100)
n1 <- 18
n2 <- round(52 * 25/100)

# gaze rate

# Experiment 2 - slow
# Combine means:

m1_1 <- 0.1
m1_2 <- -0.2

# Combine sds
sd1_1 <- 0.7
sd1_2 <- 0.6

combined_mean_1 <- mean(c(m1_1, m1_2))

combined_sd_1 <- sqrt(sd1_1^2 + sd1_2^2)



# Experiment 2 - fast

# Combine means:

m1_1 <- 0.1
m1_2 <- -0.3

# Combine sds
sd1_1 <- 0.6
sd1_2 <- 0.7

combined_mean_1 <- mean(c(m1_1, m1_2))

combined_sd_1 <- sqrt(sd1_1^2 + sd1_2^2)

n1 <- round(56 * 72/100)
n2 <- round(52 * 72/100)