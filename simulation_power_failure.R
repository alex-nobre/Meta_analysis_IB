

library(effsize)


#=========== Pseudocode ===========
# for sample size
# generate n pairs implicit-effect/awareness effect
# effect sizes are sampled from normal distribution for simplicity; this is the assumption for tests in most papers
# cohen's d is used assuming equal variances for both groups; this might not be reasonable - aware subs may have larger variance

# implicit effects are small when awareness is false; large when awareness is true
# how small and how large: use literature as guides


# awareness effect is 0 (unaware) or 1 (aware) plus some random error

# compute p-values
# estimate power for both
#=======================================================================================#


#===============================#
# Parameters to vary:
# Sample size
# Effect size
# proportion of aware x unaware
#===============================#
n_iterations_per_sample <- 10000

# Fixed sample size for now; 
# later, run simulation for sample sizes ranging from 6 (razpurker-apfeld, -1 so that then is even)
# to 186 (most et al., 2005)
sample_size <- 100
effect_size_implicit <- 1
effect_size_awareness <- 0.01
prop_aware_parts <- sample_size/2
prop_unaware_parts <- sample_size - prop_aware_parts


#====================================================================================#
#=================================== Functions ======================================#
#====================================================================================#

# Function to generate p-values
generate_p_values <- function(n_iterations = n_iterations_per_sample, 
                              effect_implicit = effect_size_implicit, 
                              effect_awareness = effect_size_awareness, 
                              n_sample = sample_size, 
                              prop_aware = prop_aware_parts) {
  # Create vectors to store t-values for implicit and awareness tests
  implicit_p_values <- vector(mode = "numeric", 
                              length=n_iterations)
  
  awareness_p_values <- vector(mode = "numeric", 
                               length=n_iterations)
  
  
  # Generate implicit and awareness t-values for each population
  # For now, variances are the same in both groups for each test
  for(iteration in 1:n_iterations){
    # Create a list of awareness-implicit score vectors for each population
    
    prop_unaware <- n_sample - prop_aware
    
    ## Unaware group
    unaware_pop <- vector(mode = "list", length=prop_unaware)
    
    for(part in 1:length(unaware_pop)) {
      unaware_pop[[part]] <- c(rnorm(1, mean = 0, sd = 1), # awareness effect = 0
                               # rnorm(1, mean = 1, sd = 1)) # implicit effect (small)
                               rnorm(1, mean = 0, sd = 1)) # implicit effect (null)
    }
    
    ## Aware group
    aware_pop <- vector(mode = "list", length=prop_aware)
    
    for(part in 1:length(aware_pop)) {
      aware_pop[[part]] <- c(rnorm(1, mean = effect_awareness, sd = 1), # awareness effect > 0 - subs are conscious
                             rnorm(1, mean = effect_implicit, sd = 1)) # implicit effect - effects are large
    }
    
    # Join values from both groups to run t-tests
    aware_scores <- c(sapply(unaware_pop, function(x) x[1]),
                      sapply(aware_pop, function(x) x[1]))
    
    implicit_scores <- c(sapply(unaware_pop, function(x) x[2]), # implicit effects from unaware subs
                         sapply(aware_pop, function(x) x[2])) # implicit effects from aware subs
    
    # Compute and store t-values
    implicit_ttest <- t.test(implicit_scores)
    awareness_ttest <- t.test(aware_scores)
    
    implicit_p_values[iteration] <- implicit_ttest$p.value
    awareness_p_values[iteration] <- awareness_ttest$p.value
  }
  return(list(implicit_p_values = implicit_p_values,
              awareness_p_values = awareness_p_values))
}

# plot in a function
plot_p_values <- function(implicit_ps, awareness_ps,
                          sample_size = "100",
                          proportion_aware = "50/50",
                          effect_size = "1") {
  # combine p-values into pairs to generate vector of color names
  pvalue_pairs <- mapply(c, 
                         implicit_ps, awareness_ps, 
                         SIMPLIFY=FALSE)
  
  # Vector of color names: "red" for false implicit effects, blue for all the rest
  color_vector <- sapply(pvalue_pairs, function(x) ifelse(x[1] < 0.05 & # implicit p-value
                                                            x[2] > 0.05, # awareness p-value
                                                          "red",
                                                          "blue"))
  # Plot
  plot(implicit_ps, 
       awareness_ps,
       col=color_vector,
       pch=16,
       xlim = c(0, 0.5),
       ylim = c(0, 1.0),
       main = paste(sample_size, proportion_aware, effect_size, sep = "_"),
       xlab = "p-values for implicit effect",
       ylab = "p-values for awareness effect")
  
  # Vertical and horizontal lines to delimit region of false implicits
  abline(h = 0.05,
         col = "red",
         lty = 2)
  abline(v = 0.05,
         col = "red",
         lty = 2)
}

# Create ps and plots
sample_sizes <- c(6, 20, 26, 32, 42, 60, 80, 120, 200, 260)
props_aware <- c(sample_size/2, sample_size/3, sample_size/4, sample_size/5)
proportion_labels <- c("50/50", "1/2", "25/75", "20/80")


par(mfrow=c(2,2))
par(mar=c(4.1,4.1,3.1,2.1))
for(proportion in 1:length(props_aware)) {
  values <- generate_p_values(prop_aware = props_aware[proportion])
  plot_p_values(values[[1]], values[[2]],
                proportion_aware = proportion_labels[proportion])
}
par(graphical_defaults)

#===== Compute and plot n of false implicits by sample size =====
false_implicits_by_n <- vector(mode = "numeric",
                             length = length(sample_sizes))
for(n in 1:length(sample_sizes)) {
  values <- generate_p_values(n_sample = sample_sizes[n],
                              prop_aware = sample_sizes[n]/2)
  
  # combine p-values into pairs to generate vector of color names
  value_pairs <- mapply(c, 
                         values[[1]], values[[2]], 
                         SIMPLIFY=FALSE)
  
  # Vector of significance values: 1 for false implicit, 0 otherwise
  significance_values <- sapply(value_pairs, function(x) ifelse(x[1] < 0.05 & # implicit p-value
                                                          x[2] > 0.05, # awareness p-value
                                                          1,
                                                          0))
  
  # Add n of false significants to vector
  false_implicits_by_n[n] <- sum(significance_values)/length(significance_values)
}

# Plot with line for proportion of false implicits by n: use continuous sequence of n of trials
plot(sample_sizes,
     false_implicits_by_n)


#===== Compute and plot n of false implicits by implicit effect size =====

implicit_effect_sizes <- seq(0.1, 1.0, by = 0.1)

false_implicits_by_effect <- vector(mode = "numeric",
                                    length = length(implicit_effect_sizes))


for(n in 1:length(implicit_effect_sizes)) {
  values <- generate_p_values(effect_implicit = implicit_effect_sizes[n])
  
  # combine p-values into pairs to generate vector of color names
  value_pairs <- mapply(c, 
                        values[[1]], values[[2]], 
                        SIMPLIFY=FALSE)
  
  # Vector of significance values: 1 for false implicit, 0 otherwise
  significance_values <- sapply(value_pairs, function(x) ifelse(x[1] < 0.05 & # implicit p-value
                                                                  x[2] > 0.05, # awareness p-value
                                                                1,
                                                                0))
  
  # Add n of false significants to vector
  false_implicits_by_effect[n] <- sum(significance_values)/length(significance_values)
}

# Plot with line for proportion of false implicits by n: use continuous sequence of n of trials
plot(implicit_effect_sizes,
     false_implicits_by_effect)


#==========================================================================================#
# Generate values outside function
# # Create vectors to store t-values for implicit and awareness tests
# implicit_p_values <- vector(mode = "numeric", 
#                             length=n_iterations_per_sample)
# 
# awareness_p_values <- vector(mode = "numeric", 
#                              length=n_iterations_per_sample)
# 
# 
# # Generate implicit and awareness t-values for each population
# # For now, variances are the same in both groups for each test
# for(iteration in 1:n_iterations_per_sample){
#   # Create a list of awareness-implicit score vectors for each population
#   
#   ## Unaware group
#   unaware_pop <- vector(mode = "list", length=sample_size/2)
#   
#   for(part in 1:length(unaware_pop)) {
#     unaware_pop[[part]] <- c(rnorm(1, mean = 0, sd = 0.05), # awareness effect = 0
#                              # rnorm(1, mean = 1, sd = 1)) # implicit effect (small)
#                              rnorm(1, mean = 0, sd = 1)) # implicit effect (null)
#   }
#   
#   ## Aware group
#   aware_pop <- vector(mode = "list", length=sample_size/2)
#   
#   for(part in 1:length(aware_pop)) {
#     aware_pop[[part]] <- c(rnorm(1, mean = effect_size_awareness, sd = 0.05), # awareness effect > 0 - subs are conscious
#                            rnorm(1, mean = effect_size_implicit, sd = 1)) # implicit effect - effects are large
#   }
#   
#   # Join values from both groups to run t-tests
#   aware_scores <- c(sapply(unaware_pop, function(x) x[1]),
#                     sapply(aware_pop, function(x) x[1]))
#   
#   implicit_scores <- c(sapply(unaware_pop, function(x) x[2]), # implicit effects from unaware subs
#                        sapply(aware_pop, function(x) x[2])) # implicit effects from aware subs
#   
#   # Compute and store p-values
#   implicit_ttest <- t.test(implicit_scores)
#   awareness_ttest <- t.test(aware_scores)
#   
#   implicit_p_values[iteration] <- implicit_ttest$p.value
#   awareness_p_values[iteration] <- awareness_ttest$p.value
# }




# Plot outside function
# # plots
# 
# # combine p-values into pairs to generate vector of color names
# pvalue_pairs <- mapply(c, 
#                        implicit_p_values, awareness_p_values, 
#                        SIMPLIFY=FALSE)
# 
# # Vector of color names: "red" for false implicit effects, blue for all the rest
# color_vector <- sapply(pvalue_pairs, function(x) ifelse(x[1] < 0.05 & # implicit p-value
#                                                           x[2] > 0.05, # awareness p-value
#                                                         "red",
#                                                         "blue"))
# # Plot
# plot(implicit_p_values, 
#      awareness_p_values,
#      col=color_vector,
#      pch=16)
# 
# # Vertical and horizontal lines to delimit region of false implicits
# abline(h = 0.05,
#        col = "red",
#        lty = 2)
# abline(v = 0.05,
#        col = "red",
#        lty = 2)





# x_axis_values <- seq(0, 1.0, length=n_iterations_per_sample)
# plot(awareness_p_values,
#      implicit_p_values,
#      col=color_vector,
#      pch=16)

# hist(implicit_p_values,
#      breaks=seq(0,1,by=0.05))
# hist(awareness_p_values,
#      breaks=seq(0,1,by=0.05))




# Compute power
# t.power <-  function(nsamp=c(sample_size/2,sample_size/2),nsim=1000,means=c(0,0),sds=c(1,1)){
#   lower = qt(.025,df=sum(nsamp) - 2)
#   upper = qt(.975,df=sum(nsamp) - 2)
#   ts = replicate(nsim,
#                  t.test(rnorm(nsamp[1],mean=means[1],sd=sds[1]),
#                         rnorm(nsamp[2],mean=means[2],sd=sds[2]))$statistic)
#   
#   sum(ts < lower | ts > upper) / nsim
# }
# 
# t.power(means = c(0,1))



# Plot distribution of t-values
# plot_upper_bound <- max(abs(min(implicit_t_values)), 
#                         abs(max(implicit_t_values)))
# # plot_bounds <- c(-1 * plot_upper_bound,
# #                  plot_upper_bound)
# plot_x_ticks <- seq(-1 * plot_upper_bound,
#                    plot_upper_bound,
#                    length=n_iterations_per_sample)
# 
# 
# plot(plot_x_ticks,
#      dt(plot_x_ticks, df = n_iterations_per_sample - 2),
#      col='red',type='l')
# lines(density(implicit_t_values))



# Cohen's d calculation
# sem_implicit <- sd(implicit_scores)/sqrt(sample_size)
# 
# mean(implicit_scores)/sd(implicit_scores)
# 
# cohen.d(sapply(unaware_pop, function(x) x[2]), # implicit effects from unaware subs
#         sapply(aware_pop, function(x) x[2]),
#         paired = TRUE)
