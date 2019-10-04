

library(effsize)

# Save defaults
graphical_defaults <- par()
options_defaults <- options()

#=========== Pseudocode ===========
# Generate n pairs implicit-effect/awareness effects, where each pair corresponds to one participant

# effect sizes are sampled from normal distribution for simplicity; this is the assumption 
# for tests in most papers

# for implicit effects, the effect size is the cohen's d value sampled from a distribution 
# with a sd of 1, to represent a standardized difference

# sampling is performed assuming equal variances for both groups; this might not be 
# reasonable - aware subs may have larger variance

# implicit effects are null when awareness is false; exisiting when awareness is true
# how small and how large: use literature as guides

# awareness effect is 0 (unaware) or 1 (aware) plus some random error

# After sampling, a t-test is computed on the values of the differences, and
# the p-values from those tests are saved in a list to compute n of significant results
# for both implicit tests and awareness tests
#=======================================================================================#


#===============================#
# Parameters to vary:
# Sample size
# Effect size
# proportion of aware x unaware
#===============================#
n_iterations_per_sample <- 10000

# Fixed sample size for now; 
# later, run simulation for sample sizes ranging from 7
# to 186 (most et al., 2005)
sample_size <- 100
effect_size_implicit <- 1
effect_size_awareness <- 0.01
prop_aware_parts <- 1/2
prop_unaware_parts <- sample_size - sample_size * prop_aware_parts


#====================================================================================#
#=================================== Functions =======================================
#====================================================================================#

# Function to generate p-values
generate_p_values <- function(n_iterations = 10000, 
                              effect_implicit = 1, 
                              effect_awareness = 0.01, 
                              n_sample = 100, 
                              prop_aware = 1/2) {
  # Create vectors to store t-values for implicit and awareness tests
  implicit_p_values <- vector(mode = "numeric", 
                              length=n_iterations)
  
  awareness_p_values <- vector(mode = "numeric", 
                               length=n_iterations)
  
  
  # Generate implicit and awareness t-values for each population
  # For now, variances are the same in both groups for each test
  for(iteration in 1:n_iterations){
    # Create a list of awareness-implicit score vectors for each population
    
    # Compute n of aware and unaware for vector lengths
    n_aware <- n_sample * prop_aware
    n_unaware <- n_sample - n_aware
    
    ## Unaware group
    unaware_pop <- vector(mode = "list", length=n_unaware)
    
    for(part in 1:length(unaware_pop)) {
      unaware_pop[[part]] <- c(rnorm(1, mean = 0, sd = 1), # awareness effect = 0
                               # rnorm(1, mean = 1, sd = 1)) # implicit effect (small)
                               rnorm(1, mean = 0, sd = 1)) # implicit effect (null)
    }
    
    ## Aware group
    aware_pop <- vector(mode = "list", length=n_aware)
    
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

#====================================================================================#
#=================================== Simulation ======================================
#====================================================================================#

# Create ps and plots
sample_sizes <- c(6, 20, 26, 32, 42, 60, 80, 120, 200, 260)
props_aware <- c(sample_size/2, sample_size/3, sample_size/4, sample_size/5)
proportion_labels <- c("50/50", "1/2", "25/75", "20/80")


# Facetted scatterplot of false implicits; one facet for sample size
par(mfrow=c(2,2))
par(mar=c(4.1,4.1,3.1,2.1))
for(proportion in 1:length(props_aware)) {
  values <- generate_p_values(prop_aware = props_aware[proportion])
  plot_p_values(values[[1]], values[[2]],
                proportion_aware = proportion_labels[proportion])
}
par(graphical_defaults)

#======== Compute and plot n of false implicits by sample size =========
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
     false_implicits_by_n,
     pch = 16)


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
     false_implicits_by_effect,
     pch = 16)
lines(implicit_effect_sizes,
      false_implicits_by_effect)

#===== Compute and plot n of false implicits by implicit effect size =====

awareness_effect_sizes <- seq(0.05, 0.5, by = 0.05)

false_implicits_by_awareness_effect <- vector(mode = "numeric",
                                              length = length(awareness_effect_sizes))


for(n in 1:length(awareness_effect_sizes)) {
  values <- generate_p_values(effect_awareness = awareness_effect_sizes[n])
  
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
  false_implicits_by_awareness_effect[n] <- sum(significance_values)/length(significance_values)
}

# Plot with line for proportion of false implicits by n: use continuous sequence of n of trials
plot(awareness_effect_sizes,
     false_implicits_by_awareness_effect,
     pch = 16)


#===== Compute and plot n of false implicits by ratio aware/unaware =====

ratios_aware <- c(floor(sample_size/2), 
                 floor(sample_size/3), 
                 floor(sample_size/4), 
                 floor(sample_size/5))

false_implicits_by_ratio <- vector(mode = "numeric",
                                    length = length(ratios_aware))


for(n in 1:length(ratios_aware)) {
  values <- generate_p_values(prop_aware = ratios_aware[n])
  
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
  false_implicits_by_ratio[n] <- sum(significance_values)/length(significance_values)
}

# Plot with line for proportion of false implicits by n: use continuous sequence of n of trials
plot(ratios_aware,
     false_implicits_by_ratio,
     pch = 16)

#===== Implicit effect size x sample size =====

# Effects
implicit_effect_sizes <- seq(0.1, 1.0, by = 0.1)

false_implicits_by_n <- vector(mode = "numeric",
                               length = length(sample_sizes))

par(mfrow=c(2,5))
par(mar=c(4.1,4.1,3.1,2.1))
for(n in 1:length(sample_sizes)) {
  
  
  false_implicits_by_effect <- vector(mode = "numeric",
                                      length = length(implicit_effect_sizes))
  
  for(es in 1:length(implicit_effect_sizes)) {
    values <- generate_p_values(n_sample = sample_sizes[n],
                                prop_aware = sample_sizes[n]/2,
                                effect_implicit = implicit_effect_sizes[es])
    
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
    false_implicits_by_effect[es] <- sum(significance_values)/length(significance_values)
    
  }
  plot(implicit_effect_sizes,
  false_implicits_by_effect,
  pch = 16,
  ylim = c(0, 1.0),
  main = paste("Sample size:", sample_sizes[n], sep = " "),
  xlab = "Implicit effect sizes",
  ylab = "Proportion of false implicits")
}

par(graphical_defaults)

#===== Implicit effect size x awareness effect size =====

# Effects
awareness_effect_sizes <- seq(0.05, 0.5, by = 0.05)
implicit_effect_sizes <- seq(0.1, 1.0, by = 0.1)

false_implicits_by_awareness_effect <- vector(mode = "numeric",
                               length = length(awareness_effect_sizes))

par(mfrow=c(2,5))
par(mar=c(4.1,4.1,3.1,2.1))
for(n in 1:length(awareness_effect_sizes)) {
  
  
  false_implicits_by_effect <- vector(mode = "numeric",
                                      length = length(implicit_effect_sizes))
  
  for(es in 1:length(implicit_effect_sizes)) {
    values <- generate_p_values(effect_implicit = implicit_effect_sizes[es],
                                effect_awareness = awareness_effect_sizes[n])
    
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
    false_implicits_by_effect[es] <- sum(significance_values)/length(significance_values)
    
  }
  plot(implicit_effect_sizes,
       false_implicits_by_effect,
       pch = 16,
       ylim = c(0, 1.0),
       main = paste("Awareness effect size:", awareness_effect_sizes[n], sep = " "),
       xlab = "Implicit effect sizes",
       ylab = "Proportion of false implicits")
  lines(implicit_effect_sizes,
        false_implicits_by_effect)
}

par(graphical_defaults)

# inverted

false_implicits_by_effect <- vector(mode = "numeric",
                                              length = length(implicit_effect_sizes))

par(mfrow=c(2,5))
par(mar=c(4.1,4.1,3.1,2.1))
for(n in 1:length(implicit_effect_sizes)) {
  
  
  false_implicits_by_awareness_effect <- vector(mode = "numeric",
                                      length = length(awareness_effect_sizes))
  
  for(es in 1:length(awareness_effect_sizes)) {
    values <- generate_p_values(effect_implicit = implicit_effect_sizes[n],
                                effect_awareness = awareness_effect_sizes[es])
    
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
    false_implicits_by_awareness_effect[es] <- sum(significance_values)/length(significance_values)
    
  }
  plot(awareness_effect_sizes,
       false_implicits_by_awareness_effect,
       pch = 16,
       ylim = c(0, 1.0),
       main = paste("Implicit effect size:", implicit_effect_sizes[n], sep = " "),
       xlab = "Awareness effect sizes",
       ylab = "Proportion of false implicits")
}

par(graphical_defaults)


#===== Implicit effect size x awareness effect size x sample size =====

# Effects
awareness_effect_sizes <- seq(0.05, 0.5, by = 0.05)
implicit_effect_sizes <- seq(0.1, 1.0, by = 0.1)
sample_sizes <- c(6, 20, 26, 32, 42, 60, 80, 120, 200, 260)

# Lists of graphical parameters
dot_pchs <- c(0, 1, 2, 7, 8, 13, 14, 15, 16, 17)
colors_plots <- c("blue", "red", "darkgreen", "darkorange", "yellow", 
                  "black", "purple4", "cyan", "violetred1", "seagreen1")

par(mfrow=c(2,5))
par(mar=c(4.1,4.1,3.1,2.1))
for(n in 1:length(awareness_effect_sizes)) {
  
  
  false_implicits_by_effect <- vector(mode = "list",
                                      length = length(implicit_effect_sizes))
  
  for(es in 1:length(implicit_effect_sizes)) {
    
    false_implicits_by_sample_sizes <- vector(mode = "numeric",
                                              length = length(sample_sizes))
    
    for(sz in 1:length(sample_sizes)) {
      
      values <- generate_p_values(effect_implicit = implicit_effect_sizes[es],
                                  effect_awareness = awareness_effect_sizes[n],
                                  n_sample = sample_sizes[sz])
      
      # combine p-values into pairs to generate vector of color names
      value_pairs <- mapply(c, 
                            values[[1]], values[[2]], 
                            SIMPLIFY=FALSE)
      
      # Vector of significance values: 1 for false implicit, 0 otherwise
      significance_values <- sapply(value_pairs, function(x) ifelse(x[1] < 0.05 & # implicit p-value
                                                                      x[2] > 0.05, # awareness p-value
                                                                    1,
                                                                    0))
      
      # Add n of false implicits for this sample size to vector
      false_implicits_by_sample_sizes[sz] <- sum(significance_values)/length(significance_values)
      
      rm(list = c("values", "value_pairs"))
    }
    
    false_implicits_by_effect[[es]] <- false_implicits_by_sample_sizes
  }
  
  # Plot panel for current awareness effect:
  # x-axis for implicit effect sizes
  # one curve with symbols for each sample size
  # one symbol type and one colored by sample size
  plot(implicit_effect_sizes,
       false_implicits_by_effect[[1]],
       pch = dot_pchs[1], #pch = 16,
       col = colors_plots[1],
       ylim = c(0, 1.0),
       main = paste("Awareness effect size:", awareness_effect_sizes[n], sep = " "),
       xlab = "Implicit effect sizes",
       ylab = "Proportion of false implicits")
  lines(implicit_effect_sizes,
        false_implicits_by_effect[[1]],
        col = colors_plots[1])
  
  # Extra curves
  for(sz in 2:length(false_implicits_by_effect)) {
    points(implicit_effect_sizes,
           false_implicits_by_effect[[sz]],
           pch = dot_pchs[sz],
           col = colors_plots[sz])
    lines(implicit_effect_sizes,
          false_implicits_by_effect[[sz]],
          col = colors_plots[sz])
  }
}

par(graphical_defaults)


#==========================================================================================#
#==========================================================================================#
#==========================================================================================#

# Generate values outside function
# Create vectors to store t-values for implicit and awareness tests
implicit_p_values <- vector(mode = "numeric",
                            length=n_iterations_per_sample)

awareness_p_values <- vector(mode = "numeric",
                             length=n_iterations_per_sample)


# Generate implicit and awareness t-values for each population
# For now, variances are the same in both groups for each test
for(iteration in 1:n_iterations_per_sample){
  # Create a list of awareness-implicit score vectors for each population

  ## Unaware group
  unaware_pop <- vector(mode = "list", length=sample_size/2)

  for(part in 1:length(unaware_pop)) {
    unaware_pop[[part]] <- c(rnorm(1, mean = 0, sd = 0.05), # awareness effect = 0
                             # rnorm(1, mean = 1, sd = 1)) # implicit effect (small)
                             rnorm(1, mean = 0, sd = 1)) # implicit effect (null)
  }

  ## Aware group
  aware_pop <- vector(mode = "list", length=sample_size/2)

  for(part in 1:length(aware_pop)) {
    aware_pop[[part]] <- c(rnorm(1, mean = effect_size_awareness, sd = 0.05), # awareness effect > 0 - subs are conscious
                           rnorm(1, mean = effect_size_implicit, sd = 1)) # implicit effect - effects are large
  }

  # Join values from both groups to run t-tests
  aware_scores <- c(sapply(unaware_pop, function(x) x[1]),
                    sapply(aware_pop, function(x) x[1]))

  implicit_scores <- c(sapply(unaware_pop, function(x) x[2]), # implicit effects from unaware subs
                       sapply(aware_pop, function(x) x[2])) # implicit effects from aware subs

  # Compute and store p-values
  implicit_ttest <- t.test(implicit_scores)
  awareness_ttest <- t.test(aware_scores)

  implicit_p_values[iteration] <- implicit_ttest$p.value
  awareness_p_values[iteration] <- awareness_ttest$p.value
}




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
