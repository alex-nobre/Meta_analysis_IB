
#=========================================================================================================================================#
#==================================================== Meta-analysis of implicit effects===================================================#
# Source the script for computation of effect sizes from paper data and 
# Run meta-analysis of implicit effects outputting meta-analytic effect size and test, and heterogeneity test
# Builds forest plot to visualize confidence intervals and heterogeneity
# Build funnel plot to check for publication bias and run trim-and-fill method
#=========================================================================================================================================#

# Load packages
library(tidyverse)
library(meta)
library(metafor)


# Save defaults
graphical_defaults <- par()
options_defaults <- options()

#============================================================================================#
#==================================== 1. Prepare data ========================================
#============================================================================================#
# Create data frame
source("create_es_data_table.R")

# Create vectors with effect sizes for implicit processing and awareness
source("calculate_implicit_effect_sizes.R")
source("calculate_awareness_effect_sizes.R")

View(es_table)


## Estimate correlation between measures (conditions) 
##to compute Cohen's d for variance from two studies:

# Schnuerch et al. (2016)
# Razpurker-apfeld and Pratt (2008)

cor_pairs <- 0.93

# replace ds by computed cohens ds
es_table$implicit_d <- implicit_cohensd
es_table$awareness_d <- awareness_cohensd

## Create variables to compute Hedges' g (formulas from Borestein's Introduction to Meta-Analysis, 2009) 
## for both implicit and awareness effect sizes

### Implicit ES

# Compute variance of d (formula 4.28)
es_table$variance_implicit_d <- (1/es_table$N_per_group + 
                          (es_table$implicit_d)^2/2*es_table$N_per_group) * 2*(1-cor_pairs)

# Compute correction factor J (formula 4.22)
es_table$J <- 1 - (3/(4*(es_table$N_per_group-1)-1))

# Compute Hedges' g (formula 4.23)
es_table$implicit_hedgesg <- es_table$J * es_table$implicit_d 

# Compute variance of g (formula 4.24)
es_table$variance_implicit_g <- (es_table$J)^2 * es_table$variance_implicit_d

# Compute standard error of g (formula 4.25)
es_table$se_implicit_g <- sqrt(es_table$variance_implicit_g)

### Awareness ES

# Compute variance of d (formula 4.28)
es_table$variance_awareness_d <- (1/es_table$N_per_group + 
                                   (es_table$awareness_d)^2/2*es_table$N_per_group) * 2*(1-cor_pairs)

# Compute correction factor J (formula 4.22)
es_table$J <- 1 - (3/(4*(es_table$N_per_group-1)-1))

# Compute Hedges' g (formula 4.23)
es_table$awareness_hedgesg <- es_table$J * es_table$awareness_d 

# Compute variance of g (formula 4.24)
es_table$variance_awareness_g <- (es_table$J)^2 * es_table$variance_awareness_d

# Compute standard error of g (formula 4.25)
es_table$se_awareness_g <- sqrt(es_table$variance_awareness_g)

# Categorize experiments as inattention paradigms or not; 0 = no, 1 = yes
es_table$inattention_paradigm <- c(rep(0, 21), #ariga_2007_exp2 to most_2005_exp1to7pooled
                                  rep(1,4), #razpurker-apfeld and pratt, 2008
                                  0, #richards_2012_tracking
                                  rep(1, 10), #russsel_driver_2005
                                  rep(0,3) #shafto_pitts_2015 and schunerch_2016
                                  ) %>%
  as.factor()

# Categorize experiments as group assessment of awareness or not; 0 = no, 1 = yes
es_table$group_aware_assess <- c(1, #ariga_2007_exp2
                                 rep(0, 6), #beanland_pammer_2010_exp1A_fixating to gabay_2012_exp2
                                 rep(1,4), #lo_yeh_2008_exp1_200ms
                                 rep(0, 5), #mack_and_rock_2000
                                 rep(1,9),  # moore_egeth_1997_exp1 to razpurker-apfeld and pratt, 2008
                                 0, #richards_2012_tracking
                                 rep(1, 10), #russsel_driver_2005
                                 rep(0,3) #shafto_pitts_2015 and schunerch_2016
                                 ) %>%
  as.factor()

#============================================================================================#
#============================ 2. Compute meta-analytic ES ====================================
#============================================================================================#

# 2.1. Implicit ES
## Build meta analytic effect model
implicit_meta_es <- metagen(TE = es_table$implicit_hedgesg, # treatment effect (Hedge's g)
                   es_table$se_implicit_g, #standard error of treatment,
                   studlab = es_table$study,
                   comb.fixed = TRUE,
                   comb.random = TRUE)

summary(implicit_meta_es)
knitr::kable(implicit_meta_es)

# Plots
dev.new(width = 20, height = 12)
forest(implicit_meta_es, # generate untrimmed forest plot
       STUDLAB = TRUE, #should study labels be printed?
       comb.fixed = FALSE, # plot fixed effect estimate?
       comb.random = TRUE # plot random effect estimate
       )
dev.copy2eps


funnel(implicit_meta_es,
       xlab = "Hedges' g")#,
       #studlab = es_table$Study)

# Tests for assymetry
metabias(meta_es,  method = "rank")

# Estimate bias
trimmed_meta <- trimfill(meta_es,
                         left = TRUE,
                         ma.fixed = FALSE)

funnel(trimmed_meta,
       xlab = "Effect size")

forest(trimmed_meta) # generate trimmed forest plot


#=========================================================================================#
#================================= 3.  Heterogeneity ======================================
#=========================================================================================#

mean(es_table$N_trials_implicit)
sd(es_table$N_trials_implicit)

mean(es_table$N_trials_awareness)

hist(es_table$N_trials_implicit,
     ylim = c(0,30))
hist(es_table$N_trials_awareness,
     ylim = c(0,30))

plot(es_table$N_trials_implicit, es_table$N_trials_awareness)

min(es_table$N_trials_implicit)
max(es_table$N_trials_implicit)


ggplot(data=es_table) +
  geom_histogram(aes(x=N_trials_implicit)) + 
  labs(title = "Frequency of N of trials for implicit processing across studies",
       x = "N of trials") +
  theme(plot.title = element_text(hjust = 0.5))




#=========================================================================================#
#=============================== 4. Moderation analysis ===================================
#=========================================================================================#


### compute subgroup analysis for binary categorical variables ###

# unexpected stimulus relevance
## 0 = irrelevant / 1 = relevant
mod_relevance_us <- update(meta_es, 
                           byvar=es_table$us_relevance, 
                           print.byvar=FALSE)
summary(mod_relevance_us)

# type of implicit measure
## 0 = attentional and/or perceptual / 1 = response
mod_implicit_type <- update(meta_es, 
                            byvar=es_table$implicit_type, 
                            print.byvar=FALSE)
summary(mod_implicit_type)

# implicit measure outcome
## 0 = RT / 1 = accuracy
mod_implicit_measure <- update(meta_es, 
                               byvar=es_table$implicit_measure, 
                               print.byvar=FALSE)
summary(mod_implicit_measure)

# unexpected stimulus presentation
## 0 = IB block or IB phase / 1 = interleaved
mod_us_presentation <- update(meta_es, 
                              byvar=es_table$us_presentation, 
                              print.byvar=FALSE)
summary(mod_us_presentation)

# unexpected stimulus delay type
## 0 = fixed / 1 = variable
mod_us_delay_type <- update(meta_es, 
                            byvar=es_table$us_delay_type, 
                            print.byvar=FALSE)
summary(mod_us_delay_type)

# awareness assessment
## 0 = after critical trial / 1 = after block/phase
mod_us_assessment <- update(meta_es, 
                            byvar=es_table$us_assessment, 
                            print.byvar=FALSE)
summary(mod_us_assessment)

# significance of implicit effect
## 0 = non-significant / 1 = significant
mod_significance <- update(meta_es, 
                           byvar=es_table$significance, 
                           print.byvar=FALSE)
summary(mod_significance)

#==== significance of inattention paradigm ====#
mod_inattention <- update(meta_es, 
                           byvar=es_table$inattention_paradigm, 
                           print.byvar=FALSE)
summary(mod_inattention)


## Build meta analytic effect model
meta_inattention <- metagen(TE = es_table$hedgesg, # treatment effect (Hedge's g)
                   es_table$se_g, #standard error of treatment,
                   studlab = es_table$study,
                   subset = es_table$inattention_paradigm == 1,
                   comb.random = TRUE)

forest(meta_inattention, # generate untrimmed forest plot
       STUDLAB = TRUE, #should study labels be printed?
       comb.random = TRUE # plot random effect estimate
)

#==== significance of group assessment of awareness ====#
mod_group_aware_assess <- update(meta_es, 
                          byvar=es_table$group_aware_assess, 
                          print.byvar=FALSE)
summary(mod_group_aware_assess)


## Build meta analytic effect model
meta_group_aware_assess <- metagen(TE = es_table$hedgesg, # treatment effect (Hedge's g)
                            es_table$se_g, #standard error of treatment,
                            studlab = es_table$group_aware_assess,
                            subset = es_table$group_aware_assess == 1,
                            comb.random = TRUE)

metagen(TE = es_table$hedgesg, # treatment effect (Hedge's g)
                                   es_table$se_g, #standard error of treatment,
                                   studlab = es_table$group_aware_assess,
                                   subset = es_table$group_aware_assess == 0,
                                   comb.random = TRUE)

forest(meta_group_aware_assess, # generate untrimmed forest plot
       STUDLAB = TRUE, #should study labels be printed?
       comb.random = TRUE # plot random effect estimate
)

### compute metaregression for non-binary categorical or continuous variables ###

# number of participants per group
mod_n_group <- metareg(meta_es, es_table$N_per_group)
summary(mod_n_group)

# number of trials for implicit processing
mod_n_trials_implicit <- metareg(meta_es, es_table$N_trials_implicit)
summary(mod_n_trials_implicit)

# number of trials for awareness
mod_n_trials_awareness <- metareg(meta_es, es_table$N_trials_awareness)
summary(mod_n_trials_awareness)

# number of participants for implicit processing
mod_N_participants_implicit <- metareg(meta_es, es_table$N_participants_awareness)
summary(mod_N_participants_implicit)

# number of participants for awareness
mod_N_participants_awareness <- metareg(meta_es, es_table$N_participants_awareness)
summary(mod_N_participants_awareness)





