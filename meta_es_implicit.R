
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
#======================================== 0. Functions ======================================#
#============================================================================================#

# Build "not in" operator
# Souce: https://stackoverflow.com/questions/5831794/opposite-of-in
'%!in%' <- function(x,y)!('%in%'(x,y))

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
# Beanland and Pammer 1A (2010)

 #cor_pairs <- 0.93

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

# Create column for Hedges' g
es_table$awareness_hedgesg <- awareness_hedgesg

# # Create column for standard error of g
es_table$se_awareness_g <- awareness_hedgesg_se

# Categorize experiments as inattention paradigms or not; 0 = no, 1 = yes
es_table$inattention_paradigm <- c(rep(0, 21), #ariga_2007_exp2 to most_2005_exp1to7pooled
                                  rep(1,4), #razpurker-apfeld and pratt, 2008
                                  0, #richards_2012_tracking
                                  rep(1, 10), #russsel_driver_2005
                                  rep(0,2) #schunerch_2016
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
                                 rep(0,2) #schunerch_2016
                                 ) %>%
  as.factor()

#============================================================================================#
#============================ 2. Compute implicit meta-analytic ES ===========================
#============================================================================================#

## Build meta analytic effect model
implicit_meta_es <- metagen(TE = es_table$implicit_hedgesg, # treatment effect (Hedge's g)
                   es_table$se_implicit_g, #standard error of treatment,
                   studlab = es_table$study,
                   comb.fixed = FALSE,
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
dev.copy2eps()

# Check assimetry with funnel plot
funnel(implicit_meta_es,
       xlab = "Hedges' g",#()
       studlab = es_table$study)

# Tests for assymetry
metabias(implicit_meta_es,  method = "rank")

# Estimate bias with trim-and-fill method
trimmed_meta <- trimfill(implicit_meta_es,
                         left = TRUE,
                         ma.fixed = FALSE)

funnel(trimmed_meta,
       xlab = "Effect size")

forest(trimmed_meta) # generate trimmed forest plot


# Remove studies with assymetric effect sizes
es_table_2 <- es_table %>%
  filter(study %!in% c("moore_egeth_1997_exp1", "moore_egeth_1997_exp3", 
                       "mack_and_rock_2000_exp1", "mack_and_rock_2000_exp2", 
                       "mack_and_rock_2000_exp3", "mack_and_rock_2000_exp4", "mack_and_rock_2000_exp5"))

implicit_meta_es_2 <- metagen(TE = es_table_2$implicit_hedgesg, # treatment effect (Hedge's g)
                            es_table_2$se_implicit_g, #standard error of treatment,
                            studlab = es_table_2$study,
                            comb.fixed = FALSE,
                            comb.random = TRUE)

summary(implicit_meta_es_2)

# Plots
forest(implicit_meta_es_2, # generate untrimmed forest plot
       STUDLAB = TRUE, #should study labels be printed?
       comb.fixed = FALSE, # plot fixed effect estimate?
       comb.random = TRUE # plot random effect estimate
)

# Check assimetry with funnel plot
funnel(implicit_meta_es_2,
       xlab = "Hedges' g")#,
       #studlab = es_table_2$study)

# Tests for assymetry
metabias(implicit_meta_es_2,  method = "rank")

# Estimate bias
trimmed_meta <- trimfill(implicit_meta_es,
                         left = TRUE,
                         ma.fixed = FALSE)

funnel(trimmed_meta,
       xlab = "Effect size")

forest(trimmed_meta) # generate trimmed forest plot

#============================================================================================#
#============================ 3. Compute awareness meta-analytic ES ==========================
#============================================================================================#

## Build meta analytic effect model
awareness_meta_es <- metagen(TE = es_table$awareness_hedgesg, # treatment effect (Hedge's g)
                            es_table$se_awareness_g, #standard error of treatment,
                            studlab = es_table$study,
                            comb.fixed = FALSE,
                            comb.random = TRUE)

summary(awareness_meta_es)
knitr::kable(awareness_meta_es)

# Plots
forest(awareness_meta_es, # generate untrimmed forest plot
       STUDLAB = TRUE, #should study labels be printed?
       comb.fixed = FALSE, # plot fixed effect estimate?
       comb.random = TRUE # plot random effect estimate
)

# Check assimetry with funnel plot
funnel(awareness_meta_es,
       xlab = "Hedges' g",
       studlab = es_table$study)

# Tests for assymetry
metabias(awareness_meta_es,  method = "rank")

# Estimate bias with trim-and-fill method
trimmed_meta <- trimfill(awareness_meta_es,
                         left = TRUE,
                         ma.fixed = FALSE)

funnel(trimmed_meta,
       xlab = "Effect size")

forest(trimmed_meta) # generate trimmed forest plot


# Remove studies with assymetric effect sizes
es_table_2 <- es_table %>%
  filter(study %!in% c("moore_egeth_1997_exp1", "moore_egeth_1997_exp3", 
                       "mack_and_rock_2000_exp1", "mack_and_rock_2000_exp2", 
                       "mack_and_rock_2000_exp3", "mack_and_rock_2000_exp4", "mack_and_rock_2000_exp5"))

awareness_meta_es_2 <- metagen(TE = es_table_2$awareness_hedgesg, # treatment effect (Hedge's g)
                              es_table_2$se_awareness_g, #standard error of treatment,
                              studlab = es_table_2$study,
                              comb.fixed = FALSE,
                              comb.random = TRUE)

summary(awareness_meta_es_2)

# Plots
forest(awareness_meta_es_2, # generate untrimmed forest plot
       STUDLAB = TRUE, #should study labels be printed?
       comb.fixed = FALSE, # plot fixed effect estimate?
       comb.random = TRUE # plot random effect estimate
)

# Check assimetry with funnel plot
funnel(awareness_meta_es_2,
       xlab = "Hedges' g")#,
#studlab = es_table_2$study)

# Tests for assymetry
metabias(awareness_meta_es_2,  method = "rank")

# Estimate bias
trimmed_meta <- trimfill(awareness_meta_es,
                         left = TRUE,
                         ma.fixed = FALSE)

funnel(trimmed_meta,
       xlab = "Effect size")

forest(trimmed_meta) # generate trimmed forest plot

#=========================================================================================#
#================= 4. Heterogeneity between implicit and awareness ES =====================
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


# Histogram of n of trials for implicit
ggplot(data=es_table) +
  geom_histogram(aes(x=N_trials_implicit)) + 
  labs(title = "Frequency of N of trials for implicit processing across studies",
       x = "N of trials") +
  theme(plot.title = element_text(hjust = 0.5))




#=========================================================================================#
#======================== 5. Moderation analysis of implicit ES ===========================
#=========================================================================================#

#====== 5.1. compute subgroup analysis for binary categorical variables ======

# unexpected stimulus relevance
mod_implicit_relevance_us <- update(implicit_meta_es, 
                           byvar=es_table$us_relevance, 
                           print.byvar=FALSE)
summary(mod_implicit_relevance_us)

# type of implicit measure
mod_implicit_type_implicit_measure <- update(implicit_meta_es, 
                            byvar=es_table$implicit_type, 
                            print.byvar=FALSE)
summary(mod_implicit_type_implicit_measure)

# implicit measure outcome
mod_implicit_outcome_of_implicit_measure <- update(implicit_meta_es, 
                               byvar=es_table$implicit_measure, 
                               print.byvar=FALSE)
summary(mod_implicit_outcome_of_implicit_measure)

# unexpected stimulus presentation
mod_implicit_us_presentation <- update(implicit_meta_es, 
                              byvar=es_table$us_presentation, 
                              print.byvar=FALSE)
summary(mod_implicit_us_presentation)

# unexpected stimulus delay type
mod_implicit_us_delay_type <- update(implicit_meta_es, 
                            byvar=es_table$us_delay_type, 
                            print.byvar=FALSE)
summary(mod_implicit_us_delay_type)

# awareness assessment
mod_implicit_us_assessment <- update(implicit_meta_es, 
                            byvar=es_table$us_assessment, 
                            print.byvar=FALSE)
summary(mod_implicit_us_assessment)

# significance of implicit effect
mod_implicit_significance <- update(implicit_meta_es, 
                           byvar=es_table$significance, 
                           print.byvar=FALSE)
summary(mod_implicit_significance)

# gray literature
mod_implicit_gray_literature <- update(implicit_meta_es, 
                           byvar=es_table$gray_literature, 
                           print.byvar=FALSE)
summary(mod_implicit_gray_literature)

#==== significance of inattention paradigm ====#
# Include moderator
mod_implicit_inattention <- update(implicit_meta_es, 
                           byvar=es_table$inattention_paradigm, 
                           print.byvar=FALSE)
summary(mod_implicit_inattention)


## Create model with subset
meta_implicit_inattention <- metagen(TE = es_table$implicit_hedgesg, # treatment effect (Hedge's g)
                   es_table$se_implicit_g, #standard error of treatment,
                   studlab = es_table$study,
                   subset = es_table$inattention_paradigm == 1,
                   comb.random = TRUE)

forest(meta_implicit_inattention, # generate untrimmed forest plot
       STUDLAB = TRUE, #should study labels be printed?
       comb.random = TRUE # plot random effect estimate
)

#==== significance of group assessment of awareness ====#

## Include moderator
mod_implicit_group_aware_assess <- update(implicit_meta_es, 
                          byvar=es_table$group_aware_assess, 
                          print.byvar=FALSE)
summary(mod_implicit_group_aware_assess)


## Create model with subset
meta_implicit_group_aware_assess_yes <- metagen(TE = es_table$implicit_hedgesg, # treatment effect (Hedge's g)
                            es_table$se_implicit_g, #standard error of treatment,
                            studlab = es_table$study,
                            subset = es_table$group_aware_assess == 1,
                            comb.random = TRUE)

meta_implicit_group_aware_assess_no <- metagen(TE = es_table$implicit_hedgesg, # treatment effect (Hedge's g)
                                   es_table$se_implicit_g, #standard error of treatment,
                                   studlab = es_table$study,
                                   subset = es_table$group_aware_assess == 0,
                                   comb.random = TRUE)

forest(meta_implicit_group_aware_assess_yes, # generate untrimmed forest plot
       STUDLAB = TRUE, #should study labels be printed?
       comb.random = TRUE # plot random effect estimate
)

#====== 5.2. compute metaregression for non-binary categorical or continuous variables ======

# number of participants per group
mod_implicit_n_group <- metareg(implicit_meta_es, 
                                es_table$N_per_group)
summary(mod_implicit_n_group)

# number of trials for implicit processing
mod_implicit_n_trials_implicit <- metareg(implicit_meta_es, 
                                          es_table$N_trials_implicit)
summary(mod_implicit_n_trials_implicit)

# number of trials for awareness
mod_implicit_n_trials_awareness <- metareg(implicit_meta_es, 
                                           es_table$N_trials_awareness)
summary(mod_implicit_n_trials_awareness)

# number of participants for implicit processing
mod_implicit_N_participants_implicit <- metareg(implicit_meta_es, 
                                                es_table$N_participants_awareness)
summary(mod_implicit_N_participants_implicit)

# number of participants for awareness
mod_implicit_N_participants_awareness <- metareg(implicit_meta_es, 
                                                 es_table$N_participants_awareness)
summary(mod_implicit_N_participants_awareness)


#=========================================================================================#
#======================== 6. Moderation analysis of awareness ES ===========================
#=========================================================================================#

# type of awareness measure
# Include moderator
mod_awareness_objective <- update(awareness_meta_es, 
                            byvar=es_table$awareness_objective, 
                            print.byvar=FALSE)
summary(mod_awareness_objective)

# create model with subset
meta_awareness_objective_aware_obj <- metagen(TE = es_table$awareness_hedgesg, # treatment effect (Hedge's g)
                                   es_table$se_awareness_g, #standard error of treatment,
                                   studlab = es_table$study,
                                   subset = es_table$awareness_objective == "objective",
                                   comb.random = TRUE)

meta_awareness_objective_aware_sub <- metagen(TE = es_table$awareness_hedgesg, # treatment effect (Hedge's g)
                                      es_table$se_awareness_g, #standard error of treatment,
                                      studlab = es_table$study,
                                      subset = es_table$awareness_objective == "subjective",
                                      comb.random = TRUE)


## include moderator for awareness measure
mod_awareness_measure_awareness <- update(awareness_meta_es, 
                               byvar=es_table$awareness_measure, 
                               print.byvar=FALSE)
summary(mod_awareness_measure_awareness)


#====== Group assessment of awareness =======#

## include moderator
mod_awareness_group_aware_assess <- update(awareness_meta_es, 
                                  byvar=es_table$group_aware_assess, 
                                  print.byvar=FALSE)
summary(mod_awareness_group_aware_assess)

## Create model with subset
meta_awareness_group_aware_assess_yes <- metagen(TE = es_table$awareness_hedgesg, # treatment effect (Hedge's g)
                                                es_table$se_awareness_g, #standard error of treatment,
                                                studlab = es_table$study,
                                                subset = es_table$group_aware_assess == 1,
                                                comb.random = TRUE)

meta_awareness_group_aware_assess_no <- metagen(TE = es_table$awareness_hedgesg, # treatment effect (Hedge's g)
                                               es_table$se_awareness_g, #standard error of treatment,
                                               studlab = es_table$study,
                                               subset = es_table$group_aware_assess == 0,
                                               comb.random = TRUE)

forest(meta_awareness_group_aware_assess_yes, # generate untrimmed forest plot
       STUDLAB = TRUE, #should study labels be printed?
       comb.random = TRUE # plot random effect estimate
)

#====== 6.2. compute metaregression for non-binary categorical or continuous variables ======

# number of trials for awareness
mod_awareness_n_trials_awareness <- metareg(awareness_meta_es, 
                                            es_table$N_trials_awareness)
summary(mod_awareness_n_trials_awareness)
