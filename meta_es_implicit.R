
# Load packages
library(tidyverse)
library(meta)
library(metafor)


# Save defaults
defaults <- par()
options_defaults <- options()

#============================================================================================#
#==================================== 1. Prepare data =======================================#
#============================================================================================#
# Create vector with effect sizes
source("Calculate_effect_sizes.R")

# Read spreadsheet
#es_table<- read_delim("./es_coding_table.txt", 
#                         delim="\t", locale = locale(decimal_mark = ".")) #%>%
  #dplyr::select(c(1:14))
es_table<- read_delim("./es_coding_table.csv", 
                      delim=";", locale = locale(decimal_mark = ".")) #%>%

View(es_table)
#str(es_table)

## Estimate correlation between measures (conditions) 
##to compute Cohen's d for variance from two studies:


# Schnuerch et al. (2016)
# Razpurker-apfeld and Pratt (2008)
cor_pairs <- 0.93

# replace ds by computed cohens ds
es_table$d <- cohensd

## Create variables to compute Hedges' g (formulas from Borestein's Introduction to Meta-Analysis, 2009)

# Compute variance of d (formula 4.28)
es_table$variance_d <- (1/es_table$N_per_group + 
                          (es_table$d)^2/2*es_table$N_per_group) * 2*(1-cor_pairs)

# Compute correction factor J (formula 4.22)
es_table$J <- 1 - (3/(4*(es_table$N_per_group-1)-1))

# Compute Hedges' g (formula 4.23)
es_table$hedgesg <- es_table$J * es_table$d 

# Compute variance of g (formula 4.24)
es_table$variance_g <- (es_table$J)^2 * es_table$variance_d

# Compute standard error of g (formula 4.25)
es_table$se_g <- sqrt(es_table$variance_g)

#============================================================================================#
#============================ 2. Compute meta-analytic ES ===================================#
#============================================================================================#

## Build meta analytic effect model
meta_es <- metagen(TE = es_table$hedgesg, # treatment effect (Hedge's g)
                   es_table$se_g, #standard error of treatment,
                   studlab = es_table$study,
                   comb.fixed = TRUE,
                   comb.random = TRUE)

summary(meta_es)
knitr::kable(meta_es)

# Plots
dev.new(width = 20, height = 12)
forest(meta_es, # RUN THIS TO GENERATE UNTRIMMED FOREST PLOT
       STUDLAB = TRUE, #should study labels be printed?
       comb.fixed = FALSE, # plot fixed effect estimate?
       comb.random = TRUE # plot random effect estimate
       )
dev.copy2eps


funnel(meta_es,
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

forest(trimmed_meta) # RUN THIS TO GENERATE TRIMMED FOREST PLOT 


#=========================================================================================#
#=================================== Heterogeneity =======================================#
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
#================================ Moderation analysis ====================================#
#=========================================================================================#

### compute subgroup analysis for binary categorical variables ###

# unexpected stimulus relevance
## 0 = irrelevant / 1 = relevant
mod_relevance_us <- update(meta_es, byvar=es_table$us_relevance, print.byvar=FALSE)
summary(mod_relevance_us)

# type of implicit measure
## 0 = attentional and/or perceptual / 1 = response / 2 = neurophysiology
#mod_implicit_type <- update(meta_es, byvar=es_table$implicit_type, print.byvar=FALSE)
#summary(mod_implicit_type)

# implicit measure outcome
## 0 = RT / 1 = accuracy
mod_implicit_measure_1 <- update(meta_es, byvar=es_table$implicit_measure_1, print.byvar=FALSE)
summary(mod_implicit_measure_1)

# unexpected stimulus presentation
## 0 = IB block / 1 = IB phase / 2 = interleaved
#mod_us_presentation <- update(meta_es, byvar=es_table$us_presentation, print.byvar=FALSE)
#summary(mod_us_presentation)

# unexpected stimulus delay type
## 0 = fixed / 1 = variable
mod_us_delay_type <- update(meta_es, byvar=es_table$us_delay_type, print.byvar=FALSE)
summary(mod_us_delay_type)

# awareness assessment
## 0 = after critical trial / 1 = after block/phase
mod_us_assessment <- update(meta_es, byvar=es_table$us_assessment, print.byvar=FALSE)
summary(mod_us_assessment)

# significance of implicit effect
## 0 = non-significant / 1 = significant
mod_significance <- update(meta_es, byvar=es_table$significance, print.byvar=FALSE)
summary(mod_significance)

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

# type of implicit measure
## 0 = attentional and/or perceptual / 1 = response / 2 = neurophysiology
mod_implicit_type <- metareg(meta_es, es_table$implicit_type)
summary(mod_implicit_type)

# unexpected stimulus presentation
## 0 = IB block / 1 = IB phase / 2 = interleaved
mod_us_presentation <- metareg(meta_es, es_table$us_presentation)
summary(mod_us_presentation)





