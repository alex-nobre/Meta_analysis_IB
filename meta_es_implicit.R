
# Load packages
library(tidyverse)
library(meta)

# Save defaults
defaults <- par()
options_defaults <- options()

# Read spreadsheet
es_table<- read_delim("./es_coding_table.txt", 
                          delim="\t", locale = locale(decimal_mark = ".")) #%>%
  #dplyr::select(c(1:14))

#View(es_table)
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


## Compute meta analytic effect size
meta_es <- metagen(TE = es_table$hedgesg, # treatment effect (Hedge's g)
                   es_table$se_g, #standard error of treatment,
                   studlab = es_table$Study,
                   comb.fixed = TRUE,
                   comb.random = TRUE)

summary(meta_es)
knitr::kable(meta_es)

# Plots

forest(meta_es, # <------------------ RUN THIS TO GENERATE UNTRIMMED FOREST PLOT  <--------------
       STUDLAB = TRUE, #should study labels be printed?
       comb.fixed = FALSE, # plot fixed effect estimate?
       comb.random = TRUE # plot random effect estimate
       )

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

forest(trimmed_meta) # <------------------ RUN THIS TO GENERATE TRIMMED FOREST PLOT #


#=============================================================================================================================#

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
