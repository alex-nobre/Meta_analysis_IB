
library(tidyverse)
library(meta)

# Read spreadsheet
es_table <- read_delim("./es_table.txt", 
                          delim="\t", locale = locale(decimal_mark = ".")) #%>%
  #dplyr::select(c(1:14))

View(es_table)
str(es_table)

# Estimate correlation between measures (conditions) to compute Cohen's d for variance from two studies:
# Schnuerch et al. (2016)
# Razpurker-apfeld and Pratt (2008)
cor_pairs <- 0.95

# Create variables to compute Hedges' g
es_table$variance_d <- (1/es_table$N_per_group + 
                          (es_table$d)^2/2*es_table$N_per_group) * 2*(1-cor_pairs)

es_table$J <- 1 - (3/(4*(es_table$N_per_group-1)-1))

es_table$hedgesg <- es_table$J * es_table$d 

es_table$variance_g <- (es_table$J)^2 * es_table$variance_d


es_table$se_g <- sqrt(es_table$variance_g)


# Compute meta analytic effect size

metagen(es_table$hedgesg, es_table$se_g)

