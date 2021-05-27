
#=============================================================================================================#
#========================================== Export table for paper ===========================================#
# Source the scripts for computation of effect sizes from paper data
# Create a table with ES and SEs of ES to export
# Export table to .txt document
#=============================================================================================================#

#================== Load packages =====================
# General data processing
library(tidyverse)

#=====================================================#

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
## Estimate correlation between measures (conditions) 
##to compute Cohen's d for variance from two studies:

# Schnuerch et al. (2016)
# Razpurker-apfeld and Pratt (2008)
# Beanland and Pammer 1A (2010)

#================================== 1.1. Run auxiliary scripts ===============================

#cor_pairs <- 0.93
source("compute_correlation_between_conditions.R")

# Create vectors with effect sizes for implicit processing and awareness
source("calculate_implicit_effect_sizes.R")
source("calculate_awareness_effect_sizes.R") # lax criterion
#source("calculate_strict_awareness_effect_sizes.R") # strict criterion

#================================= 1.2. Build data frame =====================================
source("create_es_data_table.R")

es_table_export <- data.frame(Study = studies_outcomes,
                              Fishers_z_implicit = implicit_z_r, #Implicit Fisher's z correlation
                              variance_implicit_z_rs = 1/(es_table$N_participants_implicit - 3), #Variances of Fisher's z correlation
                              #se_implicit_z_rs = sqrt(es_table$variance_implicit_z_rs), #SE of Fisher's z correlation 
                              Fishers_z_awareness = awareness_z_rs, #Awareness Fisher's z correlation
                              variance_awareness_z_rs = 1/(es_table$N_participants_awareness - 3)
                              #se_awareness_z_rs =  sqrt(es_table$variance_awareness_z_rs)
                              )

es_table_export$se_implicit_z_rs <- sqrt(es_table_export$variance_implicit_z_rs) #SE of Fisher's z correlation 
es_table_export$se_awareness_z_rs <- sqrt(es_table_export$variance_awareness_z_rs)

es_table_export <- es_table_export %>%
  dplyr::select(-c(variance_implicit_z_rs, variance_awareness_z_rs))

# Format n of decimal digits separately (because the data frame has both character and numeric columns:
# https://stackoverflow.com/questions/14260646/how-to-control-number-of-decimal-digits-in-write-table-output
# answer 2)

es_table_export <- es_table_export %>%
  mutate_if(is.numeric, round, digits = 2)

# Write table to .txt
write.table(es_table_export, file = "es_table_export.txt",
            sep = "\t",
            quote = FALSE,
            row.names = FALSE)
