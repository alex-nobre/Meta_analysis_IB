
#==================================================================================================================#
#================================= Meta-analysis of implicit and awareness effects ================================#
# Source the scripts for computation of effect sizes from paper data
# Run meta-analysis of implicit and awarejess effects with correlation ES
# Output meta-analytic effect size and test
# Builds forest plot to visualize confidence intervals
# Check heterogeneity with test, forest plots, outlier detection and  influence analysis
# Build funnel plot to check for publication bias and run trim-and-fill method
#==================================================================================================================#

#================== Load packages =====================
# General data processing
library(tidyverse)

# Outlier detection
library(outliers)

# Meta-analytic model fitting
library(meta)
library(metafor)

# Influence analysis
library(ggrepel)
library(grid)
library(gridExtra)

#P-curve
library(poibin)
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

# Add columns with effect sizes
# replace ds by computed cohens ds
#es_table$implicit_d <- implicit_cohensd
es_table$implicit_d <- implicit_cohensdrm

## Create variables to compute Hedges' g (formulas from Borestein's Introduction to Meta-Analysis, 2009) 
## for both implicit and awareness effect sizes

### Implicit ES

# # Compute variance of d (formula 4.28)
es_table$variance_implicit_d <- (1/es_table$N_participants_implicit +
                                   (es_table$implicit_d)^2/2*es_table$N_participants_implicit) * 2*(1-cor_pairs)

#es_table$variance_implicit_d <- implicit_variancedrm

# Compute correction factor J (formula 4.22)
es_table$J <- 1 - (3/(4*(es_table$N_participants_implicit-1)-1))

# Compute Hedges' g (formula 4.23)
es_table$implicit_hedgesg <- es_table$J * es_table$implicit_d

# Compute variance of g (formula 4.24)
es_table$variance_implicit_g <- (es_table$J)^2 * es_table$variance_implicit_d

# Compute standard error of g (formula 4.25)
es_table$se_implicit_g <- sqrt(es_table$variance_implicit_g)

# r and Fisher's z
es_table$implicit_rs <- implicit_r
es_table$implicit_z_rs <- implicit_z_r

# Variances of r and Fisher's z
es_table$variance_implicit_rs <- ((1 - es_table$implicit_rs**2)**2)/(es_table$N_participants_implicit - 1)
es_table$variance_implicit_z_rs <- 1/(es_table$N_participants_implicit - 3)
es_table$se_implicit_z_rs <- sqrt(es_table$variance_implicit_z_rs)

### Awareness ES
# Create column for Cohen's d and Hedges' g
es_table$awareness_d <- awareness_cohensd
es_table$awareness_hedgesg <- awareness_hedgesg

# Create column for standard error of g
es_table$se_awareness_g <- awareness_hedgesg_se

# Create column for rs without NAs
es_table$awareness_rs <- awareness_rs
es_table$awareness_z_rs <- awareness_z_rs


### Create data frame without NAs
#es_table_aware <- es_table[-which(is.na(awareness_rs)),]

# Detect outliers in sample size
n_outliers <- es_table[which(es_table$N_participants_implicit == outlier(es_table$N_participants_implicit)),]

# remove sample size outliers
# es_table <- es_table %>%
#   filter(es_table$study != n_outliers$study)

# replace outlier sample sizes by 
replace_value <- sort(es_table$N_participants_implicit)[length(es_table$N_participants_implicit) - 1]
es_table[which(es_table$study == n_outliers$study),]$N_participants_implicit <- replace_value

#============================================================================================#
#================================= 2. Implicit meta-analysis =================================
#============================================================================================#
es_table <- es_table %>%
  filter(!is.na(implicit_z_rs))

#============================== 2.1. Compute implicit meta-analytic ES ======================
source("mlm.variance.distribution_function.R")

implicit_meta_es_r <- rma.mv(implicit_z_rs, 
                           variance_implicit_z_rs, 
                           random = list(~ 1 | studies_outcomes, 
                                         ~ 1 | study), 
                           tdist = TRUE, 
                           data = es_table,
                           method = "REML")


summary(implicit_meta_es_r)
mlm.variance.distribution(x = implicit_meta_es_r)



# Significance of levels 2 and 3
implicit_meta_es_r_l2removed <- rma.mv(implicit_z_rs, 
                             variance_implicit_z_rs, 
                             random = list(~ 1 | studies_outcomes, 
                                           ~ 1 | study), 
                             tdist = TRUE, 
                             data = es_table,
                             method = "REML",
                             sigma2 = c(0,NA))
implicit_meta_es_r_l3removed <- rma.mv(implicit_z_rs, 
                                      variance_implicit_z_rs, 
                                      random = list(~ 1 | studies_outcomes, 
                                                    ~ 1 | study), 
                                      tdist = TRUE, 
                                      data = es_table,
                                      method = "REML",
                                      sigma2 = c(NA,0))
anova.rma(implicit_meta_es_r, implicit_meta_es_r_l2removed)
anova.rma(implicit_meta_es_r, implicit_meta_es_r_l3removed)

# Plots
forest(implicit_meta_es_r, # generate untrimmed forest plot
       #sortvar = cor,
       STUDLAB = TRUE, #should study labels be printed?
       comb.fixed = FALSE, # plot fixed effect estimate?
       comb.random = TRUE, # plot random effect estimate
       print.tau2 = FALSE,
       digits.sd = 2,
       cex = 0.9,
       pooled.totals = TRUE
)

# Save as pdf
pdf(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/implicit_forest_plot_rs.pdf", width=16,height=14)
forest(implicit_meta_es_r, # generate untrimmed forest plot
       #sortvar = cor,
       STUDLAB = TRUE, #should study labels be printed?
       comb.fixed = FALSE, # plot fixed effect estimate?
       comb.random = TRUE, # plot random effect estimate
       print.tau2 = FALSE,
       digits.sd = 2,
       pooled.totals = TRUE
)

dev.off()

# Save as tiff
tiff(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/implicit_forest_plot_rs.tiff", width=16,height=14, units = "in", res = 300)
forest(implicit_meta_es_r, # generate untrimmed forest plot
       #sortvar = cor,
       STUDLAB = TRUE, #should study labels be printed?
       comb.fixed = FALSE, # plot fixed effect estimate?
       comb.random = TRUE, # plot random effect estimate
       print.tau2 = FALSE,
       digits.sd = 2,
       pooled.totals = TRUE
)

dev.off()

#======================= 2.2. Heterogeneity check for implicit model ======================

# Find outliers by comparing CIs
ind_cis <- data.frame(studies = paste("Study", 1:63),
                      low.ci = c(-0.61, 0.20, 0.60, 0.05, 0.17, -0.05, -0.30, -0.19, 0.35, -0.08, -0.42,
                                 0.06, 0.10, -0.16, 0.01, -0.19, 0.22, 0.62, -0.14, -0.31, 0.07, 0.02, -0.01,
                                 -0.60, -0.18, 0.45, -0.02, 0.63, -0.69, 0.12, -0.79, 0.66, 0.61, -0.01, -0.15,
                                 -0.04, -0.06, 0.68, -2.47, 0.13, -0.65, -0.20, 0.17, 0.12, 0.01, -0.06, -0.16,
                                 -0.18, 0.38, -1.04, -0.12, -1.05, 0.04, 0.55, 0.36, 0.80, 0.09, 0.06, -0.33, -0.62,
                                 0.17, 0.53, 0.21),
                      high.ci = c(0.34, 1.00, 1.34, 0.68, 0.80, 0.96, 1.18, 0.43, 0.98, 0.80, 0.42, 0.64,
                                  0.73, 0.76, 0.78, 0.79, 1.17, 1.57, 0.95, 0.52, 0.36, 1.20, 1.18, 0.58, 1.00, 1.29,
                                  0.81, 1.41, 0.10, 0.98, 0.07, 1.61, 1.56, 0.85, 0.71, 0.47, 0.47, 0.98, -2.20,
                                  1.08, 0.13, 0.81, 1.30, 1.25, 1.02, 0.95, 0.85, 0.83, 1.56, 0.15, 1.06,
                                  0.26, 1.35, 1.85, 1.66, 2.11, 0.68, 0.66, 1.43, 1.13, 1.77, 2.13, 1.60)
                      )

low.meta.ci <- implicit_meta_es_r$ci.lb
high.meta.ci <- implicit_meta_es_r$ci.ub

outlier_studies <- ind_cis %>%
  dplyr::filter(high.ci  < low.meta.ci) %>%
  dplyr::filter(low.ci > high.meta.ci)

# Influence analysis
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/influenceanalyses.html
labels <- es_table$study

level2vars <- numeric(length = length(es_table$study))
level2heterogeneity <- numeric(length = length(es_table$study))

for(indstudy in 1:length(es_table$study)) {
  removedstudy <- es_table$study[indstudy]
  datasubset <- es_table %>%
    filter(study != removedstudy)
  partialmodel <- rma.mv(implicit_z_rs, 
                         variance_implicit_z_rs, 
                         random = list(~ 1 | studies_outcomes, 
                                       ~ 1 | study), 
                         tdist = TRUE, 
                         data = datasubset,
                         method = "REML")
  level2vars[indstudy] <- partialmodel$sigma2[2]
  
  level2heterogeneity[indstudy] <- partialmodel$QE
}
names(level2vars) <- labels
names(level2heterogeneity) <- labels

plot(1:length(es_table$study),
     es_table$implicit_z_rs)
plot(1:length(es_table$study),
     es_table$variance_implicit_z_rs)

# Plot meta-analytic estimates with the leave-one-out method
plot(1:length(es_table$study),
     level2vars)
text(1:length(es_table$study),
     level2vars,
     labels = labels,
     cex = 0.6)

# Plot meta-analytic heterogeneity values with the leave-one-out method
plot(1:length(es_table$study),
     level2heterogeneity)
text(1:length(es_table$study),
     level2heterogeneity,
     labels = labels,
     cex = 0.6)

# # Remove outliers
# es_table_clean <- es_table[es_table$study %!in% as.character(implicit_r_outliers$Author),]

# remove outliers detected by influence and confidence interval
es_table_clean <- es_table %>%
  filter(es_table$study %!in% c("russell_driver_2005_exp2", "wood_simons_2019_exp2"))

# Fit model without outliers
implicit_meta_es_r_clean <- rma.mv(implicit_z_rs, 
                             variance_implicit_z_rs, 
                             random = list(~ 1 | studies_outcomes, 
                                           ~ 1 | study), 
                             tdist = TRUE, 
                             data = es_table_clean,
                             method = "REML")


summary(implicit_meta_es_r_clean)
mlm.variance.distribution(x = implicit_meta_es_r_clean)



# Significance of levels 2 and 3
implicit_meta_es_r_clean_l2removed <- rma.mv(implicit_z_rs, 
                                       variance_implicit_z_rs, 
                                       random = list(~ 1 | studies_outcomes, 
                                                     ~ 1 | study), 
                                       tdist = TRUE, 
                                       data = es_table_clean,
                                       method = "REML",
                                       sigma2 = c(0,NA))
implicit_meta_es_r_clean_l3removed <- rma.mv(implicit_z_rs, 
                                       variance_implicit_z_rs, 
                                       random = list(~ 1 | studies_outcomes, 
                                                     ~ 1 | study), 
                                       tdist = TRUE, 
                                       data = es_table_clean,
                                       method = "REML",
                                       sigma2 = c(NA,0))
anova.rma(implicit_meta_es_r_clean, implicit_meta_es_r_clean_l2removed)
anova.rma(implicit_meta_es_r_clean, implicit_meta_es_r_clean_l3removed)

# Plots
forest(implicit_meta_es_r_clean, # generate untrimmed forest plot
       #sortvar = cor,
       STUDLAB = TRUE, #should study labels be printed?
       comb.fixed = FALSE, # plot fixed effect estimate?
       comb.random = TRUE, # plot random effect estimate
       print.tau2 = FALSE,
       digits.sd = 2,
       cex = 0.9,
       pooled.totals = TRUE
)

# Save as pdf
pdf(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/implicit_forest_plot_rs_clean.pdf", width=16,height=14)
forest(implicit_meta_es_r_clean, # generate untrimmed forest plot
       #sortvar = cor,
       STUDLAB = TRUE, #should study labels be printed?
       comb.fixed = FALSE, # plot fixed effect estimate?
       comb.random = TRUE, # plot random effect estimate
       print.tau2 = FALSE,
       digits.sd = 2,
       pooled.totals = TRUE
)

dev.off()

# Save as tiff
tiff(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/implicit_forest_plot_rs_clean.tiff", width=16,height=14, units = "in", res = 300)
forest(implicit_meta_es_r_clean, # generate untrimmed forest plot
       #sortvar = cor,
       STUDLAB = TRUE, #should study labels be printed?
       comb.fixed = FALSE, # plot fixed effect estimate?
       comb.random = TRUE, # plot random effect estimate
       print.tau2 = FALSE,
       digits.sd = 2,
       pooled.totals = TRUE
)

dev.off()


#========================= 2.3. Publication bias for implicit ES ============================

implicit_meta_es_r_twolevel <- metacor(cor = es_table_clean$implicit_rs, # treatment effect (Hedge's g)
                                       n = es_table_clean$N_participants_implicit,
                                       data = es_table_clean,
                                       studlab = es_table_clean$study,
                                       comb.fixed = FALSE,
                                       comb.random = TRUE,
                                       sm = "ZCOR",
                                       method.tau = "SJ")

# Check assimetry with funnel plot without outliers
tiff(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/implicit_funnel_plot_rs.tiff", 
    width=16,height=14, units = "in", res = 100, type = c("cairo"))
par(mar = c(5,6,4,2))
funnel(x = implicit_meta_es_r_twolevel,
       xlab = "Correlation", 
       #main = "Funnel Plot of Implicit Effect Sizes",
       lwd = 1.5,
       cex = 1.5,
       cex.lab = 2,
       cex.main = 2,
       cex.axis = 2,
       contour.levels = c(0.95, 0.975, 0.99), 
       col.contour = c("darkblue","blue","lightblue")
)
legend(0.9, 0,
       legend = c("p < .05", "p < .025", "p < .01"),
       bty = "n",
       cex = 2,
       fill=c("darkblue","blue","lightblue"))
par(graphical_defaults)
dev.off()

# Tests for assymetry using egger's test
source("eggers.test_function.R")
#eggerstestresult <- eggers.test(x = implicit_meta_es_r_clean)
eggerstestresult <- eggers.test(x = implicit_meta_es_r_twolevel)

# Estimate bias with trim-and-fill method
# trimmed_implicit_meta_r <- trimfill(implicit_meta_es_r_clean,
#                                           left = TRUE,
#                                           ma.fixed = FALSE)
# trimmed_implicit_meta_r_clean <- trimfill(implicit_meta_es_r_clean,
#                                   left = TRUE,
#                                   ma.fixed = FALSE)

# funnel(x = trimmed_implicit_meta_r_clean,#trimmed_implicit_meta_r_clean,
#        xlab = "Correlation", 
#        contour.levels = c(0.95, 0.975, 0.99), 
#        col.contour = c("darkblue","blue","lightblue")
# )
# legend(0.6, 0,legend = c("p < .05", "p < .025", "p < .01"),
#        bty = "n",
#        fill=c("darkblue","blue","lightblue"))

# # Using p-curve - cannot use because several of the results are secondary analyses
# source("pcurve_function.R")
# pcurve(implicit_meta_es_r_clean,
#        )


#======================== 2.4. Moderation analysis of implicit ES ===========================

# Load function
source("subgroup.analysis.mixed.effects_function.R")

#=============== 2.4.1. Subgroup analysis for binary categorical variables ==================

implicit_meta_es_r_us_relevance <- rma.mv(implicit_z_rs, 
                             variance_implicit_z_rs, 
                             random = list(~ 1 | studies_outcomes, 
                                           ~ 1 | study), 
                             tdist = TRUE, 
                             data = es_table_clean,
                             method = "REML",
                             mods = ~ us_relevance)


# type of implicit measure - group sizes are too unbalanced
implicit_meta_es_r_implicit_measure <- rma.mv(implicit_z_rs,
                                         variance_implicit_z_rs,
                                         random = list(~ 1 | studies_outcomes,
                                                       ~ 1 | study),
                                         tdist = TRUE,
                                         data = es_table_clean,
                                         method = "REML",
                                         mods = ~ implicit_measure)

# Inattention paradigm
implicit_meta_es_r_inattention <- rma.mv(implicit_z_rs, 
                                          variance_implicit_z_rs, 
                                          random = list(~ 1 | studies_outcomes, 
                                                        ~ 1 | study), 
                                          tdist = TRUE, 
                                          data = es_table_clean,
                                          method = "REML",
                                          mods = ~ inattention)

implicit_meta_es_r_inattentionsubset <- rma.mv(implicit_z_rs, 
                                         variance_implicit_z_rs, 
                                         random = list(~ 1 | studies_outcomes, 
                                                       ~ 1 | study), 
                                         tdist = TRUE, 
                                         data = filter(es_table_clean, inattention == "yes"),
                                         method = "REML")

implicit_meta_es_r_noninattentionsubset <- rma.mv(implicit_z_rs, 
                                               variance_implicit_z_rs, 
                                               random = list(~ 1 | studies_outcomes, 
                                                             ~ 1 | study), 
                                               tdist = TRUE, 
                                               data = filter(es_table_clean, inattention == "no"),
                                               method = "REML")

# gray literature
implicit_meta_es_r_gray_literature <- rma.mv(implicit_z_rs, 
                                         variance_implicit_z_rs, 
                                         random = list(~ 1 | studies_outcomes, 
                                                       ~ 1 | study), 
                                         tdist = TRUE, 
                                         data = es_table_clean,
                                         method = "REML",
                                         mods = ~ gray_literature)



# mixed effects model for group assessment of awareness as a moderator
implicit_meta_es_r_group_awareness <- rma.mv(implicit_z_rs, 
                                         variance_implicit_z_rs, 
                                         random = list(~ 1 | studies_outcomes, 
                                                       ~ 1 | study), 
                                         tdist = TRUE, 
                                         data = es_table_clean,
                                         method = "REML",
                                         mods = ~ group_awareness)

# Mixed effects model for gestalt studies
implicit_meta_es_r_gestalt <- rma.mv(implicit_z_rs, 
                                             variance_implicit_z_rs, 
                                             random = list(~ 1 | studies_outcomes, 
                                                           ~ 1 | study), 
                                             tdist = TRUE, 
                                             data = es_table_clean,
                                             method = "REML",
                                             mods = ~ gestalt_study)

# implicit_meta_es_r_implicit_significance <- rma.mv(implicit_z_rs, 
#                                              variance_implicit_z_rs, 
#                                              random = list(~ 1 | studies_outcomes, 
#                                                            ~ 1 | study), 
#                                              tdist = TRUE, 
#                                              data = es_table,
#                                              method = "REML",
#                                              mods = ~ implicit_significance)

#==================== 2.4.2. compute metaregression for continuous variables ==========================

# number of trials for implicit processing
implicit_meta_es_r_N_trials_implicit <- rma.mv(implicit_z_rs,
                                              variance_implicit_z_rs,
                                              random = list(~ 1 | studies_outcomes,
                                                            ~ 1 | study),
                                              tdist = TRUE,
                                              data = es_table_clean,
                                              method = "REML",
                                              mods = ~ N_trials_implicit)

# Plot with meta package
implicit_meta_es_r_twolevel <- metacor(cor = es_table_clean$implicit_rs, # treatment effect (Hedge's g)
                                    n = es_table_clean$N_participants_implicit,
                                    data = es_table_clean,
                                    studlab = es_table_clean$study,
                                    comb.fixed = FALSE,
                                    comb.random = TRUE,
                                    sm = "ZCOR",
                                    method.tau = "SJ")

mod_implicit_n_trials_implicit <- metareg(~ N_trials_implicit,
                                          x = implicit_meta_es_r_twolevel)


tiff(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/mod_implicit_n_trials_implicit.tiff", 
    width=16,height=14, units = "in", res = 600, type = c("cairo"))
par(mar = c(5,6,4,2))
bubble(x = mod_implicit_n_trials_implicit,
       xlab = "N of trials",
       ylab = "Fisher's Z transformed correlation",
       col.line = "black",
       cex.lab = 3,
       cex.main = 4,
       cex.axis = 3,
       main = "Effect sizes by N of trials",
       studlab = FALSE)
par(graphical_defaults)
dev.off()

# number of participants for implicit processing
implicit_meta_es_r_N_participants_implicit <- rma.mv(implicit_z_rs,
                                               variance_implicit_z_rs,
                                               random = list(~ 1 | studies_outcomes,
                                                             ~ 1 | study),
                                               tdist = TRUE,
                                               data = es_table_clean,
                                               method = "REML",
                                               mods = ~ N_participants_implicit)


# Plot with meta package
mod_implicit_n_participants_implicit <- metareg(~ N_participants_implicit,
                                                x = implicit_meta_es_r_twolevel, 
)

tiff(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/mod_implicit_n_participants_implicit.tiff", 
     width=16,height=14, units = "in", res = 600, type = c("cairo"))
par(mar = c(5,6,4,2))
bubble(x = mod_implicit_n_participants_implicit,
       xlab = "N of participants",
       ylab = "Fisher's Z transformed correlation",
       col.line = "black",
       cex.lab = 3,
       cex.main = 4,
       cex.axis = 3,
       main = "Effect sizes by N of participants",
       studlab = FALSE)
par(graphical_defaults)
dev.off()

# Both plots within in a single figure
tiff(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/mod_implicit_n_parts_trials_implicit.tiff", 
     width=24,height=14, units = "in", res = 100, type = c("cairo"))
par(mfrow = c(1,2))
par(mar = c(5,6,4,4))
bubble(x = mod_implicit_n_trials_implicit,
       xlab = "No. of trials",
       ylab = "Fisher's Z transformed correlation",
       col.line = "black",
       cex.lab = 3,
       cex.main = 4,
       cex.axis = 3,
       studlab = FALSE)
title("A. Effect sizes by no. of trials",adj = 0, cex.main = 3)
bubble(x = mod_implicit_n_participants_implicit,
       xlab = "No. of participants",
       ylab = "Fisher's Z transformed correlation",
       col.line = "black",
       cex.lab = 3,
       cex.main = 4,
       cex.axis = 3,
       #main = "B. Effect sizes by N of participants",
       studlab = FALSE)
title("B. Effect sizes by no. of participants",adj = 0, cex.main = 3)
par(graphical_defaults)
dev.off()

#============================================================================================#
#================================ 3. Awareness meta-analysis =================================
#============================================================================================#

#============================== 3.1. Compute awareness meta-analytic ES ======================

#remove NAs and Infs
es_table_aware <- es_table %>%
  filter(es_table$awareness_rs != Inf) 
es_table_aware <- es_table_aware %>%
  filter(es_table_aware$awareness_rs != -Inf) 
# es_table_aware <- es_table_aware %>%
#   filter(!is.na(es_table_aware$awareness_rs))
es_table_aware <- es_table_aware %>%
  filter(es_table_aware$awareness_z_rs != Inf)
es_table_aware <- es_table_aware %>%
  filter(es_table_aware$awareness_z_rs != -Inf)

# N of studies removed due to Inf cors
# sum(is.na(es_table$awareness_rs)) #10
# sum(es_table[!is.na(es_table$awareness_z_rs),]$awareness_z_rs == Inf) # 3
# sum(es_table[!is.na(es_table$awareness_z_rs),]$awareness_z_rs == -Inf) #1

# Fit model
awareness_meta_es_r <- metacor(cor = es_table_aware$awareness_rs, #r
                              n = es_table_aware$N_participants_awareness,
                              data = es_table_aware,
                              studlab = es_table_aware$study,
                              comb.fixed = FALSE,
                              comb.random = TRUE,
                              sm = "ZCOR", # use Fisher's z instead of raw correlation
                              method.tau = "SJ")


summary(awareness_meta_es_r)

# Plots
pdf(file="awareness_forest_plot_rs.pdf", width=16,height=14)
forest(awareness_meta_es_r, # generate untrimmed forest plot
       STUDLAB = TRUE, #should study labels be printed?
       comb.fixed = FALSE, # plot fixed effect estimate?
       comb.random = TRUE, # plot random effect estimate
       print.tau2 = FALSE,
       digits.sd = 2,
       pooled.totals = TRUE
)

dev.off()

#======================= 3.2. Heterogeneity check for awareness model ======================
# Find outliers with function
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/detecting-outliers-influential-cases.html
spot.outliers.random<-function(data){
  data<-data
  Author<-data$studlab
  lowerci<-data$lower
  upperci<-data$upper
  m.outliers<-data.frame(Author,lowerci,upperci)
  te.lower<-data$lower.random
  te.upper<-data$upper.random
  dplyr::filter(m.outliers,upperci < te.lower)
  dplyr::filter(m.outliers,lowerci > te.upper)
}

awareness_r_outliers <- spot.outliers.random(awareness_meta_es_r) # 5 outliers

# add outliers by visual inspection
# moore & egeth exp 3
# schnuerch exps 1 and 2
visual_outliers <- c("moore_2003_exp3", "schnuerch_2016_exp1", "schnuerch_2016_exp2")

# Influence analysis
# https://raw.githubusercontent.com/MathiasHarrer/dmetar/master/R/influence.analysis.R

# Load function
source("influence_analysis_function.R")

pdf("Influence_analysis_awareness_r.pdf", height = 16, width = 16)
InfluenceAnalysis(x = awareness_meta_es_r,
                  random = TRUE)
dev.off()

# Remove outliers
es_table_aware_clean <- es_table_aware[es_table_aware$study %!in% as.character(awareness_r_outliers$Author),]
#es_table_aware_clean <- es_table_aware[es_table_aware$study %!in% c(as.character(awareness_r_outliers$Author), 
#                                                                    visual_outliers),]

# Fit model without outliers
awareness_meta_es_r_clean <- metacor(cor = es_table_aware_clean$awareness_rs, #r
                                    n = es_table_aware_clean$N_participants_awareness,
                                    data = es_table_aware_clean,
                                    studlab = es_table_aware_clean$study,
                                    comb.fixed = FALSE,
                                    comb.random = TRUE,
                                    sm = "ZCOR", # use Fisher's z instead of raw correlation
                                    method.tau = "SJ")

summary(awareness_meta_es_r_clean)

# Plots
pdf(file="awareness_forest_plot_rs_clean.pdf", width=16,height=14)
forest(awareness_meta_es_r_clean, # generate untrimmed forest plot
       sortvar = cor,
       STUDLAB = TRUE, #should study labels be printed?
       comb.fixed = FALSE, # plot fixed effect estimate?
       comb.random = TRUE, # plot random effect estimate
       print.tau2 = FALSE,
       digits.sd = 2,
       pooled.totals = TRUE
)
dev.off()

#========================= 3.3. Publication bias for awareness ES ==========================
# Check assimetry with funnel plot
funnel(x = awareness_meta_es_r_clean,
       xlab = "Correlation", 
       contour.levels = c(0.95, 0.975, 0.99), 
       col.contour = c("darkblue","blue","lightblue")
)
legend(-1.0, 0,legend = c("p < 0.05", "p<0.025", "p < 0.01"),
       bty = "n",
       fill=c("darkblue","blue","lightblue"))

# Tests for assymetry using egger's test
source("eggers.test_function.R")
eggerstestresult <- eggers.test(x = awareness_meta_es_r_clean)

# Estimate bias with trim-and-fill method
trimmed_awareness_meta_r_clean <- trimfill(awareness_meta_es_r_clean,
                                  left = FALSE,
                                  ma.fixed = FALSE)

funnel(trimmed_awareness_meta_r_clean,
       xlab = "Correlation")

# Using p-curve
source("pcurve_function.R")
pcurve(awareness_meta_es_r_clean)


#======================== 2.4. Moderation analysis of awareness ES ===========================

# Load function
source("subgroup.analysis.mixed.effects_function.R")

#====== 5.1. compute subgroup analysis for binary categorical variables ======

# unexpected stimulus presentation
mod_implicit_us_presentation <- update(implicit_meta_es, 
                                       byvar=es_table$us_presentation, 
                                       print.byvar=FALSE)
summary(mod_implicit_us_presentation)



# mixed effects model for inattention
pdf("subgroup_inattention_awareness_r_clean.pdf", height = 16, width = 16)
subgroup.analysis.mixed.effects(x = awareness_meta_es_r_clean,
                                subgroups = es_table_aware_clean$inattention)
dev.off()

# mixed effects model for group assessment of awareness
pdf("subgroup_groupaware_awareness_r_clean.pdf", height = 16, width = 16)
subgroup.analysis.mixed.effects(x = awareness_meta_es_r_clean,
                                subgroups = es_table_aware_clean$group_awareness)
dev.off()


# mixed effects model for objective/subjective measures of consciousness
# pdf("subgroup_awaremeasure_awareness_r_clean.pdf", height = 16, width = 16)
# subgroup.analysis.mixed.effects(x = awareness_meta_es_r_clean,
#                                 subgroups = es_table_aware_clean$awareness_objective)
# dev.off()


# unexpected stimulus delay type
# mod_awareness_r_clean_usdelay <- update(awareness_meta_es_r_clean, 
#                                      byvar=es_table_aware_clean$us_delay_type, 
#                                      print.byvar=FALSE)
# summary(mod_awareness_r_clean_usdelay)
# 
# mod_awareness_r_clean_usassessment <- update(awareness_meta_es_r_clean, 
#                                         byvar=es_table_aware_clean$us_assessment, 
#                                         print.byvar=FALSE)
# summary(mod_awareness_r_clean_usassessment)



# number of participants for awareness assessment
mod_awareness_n_participants_awareness <- metareg(~ N_participants_awareness,
                                                x = awareness_meta_es_r_clean, 
)
summary(mod_awareness_n_participants_awareness)
