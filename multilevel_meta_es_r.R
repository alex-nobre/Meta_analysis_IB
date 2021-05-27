
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
library(dmetar)

# Influence analysis
library(ggrepel)
library(grid)
library(gridExtra)

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
es_table$variance_awareness_z_rs <- 1/(es_table$N_participants_awareness - 3)
es_table$se_awareness_z_rs <- sqrt(es_table$variance_awareness_z_rs)


### Create data frame without NAs
#es_table_aware <- es_table[-which(is.na(awareness_rs)),]

# Detect outliers in sample size
n_outliers <- es_table[which(es_table$N_participants_implicit == outlier(es_table$N_participants_implicit)),]

# remove sample size outliers
# es_table <- es_table %>%
#   filter(es_table$study != n_outliers$study)

# replace outlier sample sizes by next smallest value
replace_value <- sort(es_table$N_participants_implicit)[length(es_table$N_participants_implicit) - 1]
es_table[which(es_table$study == n_outliers$study),]$N_participants_implicit <- replace_value

#============================================================================================#
#================================= 2. Implicit meta-analysis =================================
#============================================================================================#
es_table <- es_table %>%
  filter(!is.na(implicit_z_rs))

#============================== 2.1. Compute implicit meta-analytic ES ======================
#source("mlm.variance.distribution_function.R")

implicit_meta_es_r <- rma.mv(implicit_z_rs, 
                           variance_implicit_z_rs, 
                           # random = list(~ 1 | studies_outcomes, 
                           #               ~ 1 | study),
                           random = list(~ 1 | study / studies_outcomes), 
                           tdist = TRUE, 
                           data = es_table,
                           method = "REML")


summary(implicit_meta_es_r)
mlm.variance.distribution(x = implicit_meta_es_r)



# Significance of levels 2 and 3
implicit_meta_es_r_l2removed <- rma.mv(implicit_z_rs, 
                             variance_implicit_z_rs, 
                             # random = list(~ 1 | studies_outcomes, 
                             #               ~ 1 | study),
                             random = list(~ 1 | study / studies_outcomes),
                             tdist = TRUE, 
                             data = es_table,
                             method = "REML",
                             sigma2 = c(0,NA))
implicit_meta_es_r_l3removed <- rma.mv(implicit_z_rs, 
                                      variance_implicit_z_rs, 
                                      # random = list(~ 1 | studies_outcomes, 
                                      #               ~ 1 | study),
                                      random = list(~ 1 | study / studies_outcomes),
                                      data = es_table,
                                      method = "REML",
                                      sigma2 = c(NA,0))
anova.rma(implicit_meta_es_r, implicit_meta_es_r_l2removed)
anova.rma(implicit_meta_es_r, implicit_meta_es_r_l3removed)

# Plots
forest(implicit_meta_es_r, # generate untrimmed forest plot
       slab = es_table$studies_outcomes,
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
pdf(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/implicit_forest_plot_rs_revision.pdf", 
    width=16,height=14)
forest(implicit_meta_es_r, # generate untrimmed forest plot
       #sortvar = cor,
       slab = es_table$studies_outcomes,
       STUDLAB = TRUE, #should study labels be printed?
       comb.fixed = FALSE, # plot fixed effect estimate?
       comb.random = TRUE, # plot random effect estimate
       print.tau2 = FALSE,
       digits.sd = 2,
       pooled.totals = TRUE
)

dev.off()

# Save as tiff
tiff(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/implicit_forest_plot_rs_revision.tiff", 
     width=16,height=14, units = "in", res = 600)
forest(implicit_meta_es_r, # generate untrimmed forest plot
       #sortvar = cor,
       slab = es_table$studies_outcomes,
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

# Compute upper and lower bounds of CIs
# Source: https://stat.ethz.ch/pipermail/r-help/2011-July/284101.html
ind_cis_lb <- implicit_meta_es_r$yi - 
                    1.96*sqrt(implicit_meta_es_r$vi)
ind_cis_up <- implicit_meta_es_r$yi + 
                    1.96*sqrt(implicit_meta_es_r$vi)

ind_cis <- data.frame(study_index = paste("Study", 1:implicit_meta_es_r$k),
                      study_name = es_table$studies_outcomes,
                      low.ci = ind_cis_lb,
                      high.ci = ind_cis_up)

# Lower and upper boundaries of meta-analytic CI
low.meta.ci <- implicit_meta_es_r$ci.lb
high.meta.ci <- implicit_meta_es_r$ci.ub

outlier_studies <- ind_cis %>%
  dplyr::filter(high.ci < low.meta.ci |
                low.ci > high.meta.ci)


# Influence analysis
# https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/influenceanalyses.html
# labels <- es_table$study
# 
# # Vectors to store meta-analytic estimates and heterogeneity values
# level2vars <- numeric(length = length(es_table$study))
# level2heterogeneity <- numeric(length = length(es_table$study))
# 
# # Run the analysis once without each dataset
# for(indstudy in 1:length(es_table$study)) {
#   removedstudy <- es_table$study[indstudy]
#   datasubset <- es_table %>%
#     filter(study != removedstudy)
#   partialmodel <- rma.mv(implicit_z_rs, 
#                          variance_implicit_z_rs, 
#                          # random = list(~ 1 | studies_outcomes, 
#                          #               ~ 1 | study),
#                          random = list(~ 1 | study / studies_outcomes),
#                          tdist = TRUE, 
#                          data = datasubset,
#                          method = "REML")
#   level2vars[indstudy] <- partialmodel$beta[1] #partialmodel$sigma2[2]
#   
#   level2heterogeneity[indstudy] <- partialmodel$QE
# }
# 
# # Assign study names labels to values
# names(level2vars) <- labels
# names(level2heterogeneity) <- labels
# 
# plot(1:length(es_table$study),
#      es_table$implicit_z_rs,
#      pch = 16)
# plot(1:length(es_table$study),
#      es_table$variance_implicit_z_rs,
#      pch = 16)


# # Plot meta-analytic estimates with the leave-one-out method with study labels - save as pdf
# pdf(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/influence_analysis_implicit_rs_revision.pdf", 
#     width=16,height=14)
# plot(1:length(es_table$study),
#      level2vars,
#      pch = 16,
#      ylim = c(0.2, 0.4))
# text(1:length(es_table$study),
#      level2vars,
#      labels = labels,
#      cex = 0.6)
# dev.off()


# Cooks distance and dfbetas for rma.mv
cooksimplicit <- cooks.distance.rma.mv(implicit_meta_es_r)
plot(1:length(es_table$study), cooksimplicit)
text(1:length(es_table$study),
     cooksimplicit,
     labels = labels,
     cex = 0.6)

# DF betas
dfbetasimplicit <- dfbetas.rma.mv(implicit_meta_es_r)
plot(1:length(es_table$study), dfbetasimplicit[,1])
text(1:length(es_table$study),
     dfbetasimplicit[,1],
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
  #filter(es_table$studies_outcomes %!in% outlier_studies$study_name)
  filter(es_table$study %!in% c("russell_driver_2005_exp2", "wood_simons_2019_exp2"))

# Fit model without outliers
implicit_meta_es_r_clean <- rma.mv(implicit_z_rs, 
                             variance_implicit_z_rs, 
                             # random = list(~ 1 | studies_outcomes, 
                             #               ~ 1 | study),
                             random = list(~ 1 | study / studies_outcomes),
                             tdist = TRUE, 
                             data = es_table_clean,
                             method = "REML")


summary(implicit_meta_es_r_clean)
mlm.variance.distribution(x = implicit_meta_es_r_clean)



# Significance of levels 2 and 3
implicit_meta_es_r_clean_l2removed <- rma.mv(implicit_z_rs, 
                                       variance_implicit_z_rs, 
                                       # random = list(~ 1 | studies_outcomes, 
                                       #               ~ 1 | study),
                                       random = list(~ 1 | study / studies_outcomes),
                                       tdist = TRUE, 
                                       data = es_table_clean,
                                       method = "REML",
                                       sigma2 = c(0,NA))
implicit_meta_es_r_clean_l3removed <- rma.mv(implicit_z_rs, 
                                       variance_implicit_z_rs, 
                                       # random = list(~ 1 | studies_outcomes, 
                                       #               ~ 1 | study),
                                       random = list(~ 1 | study / studies_outcomes),
                                       tdist = TRUE, 
                                       data = es_table_clean,
                                       method = "REML",
                                       sigma2 = c(NA,0))

# Test if multileveling makes a difference
anova.rma(implicit_meta_es_r_clean, implicit_meta_es_r_clean_l2removed) #L3 makes a difference
anova.rma(implicit_meta_es_r_clean, implicit_meta_es_r_clean_l3removed) #L2 does not explain extra variance

# Plots
forest(implicit_meta_es_r_clean, # generate untrimmed forest plot
       slab = es_table_clean$studies_outcomes,
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
pdf(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/implicit_forest_plot_rs_clean_revision.pdf", 
    width=16,height=14)
forest(implicit_meta_es_r_clean, # generate untrimmed forest plot
       slab = es_table_clean$studies_outcomes,
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
tiff(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/implicit_forest_plot_rs_clean_revision.tiff", 
     width=16,height=14, units = "in", res = 300)
forest(implicit_meta_es_r_clean, # generate untrimmed forest plot
       slab = es_table_clean$studies_outcomes,
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

tiff(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/implicit_funnel_plot_rs_revision.tiff", 
     width=10,height=8.75, units = "in", res = 500, type = c("cairo"))
par(mar = c(5,6,4,2))
funnel.rma(implicit_meta_es_r_clean,
           xlab = "Correlation", 
           #main = "Funnel Plot of Implicit Effect Sizes",
           lwd = 1.5,
           cex = 1.5,
           cex.lab = 1.5,
           cex.main = 1.5,
           cex.axis = 1.5,
           level = c(0.95, 0.975, 0.99), 
           shade = c("lightblue","blue","darkblue"),
           back = "white"
)
legend(0.9, 0,
       legend = c("p < .05", "p < .025", "p < .01"),
       bty = "n",
       cex = 1.5,
       fill=c("lightblue","blue","darkblue"))
par(graphical_defaults)
dev.off()

# Tests for assymetry using egger's test
# source("eggers.test_function.R")
#eggerstestresult <- eggers.test(x = implicit_meta_es_r_clean)
#eggerstestresult <- eggers.test(x = implicit_meta_es_r_twolevel)

# Estimate bias with trim-and-fill method
# trimmed_implicit_meta_r <- trimfill(implicit_meta_es_r_clean,
#                                           left = TRUE,
#                                           ma.fixed = FALSE)
# trimmed_implicit_meta_r_clean <- trimfill(implicit_meta_es_r_clean,
#                                   left = TRUE,
#                                   ma.fixed = FALSE)


# )
# legend(0.6, 0,legend = c("p < .05", "p < .025", "p < .01"),
#        bty = "n",
#        fill=c("darkblue","blue","lightblue"))

#        )

# Rank test
ranktest(implicit_meta_es_r_clean)


implicit_meta_es_r_twolevel <- metacor(cor = es_table_clean$implicit_rs, # treatment effect (Hedge's g)
                                       n = es_table_clean$N_participants_implicit,
                                       data = es_table_clean,
                                       studlab = es_table_clean$study,
                                       comb.fixed = FALSE,
                                       comb.random = TRUE,
                                       sm = "ZCOR",
                                       method.tau = "SJ")

implicit_meta_es_r_twolevel <- rma.uni(implicit_z_rs, 
                             variance_implicit_z_rs, 
                             # random = list(~ 1 | studies_outcomes, 
                             #               ~ 1 | study),
                             #random = list(~ 1 | studies_outcomes), 
                             test = "knha",
                             data = es_table_clean,
                             method = "REML")

trimmed_implicit_meta_r_clean <- trimfill(implicit_meta_es_r_twolevel,
                                          test = "knha")
funnel(x = trimmed_implicit_meta_r_clean,#trimmed_implicit_meta_r_clean,
       xlab = "Correlation")
       #contour.levels = c(0.95, 0.975, 0.99),
       #col.contour = c("darkblue","blue","lightblue"))

#======================== 2.4. Moderator analysis of implicit ES ===========================

#=============== 2.4.1. Subgroup analysis for binary categorical variables ==================
implicit_meta_es_r_us_relevance <- rma.mv(implicit_z_rs, 
                             variance_implicit_z_rs, 
                             # random = list(~ 1 | studies_outcomes, 
                             #               ~ 1 | study),
                             random = list(~ 1 | study / studies_outcomes),
                             tdist = TRUE, 
                             data = es_table_clean,
                             method = "REML",
                             mods = ~ us_relevance)


# type of implicit measure - group sizes are too unbalanced
implicit_meta_es_r_implicit_measure <- rma.mv(implicit_z_rs,
                                         variance_implicit_z_rs,
                                         # random = list(~ 1 | studies_outcomes, 
                                         #               ~ 1 | study),
                                         random = list(~ 1 | study / studies_outcomes),
                                         tdist = TRUE,
                                         data = es_table_clean,
                                         method = "REML",
                                         mods = ~ implicit_measure)

# Inattention paradigm
implicit_meta_es_r_inattention <- rma.mv(implicit_z_rs, 
                                          variance_implicit_z_rs, 
                                         # random = list(~ 1 | studies_outcomes, 
                                         #               ~ 1 | study),
                                         random = list(~ 1 | study / studies_outcomes),
                                          tdist = TRUE, 
                                          data = es_table_clean,
                                          method = "REML",
                                          mods = ~ inattention)

implicit_meta_es_r_inattentionsubset <- rma.mv(implicit_z_rs, 
                                         variance_implicit_z_rs, 
                                         # random = list(~ 1 | studies_outcomes, 
                                         #               ~ 1 | study),
                                         random = list(~ 1 | study / studies_outcomes),
                                         tdist = TRUE, 
                                         data = filter(es_table_clean, inattention == "yes"),
                                         method = "REML")

implicit_meta_es_r_noninattentionsubset <- rma.mv(implicit_z_rs, 
                                               variance_implicit_z_rs, 
                                               # random = list(~ 1 | studies_outcomes, 
                                               #               ~ 1 | study),
                                               random = list(~ 1 | study / studies_outcomes), 
                                               tdist = TRUE, 
                                               data = filter(es_table_clean, inattention == "no"),
                                               method = "REML")

# gray literature
implicit_meta_es_r_gray_literature <- rma.mv(implicit_z_rs, 
                                         variance_implicit_z_rs, 
                                         # random = list(~ 1 | studies_outcomes, 
                                         #               ~ 1 | study),
                                         random = list(~ 1 | study / studies_outcomes),
                                         tdist = TRUE, 
                                         data = es_table_clean,
                                         method = "REML",
                                         mods = ~ gray_literature)



# mixed effects model for group assessment of awareness as a moderator
implicit_meta_es_r_group_awareness <- rma.mv(implicit_z_rs, 
                                         variance_implicit_z_rs, 
                                         # random = list(~ 1 | studies_outcomes, 
                                         #               ~ 1 | study),
                                         random = list(~ 1 | study / studies_outcomes),
                                         tdist = TRUE, 
                                         data = es_table_clean,
                                         method = "REML",
                                         mods = ~ group_awareness)

implicit_meta_es_r_groupsubset <- rma.mv(implicit_z_rs, 
                                             variance_implicit_z_rs, 
                                             # random = list(~ 1 | studies_outcomes, 
                                             #               ~ 1 | study),
                                             random = list(~ 1 | study / studies_outcomes),
                                             tdist = TRUE, 
                                             data = filter(es_table_clean, 
                                                           group_awareness == "yes"),
                                             method = "REML")

implicit_meta_es_r_nogroupsubset <- rma.mv(implicit_z_rs, 
                                         variance_implicit_z_rs, 
                                         # random = list(~ 1 | studies_outcomes, 
                                         #               ~ 1 | study),
                                         random = list(~ 1 | study / studies_outcomes),
                                         tdist = TRUE, 
                                         data = filter(es_table_clean, 
                                                       group_awareness == "no"),
                                         method = "REML")

# Mixed effects model for gestalt studies
implicit_meta_es_r_gestalt <- rma.mv(implicit_z_rs, 
                                             variance_implicit_z_rs, 
                                     # random = list(~ 1 | studies_outcomes, 
                                     #               ~ 1 | study),
                                     random = list(~ 1 | study / studies_outcomes),
                                             tdist = TRUE, 
                                             data = es_table_clean,
                                             method = "REML",
                                             mods = ~ gestalt_study)

# es_assoc_table <- es_table_clean %>%
#   dplyr::select(studies_outcomes, inattention, gestalt_study, group_awareness)


# Static vs dynamic
implicit_meta_es_r_staticsubset <- rma.mv(implicit_z_rs, 
                                     variance_implicit_z_rs, 
                                     # random = list(~ 1 | studies_outcomes, 
                                     #               ~ 1 | study),
                                     random = list(~ 1 | study / studies_outcomes),
                                     tdist = TRUE, 
                                     data = filter(es_table_clean, static_dynamic == "static"),
                                     method = "REML")

implicit_meta_es_r_dynamicsubset <- rma.mv(implicit_z_rs, 
                                          variance_implicit_z_rs, 
                                          # random = list(~ 1 | studies_outcomes, 
                                          #               ~ 1 | study),
                                          random = list(~ 1 | study / studies_outcomes),
                                          tdist = TRUE, 
                                          data = filter(es_table_clean, static_dynamic == "dynamic"),
                                          method = "REML")

  
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
                                              # random = list(~ 1 | studies_outcomes, 
                                              #               ~ 1 | study),
                                              random = list(~ 1 | study / studies_outcomes),
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


# tiff(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/mod_implicit_n_trials_implicit.tiff", 
#     width=16,height=14, units = "in", res = 600, type = c("cairo"))
# par(mar = c(5,6,4,2))
# bubble(x = mod_implicit_n_trials_implicit,
#        xlab = "N of trials",
#        ylab = "Fisher's Z transformed correlation",
#        col.line = "black",
#        cex.lab = 3,
#        cex.main = 4,
#        cex.axis = 3,
#        main = "Effect sizes by N of trials",
#        studlab = FALSE)
# par(graphical_defaults)
# dev.off()

# number of participants for implicit processing
implicit_meta_es_r_N_participants_implicit <- rma.mv(implicit_z_rs,
                                               variance_implicit_z_rs,
                                               # random = list(~ 1 | studies_outcomes, 
                                               #               ~ 1 | study),
                                               random = list(~ 1 | study / studies_outcomes),
                                               tdist = TRUE,
                                               data = es_table_clean,
                                               method = "REML",
                                               mods = ~ N_participants_implicit)


# Plot with meta package
mod_implicit_n_participants_implicit <- metareg(~ N_participants_implicit,
                                                x = implicit_meta_es_r_twolevel, 
)

# tiff(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/mod_implicit_n_participants_implicit.tiff", 
#      width=16,height=14, units = "in", res = 600, type = c("cairo"))
# par(mar = c(5,6,4,2))
# bubble(x = mod_implicit_n_participants_implicit,
#        xlab = "N of participants",
#        ylab = "Fisher's Z transformed correlation",
#        col.line = "black",
#        cex.lab = 3,
#        cex.main = 4,
#        cex.axis = 3,
#        main = "Effect sizes by N of participants",
#        studlab = FALSE)
# par(graphical_defaults)
# dev.off()

# Vector with colors
significance_colors <- ifelse(implicit_significance == "yes",
                               "black",
                               "grey")

# Both plots within in a single figure
tiff(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/mod_implicit_n_parts_trials_implicit_revision.tiff", 
     width=16,height=10, units = "in", res = 500, type = c("cairo"))
par(mfrow = c(1,2))
par(mar = c(5,6,4,4))
bubble(x = mod_implicit_n_trials_implicit,
       xlab = "Number of trials",
       ylab = "Fisher's Z-transformed correlation",
       col.line = "black",
       cex.lab = 2,
       #cex.main = 4,
       cex.axis = 1.5,
       studlab = FALSE,
       pch = 16,
       col = significance_colors)
title("A. Effect sizes by number of trials",adj = 0, cex.main = 2)
bubble(x = mod_implicit_n_participants_implicit,
       xlab = "Number of participants",
       ylab = "Fisher's Z-transformed correlation",
       col.line = "black",
       cex.lab = 2,
       #cex.main = 4,
       cex.axis = 1.5,
       #main = "B. Effect sizes by N of participants",
       studlab = FALSE,
       pch = 16,
       col = significance_colors)
title("B. Effect sizes by number of participants",adj = 0, cex.main = 2)
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

# Keep only studies with group assessment of awareness for global analysis
es_table_aware <- es_table_aware %>%
  filter(group_awareness == "yes")

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
pdf(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/awareness_forest_plot_rs_revision.pdf", 
    width=16,height=14)
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

#awareness_r_outliers <- spot.outliers.random(awareness_meta_es_r) # 5 outliers
awareness_r_outliers <- c()

# add outliers by visual inspection
# moore & egeth exp 3
# schnuerch exps 1 and 2
visual_outliers <- c("moore_2003_exp3", "schnuerch_2016_exp1", "schnuerch_2016_exp2")

# Influence analysis
# https://raw.githubusercontent.com/MathiasHarrer/dmetar/master/R/influence.analysis.R

# Load function
#source("influence_analysis_function.R")

pdf("C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/Influence_analysis_awareness_r_revision.pdf", height = 16, width = 16)
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
# Not applicable

#======================== 2.4. Moderator analysis of awareness ES ===========================

#====== 5.1. compute subgroup analysis for binary categorical variables ======

# unexpected stimulus presentation
mod_awareness_meta_es_r_clean_usrelevance <- update(awareness_meta_es_r_clean, 
                                       byvar=es_table_aware_clean$us_relevance, 
                                       print.byvar=FALSE)

summary(mod_awareness_meta_es_r_clean_usrelevance)



# mixed effects model for inattention
pdf("subgroup_inattention_awareness_r_clean.pdf", height = 16, width = 16)
subgroup.analysis.mixed.effects(x = awareness_meta_es_r_clean,
                                subgroups = es_table_aware_clean$inattention)
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
