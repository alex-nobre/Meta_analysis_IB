
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

# Create table for awareness ES without NAs
#es_table_aware <- es_table[-which(is.na(awareness_rs)),]

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

# r
es_table$implicit_rs <- implicit_r
es_table$implicit_z_rs <- implicit_z_r

# # Categorize experiments as inattention paradigms or not; 0 = no, 1 = yes
# es_table$inattention_paradigm <- c(rep(0, 21), #ariga_2007_exp2 to most_2005_exp1to7pooled
#                                   rep(1,4), #razpurker-apfeld and pratt, 2008
#                                   0, #richards_2012_tracking
#                                   rep(1, 10), #russel_driver_2005
#                                   rep(0,2) #schunerch_2016
#                                   ) %>%
#   as.factor()
# 
# # Categorize experiments as group assessment of awareness or not; 0 = no, 1 = yes
# es_table$group_aware_assess <- c(1, #ariga_2007_exp2
#                                  rep(0, 6), #beanland_pammer_2010_exp1A_fixating to gabay_2012_exp2
#                                  rep(1,4), #lo_yeh_2008_exp1_200ms
#                                  rep(0, 5), #mack_and_rock_2000
#                                  rep(1,9),  # moore_egeth_1997_exp1 to razpurker-apfeld and pratt, 2008
#                                  0, #richards_2012_tracking
#                                  rep(1, 10), #russsel_driver_2005
#                                  rep(0,2) #schunerch_2016
#                                  ) %>%
#   as.factor()

# Create column for Cohen's d and Hedges' g
es_table$awareness_d <- awareness_cohensd
es_table$awareness_hedgesg <- awareness_hedgesg

# Create column for standard error of g
es_table$se_awareness_g <- awareness_hedgesg_se

# Create column for rs without NAs
es_table$awareness_rs <- awareness_rs
es_table$awareness_z_rs <- awareness_z_rs


#remove infs
# es_table_aware <- es_table_aware %>%
#   filter(es_table_aware$awareness_z_rs != Inf)

### Create data frame without NAs
#es_table_aware <- es_table[-which(is.na(awareness_rs)),]

# Detect outliers in sample size
n_outliers <- es_table[which(es_table$N_participants_implicit == outlier(es_table$N_participants_implicit)),]

# remove outliers
es_table <- es_table %>%
  filter(es_table$study != n_outliers$study)

#============================================================================================#
#================================= 2. Implicit meta-analysis =================================
#============================================================================================#
es_table <- es_table %>%
  filter(!is.na(implicit_z_rs))

# es_table <- es_table %>%
#   filter(study %!in% allawarestudies)
#============================== 2.1. Compute implicit meta-analytic ES ======================
implicit_meta_es_r <- metacor(cor = es_table$implicit_rs, # r
                              n = es_table$N_participants_implicit,
                              data = es_table,
                              studlab = es_table$study,
                              comb.fixed = FALSE,
                              comb.random = TRUE,
                              sm = "ZCOR", # use Fisher's z instead of raw correlation
                              method.tau = "SJ")

summary(implicit_meta_es_r)

# Plots
pdf(file="implicit_forest_plot_rs.pdf", width=16,height=14)
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

# Png format
png(file="implicit_forest_plot_rs.png", width=16,height=14, units = "in", res = 300)
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

implicit_r_outliers <- spot.outliers.random(implicit_meta_es_r) # 6 outliers

# Influence analysis
# https://raw.githubusercontent.com/MathiasHarrer/dmetar/master/R/influence.analysis.R

# Load function
source("influence_analysis_function.R")

pdf("Influence_analysis_implicit_r.pdf", height = 16, width = 16)
InfluenceAnalysis(x = implicit_meta_es_r,
                  random = TRUE)
dev.off()


# Remove outliers
es_table_clean <- es_table[es_table$study %!in% as.character(implicit_r_outliers$Author),]

# Fit model without outliers
implicit_meta_es_r_clean <- metacor(cor = es_table_clean$implicit_rs, # treatment effect (Hedge's g)
                              n = es_table_clean$N_participants_implicit,
                              data = es_table_clean,
                              studlab = es_table_clean$study,
                              comb.fixed = FALSE,
                              comb.random = TRUE,
                              sm = "ZCOR",
                              method.tau = "SJ")

summary(implicit_meta_es_r_clean)

#========================= 2.3. Publication bias for implicit ES ============================
# Check assimetry with funnel plot without outliers
png(file="implicit_funnel_plot_rs.png", width=16,height=14, units = "in", res = 300, type = c("cairo"))
funnel(x = implicit_meta_es_r,#implicit_meta_es_r_clean,
       xlab = "Correlation", 
       main = "Funnel Plot of Implicit Effect Sizes",
       cex.lab = 2,
       cex.main = 2,
       cex.axis = 2,
       contour.levels = c(0.95, 0.975, 0.99), 
       col.contour = c("darkblue","blue","lightblue")
)
legend(0.9, 0,legend = c("p < .05", "p < .025", "p < .01"),
       bty = "n",
       cex = 2,
       fill=c("darkblue","blue","lightblue"))
dev.off()

# Tests for assymetry using egger's test
source("eggers.test_function.R")
#eggerstestresult <- eggers.test(x = implicit_meta_es_r_clean)
eggerstestresult <- eggers.test(x = implicit_meta_es_r)

# Estimate bias with trim-and-fill method
trimmed_implicit_meta_r <- trimfill(implicit_meta_es_r,
                                          left = TRUE,
                                          ma.fixed = FALSE)
# trimmed_implicit_meta_r_clean <- trimfill(implicit_meta_es_r_clean,
#                                   left = TRUE,
#                                   ma.fixed = FALSE)

funnel(x = trimmed_implicit_meta_r,#trimmed_implicit_meta_r_clean,
       xlab = "Correlation", 
       contour.levels = c(0.95, 0.975, 0.99), 
       col.contour = c("darkblue","blue","lightblue")
)
legend(0.6, 0,legend = c("p < .05", "p < .025", "p < .01"),
       bty = "n",
       fill=c("darkblue","blue","lightblue"))

# Using p-curve
source("pcurve_function.R")
pcurve(implicit_meta_es_r_clean,
       )


#======================== 2.4. Moderation analysis of implicit ES ===========================

# Load function
source("subgroup.analysis.mixed.effects_function.R")

#=============== 2.4.1. Subgroup analysis for binary categorical variables ==================

# mixed effects model for group assessment of awareness as a moderator
pdf("subgroup_usrelevance_implicit_r_clean.pdf", height = 16, width = 16)
subgroup.analysis.mixed.effects(x = implicit_meta_es_r_clean,
                                subgroups = es_table_clean$us_relevance)
dev.off()

mod_implicit_r_us_relevance <- update(implicit_meta_es_r_clean,
                                             byvar=es_table_clean$us_relevance,
                                             print.byvar=FALSE)


# type of implicit measure - group sizes are too unbalanced
mod_implicit_type_implicit_measure <- update(implicit_meta_es_r_clean, 
                                              byvar=es_table_clean$implicit_measure, 
                                              print.byvar=FALSE)
# summary(mod_implicit_type_implicit_measure)

subgroup.analysis.mixed.effects(x = implicit_meta_es_r_clean,
                                subgroups = es_table_clean$inattention)

mod_implicit_r_clean_inattention <- update(implicit_meta_es_r_clean,
                                               byvar=es_table_clean$inattention,
                                               print.byvar=TRUE)

subgroup_implicit_r_clean_inattention <- metacor(cor = es_table_clean$implicit_rs,
                                               studlab = es_table_clean$study,
                                               n = es_table_clean$N_participants_implicit,
                                               subset = es_table_clean$inattention == "yes",
                                               comb.random = TRUE,
                                               sm = "ZCOR",
                                               method.tau = "SJ")


# gray literature
mod_implicit_r_clean_gray_literature <- update(implicit_meta_es_r_clean, 
                                       byvar=es_table_clean$gray_literature, 
                                       print.byvar=FALSE)
summary(mod_implicit_r_clean_gray_literature)

# mixed effects model for measure as moderator
pdf("subgroup_implicitmeasure_implicit_r_clean.pdf", height = 16, width = 16)
subgroup.analysis.mixed.effects(x = implicit_meta_es_r_clean,
                                subgroups = es_table_clean$implicit_measure)
dev.off()


# mixed effects model for group assessment of awareness as a moderator
pdf("subgroup_groupaware_implicit_r_clean.pdf", width=16,height=14, units = "in", res = 300, type = c("cairo"))
subgroup.analysis.mixed.effects(x = implicit_meta_es_r_clean,
                                subgroups = es_table_clean$group_awareness)
dev.off()

# mixed effects model for group assessment of awareness as a moderator
pdf("subgroup_significance_implicit_r_clean.pdf", height = 16, width = 16)
subgroup.analysis.mixed.effects(x = implicit_meta_es_r_clean,
                                subgroups = es_table_clean$implicit_significance)
dev.off()

#==================== 2.4.2. compute metaregression for continuous variables ==========================

# number of trials for implicit processing
mod_implicit_n_trials_implicit <- metareg(~ N_trials_implicit,
                                          x = implicit_meta_es_r_clean)

summary(mod_implicit_n_trials_implicit)

png(file="mod_implicit_n_trials_implicit.png", width=16,height=14, units = "in", res = 300, type = c("cairo"))
bubble(x = mod_implicit_n_trials_implicit,
       xlab = "N of trials",
       ylab = "Fisher's Z transformed correlation",
       col.line = "black",
       cex.lab = 3,
       cex.main = 4,
       cex.axis = 3,
       main = "Effect sizes by N of trials for implicit assessment",
       studlab = FALSE)
dev.off()

# number of participants for implicit processing
# mod_implicit_N_participants_implicit <- metareg(implicit_meta_es_r_clean, 
#                                                 es_table_clean$N_participants_implicit)
mod_implicit_n_participants_implicit <- metareg(~ N_participants_implicit,
                                                x = implicit_meta_es_r_clean, 
                                                )
summary(mod_implicit_n_participants_implicit)

png(file="mod_implicit_n_participants_implicit.png", width=16,height=14, units = "in", res = 300, type = c("cairo"))
bubble(x = mod_implicit_n_participants_implicit,
       xlab = "N of participants",
       ylab = "Fisher's Z transformed correlation",
       col.line = "black",
       cex.lab = 4,
       cex.main = 5,
       cex.axis = 4,
       main = "Effect sizes by N of participants for implicit assessment",
       studlab = FALSE)
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
