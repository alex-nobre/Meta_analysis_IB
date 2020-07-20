
#=========================================================================================================================================#
#==================================================== Meta-analysis of implicit effects===================================================#
# Source the script for computation of effect sizes from paper data and 
# Run meta-analysis of implicit effects outputting meta-analytic effect size and test, and heterogeneity test
# Builds forest plot to visualize confidence intervals and heterogeneity
# Build funnel plot to check for publication bias and run trim-and-fill method
#=========================================================================================================================================#

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
# Create data frame
source("create_es_data_table.R")

## Estimate correlation between measures (conditions) 
##to compute Cohen's d for variance from two studies:

# Schnuerch et al. (2016)
# Razpurker-apfeld and Pratt (2008)
# Beanland and Pammer 1A (2010)

#cor_pairs <- 0.93
source("compute_correlation_between_conditions.R")

# Create vectors with effect sizes for implicit processing and awareness
source("calculate_implicit_effect_sizes.R")
source("calculate_awareness_effect_sizes.R")


# Add columns with effect sizes
# replace ds by computed cohens ds
#es_table$implicit_d <- implicit_cohensd
es_table$implicit_d <- implicit_cohensdrm
es_table$awareness_d <- awareness_cohensd

## Create variables to compute Hedges' g (formulas from Borestein's Introduction to Meta-Analysis, 2009) 
## for both implicit and awareness effect sizes

### Implicit ES

# # Compute variance of d (formula 4.28)
es_table$variance_implicit_d <- (1/es_table$N_per_group +
                                   (es_table$implicit_d)^2/2*es_table$N_per_group) * 2*(1-cor_pairs)

#es_table$variance_implicit_d <- implicit_variancedrm

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

# Create column for rs
es_table$implicit_rs <- implicit_r
es_table$awareness_rs <- awareness_rs

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

# Detect outliers in sample size
n_outliers <- es_table[which(es_table$N_per_group == outlier(es_table$N_per_group)),]

# remove outliers
es_table <- es_table %>%
  filter(es_table$study != n_outliers$study)


es_table_2 <- es_table %>%
  filter(!is.na(es_table$awareness_rs))
#============================================================================================#
#========================= 2. Fit meta-analytic model for implicit ES ========================
#============================================================================================#

## Build meta analytic effect model
implicit_meta_es <- metagen(TE = es_table$implicit_hedgesg, # treatment effect (Hedge's g)
                            seTE = es_table$se_implicit_g, #standard error of treatment,
                            studlab = es_table$study,
                            comb.fixed = FALSE,
                            comb.random = TRUE,
                            title = "Implicit effect sizes",
                            method.tau = "SJ",
                            hakn = TRUE,
                            prediction=TRUE,
                            sm="SMD"#,
                            #exclude = c(40:54), # Rashal et al. (2017) and Kimchi (2004)
                            #exclude = which(es_table$study=="wood_simons_2019_exp2"),
)




summary(implicit_meta_es)


# Plots
pdf(file="implicit_forest_plot.pdf", width=16,height=14)
forest(implicit_meta_es, # generate untrimmed forest plot
       sortvar = TE,
       STUDLAB = TRUE, #should study labels be printed?
       comb.fixed = FALSE, # plot fixed effect estimate?
       comb.random = TRUE, # plot random effect estimate
       print.tau2 = FALSE,
       digits.sd = 2,
       pooled.totals = TRUE
)

dev.off()

#=============================== 3 level model==========================
### Create vectors for columns
study_names_3level <- c("ariga_2007", 
                        "beanland_pammer_2010_exp1A", 
                        "beanland_pammer_2010_exp1A", 
                        "beanland_pammer_2010_exp2", 
                        "beanland_pammer_2010_exp2", 
                        "gabay_2012_exp1",
                        "gabay_2012_exp2", 
                        "lo_yeh_2008_exp1_200ms", 
                        "lo_yeh_2008_exp1_500ms", 
                        "lo_yeh_2008_exp2_200ms",
                        "lo_yeh_2008_exp2_500ms",
                        "mack_and_rock_2000_exp1",
                        "mack_and_rock_2000_exp2",
                        "mack_and_rock_2000_exp3",
                        "mack_and_rock_2000_exp4",
                        "mack_and_rock_2000_exp5",
                        "moore_egeth_1997_exp1", 
                        "moore_egeth_1997_exp3", 
                        "moore_2003_exp3",
                        "moore_2004", 
                        "most_2005_exp1to7pooled", 
                        "razpurker_pratt_2008_columns_rows", 
                        "razpurker_pratt_2008_columns_rows", 
                        "razpurker_pratt_2008_triangle_arrow",
                        "razpurker_pratt_2008_triangle_arrow", 
                        #"richards_2012_tracking", 
                        "russell_driver_2005_exp1", 
                        "russell_driver_2005_exp1", 
                        "russell_driver_2005_exp2",
                        "russell_driver_2005_exp2", 
                        "russell_driver_2005_exp3", 
                        "russell_driver_2005_exp3",
                        "russell_driver_2005_exp4a", 
                        "russell_driver_2005_exp4b", 
                        "russell_driver_2005_exp5",
                        "russell_driver_2005_exp5", 
                        #"shafto_pitts_2015",
                        "schnuerch_2016_exp1",
                        "schnuerch_2016_exp2",
                        "wood_simons_2019_exp1",
                        "rashal_2017_exp1",
                        "rashal_2017_exp2",
                        "rashal_2017_exp3",
                        "rashal_2017_exp4",
                        "rashal_2017_exp4",
                        "rashal_2017_exp5",
                        "rashal_2017_exp5",
                        "rashal_2017_exp6",
                        "rashal_2017_exp6",
                        "kimchi_2004_exp_1_column_row_color",
                        "kimchi_2004_exp_1_triangle_arrow_color",
                        "kimchi_2004_exp_1_triangle_arrow",
                        "kimchi_2004_exp_2_square_cross_color",
                        "kimchi_2004_exp_2_square_cross",
                        "kimchi_2004_exp_2_square_cross"
)

# study_effects_3level <- c("ariga_2007_exp2", 
#                         "beanland_pammer_2010_exp1A", 
#                         "beanland_pammer_2010_exp1A", 
#                         "beanland_pammer_2010_exp2", 
#                         "beanland_pammer_2010_exp2", 
#                         "gabay_2012_exp1",
#                         "gabay_2012_exp2", 
#                         "lo_yeh_2008_exp1_200ms", 
#                         "lo_yeh_2008_exp1_500ms", 
#                         "lo_yeh_2008_exp2_200ms",
#                         "lo_yeh_2008_exp2_500ms",
#                         "mack_and_rock_2000_exp1",
#                         "mack_and_rock_2000_exp2",
#                         "mack_and_rock_2000_exp3",
#                         "mack_and_rock_2000_exp4",
#                         "mack_and_rock_2000_exp5",
#                         "moore_egeth_1997_exp1", 
#                         "moore_egeth_1997_exp3", 
#                         "moore_2003_exp3",
#                         "moore_2004", 
#                         "most_2005_exp1to7pooled", 
#                         "razpurker_pratt_2008_columns_rows_rt", 
#                         "razpurker_pratt_2008_columns_rows_acc", 
#                         "razpurker_pratt_2008_triangle_arrow_rt",
#                         "razpurker_pratt_2008_triangle_arrow_acc", 
#                         #"richards_2012_tracking", 
#                         "russell_driver_2005_exp1_acc", 
#                         "russell_driver_2005_exp1_rt", 
#                         "russell_driver_2005_exp2_acc",
#                         "russell_driver_2005_exp2_rt", 
#                         "russell_driver_2005_exp3_acc", 
#                         "russell_driver_2005_exp3_rt",
#                         "russell_driver_2005_exp4a_acc", 
#                         "russell_driver_2005_exp4b_acc", 
#                         "russell_driver_2005_exp5_acc",
#                         "russell_driver_2005_exp5_rt", 
#                         #"shafto_pitts_2015",
#                         "schnuerch_2016_exp1",
#                         "schnuerch_2016_exp2",
#                         "wood_simons_2019_exp1",
#                         "wood_simons_2019_exp2",
#                         "rashal_2017_exp1_RT",
#                         "rashal_2017_exp2_acc",
#                         "rashal_2017_exp3_RT",
#                         "rashal_2017_exp4_acc",
#                         "rashal_2017_exp4_RT",
#                         "rashal_2017_exp5_acc",
#                         "rashal_2017_exp5_RT",
#                         "rashal_2017_exp6_acc",
#                         "rashal_2017_exp6_RT",
#                         "kimchi_2004_exp_1_column_row_color_RT",
#                         "kimchi_2004_exp_1_triangle_arrow_color_acc",
#                         "kimchi_2004_exp_1_triangle_arrow_acc",
#                         "kimchi_2004_exp_2_square_cross_color_acc",
#                         "kimchi_2004_exp_2_square_cross_RT",
#                         "kimchi_2004_exp_2_square_cross_acc"
# )

study_effects_3level <- seq(1:length(study_names_3level))
es_table_3_level <- es_table

es_table_3_level$study <- study_names_3level
es_table_3_level$effect <- study_effects_3level

implicit_meta_es_3level<-rma.mv(implicit_hedgesg, # treatment effect (Hedge's g)
                                se_implicit_g,
                                random = list(~ 1 | effect, 
                                              ~ 1 | study), 
                                tdist = TRUE, 
                                data = es_table_3_level,
                                method = "REML")

summary(implicit_meta_es_3level)
#========================= 3. Publication bias for implicit ES ============================
# Check assimetry with funnel plot
funnel(x = implicit_meta_es,
       xlab = "Hedges' g", 
       contour.levels = c(0.95, 0.975, 0.99), 
       col.contour = c("darkblue","blue","lightblue")
)
legend(5, 0,legend = c("p < 0.05", "p<0.025", "p < 0.01"),
       bty = "n",
       fill=c("darkblue","blue","lightblue"))

# Tests for assymetry using egger's test
source("eggers.test_function.R")
eggerstestresult <- eggers.test(x = implicit_meta_es)

# Estimate bias with trim-and-fill method
trimmed_implicit_meta <- trimfill(implicit_meta_es,
                         left = TRUE,
                         ma.fixed = FALSE)

funnel(trimmed_implicit_meta,
       xlab = "Hedge's g")

# Using p-curve
source("pcurve_function.R")
pcurve(implicit_meta_es)

#======================= 4. Heterogeneity check for implicit model ======================
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

spot.outliers.random(implicit_meta_es) # 0 outliers

# Influence analysis
# https://raw.githubusercontent.com/MathiasHarrer/dmetar/master/R/influence.analysis.R

# Load function
source("influence_analysis_function.R")

pdf("Influence_analysis.pdf", height = 16, width = 16)
InfluenceAnalysis(x = implicit_meta_es,
                  random = TRUE)
dev.off()


#======================== 5. Moderation analysis of implicit ES ===========================

# Load function
source("subgroup.analysis.mixed.effects_function.R")

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

# # significance of implicit effect
# mod_implicit_significance <- update(implicit_meta_es, 
#                            byvar=es_table$significance, 
#                            print.byvar=FALSE)
# summary(mod_implicit_significance)

# gray literature
mod_implicit_gray_literature <- update(implicit_meta_es, 
                           byvar=es_table$gray_literature, 
                           print.byvar=FALSE)
summary(mod_implicit_gray_literature)

#==== significance of inattention paradigm ====#
# Include moderator
# mod_implicit_inattention <- update(implicit_meta_es, 
#                            byvar=es_table$inattention_paradigm, 
#                            print.byvar=FALSE)
# summary(mod_implicit_inattention)

## Create model with subset
# meta_implicit_inattention <- metagen(TE = es_table$implicit_hedgesg, # treatment effect (Hedge's g)
#                    es_table$se_implicit_g, #standard error of treatment,
#                    studlab = es_table$study,
#                    subset = es_table$inattention_paradigm == 1,
#                    comb.random = TRUE)
# 
# forest(meta_implicit_inattention, # generate untrimmed forest plot
#        STUDLAB = TRUE, #should study labels be printed?
#        comb.random = TRUE # plot random effect estimate
# )

# mixed effects model
pdf("subgroup_analysis_inattention.pdf", height = 16, width = 16)
subgroup.analysis.mixed.effects(x = implicit_meta_es,
                                subgroups = es_table$inattention)
dev.off()

#==== significance of group assessment of awareness ====#

## Include moderator
# mod_implicit_group_aware_assess <- update(implicit_meta_es, 
#                           byvar=es_table$group_aware_assess, 
#                           print.byvar=FALSE)
# summary(mod_implicit_group_aware_assess)
# 
# 
# ## Create model with subset
# meta_implicit_group_aware_assess_yes <- metagen(TE = es_table$implicit_hedgesg, # treatment effect (Hedge's g)
#                             es_table$se_implicit_g, #standard error of treatment,
#                             studlab = es_table$study,
#                             subset = es_table$group_aware_assess == 1,
#                             comb.random = TRUE)
# 
# meta_implicit_group_aware_assess_no <- metagen(TE = es_table$implicit_hedgesg, # treatment effect (Hedge's g)
#                                    es_table$se_implicit_g, #standard error of treatment,
#                                    studlab = es_table$study,
#                                    subset = es_table$group_aware_assess == 0,
#                                    comb.random = TRUE)
# 
# forest(meta_implicit_group_aware_assess_yes, # generate untrimmed forest plot
#        STUDLAB = TRUE, #should study labels be printed?
#        comb.random = TRUE # plot random effect estimate
# )

# mixed effects model
pdf("subgroup_analysis_group_awareness.pdf", height = 16, width = 16)
subgroup.analysis.mixed.effects(x = implicit_meta_es,
                                subgroups = es_table$group_awareness)
dev.off()

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
