
#=============================================================================================#
# Compute correlation between conditions for implicit effect
# to calculate the variance of cohen's d, which will be used to 
# compute hedge's g
#=============================================================================================#

library(tidyverse)
library(xlsx)
library(DescTools)

#========================= Schnuerch et al.(2016) data =========================

# Import data
data_schnuerch <- read.xlsx("completeDataFrame.xlsx", sheetIndex = 1)

# Keep only unaware subjects who did not meet any exclusion criterion
data_nonoticers <- subset(data_schnuerch, include == 1)

# Compute correlation between conditions (incongruent and neutral)
cor_schnuerch <- cor(data_nonoticers$mean.neutral, 
    data_nonoticers$mean.incong)


#==================== Razpurker-apfeld and Pratt (2008) ========================
# Import data
data_irene <- read.xlsx("RT_data_Razpurker-apfeld.xlsx", sheetIndex = 1)

# M and SD by condition (columns/rows and triangle/arrow)
mean_cond <- by(data_irene$Latency.mS., INDICES = data_irene$cond, FUN = mean)
sd_cond <- by(data_irene$Latency.mS., INDICES = data_irene$cond, FUN = sd)

# correlation between conditions
mean_cond_sub <- tapply(data_irene$Latency.mS., 
                        INDEX = list(data_irene$cond, as.factor(data_irene$sub)), 
               FUN = mean, simplify = TRUE)
cor_cond <- cor(mean_cond_sub[1,], mean_cond_sub[2,])


# M and SD
mean_background <- by(data_irene$Latency.mS., 
                      INDICES = data_irene$Background, 
                      FUN = mean)
sd_background <- by(data_irene$Latency.mS., 
                    INDICES = data_irene$Background, 
                    FUN = sd)

# correlation between backgrounds
mean_background_sub <- tapply(data_irene$Latency.mS., 
                              INDEX = list(data_irene$Background, as.factor(data_irene$sub)), 
                              FUN = mean, simplify = TRUE)
cor_background <- cor(mean_background_sub[1,], 
                      mean_background_sub[2,])

# M and SD
mean_interaction <- aggregate(Latency.mS. ~ cond + Background, 
                              data = data_irene, 
                              FUN = "mean")

sd_interaction <- aggregate(Latency.mS. ~ cond + Background, 
                            data = data_irene, 
                            FUN = "sd")

cor_razpurker <- mean(c(cor_cond,
                        cor_background))

# Create dataset with means by subject
data_irene$sub <- factor(data_irene$sub)

data_irene_means <- data_irene %>%
  group_by(sub, cond, Target, Background) %>% 
  summarise(mean_RT = mean(Latency.mS.)) %>%
  ungroup()


###### Across target types
#======== column/row correlations========#
# same background
data_irene_sb_col <- data_irene_means %>%
  filter(cond == "col", Background == "sb")

# different background
data_irene_db_col <- data_irene_means %>%
  filter(cond == "col", Background == "db")

irene_cor_col <- cor(data_irene_sb_col$mean_RT, data_irene_db_col$mean_RT)

#======== triangle/arrow correlations========#
# same background
data_irene_sb_tri <- data_irene_means %>%
  filter(cond == "tri", Background == "sb")

# different background
data_irene_db_tri <- data_irene_means %>%
  filter(cond == "tri", Background == "db")

irene_cor_tri <- cor(data_irene_sb_tri$mean_RT, 
                     data_irene_db_tri$mean_RT)

###### Across background types
#======== column/row correlations========#
# same target
data_irene_st_col <- data_irene_means %>%
  filter(cond == "col", Target == "st")

# different target
data_irene_dt_col <- data_irene_means %>%
  filter(cond == "col", Target == "dt")

irene_target_cor_col <- cor(data_irene_st_col$mean_RT, 
                            data_irene_dt_col$mean_RT)

#======== triangle/arrow correlations========#
# same target
data_irene_st_tri <- data_irene_means %>%
  filter(cond == "tri", Target == "st")

# different target
data_irene_dt_tri <- data_irene_means %>%
  filter(cond == "tri", Target == "dt")

irene_target_cor_tri <- cor(data_irene_st_tri$mean_RT, 
                            data_irene_dt_tri$mean_RT)
  
#========================= Beanland and Pammer Exp 1A (2010) ==============================
# Import data
data_beanland_1A <- read.xlsx("Beanland&Pammer 2010 Exp1AB.xlsx", sheetIndex = 1)

data_beanland_1A <- data_beanland_1A[data_beanland_1A$notice == 0,]

data_beanland_1A$control_trials_mean_err_raw <- rowMeans(cbind(data_beanland_1A$t1err_raw,
                                                           data_beanland_1A$t2err_raw,
                                                           data_beanland_1A$t4err_raw))

data_beanland_1A$crit_trials_mean_err_raw <- rowMeans(cbind(data_beanland_1A$t3err_raw,
                                                               data_beanland_1A$t5err_raw))

cor_beanland_1A <- cor(data_beanland_1A$crit_trials_mean_err_raw,
    data_beanland_1A$control_trials_mean_err_raw)

# only fixating condition
data_beanland_1A_fixating <- data_beanland_1A %>%
  filter(condition == 1)
cor_beanland_1A_fixating <- cor(data_beanland_1A_fixating$crit_trials_mean_err_raw,
                                data_beanland_1A_fixating$control_trials_mean_err_raw)

# only moving condition
data_beanland_1A_moving <- data_beanland_1A %>%
  filter(condition == 2)
cor_beanland_1A_moving <- cor(data_beanland_1A_moving$crit_trials_mean_err_raw,
                                data_beanland_1A_moving$control_trials_mean_err_raw)

#========================= Beanland and Pammer Exp 2 (2010) ==============================
# Import data
data_beanland_2 <- read.xlsx("Beanland&Pammer 2010 Exp 2_edited.xlsx", sheetIndex = 1)

data_beanland_2 <- data_beanland_2[data_beanland_2$notice == 2,]

data_beanland_2$control_trials_mean_err_raw <- rowMeans(cbind(data_beanland_2$t1err_raw,
                                                               data_beanland_2$t2err_raw,
                                                               data_beanland_2$t4err_raw))

data_beanland_2$crit_trials_mean_err_raw <- rowMeans(cbind(data_beanland_2$t3err_raw,
                                                            data_beanland_2$t5err_raw))

cor_beanland_2 <- cor(data_beanland_2$crit_trials_mean_err_raw,
                       data_beanland_2$control_trials_mean_err_raw)

# only slow condition
data_beanland_2_slow <- data_beanland_2 %>%
  filter(condition == 1)
cor_beanland_2_slow <- cor(data_beanland_2_slow$crit_trials_mean_err_raw,
                                data_beanland_2_slow$control_trials_mean_err_raw)

# only fast condition
data_beanland_2_fast <- data_beanland_2 %>%
  filter(condition == 2)
cor_beanland_2_fast <- cor(data_beanland_2_fast$crit_trials_mean_err_raw,
                           data_beanland_2_fast$control_trials_mean_err_raw)


#=================== compute mean correlation ========================
# sample sizes
n_schnuerch <- nrow(data_nonoticers)
#n_razpurker <- length(unique(data_irene_means$sub))
n_razpuker_col <- nrow(data_irene_sb_col)
n_razpuker_tri <- nrow(data_irene_sb_tri)
n_beanland1A_fixating <- nrow(data_beanland_1A_fixating)
n_beanland1A_moving <- nrow(data_beanland_1A_moving)
n_beanland2_slow <- nrow(data_beanland_2_slow)
n_beanland2_fast <- nrow(data_beanland_2_fast)

# Concatente all ns
cor_samplesizes <- c(n_schnuerch,
                     #n_razpurker,
                     n_razpuker_col,
                     n_razpuker_tri,
                     n_beanland1A_fixating,
                     n_beanland1A_moving,
                     n_beanland2_slow,
                     n_beanland2_fast)

# Vector with correlations
corvalues <- c(cor_schnuerch,
               #cor_razpurker,
               irene_target_cor_col,
               irene_target_cor_tri,
               cor_beanland_1A_fixating,
               cor_beanland_1A_moving,
               cor_beanland_2_slow,
               cor_beanland_2_fast)

# Transform cors to z scores
z_corvalues <- FisherZ(corvalues)

# Compute weighted average of z scores
z_weighted_avg <- weighted.mean(z_corvalues,
                                cor_samplesizes)

# Transform weighted mean z score back to weighted correlation
cor_pairs <- FisherZInv(z_weighted_avg)


