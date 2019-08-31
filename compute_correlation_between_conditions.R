
#=============================================================================================#
# Compute correlation between conditions for implicit effect
# to calculate the variance of cohen's d, which will be used to 
# compute hedge's g
#=============================================================================================#

library(xlsx)

#========================= Schnuerch et al.(2016) data =========================

# Import data
data_schnuerch <- read.xlsx("completeDataFrame.xlsx", sheetIndex = 1)
View(data_schnuerch)

# Keep only unaware subjects who did not meet any exclusion criterion
data_nonoticers <- subset(data_schnuerch, include == 1)
View(data_nonoticers)

# Compute correlation between conditions (incongruent and neutral)
cor_schnuerch <- cor(data_nonoticers$median.neutral, 
    data_nonoticers$median.incong)


#==================== Razpurker-apfeld and Pratt (2008) ========================
# Import data
data_irene <- read.xlsx("RT_data_Razpurker-apfeld.xlsx", sheetIndex = 1)
View(data_irene)

# M and SD by condition (columns/rows and triangle/arrow)
mean_cond <- by(data_irene$Latency.mS., INDICES = data_irene$cond, FUN = mean)
sd_cond <- by(data_irene$Latency.mS., INDICES = data_irene$cond, FUN = sd)

# correlation between conditions
mean_cond_sub <- tapply(data_irene$Latency.mS., 
                        INDEX = list(data_irene$cond, as.factor(data_irene$sub)), 
               FUN = mean, simplify = TRUE)
View(mean_cond_sub)
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
#========================= Beanland and Pammer Exp 1A (2010) ==============================
# Import data
data_beanland_1A <- read.xlsx("Beanland&Pammer 2010 Exp1AB.xlsx", sheetIndex = 1)
View(data_beanland_1A)

data_beanland_1A <- data_beanland_1A[data_beanland_1A$notice == 0,]

data_beanland_1A$control_trials_mean_err_raw <- rowMeans(cbind(data_beanland_1A$t1err_raw,
                                                           data_beanland_1A$t2err_raw,
                                                           data_beanland_1A$t4err_raw))

data_beanland_1A$crit_trials_mean_err_raw <- rowMeans(cbind(data_beanland_1A$t3err_raw,
                                                               data_beanland_1A$t5err_raw))

cor_beanland_1A <- cor(data_beanland_1A$crit_trials_mean_err_raw,
    data_beanland_1A$control_trials_mean_err_raw)

cor_pairs <- mean(c(cor_schnuerch,
                    cor_razpurker,
                    cor_beanland_1A))
