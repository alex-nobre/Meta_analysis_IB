
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
cor(data_nonoticers$median.neutral, 
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
