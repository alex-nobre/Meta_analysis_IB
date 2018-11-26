
library(xlsx)

setwd("/home/alexandre/MEGA/Doutorado/Tese/Meta-analysis/Dados/Analysis")

# Kreitz et al.(2016)
data_kreitz <- read.xlsx("completeDataFrame.xlsx", sheetIndex = 1)
View(data_kreitz)

data_nonoticers <- subset(data_kreitz, include == 1)
View(data_nonoticers)

# Razpurker-apfeld and Pratt (2008)
data_irene <- read.xlsx("RT_data_Razpurker-apfeld.xlsx", sheetIndex = 1)
View(data_irene)

# M and SD
mean_cond <- by(data_irene$Latency.mS., INDICES = data_irene$cond, FUN = mean)
sd_cond <- by(data_irene$Latency.mS., INDICES = data_irene$cond, FUN = sd)

# correlation
mean_cond_sub <- tapply(data_irene$Latency.mS., INDEX = list(data_irene$cond, as.factor(data_irene$sub)), 
               FUN = mean, simplify = TRUE)
View(mean_cond_sub)
cor_cond <- cor(mean_cond_sub[1,], mean_cond_sub[2,])


# M and SD
mean_background <- by(data_irene$Latency.mS., INDICES = data_irene$Background, FUN = mean)
sd_background <- by(data_irene$Latency.mS., INDICES = data_irene$Background, FUN = sd)


mean_background_sub <- tapply(data_irene$Latency.mS., 
                              INDEX = list(data_irene$Background, as.factor(data_irene$sub)), 
                              FUN = mean, simplify = TRUE)
cor_background <- cor(mean_background_sub[1,], mean_background_sub[2,])

# M and SD
mean_interaction <- aggregate(Latency.mS. ~ cond + Background, 
                              data = data_irene, 
                              FUN = "mean")

sd_interaction <- aggregate(Latency.mS. ~ cond + Background, 
                            data = data_irene, 
                            FUN = "sd")
