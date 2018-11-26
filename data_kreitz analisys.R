
setwd("/home/gabi/Downloads")

library(xlsx)

data_kreitz <- read.xlsx("completeDataFrame.xlsx", sheetIndex = 1)
View(data_kreitz)

data_include <- subset(data_kreitz, include == 1)
View(data_include)

test_cong <- t.test(data_include$mean.congruent, data_include$mean.incong,
                    paired = TRUE, alternative = "less")
test_cong_med <- t.test(data_include$median.congruent, data_include$median.incong,
                    paired = TRUE, alternative = "less")