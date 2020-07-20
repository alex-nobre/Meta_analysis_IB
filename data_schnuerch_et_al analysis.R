
library(xlsx)
library(effsize)

data_kreitz <- read.xlsx("completeDataFrame.xlsx", sheetIndex = 1)
View(data_kreitz)

data_include <- subset(data_kreitz, include == 1)
View(data_include)

# test for differences between means
test_cong <- t.test(data_include$mean.congruent, data_include$mean.incong,
                    paired = TRUE, alternative = "less")

# Compute mean and sd
cong_incong_diffs <- data_include$mean.congruent - data_include$mean.incong
mean(cong_incong_diffs)
sd(cong_incong_diffs)
hist(cong_incong_diffs)

cong_es <- cohen.d(data_include$mean.congruent, data_include$mean.incong,
        paired = TRUE)

var_cong_es <- ((cong_es$conf.int[2]-cong_es$conf.int[1])/2*1.96)**2


# test with median (inappropriate)
# test_cong_med <- t.test(data_include$median.congruent, data_include$median.incong,
#                     paired = TRUE, alternative = "less")