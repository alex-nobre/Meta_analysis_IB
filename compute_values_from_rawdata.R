#============================================================================#
#============= Compute effect sizes from fully available data ===============#
# 1. Schnuerch et al. (2016)
# 2. Pugnaghi et al. (2020)
#============================================================================#

# Load packages
library(xlsx)
library(effsize)
library(magrittr)
library(tidyverse)

# Analysis directory
analysis_dir <- "C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Dados/Data_sent_by_researchers/"

#=========================================#
#======= 1. Schnuerch et al. (2016) =======
#=========================================#

data_kreitz <- read.xlsx(paste(analysis_dir, "completeDataFrame.xlsx", sep = ""), sheetIndex = 1)
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

#=========================================#
#======== 2. Pugnaghi et al. (2020) =======
#=========================================#

data_pugnaghi <- read.xlsx(paste(analysis_dir, "Revision_data/DataFrame&Legend_iPrep Load3.xlsx", sep = ""), sheetIndex = 1)
View(data_pugnaghi)

# Keep only IB participants and those fitting the inclusion criteria
data_pugnaghi_include <- subset(data_pugnaghi, Inclusion == 1)

# Create columns with data collapsed across perceptual load conditions
data_pugnaghi_include <- data_pugnaghi_include %>%
  mutate(mean.congruent = rowMeans(select(., mean_Kong_LL, mean_Kong_HL)),
         mean.incongruent = rowMeans(select(., mean_Inkong_LL, mean_Inkong_HL)),
         count_congruent = rowMeans(select(., count_correct_RT_kong_lload, count_correct_RT_kong_hload)),
         count_incongruent = rowMeans(select(., count_correct_RT_inkong_lload, count_correct_RT_inkong_hload)))
  

# test for differences between means
test_cong_pugnaghi <- t.test(data_pugnaghi_include$mean.congruent, data_pugnaghi_include$mean.incong,
                    paired = TRUE)

# Compute mean and sd
cong_incong_diffs <- data_pugnaghi_include$mean.congruent - data_pugnaghi_include$mean.incong
mean(cong_incong_diffs)
sd(cong_incong_diffs)
hist(cong_incong_diffs)

cong_es <- cohen.d(data_pugnaghi_include$mean.congruent, data_pugnaghi_include$mean.incong,
                   paired = TRUE)

var_cong_es <- ((cong_es$conf.int[2]-cong_es$conf.int[1])/2*1.96)**2


#=========================================#
#=========== Nobre et al. (2020) ==========
#=========================================#

load("C:/Users/Biosig/Google Drive/Doutorado/Tese/Replication_Contour_integration/Data/questionnaire_ERPs")

meta_analysis_data <- questionnaire.ERPs[,questionnaire.ERPs$group.original == "unaware"]

t.test(questionnaire.ERPs$RT.mean.sqr_1, RT.mean.rand_1,
       paired = TRUE)

