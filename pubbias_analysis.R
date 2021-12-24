
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

# PET-PEESE
source("./Publication_bias_functions/PETPEESE_functions.R")

# Selection models
library(weightr)

# RoBMA
library(RoBMA)

#=====================================================#

# Save defaults
graphical_defaults <- par()
options_defaults <- options()

#======================================== 0. Functions ======================================#
# Build "not in" operator
# Souce: https://stackoverflow.com/questions/5831794/opposite-of-in
'%!in%' <- function(x,y)!('%in%'(x,y))

#============================== 1. Prepare data ================================#
# Create the same data frame used in the original analysis
source("./Publication_bias_functions/Prepare_estable.R")

#============================== 2. PET and PEESE ===============================#
# Check that equivalent results to Shanks' are obtained with all studies
naive_meta_full <- naive(es_table)
pet_meta_full <- PET(es_table)

funnelPETPEESE(es_table)

# Build models for random effects meta-analysis, PET, and PEESE

# Esse é o mesmo modelo usado no artigo (random effects), mas usando só dois níveis
# em vez de três níveis
naive_meta <- naive(es_table_clean)

# Esse é o modelo PET. A estimativa do efeito é dada pelo interceptio, e a influência 
# do erro padrão é dado embaixo (se_implicit_z_rs). Dá pra ver que é uma influência
# grande.
pet_meta <- PET(es_table_clean)

# Esse é o modelo PEESE, o output é bem parecido com o do PET. No nosso caso, o resultado
# não é confiável, como eu expliquei no e-mail pro Shanks
PEESE_meta <- PEESE(es_table_clean)


# When using original fuctions for two-level meta-analysis
tiff(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/pet-peese-analysis.tiff", 
     width=10,height=8.75, units = "in", res = 250, type = c("cairo"))
funnelPETPEESE(es_table_clean)
dev.off()

# When using fuctions adapted for three-level meta-analysis
tiff(file="C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis_IB/Plots/pet-peese-analysis_mv.tiff", 
     width=10,height=8.75, units = "in", res = 250, type = c("cairo"))
funnelPETPEESE(es_table_clean)
dev.off()

# They very similar, so we can go with the two-level model

# ========= 3.1. Using subgroups ===========

# Isso é o que eu mencionei pro Shanks no e-mail; tem umas diferenças entre 
# os resultados com todos os artigos e com subgrupos. Eu peguei os moderadores
# mostraram uma diferença nos tamanhos de efeito, e fiz modelos PET separados 
# pra cada nível de cada. Dá pra ver que o erro influencia o tamanho de efeito 
# pros estudos que não são inattention e pros que usam group assessment of 
# awareness.
#Inattention
pet_inattention <- PET(filter(es_table_clean, inattention == "yes"))
pet_noinattention <- PET(filter(es_table_clean, inattention == "no"))

funnelPETPEESE(filter(es_table_clean, inattention == "yes"))
funnelPETPEESE(filter(es_table_clean, inattention == "no"))

# Group assessment of awareness
pet_groupaware <- PET(filter(es_table_clean, group_awareness == "yes"))
pet_nogroupaware <- PET(filter(es_table_clean, group_awareness == "no"))

funnelPETPEESE(filter(es_table_clean, group_awareness == "yes"))
funnelPETPEESE(filter(es_table_clean, group_awareness == "no"))

# Gestalt study
# pet_gestalt <- PET(filter(es_table_clean, gestalt_study == "yes"))
# pet_nogestalt <- PET(filter(es_table_clean, gestalt_study == "no"))
# 
# funnelPETPEESE(filter(es_table_clean, gestalt_study == "yes"))
# funnelPETPEESE(filter(es_table_clean, gestalt_study == "no"))

#=========================== 4. Selection models ============================#

# 3PSM to compare with PET results
implicit_3PSM <- weightr::weightfunct(es_table_clean$implicit_z_rs, 
            es_table_clean$variance_implicit_z_rs,
            table = TRUE)

# 4PSM
implicit_4PSM <- weightr::weightfunct(es_table_clean$implicit_z_rs, 
                     es_table_clean$variance_implicit_z_rs,
                     steps = c(0.025, 0.05),
                     table = TRUE)

#============================ 5. RoBMA ======================================#
# Fit RoBMA
implicit_RoBMA <- RoBMA(r = es_table_clean$implicit_rs,
                        n = es_table_clean$N_participants_implicit,
                        seed = 1, model = "PSMA", parallel = TRUE)

summary(implicit_RoBMA)
interpret(implicit_RoBMA, output_scale = "r")

# ========== 5.1. Using subgroups ============

#  Create datasets for subgroups by inattention studies
es_table_clean_inattention <- filter(es_table_clean, inattention == "yes")
es_table_clean_notinattention <- filter(es_table_clean, inattention == "no")

# Run and interpret RoBMA by subgroup
inattention_RoBMA <- RoBMA(r = es_table_clean_inattention$implicit_rs,
                        n = es_table_clean_inattention$N_participants_implicit,
                        seed = 1, model = "PSMA", parallel = TRUE)
notinattention_RoBMA <- RoBMA(r = es_table_clean_notinattention$implicit_rs,
                           n = es_table_clean_notinattention$N_participants_implicit,
                           seed = 1, model = "PSMA", parallel = TRUE)

summary(inattention_RoBMA)
interpret(inattention_RoBMA, output_scale = "r")

summary(notinattention_RoBMA)
interpret(notinattention_RoBMA, output_scale = "r")

#  Create datasets for subgroups by use of group assessment of awareness
es_table_clean_groupaware <- filter(es_table_clean, group_awareness == "yes")
es_table_clean_nogroupaware <- filter(es_table_clean, group_awareness == "no")

# Run and interpret RoBMA by subgroup
groupaware_RoBMA <- RoBMA(r = es_table_clean_groupaware$implicit_rs,
                          n = es_table_clean_groupaware$N_participants_implicit,
                          seed = 1, model = "PSMA", parallel = TRUE)

nogroupaware_RoBMA <- RoBMA(r = es_table_clean_nogroupaware$implicit_rs,
                          n = es_table_clean_nogroupaware$N_participants_implicit,
                          seed = 1, model = "PSMA", parallel = TRUE)

summary(groupaware_RoBMA)
interpret(groupaware_RoBMA, output_scale = "r")

summary(nogroupaware_RoBMA)
interpret(nogroupaware_RoBMA, output_scale = "r")

# ======================= 6. Kreitz et al.'s data ======================
kreitz_data <- es_table_clean[69:83,]


#============= 6.1. PET and PEESE ===========
naive_kreitz_meta <- naive(kreitz_data)

# kreitz_meta <- metacor(cor = kreitz_data$implicit_rs,
#                        n = kreitz_data$N_participants_implicit,
#                        data = kreitz_data,
#                        studlab = kreitz_data$studies_outcomes,
#                        comb.fixed = FALSE,
#                        comb.random = TRUE,
#                        sm = "ZCOR", # use Fisher's z instead of raw correlation
#                        method.tau = "SJ")
# 
# kreitz_meta <- rma(implicit_z_rs, 
#                              variance_implicit_z_rs, 
#                              data = kreitz_data,
#                              method = "REML")
# 
# source("eggers.test_function.R")
# 
# eggers.test(kreitz_meta)

ranktest(naive_kreitz_meta)

pet_kreitz_meta <- PET(kreitz_data)
peese_kreitz_meta <- PEESE(kreitz_data)

#========== 6.2. Selection models ===========
# 3PSM to compare with PET results
kreitz_3PSM <- weightr::weightfunct(kreitz_data$implicit_z_rs, 
                                    kreitz_data$variance_implicit_z_rs,
                                      table = TRUE)

# 4PSM
kreitz_4PSM <- weightr::weightfunct(kreitz_data$implicit_z_rs, 
                                      kreitz_data$variance_implicit_z_rs,
                                      steps = c(0.025, 0.05),
                                      table = TRUE)

#================ 6.3. RoBMA ===============
kreitz_RoBMA <- RoBMA(r = kreitz_data$implicit_rs,
                        n = kreitz_data$N_participants_implicit,
                        seed = 1, model = "PSMA", parallel = TRUE)

summary(kreitz_RoBMA, output_scale = "r")
