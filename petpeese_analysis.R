
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
source("./PETPEESE/PETPEESE_functions.R")

# 3PSM
library(weightr)
#=====================================================#

# Save defaults
graphical_defaults <- par()
options_defaults <- options()

#======================================== 0. Functions ======================================#
# Build "not in" operator
# Souce: https://stackoverflow.com/questions/5831794/opposite-of-in
'%!in%' <- function(x,y)!('%in%'(x,y))

#==================================== 1. Prepare data ========================================
# Create the same data frame used in the original analysis
source("./PETPEESE/Prepare_estable.R")

#================================= 2. Implicit meta-analysis =================================
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

# 3PSM to compare with PET results
weightfunct(es_table_clean$implicit_z_rs, 
            es_table_clean$variance_implicit_z_rs)


#======================== Using subgroups =================================
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

