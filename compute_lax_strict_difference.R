
# Papers using more than one measure (forced-choice or recall) but not reporting results separately:

# Most et al
# Lo and Yeh
# Gabay
# Schnuerch
# Beanland
# mack rock

# Papers using forced-choice as alternative to yes-no instead of together

# Moore and egeth
# Moore (2003)
# Ariga (2007)
# russel
# rashal

# List of studies with lax and strict (either a cojunction of measures or a recall measure) measures

# Moore et al. (2004)
# Wood & simons (2019)
# Razpurker-apfeld
# kimchi (2004)



group_effect_indices <- c(1:11, 41:45, 12:20, 22:28, 30, 32, 33, 35, 36, 
                    38, 39, 46:57, 68, 69, 58:60, 70, 71, 61:67,72:78, 80:93)

group_aware_assess <- c("yes", #ariga_2007_exp2
                        rep("no", 6), #beanland_pammer_2010_exp1A_fixating to gabay_2012_exp2
                        rep("yes",4), #lo_yeh_2008_exp1_200ms
                        rep("no", 5), #mack_and_rock_2000
                        rep("yes",4), # moore_egeth_1997_exp1 to moore_2004  
                        rep("no",1), #most_2005_exp1to7pooled
                        rep("yes", 4), #razpurker_pratt_2008
                        rep("yes", 10), #russsel_driver_2005
                        rep("no",2), #schunerch_2016
                        rep("no", 2), #wood_simons_2019
                        rep("yes", 26), #rashal_2017, kimchi_2004, kimchi_2008,lamy_2006
                        rep("no", 21) #pughnaghi_2020, nobre_2020, pugnaghi_2019, kreitz_2020
)

group_effect_indices <- group_effect_indices[which(group_aware_assess == "yes")]
  
  

# Lax criterion
source("calculate_awareness_effect_sizes.R")
lax_proportion_names <- paste("c", group_effect_indices, "_success_proportion",
                              sep = "")

lax_proportions <- map_dbl(lax_proportion_names, get)

mean_lax_proportions <- mean(lax_proportions, na.rm = TRUE)
sd_lax_proportions <- sd(lax_proportions, na.rm = TRUE)

# Check that percentages and proportions are coded correctly
lax_percentage_names <- paste("c", group_effect_indices, "_success_percentage",
                              sep = "")

lax_percentages <- map_dbl(lax_percentage_names, get)
mean_lax_percentages <- mean(lax_percentages, na.rm = TRUE)


# Strict criterion
source("calculate_strict_awareness_effect_sizes.R")
strict_proportion_names <- paste("c", group_effect_indices, "_success_proportion",
                                 sep = "")

strict_proportions <- map_dbl(strict_proportion_names, get)
mean_strict_proportions <- mean(strict_proportions, na.rm = TRUE)

# Percentages to check that percentages and proportions are coded correctly
strict_percentage_names <- paste("c", group_effect_indices, "_success_percentage",
                                 sep = "")

strict_percentages <- map_dbl(strict_percentage_names, get)
mean_strict_percentages <- mean(strict_percentages, na.rm = TRUE)

# Difference in mean proportions
mean_diff_proportions <- mean_lax_proportions - mean_strict_proportions

#====#
# z_score_schnuerch_exp1 <- (c35_success_proportion - mean_lax_proportions)/sd_lax_proportions
# z_score_schnuerch_exp2 <- (c36_success_proportion - mean_lax_proportions)/sd_lax_proportions
# 
# 
# #==============#
# effect_indices[which(lax_proportions == 1.0)] 
# 
# # razpurker-apfeld
# # kimchi triangle/arrow by color similarity acc
# # kimchi square/cross by color similarity acc
# # kimchi square/cross RT
# 
# allawarestudies <- c("razpurker_pratt_2008_columns_rows_rt", 
#                      "razpurker_pratt_2008_triangle_arrow_rt",
#                      "kimchi_2004_exp_1_triangle_arrow_color_acc", 
#                      "kimchi_2004_exp_2_square_cross_color_acc",
#                      "kimchi_2004_exp_2_square_cross_RT")
