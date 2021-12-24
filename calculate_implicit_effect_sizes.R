

#===============================================================================================#
#=================================== Compute effect sizes ======================================#
# Compute effect sizes for implicit effects
# Effects are r and Cohen's d
#===============================================================================================#

library(esc)
library(tidyverse)
library(magrittr)


#==== 1. Ariga et al. (2007) exp 2 ====

# Compute effect sizes from t-value and sample size
c1_n <- 20
c1_df <- 19
c1_tvalue <- 0.61

c1_cohensd <- c1_tvalue/sqrt(c1_df) * (-1) # ES is negative
c1_r <- sqrt((c1_tvalue**2)/(c1_tvalue**2 + c1_df)) * (-1) # ES is negative
# c1_cohensd <- c1_r/sqrt(1-(c1_r**2)) * (-1) # ES is negative


#==== 2 Beanland and Pammer (2010) exp. 1A, fixating ====

#  Compute effect sizes from mean, sds and N
c2_n <- 27

# Mean and SD for control trials
c2_control_mean <- 6.20
c2_control_sd <- 3.26
c2_control_variance <- (c2_control_sd)^2

# Mean and SD for critical trials
c2_critical_mean <- 4.93
c2_critical_sd <- 2.76
c2_critical_variance <- (c2_critical_sd)^2


c2_m_difference <- c2_control_mean-c2_critical_mean #control - critical because lower scores are better
c2_cohensd <- c2_m_difference/sqrt(mean(c(c2_critical_variance,
                                          c2_control_variance)))

c2_r <- 0.54

# correlations between pairs of observations for cohen's drm
c2_cor_pairs <- cor_beanland_1A_fixating

#==== 3. Beanland and Pammer (2010) exp. 1A, moving ====

#  Compute effect sizes from mean, sds and N
c3_n <- 31

# Mean and SD for control trials
c3_control_mean <- 4.10
c3_control_sd <- 2.76
c3_control_variance <- (c3_control_sd)^2


# Mean and SD for critical trials
c3_critical_mean <- 2.76
c3_critical_sd <- 2.49
c3_critical_variance <- (c3_critical_sd)^2


c3_m_difference <- c3_control_mean-c3_critical_mean #control - critical because lower scores are better
c3_cohensd <- c3_m_difference/sqrt(mean(c(c3_critical_variance,
                                          c3_control_variance)))

c3_r <- 0.75

# correlations between pairs of observations for cohen's drm
c3_cor_pairs <- cor_beanland_1A_moving

#==== 4. Beanland and Pammer (2010) exp. 2, slow US ====

#  Compute effect sizes from mean, sds and N
c4_n <- 41


# Mean and SD for control trials
c4_control_mean <- 0.21
c4_control_sd <- 0.75
c4_control_variance <- (c4_control_sd)^2


# Mean and SD for critical trials
c4_critical_mean <- -0.04
c4_critical_sd <- 0.50
c4_critical_variance <- (c4_critical_sd)^2


c4_m_difference <- c4_control_mean-c4_critical_mean #control - critical because lower scores are better
c4_cohensd <- c4_m_difference/sqrt(mean(c(c4_critical_variance,
                                          c4_control_variance)))

c4_r <- 0.35

# correlations between pairs of observations for cohen's drm
c4_cor_pairs <- cor_beanland_2_slow

#==== 5. Beanland and Pammer (2010) exp. 2, fast US ====

#  Compute effect sizes from mean, sds and N
c5_n <- 41

# Mean and SD for control trials
c5_control_mean <- 0.15
c5_control_sd <- 0.43
c5_control_variance <- (c5_control_sd)^2

# Mean and SD for critical trials
c5_critical_mean <- -0.15
c5_critical_sd <- 0.43
c5_critical_variance <- (c5_critical_sd)^2


c5_m_difference <- c5_control_mean-c5_critical_mean #control - critical because lower scores are better
c5_cohensd <- c5_m_difference/sqrt(mean(c(c5_critical_variance,
                                          c5_control_variance)))

c5_r <- 0.45

# correlations between pairs of observations for cohen's drm
c5_cor_pairs <- cor_beanland_2_fast

#==== 6. Gabay et al. (2012) - Exp. 1 ====

# Compute effect sizes from F-value and dfs
c6_n <- 18
c6_dfeffect <- 1
c6_dferror <- 28
c6_fvalue <- 6.1

c6_r <- sqrt(c6_fvalue/(c6_fvalue + c6_dferror))
c6_cohensd <- c6_r/sqrt(1-(c6_r**2))


#==== 7. Gabay et al. (2012) - Exp. 2 ====

# Compute effect sizes from F-value and dfs
c7_n <- 10
c7_dfeffect <- 1
c7_dferror <- 21
c7_fvalue <- 4.4

c7_r <- sqrt(c7_fvalue/(c7_fvalue + c7_dferror))
c7_cohensd <- c7_r/sqrt(1-(c7_r**2))


#==== 8. Lo e Yeh (2008) - exp. 1 (200 ms) ====

# Cohen's d is given; positive value because the effect is facilitatory
c8_n <- 43
c8_df <- 42
c8_tvalue <- 0.77
c8_cohensd <- 0.12 # reported

c8_r <- sqrt((c8_tvalue**2)/(c8_tvalue**2 + c8_df))
# c8_cohensd <- c8_r/sqrt(1-(c8_r**2))
# c8_cohensd <- c8_tvalue/sqrt(c8_df)

#==== 9. Lo e Yeh (2008) - exp. 1 (500 ms) ====

# Compute effect sizes from t-value and sample size
c9_n <- 41
c9_df <- 40
c9_tvalue <- 4.53  
c9_cohensd <- 0.71 # reported

c9_r <- sqrt((c9_tvalue**2)/(c9_tvalue**2 + c9_df))

#==== 10. Lo e Yeh (2008) - exp. 2 (200 ms) ====

# Compute effect sizes from t-value and sample size
c10_n <- 23
c10_df <- 22
c10_tvalue <- 1.73
c10_cohensd <- 0.36  # reported

c10_r <- sqrt((c10_tvalue**2)/(c10_tvalue**2 + c10_df))


#==== 11. Lo e Yeh (2008) - exp. 2 (500 ms) ====

# Compute effect sizes from t-value and sample size
c11_n <- 25
c11_df <- 24
c11_tvalue <- 0.01
c11_cohensd <- 0.002 #reported

c11_r <- sqrt((c11_tvalue**2)/(c11_tvalue**2 + c11_df))


#==== 12. Moore and Egeth (1997), exp. 1 ====

# Compute effect size from proportions
c12_n <- 20
c12_pop <- 20
c12_success_percentage <- 79.33
c12_success_proportion <- c12_success_percentage/100
c12_base_chance <- 0.5 #top/bottom
c12_successes <- round(c12_success_percentage*c12_pop/100)
c12_failures <- c12_pop - c12_successes 

c12_chisquared <- unname(chisq.test(c(c12_successes, c12_failures), 
                                p = c(0.5, 0.5))$statistic)


# c12_cohensd <- esc_chisq(c12_chisquared, 
#                          es.type = "d", 
#                          totaln = c12_pop)$es

c12_phi <- sqrt(c12_chisquared/c12_pop)

c12_r <- c12_phi

c12_cohensd <- c12_r/sqrt(1-(c12_r**2))


#==== 13. Moore and Egeth (1997), exp. 3 ====

# Compute effect size from proportions
c13_n <- 20
c13_pop <- 20
C13_success_percentage <- 88.12
C13_success_proportion <- C13_success_percentage/100
C13_base_chance <- 0.5 #top/bottom
c13_successes <- round(C13_success_percentage*c13_pop/100)
c13_failures <- c13_pop - c13_successes

c13_chisquared <- unname(chisq.test(c(c13_successes, c13_failures), 
                                p = c(0.5, 0.5))$statistic)

# c13_cohensd <- esc_chisq(c13_chisquared,
#                          es.type = "d",
#                          totaln = c13_pop)$es

c13_phi <- sqrt(c13_chisquared/c13_pop)

c13_r <- c13_phi

c13_cohensd <- c13_r/sqrt(1-(c13_r**2))


#==== 14. Moore et al. (2003) ====

# Compute effect sizes from t-value and sample size
c14_n <- 44
c14_df <- 43
c14_tvalue <- 2.72 # positive value

c14_cohensd <- c14_tvalue/sqrt(c14_df)

c14_r <- sqrt((c14_tvalue**2)/(c14_tvalue**2 + c14_df))


#==== 15. Moore et al. (2004) ====

# Compute effect sizes from t-value and sample size
c15_n <- 25
c15_df <- 24
c15_tvalue <- 0.51 # positive value

c15_cohensd <- c15_tvalue/sqrt(c15_df)

c15_r <- sqrt((c15_tvalue**2)/(c15_tvalue**2 + c15_df))


#==== 16. Most et al. (2005) ====

# Compute effect sizes from t-value and sample size
c16_n <- 181
c16_df <- 180
c16_tvalue <- 2.90 # positive value

c16_cohensd <- c16_tvalue/sqrt(c16_df)

c16_r <- sqrt((c16_tvalue**2)/(c16_tvalue**2 + c16_df))


#==== 17. Razpurker-Apfeld et al. (2008) - columns/rows, RT ====

# Compute effect sizes from F-value and dfs
c17_n <- 7
c17_dfeffect <- 1
c17_dferror <- 13
c17_fvalue <- 5.41


c17_r <- sqrt(c17_fvalue/(c17_fvalue + c17_dferror))
c17_cohensd <- c17_r/sqrt(1-(c17_r**2))

# c17_cohensd <- (c17_fvalue * c17_dfeffect)/(c17_fvalue*c17_dfeffect+c17_dferror)
# correlations between pairs of observations for cohen's drm
# c17_cor_pairs <- irene_target_cor_col


#==== 18. Razpurker-Apfeld et al. (2008) - columns/rows, d' ====

# Compute effect sizes from F-value and dfs
c18_n <- 7
c18_dfeffect <- 1
c18_dferror <- 13
c18_fvalue <- 4.98 # positive value, because effect is facilitatory

c18_r <- sqrt(c18_fvalue/(c18_fvalue + c18_dferror))
c18_cohensd <- c18_r/sqrt(1-(c18_r**2))
# c18_cohensd <- (c18_fvalue * c18_dfeffect)/(c18_fvalue*c18_dfeffect+c18_dferror)
# correlations between pairs of observations for cohen's drm
# c18_cor_pairs <- irene_target_cor_col


#==== 19. Razpurker-Apfeld et al. (2008) - triangle/arrow, RT ====

# Compute effect sizes from F-value and dfs
c19_n <- 7
c19_fvalue <- 0.001 # negative value, because experimental condition is smaller
c19_dfeffect <- 1
c19_dferror <- 13
 
c19_r <- sqrt(c19_fvalue/(c19_fvalue + c19_dferror)) * (-1) # ES is negative
c19_cohensd <- c19_r/sqrt(1-(c19_r**2))

# c19_cohensd <- (c19_fvalue * c19_dfeffect)/(c19_fvalue*c19_dfeffect+c19_dferror) * (-1) # ES is negative
# correlations between pairs of observations for cohen's drm
# c19_cor_pairs <- irene_target_cor_tri

  
#==== 20. Razpurker-Apfeld et al. (2008) - triangle/arrow, d' ====

# Compute effect sizes from F-value and dfs
c20_n <- 7
c20_dfeffect <- 1
c20_dferror <- 13
c20_fvalue <- 2.3 # positive value, because effect is facilitatory

c20_r <- sqrt(c20_fvalue/(c20_fvalue + c20_dferror))
c20_cohensd <- c20_r/sqrt(1-(c20_r**2))

# c20_cohensd <- (c20_fvalue * c20_dfeffect)/(c20_fvalue*c20_dfeffect+c20_dferror)
# correlations between pairs of observations for cohen's drm
# c20_cor_pairs <- irene_target_cor_tri


#==== 21. Richards et al. (2012), tracking task  ====

# # Compute effect sizes from proportions
# c21_pop <- 25+29
# c21_successes <- 25
# c21_failures <- c21_pop - c21_successes
# 
# c21_chisquared <- unname(chisq.test(c(c21_successes, c21_failures), 
#                                     p = c(0.5, 0.5))$statistic)
# 
# c21_cohensd <- (-1) * esc_chisq(c21_chisquared, 
#                                 es.type = "d", 
#                                 totaln = c21_pop)$es #negative, because effect is not facilitatory
# 
# # c21_etasquared <- esc_chisq(c21_chisquared, 
# #                             es.type = "eta", 
# #                             totaln = c21_pop)$es
# 
# c21_phi <- sqrt(c21_chisquared/c21_pop)
# 
# c21_r <- c21_phi #esc_phi(phi = c21_phi, totaln = c21_pop, es.type = "r")

#c21_r <- 0.53

#==== 22. Russell et al. (2008) - exp. 1, acc ====

# Compute effect sizes from F-value and dfs
c22_n <- 25
c22_fvalue <- 23.70 # positive value, because effect is facilitatory
c22_dfeffect <- 1
c22_dferror <- 24

c22_r <- sqrt(c22_fvalue/(c22_fvalue + c22_dferror))
c22_cohensd <- c22_r/sqrt(1-(c22_r**2))

# c22_cohensd <- (c22_fvalue * c22_dfeffect)/(c22_fvalue*c22_dfeffect+c22_dferror)

#==== 23. Russell et al. (2008) - exp. 1, RT ====

# Compute effect sizes from F-value and dfs
c23_n <- 25
c23_fvalue <- 3.9 # positive value, because effect is facilitatory
c23_dfeffect <- 1
c23_dferror <- 24

c23_r <- sqrt(c23_fvalue/(c23_fvalue + c23_dferror))
c23_cohensd <- c23_r/sqrt(1-(c23_r**2))

# c23_cohensd <- (c23_fvalue * c23_dfeffect)/(c23_fvalue*c23_dfeffect+c23_dferror)

#==== 24. Russell et al. (2008) - exp. 2, acc ====

# Compute effect sizes from F-value and dfs
c24_n <- 28
c24_fvalue <- 39.20 # positive value, because effect is facilitatory
c24_dfeffect <- 1
c24_dferror <- 27

c24_r <- sqrt(c24_fvalue/(c24_fvalue + c24_dferror))
c24_cohensd <- c24_r/sqrt(1-(c24_r**2))

# c24_cohensd <- (c24_fvalue * c24_dfeffect)/(c24_fvalue*c24_dfeffect+c24_dferror)

#==== 25. Russell et al. (2008) - exp. 2, RT ====

# Compute effect sizes from F-value and dfs
c25_n <- 28
c25_fvalue <- 2.39 
c25_dfeffect <- 1
c25_dferror <- 27

c25_r <- sqrt(c25_fvalue/(c25_fvalue + c25_dferror)) * (-1) # ES is negative
c25_cohensd <- c25_r/sqrt(1-(c25_r**2))

# c25_cohensd <- (c25_fvalue * c25_dfeffect)/(c25_fvalue*c25_dfeffect+c25_dferror) * (-1) # ES is negative

#==== 26. Russell et al. (2008) - exp. 3, acc ====

# Compute effect sizes from F-value and dfs
c26_n <- 24
c26_fvalue <- 7.7
c26_dfeffect <- 1
c26_dferror <- 23

c26_r <- sqrt(c26_fvalue/(c26_fvalue + c26_dferror))
c26_cohensd <- c26_r/sqrt(1-(c26_r**2))

# c26_cohensd <- (c26_fvalue * c26_dfeffect)/(c26_fvalue*c26_dfeffect+c26_dferror)

#==== 27. Russell et al. (2008) - exp. 3, RT ====

# Compute effect sizes from F-value and dfs
c27_n <- 24
c27_fvalue <- 3.1 # positive value, because effect is facilitatory
c27_dfeffect <- 1
c27_dferror <- 23

c27_r <- sqrt(c27_fvalue/(c27_fvalue + c27_dferror)) * (-1) # ES is negative
c27_cohensd <- c27_r/sqrt(1-(c27_r**2))

# c27_cohensd <- (c27_fvalue * c27_dfeffect)/(c27_fvalue*c27_dfeffect+c27_dferror) * (-1) # ES is negative

#==== 28. Russell et al. (2008) - exp. 4A, acc ====

# Compute effect sizes from F-value and dfs
c28_n <- 20
c28_fvalue <- 37.10 # positive value, because effect is facilitatory
c28_dfeffect <- 1
c28_dferror <- 19

c28_r <- sqrt(c28_fvalue/(c28_fvalue + c28_dferror))
c28_cohensd <- c28_r/sqrt(1-(c28_r**2))

# c28_cohensd <- (c28_fvalue * c28_dfeffect)/(c28_fvalue*c28_dfeffect+c28_dferror)

#==== 29. Russell et al. (2008) - exp. 4A, RT ====

# Compute effect sizes from F-value and dfs
c29_n <- 20
c29_fvalue <- ? # positive value, because effect is facilitatory
c29_dfeffect <- NA
c29_dferror <- NA

c29_cohensd <- NA #(c29_fvalue * c29_dfeffect)/(c29_fvalue*c29_dfeffect+c29_dferror)

c29_r <- NA


#==== 30. Russell et al. (2008) - exp. 4B, acc ====

# Compute effect sizes from F-value and dfs
c30_n <- 21
c30_fvalue <- 34.08 # positive value, because effect is facilitatory
c30_dfeffect <- 1
c30_dferror <- 20

c30_r <- sqrt(c30_fvalue/(c30_fvalue + c30_dferror))
c30_cohensd <- c30_r/sqrt(1-(c30_r**2))

# c30_cohensd <- (c30_fvalue * c30_dfeffect)/(c30_fvalue*c30_dfeffect+c30_dferror)

#==== 31. Russell et al. (2008) - exp. 4B, RT ====

# Compute effect sizes from F-value and dfs
c31_n <- 21
c31_fvalue <- NA # positive value, because effect is facilitatory
c31_dfeffect <- NA
c31_dferror <- NA
  
c31_cohensd <- NA #(c31_fvalue * c31_dfeffect)/(c31_fvalue*c31_dfeffect+c31_dferror)

c31_r <- NA


#==== 32. Russell et al. (2008) - exp. 5, acc ====

# Compute effect sizes from F-value and dfs
c32_n <- 24
c32_fvalue <- 4.3 # positive value, because effect is facilitatory
c32_dfeffect <- 1
c32_dferror <- 23

c32_r <- sqrt(c32_fvalue/(c32_fvalue + c32_dferror))
c32_cohensd <- c32_r/sqrt(1-(c32_r**2))

# c32_cohensd <- (c32_fvalue * c32_dfeffect)/(c32_fvalue*c32_dfeffect+c32_dferror)

#==== 33. Russell et al. (2008) - exp. 5, RT ====

# Compute effect sizes from F-value and dfs
c33_n <- 24
c33_fvalue <- 1.83 # positive value, because effect is facilitatory
c33_dfeffect <- 1
c33_dferror <- 23
  
c33_r <- sqrt(c33_fvalue/(c33_fvalue + c33_dferror))
c33_cohensd <- c33_r/sqrt(1-(c33_r**2))

# c33_cohensd <- (c33_fvalue * c33_dfeffect)/(c33_fvalue*c33_dfeffect+c33_dferror)

#==== 34. Shafto and Pitts (2015) ====

# Compute effect sizes from t-value and sample size

#c34_tvalue <- -1.57 #negative value, because the control condition is larger (effect is also non-significant)
#c34_n_pairs <- 14

#c34_cohensd <- c34_tvalue/sqrt(c34_n_pairs)


#==== 35. Schnuerch et al. (2016) - exp. 1 ====

# Compute effect sizes from t-value and sample size
c35_n <- 61
c35_tvalue <- 3.15 # positive value, because the effect is facilitatory
c35_df <- 60

c35_r <- sqrt(c35_tvalue/(c35_tvalue**2 + c35_df))
c35_cohensd <- c35_tvalue/sqrt(c35_df)

# correlations between pairs of observations for cohen's drm
c35_cor_pairs <- cor_schnuerch


#==== 36. Schnuerch et al. (2016) - exp. 2 ====

# Compute effect sizes from t-value and sample size
c36_n <- 58
c36_tvalue <- 2.68 # positive value, because the effect is facilitatory
c36_df <- 57

c36_r <- sqrt(c36_tvalue/(c36_tvalue**2 + c36_df))
c36_cohensd <- c36_tvalue/sqrt(c36_df)

# correlations between pairs of observations for cohen's drm
c36_cor_pairs <- cor_schnuerch


#==== 37. Wiemer et al. (2013) ====

# # Compute effect sizes from t-value and sample size
# 
# c37_tvalue <- 3.27 # positive value, because the effect is facilitatory
# 
# c37_m1 <- -0.14
# c37_m2 <- -0.71
# c37_SD1 <- 0.76
# c37_SD2 <- 0.47
# c37_n1 <- 60
# c37_n2 <- 60
# 
# #c37_cohensd <- c37_tvalue*sqrt(c37_n1+c37_n2)/sqrt(c37_n1*c37_n2)
# c37_cohensd <- abs(c37_m1 - c37_m2)/sqrt((((c37_n1-1)*c37_SD1**2)+
#                                             ((c37_n2-1)*c37_SD2**2))/(c37_n1+c37_n2))


#==== 38. Wood and Simons (2019), exp. 1 ====

# Compute effect size from proportions
c38_pop <- 58+62+32+23
c38_n <- c38_pop
c38_success_percentage <- 84.2
c38_success_proportion <- c38_success_percentage/100
c38_base_chance <- 0.5 #top/bottom
c38_successes <- round(c38_success_percentage*c38_pop/100)
c38_failures <- c38_pop - c38_successes

c38_chisquared <- unname(chisq.test(c(c38_successes, c38_failures), 
                                p = c(c38_base_chance, 1 - c38_base_chance))$statistic)

# c38_cohensd <- esc_chisq(c38_chisquared, 
#                          es.type = "d", 
#                          totaln = c38_pop)$es

c38_phi <- sqrt(c38_chisquared/c38_pop)

c38_r <- c38_phi
c38_cohensd <- c38_r/sqrt(1-(c38_r**2))


#==== 39. Wood and Simons (2019), exp. 2 ====

# Compute effect size from proportions
c39_pop <- 30+29+29+32+35+29+32
c39_n <- c39_pop
c39_success_percentage <- 0.9
c39_success_proportion <- c39_success_percentage/100
c39_base_chance <- 0.5 #???
c39_successes <- round(c39_success_percentage*c39_pop/100)
c39_failures <- c39_pop - c39_successes

c39_chisquared <- unname(chisq.test(c(c39_successes, c39_failures), 
                                p = c(0.5, 0.5))$statistic)

# c39_cohensd <- (-1) * esc_chisq(c39_chisquared, # ES is negative
#                          es.type = "d", 
#                          totaln = c39_pop)$es

c39_phi <- sqrt(c39_chisquared/c39_pop) * (-1) # ES is negative

c39_r <- c39_phi
c39_cohensd <- c39_r/sqrt(1-(c39_r**2))

#==== 41. Mack and Rock. exp 1. (2000) ====
c41_pop <- 50 + 80
c41_n <- c41_pop

# compute correct number of completers in control group from percentages
c41_control_completers <- round(10*20/100) + # yes in control group - flake
  round(0*20/100) + # yes in control group - grace
  round(15*20/100) + # yes in control group - short
  round(5*20/100) # yes in control group - prize

c41_chisquared_table <- matrix(c(80 - c41_control_completers, # no in control group
                                  32, # no in ib group
                                  c41_control_completers, # yes in control group
                                  18), # yes in ib group
                                nrow = 2,
                                ncol = 2,
                                dimnames = list(c("control", "ib"), #row names
                                                c("no", "yes") #column names
                                )
)

# Compute chisquare from table
c41_chisquared <- unname(chisq.test(c41_chisquared_table)$statistic)


# c41_cohensd <- esc_chisq(c41_chisquared,
#                          es.type = "d",
#                          totaln = c41_pop)$es

c41_phi <- sqrt(c41_chisquared/c41_pop)

c41_r <- c41_phi
c41_cohensd <- c41_r/sqrt(1-(c41_r**2))


#==== 42. Mack and Rock. exp 2. (2000) ====
c42_pop <- 41 + 60
c42_n <- c42_pop

c42_chisquared_table <- matrix(c(55, # no in control group
                                 34, # no in ib group
                                 5, # yes in control group
                                 24), # yes in ib group
                               nrow = 2,
                               ncol = 2,
                               dimnames = list(c("control", "ib"), #row names
                                               c("no", "yes") #column names
                               )
)

# Compute chisquare from table
c42_chisquared <- unname(chisq.test(c42_chisquared_table)$statistic)

# c42_cohensd <- esc_chisq(c42_chisquared,
#                          es.type = "d",
#                          totaln = c42_pop)$es

c42_phi <- sqrt(c42_chisquared/c42_pop)

c42_r <- c42_phi
c42_cohensd <- c42_r/sqrt(1-(c42_r**2))


#==== 43. Mack and Rock. exp 3. (2000) ====
c43_pop <- 21 + #exp. 3
  20 + #control for exp 1 - short
  20 #control for exp 1 - flake
c43_n <- c43_pop

c43_chisquared_table <- matrix(c(40 - (round(10*20/100) + round(15*20/100)), # no in control group
                                 32, # no in ib group
                                 round(10*20/100) + # yes in control group - flake
                                   round(15*20/100), # yes in control group - short
                                 18), # yes in ib group
                               nrow = 2,
                               ncol = 2,
                               dimnames = list(c("control", "ib"), #row names
                                               c("no", "yes") #column names
                               )
)

# Compute chisquare from table
c43_chisquared <- unname(chisq.test(c43_chisquared_table)$statistic)

# c43_cohensd <- esc_chisq(c43_chisquared,
#                          es.type = "d",
#                          totaln = c43_pop)$es

c43_phi <- sqrt(c43_chisquared/c43_pop)

c43_r <- c43_phi
c43_cohensd <- c43_r/sqrt(1-(c43_r**2))


#==== 44. Mack and Rock. exp 4 (2000) ====
c44_pop <- 60 + 29
c44_n <- c44_pop

c44_chisquared_table <- matrix(c(53, # no in control group
                                  15, # no in ib group
                                  7, # yes in control group
                                  14), # yes in ib group
                                nrow = 2,
                                ncol = 2,
                                dimnames = list(c("control", "ib"), #row names
                                                c("no", "yes") #column names
                                                ) 
                                )

c44_chisquared <- unname(chisq.test(c44_chisquared_table)$statistic)

# c44_cohensd <- esc_chisq(c44_chisquared,
#                          es.type = "d",
#                          totaln = c44_pop)$es

c44_phi <- sqrt(c44_chisquared/c44_pop)

c44_r <- c44_phi
c44_cohensd <- c44_r/sqrt(1-(c44_r**2))

#==== 45. Mack and Rock. exp 5 (2000) ====
c45_pop <- 60+10+9
c45_n <- c45_pop

# Build variables for contingency table
c45_group_control <- rep("control", 60)
c45_group_ib <- rep("ib", 10+9)

c45_implicit_control <- c(rep("yes", 7), rep("no", 53))
c45_implicit_ib <- c(rep("yes", 8), rep("no", 11))

c45_group <- c(c45_group_control,
                c45_group_ib)
c45_implicit <- c(c45_implicit_control,
                          c45_implicit_ib)

c45_data <- data.frame("group" = c45_group,
                        "implicit" = c45_implicit)

# Create contingency table from data frame
c45_chisquared_table <- table(c45_data)

# Compute chisquare from table
c45_chisquared <- unname(chisq.test(c45_chisquared_table)$statistic)

# c45_cohensd <- esc_chisq(c45_chisquared,
#                          es.type = "d",
#                          totaln = c45_pop)$es

c45_phi <- sqrt(c45_chisquared/c45_pop)

c45_r <- c45_phi
c45_cohensd <- c45_r/sqrt(1-(c45_r**2))

#==== 46. Rashal et al. exp 1 RT (2017) ====

# Compute effect sizes from F-value and dfs
c46_fvalue <- 7.92
c46_dfeffect <- 1
c46_dferror <- 19
c46_n <- 20
c46_partialetasquared <- 0.29

c46_r <- sqrt(c46_fvalue/(c46_fvalue + c46_dferror))
c46_cohensd <- c46_r/sqrt(1 - (c46_r**2))


#==== 47. Rashal et al. exp 2 acc (2017) ====

# Compute effect sizes from F-value and dfs
c47_fvalue <- 1.84
c47_dfeffect <- 1
c47_dferror <- 27
c47_n <- 28
c47_partialetasquared <- 0.06

c47_r <- sqrt(c47_fvalue/(c47_fvalue + c47_dferror)) * (-1) # ES is negative
c47_cohensd <- c47_r/sqrt(1 - (c47_r**2)) * (-1) # ES is negative


#==== 48. Rashal et al. exp 3 RT (2017) ====

# Compute effect sizes from F-value and dfs
c48_fvalue <- 1.6
c48_dfeffect <- 1
c48_dferror <- 17
c48_n <- 18
c48_partialetasquared <- 0.09

c48_r <- sqrt(c48_fvalue/(c48_fvalue + c48_dferror))
c48_cohensd <- c48_r/sqrt(1 - (c48_r**2))


#==== 49. Rashal et al. exp 4 acc (2017) ====

# Compute effect sizes from F-value and dfs
c49_fvalue <- 9.08
c49_dfeffect <- 1
c49_dferror <- 14
c49_n <- 15
c49_partialetasquared <- 0.39

c49_r <- sqrt(c49_fvalue/(c49_fvalue + c49_dferror))
c49_cohensd <- c49_r/sqrt(1 - (c49_r**2))


#==== 50. Rashal et al. exp 4 RT (2017) ====

# Compute effect sizes from F-value and dfs
c50_fvalue <- 7.72
c50_dfeffect <- 1
c50_dferror <- 14
c50_n <- 15
c50_partialetasquared <- 0.36

c50_r <- sqrt(c50_fvalue/(c50_fvalue + c50_dferror))
c50_cohensd <- c50_r/sqrt(1 - (c50_r**2))


#==== 51. Rashal et al. exp 5 acc (2017) ====

# Compute effect sizes from F-value and dfs
c51_fvalue <- 4.96
c51_dfeffect <- 1
c51_dferror <- 17
c51_n <- 18
c51_partialetasquared <- 0.23

c51_r <- sqrt(c51_fvalue/(c51_fvalue + c51_dferror))
c51_cohensd <- c51_r/sqrt(1 - (c51_r**2))


#==== 52. Rashal et al. exp 5 RT (2017) ====

# Compute effect sizes from F-value and dfs
c52_fvalue <- 3.56
c52_dfeffect <- 1
c52_dferror <- 17
c52_n <- 18
c52_partialetasquared <- 0.17

c52_r <- sqrt(c52_fvalue/(c52_fvalue + c52_dferror))
c52_cohensd <- c52_r/sqrt(1 - (c52_r**2))


#==== 53. Rashal et al. exp 6 acc (2017) ====

# Compute effect sizes from F-value and dfs
c53_fvalue <- 2.07
c53_dfeffect <- 1
c53_dferror <- 17
c53_n <- 18
c53_partialetasquared <- 0.11

c53_r <- sqrt(c53_fvalue/(c53_fvalue + c53_dferror))
c53_cohensd <- c53_r/sqrt(1 - (c53_r**2))


#==== 54. Rashal et al. exp 6 RT (2017) ====

# Compute effect sizes from F-value and dfs
c54_fvalue <- 1.85
c54_dfeffect <- 1
c54_dferror <- 17
c54_n <- 18
c54_partialetasquared <- 0.1

c54_r <- sqrt(c54_fvalue/(c54_fvalue + c54_dferror))
c54_cohensd <- c54_r/sqrt(1 - (c54_r**2))


#==== 55. Kimchi et al. (2004) exp. 1 - Column/row by color similarity, RT ====

# Compute effect sizes from F-value and dfs
c55_fvalue <- 16.74
c55_dfeffect <- 1
c55_dferror <- 13
c55_n <- 14

c55_r <- sqrt(c55_fvalue/(c55_fvalue + c55_dferror))
c55_cohensd <- c55_r/sqrt(1 - (c55_r**2))


#==== 56. Kimchi et al. (2004) exp. 1 - triangle/arrow by color similarity, acc ====

# Compute effect sizes from F-value and dfs
c56_fvalue <- 2.74
c56_dfeffect <- 1
c56_dferror <- 13
c56_n <- 14

c56_r <- sqrt(c56_fvalue/(c56_fvalue + c56_dferror)) * (-1) # ES is negative
c56_cohensd <- c56_r/sqrt(1 - (c56_r**2)) # ES is negative


#==== 57. Kimchi et al. (2004) exp. 1 - triangle/arrow, acc ====

# Compute effect sizes from F-value and dfs
c57_fvalue <- 3.04
c57_dfeffect <- 1
c57_dferror <- 13
c57_n <- 14

c57_r <- sqrt(c57_fvalue/(c57_fvalue + c57_dferror))
c57_cohensd <- c57_r/sqrt(1 - (c57_r**2))


#==== 58. Kimchi et al. (2004) exp. 2 - square/cross by color similarity, acc ====

# Compute effect sizes from F-value and dfs
c58_fvalue <- 1.82
c58_dfeffect <- 1
c58_dferror <- 11
c58_n <- 12

c58_r <- sqrt(c58_fvalue/(c58_fvalue + c58_dferror)) * (-1) # ES is negative
c58_cohensd <- c58_r/sqrt(1 - (c58_r**2)) # ES is negative


#==== 59. Kimchi et al. (2004) exp. 2 - square/cross, RT ====

# Compute effect sizes from F-value and dfs
c59_fvalue <- 6.27
c59_dfeffect <- 1
c59_dferror <- 11
c59_n <- 12

c59_r <- sqrt(c59_fvalue/(c59_fvalue + c59_dferror))
c59_cohensd <- c59_r/sqrt(1 - (c59_r**2))


#==== 60. Kimchi et al. (2004) exp. 2 - square/cross, acc ====

# Compute effect sizes from F-value and dfs
c60_fvalue <- 25.06
c60_dfeffect <- 1
c60_dferror <- 11
c60_n <- 12

c60_r <- sqrt(c60_fvalue/(c60_fvalue + c60_dferror))
c60_cohensd <- c60_r/sqrt(1 - (c60_r**2))


#==== 61. Kimchi et al. (2008) exp. 1 RT ====

# Compute effect sizes from F-value and dfs
c61_fvalue <- 7.03
c61_dfeffect <- 1
c61_dferror <- 45
c61_n <- 46

c61_r <- sqrt(c61_fvalue/(c61_fvalue + c61_dferror))
c61_cohensd <- c61_r/sqrt(1 - (c61_r**2))


#==== 62. Kimchi et al. (2008) exp. 1 acc ====

# Compute effect sizes from F-value and dfs
c62_fvalue <- 5.96
c62_dfeffect <- 1
c62_dferror <- 45
c62_n <- 46

c62_r <- sqrt(c62_fvalue/(c62_fvalue + c62_dferror))
c62_cohensd <- c62_r/sqrt(1 - (c62_r**2))


#==== 63. Lamy et al. (2006) exp. 2 same ====

# Compute effect size from proportion
c63_pop <- 8
c63_n <- c63_pop
c63_success_percentage <- 72.66
c63_success_proportion <- c63_success_percentage/100
c63_base_chance <- 0.5 #top/bottom
c63_successes <- round(c63_success_percentage*c63_pop/100)
c63_failures <- c63_pop - c63_successes 

c63_chisquared <- unname(chisq.test(c(c63_successes, c63_failures), 
                                    p = c(0.5, 0.5))$statistic)

# c63_cohensd <- esc_chisq(c63_chisquared, 
#                          es.type = "d", 
#                          totaln = c63_pop)$es

c63_phi <- sqrt(c63_chisquared/c63_pop)

c63_r <- c63_phi
c63_cohensd <- c63_r/sqrt(1 - (c63_r**2))

#==== 64. Lamy et al. (2006) exp. 2 different ====

# Compute effect size from proportion
c64_pop <- 8
c64_n <- c64_pop
c64_success_percentage <- 58.59
c64_success_proportion <- c64_success_percentage/100
c64_base_chance <- 0.5 #top/bottom
c64_successes <- round(c64_success_percentage*c64_pop/100)
c64_failures <- c64_pop - c64_successes 

c64_chisquared <- unname(chisq.test(c(c64_successes, c64_failures), 
                                    p = c(0.5, 0.5))$statistic)


# c64_cohensd <- esc_chisq(c64_chisquared, 
#                          es.type = "d", 
#                          totaln = c64_pop)$es

c64_phi <- sqrt(c64_chisquared/c64_pop)

c64_r <- c64_phi
c64_cohensd <- c64_r/sqrt(1 - (c64_r**2))

#==== 65. Lamy et al. (2006) exp. 3 RT ====

# Compute effect sizes from F-value and dfs
c65_fvalue <- 10.13
c65_dfeffect <- 1
c65_dferror <- 8
c65_n <- 9

c65_r <- sqrt(c65_fvalue/(c65_fvalue + c65_dferror))
c65_cohensd <- c65_r/sqrt(1 - (c65_r**2))


#==== 66. Lamy et al. (2006) exp. 4 RT ====

# Compute effect sizes from F-value and dfs
c66_fvalue <- 24.51
c66_dfeffect <- 1
c66_dferror <- 8
c66_n <- 9

c66_r <- sqrt(c66_fvalue/(c66_fvalue + c66_dferror))
c66_cohensd <- c66_r/sqrt(1 - (c66_r**2))


#==== 67. Lamy et al. (2006) exp. 5 easy ====

# Compute effect sizes from F-value and dfs
c67_fvalue <- 10.60
c67_dfeffect <- 1
c67_dferror <- 10
c67_n <-11

c67_r <- sqrt(c67_fvalue/(c67_fvalue + c67_dferror))
c67_cohensd <- c67_r/sqrt(1 - (c67_r**2))


#==== 68. Kimchi et al. (2004) exp. 1 - connected triangle/arrow, RT ====

# Compute effect sizes from F-value and dfs
c68_fvalue <- 32.95
c68_dfeffect <- 1
c68_dferror <- 13
c68_n <- 14

c68_r <- sqrt(c68_fvalue/(c68_fvalue + c68_dferror))
c68_cohensd <- c68_r/sqrt(1 - (c68_r**2))


#==== 69. Kimchi et al. (2004) exp. 1 - connected triangle/arrow, accuracy ====

# Compute effect sizes from F-value and dfs
c69_fvalue <- 10.4
c69_dfeffect <- 1
c69_dferror <- 13
c69_n <- 14

c69_r <- sqrt(c69_fvalue/(c69_fvalue + c69_dferror))
c69_cohensd <- c69_r/sqrt(1 - (c69_r**2))


#==== 70. Kimchi et al. (2004) exp. 2 - disconnected square/cross, RT ====

# Compute effect sizes from F-value and dfs
c70_fvalue <- 15.54
c70_dfeffect <- 1
c70_dferror <- 11
c70_n <- 12

c70_r <- sqrt(c70_fvalue/(c70_fvalue + c70_dferror))
c70_cohensd <- c70_r/sqrt(1 - (c70_r**2))


#==== 71. Kimchi et al. (2004) exp. 2 - disconnected square/cross, acc ====

# Compute effect sizes from F-value and dfs
c71_fvalue <- 45.08
c71_dfeffect <- 1
c71_dferror <- 11
c71_n <- 12

c71_r <- sqrt(c71_fvalue/(c71_fvalue + c71_dferror))
c71_cohensd <- c71_r/sqrt(1 - (c71_r**2))


#==== 72. Pugnaghi et al. (2020) exp 1 RT =====

# Compute effect sizes from F-value and sample size
c72_n <- 65
c72_fvalue <- 7.40 # positive sign, because the effect is in the predicted direction
c72_dfeffect <- 1
c72_dferror <- 64
c72_partialetasquared <- 0.1

c72_r <- sqrt(c72_fvalue/(c72_fvalue + c72_dferror))
c72_cohensd <- c72_r/sqrt(1 - (c72_r**2))

# correlations between pairs of observations for cohen's drm
c72_cor_pairs <- cor_pugnaghi2020_exp1_RT


#============ 73. Pugnaghi et al. (2020) exp 1 accuracy ============

# Compute effect sizes from F-value and sample size
c73_n <- 65
c73_fvalue <- 7.84 # positive sign, because the effect is in the predicted direction
c73_dfeffect <- 1
c73_dferror <- 64
c73_partialetasquared <- 0.11

c73_r <- sqrt(c73_fvalue/(c73_fvalue + c73_dferror))
c73_cohensd <- c73_r/sqrt(1 - (c73_r**2))

# correlations between pairs of observations for cohen's drm
c73_cor_pairs <- cor_pugnaghi2020_exp1_acc

 
#============ 74. Pugnaghi et al. (2020) exp 2 RT ============

# Compute effect sizes from F-value and sample size
c74_n <- 102
c74_fvalue <- 1.12 # positive sign, because the effect is in the predicted direction
c74_dfeffect <- 1
c74_dferror <- 101
c74_partialetasquared <- 0.01

c74_r <- sqrt(c74_fvalue/(c74_fvalue + c74_dferror))
c74_cohensd <- c74_r/sqrt(1 - (c74_r**2))

# correlations between pairs of observations for cohen's drm
c74_cor_pairs <- cor_pugnaghi2020_exp2_RT


#============ 75. Pugnaghi et al. (2020) exp 2 accuracy ============

# Compute effect sizes from F-value and sample size
c75_n <- 102
c75_fvalue <- 4.26 # positive sign, because the effect is in the predicted direction
c75_dfeffect <- 1
c75_dferror <- 101
c75_partialetasquared <- 0.04

c75_r <- sqrt(c75_fvalue/(c75_fvalue + c75_dferror))
c75_cohensd <- c75_r/sqrt(1 - (c75_r**2))

# correlations between pairs of observations for cohen's drm
c75_cor_pairs <- cor_pugnaghi2020_exp2_acc


#============ 76. Nobre et al. (2020) ============

# Compute effect sizes from t-value and sample size
c76_n <- 30
c76_tvalue <- 0.66 # positive sign, because the effect is in the predicted direction
c76_df <- 29

c76_r <- sqrt(c76_tvalue/(c76_tvalue**2 + c76_df))
c76_cohensd <- c76_tvalue/sqrt(c76_df) 


#=============== 77. Pugnaghi et al. (2019) =============

# Compute effect sizes from t-value and sample size
c77_n <- 202
c77_tvalue <- 3.31 # positive sign, because the effect is in the predicted direction
c77_df <- 201

c77_r <- sqrt(c77_tvalue/(c77_tvalue**2 + c77_df))
c77_cohensd <- c77_tvalue/sqrt(c77_df) 


#============== 78. Kreitz et al. (2020) - Dataset 1 ===============

# Compute r from d
c78_n <- 69
c78_cohensd <- 0.123
c78_r <- sqrt((c78_cohensd**2)/(c78_cohensd**2 + 1))


#============== 79. Kreitz et al. (2020) - Datasets 2.1 & 2.2 ===============
# 
# # Compute r from d
# c79_n <- 86
# c79_cohensd <- 0.509
# c79_r <- sqrt((c79_cohensd**2)/(c79_cohensd**2 + 1))


#============== 80. Kreitz et al. (2020) - Dataset 3 ===============

# Compute r from d
c80_n <- 62
c80_cohensd <- 0.172
c80_r <- sqrt((c80_cohensd**2)/(c80_cohensd**2 + 1))


#============== 81. Kreitz et al. (2020) - Dataset 4 ===============

# Compute r from d
c81_n <- 64
c81_cohensd <- 0.037
c81_r <- sqrt((c81_cohensd**2)/(c81_cohensd**2 + 1))


#============== 82. Kreitz et al. (2020) - Dataset 5 ===============

# Compute r from d
c82_n <- 38
c82_cohensd <- 0.102
c82_r <- sqrt((c82_cohensd**2)/(c82_cohensd**2 + 1))


#============== 83. Kreitz et al. (2020) - Dataset 6 ===============

# Compute r from d
c83_n <- 290
c83_cohensd <- 0.115
c83_r <- sqrt((c83_cohensd**2)/(c83_cohensd**2 + 1))


#============== 84. Kreitz et al. (2020) - Dataset 7 ===============

# Compute r from d
c84_n <- 42
c84_cohensd <- 0.358
c84_r <- sqrt((c84_cohensd**2)/(c84_cohensd**2 + 1))


#============== 85. Kreitz et al. (2020) - Dataset 8 ===============

# Compute r from d
c85_n <- 33
c85_cohensd <- 0.410
c85_r <- sqrt((c85_cohensd**2)/(c85_cohensd**2 + 1))


#============== 86. Kreitz et al. (2020) - Dataset 9 ===============

# Compute r from d
c86_n <- 21
c86_cohensd <- 0.398
c86_r <- sqrt((c86_cohensd**2)/(c86_cohensd**2 + 1))


#============== 87. Kreitz et al. (2020) - Dataset 10 ===============

# Compute r from d
c87_n <- 34
c87_cohensd <- -0.050
c87_r <- sqrt((c87_cohensd**2)/(c87_cohensd**2 + 1)) * (-1) # Negative sign because effect is in the opposite direction of the hypothesis (i.e., below chance, as in p. 1058 of the paper)


#============== 88. Kreitz et al. (2020) - Dataset 13 ===============

# Compute r from d
c88_n <- 57
c88_cohensd <- 0.008
c88_r <- sqrt((c88_cohensd**2)/(c88_cohensd**2 + 1))


#============== 89. Kreitz et al. (2020) - Dataset 14 ===============

# Compute r from d
c89_n <- 64
c89_cohensd <- 0.035
c89_r <- sqrt((c89_cohensd**2)/(c89_cohensd**2 + 1))


#============== 90. Kreitz et al. (2020) - Dataset 15 ===============

# Compute r from d
c90_n <- 68
c90_cohensd <- 0.040
c90_r <- sqrt((c90_cohensd**2)/(c90_cohensd**2 + 1)) 


#============== 91. Kreitz et al. (2020) - Dataset 16 ===============

# Compute r from d
c91_n <- 178
c91_cohensd <- -0.009
c91_r <- sqrt((c91_cohensd**2)/(c91_cohensd**2 + 1)) * (-1) # Negative sign because effect is in the opposite direction of the hypothesis (i.e., below chance, as in p. 1058 of the paper)


#============== 92. Kreitz et al. (2020) - Dataset 2.1 ===============

# Compute r from d
c92_n <- 86
c92_cohensd <- 0.483
c92_r <- sqrt((c92_cohensd**2)/(c92_cohensd**2 + 1))


#============== 93. Kreitz et al. (2020) - Dataset 2.2 ===============

# Compute r from d
c93_n <- 86
c93_cohensd <- 0.531
c93_r <- sqrt((c93_cohensd**2)/(c93_cohensd**2 + 1))


#========================================#
#==== Build vector with effect sizes ====#
#========================================#
effect_indices <- c(1:11, 41:45, 12:20, 22:28, 30, 32, 33, 35, 36, 
                    38, 39, 46:57, 68, 69, 58:60, 70, 71, 61:67, 72:78, 80:93)

implicit_totaln <- map_dbl(paste("c", effect_indices, "_n",
                                 sep = ""), get)

#============== Cohen's d ================
implicit_cohensd_names <- paste("c", effect_indices, "_cohensd",
                 sep = "")

implicit_cohensd <- map_dbl(implicit_cohensd_names, get)

# Compute hedge's g
implicit_hedgesg <- hedges_g(implicit_cohensd, implicit_totaln)


#================== r ====================
implicit_r_names <- paste("c", effect_indices, "_r",
                          sep = "")

implicit_r <- map_dbl(implicit_r_names, get)

# Transform to Fisher's z (Viechtbauer and Cheung (2010))
implicit_z_r <- FisherZ(implicit_r)

# with labels
implicit_z_r_labels <- implicit_z_r
names(implicit_z_r_labels) <- implicit_r_names

