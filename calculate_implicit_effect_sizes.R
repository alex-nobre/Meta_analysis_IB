
library(esc)
library(tidyverse)

# Effect sizes:

# Formulas are taken from Lakens et al.'s sheet for effect sizes


#==== 1. Ariga et al. (2007) exp 2 ====

# Compute effect sizes from t-value and sample size

# Experimental condition: Invalid same-object
# Control condition: Invalid different object

c1_tvalue <- 0.61 # positive value, because experimental condition is larger
c1_n_pairs <- 20

c1_cohensd <- c1_tvalue/sqrt(c1_n_pairs)


#==== 2 Beanland and Pammer (2010) exp. 1A, fixating ====

#  Compute effect sizes from mean, sds and N

c2_n_pairs <- 27

# # Control trial 1
# c2_control_t1_mean <- 4.8
# c2_control_t1_sd <- 3.6
# 
# # Control trial 2
# c2_control_t2_mean <- 7.6
# c2_control_t2_sd <- 3.5
# 
# # Combined mean and SD for control trials
# c2_control_mean <- mean(c(c2_control_t1_mean, c2_control_t2_mean))
# c2_control_sd <- mean(c(c2_control_t1_sd, c2_control_t2_sd))

# Mean and SD for control trials
c2_control_mean <- 6.20
c2_control_sd <- 3.26
c2_control_variance <- (c2_control_sd)^2

# # Critical trial 1
# c2_critical_t1_mean <- 4.5
# c2_critical_t1_sd <- 3.6
# 
# # Critical trial 2
# c2_critical_t2_mean <- 5.4
# c2_critical_t2_sd <- 2.5
# 
# # Combined mean and SD for critical trials
# c2_critical_mean <- mean(c(c2_critical_t1_mean, c2_critical_t2_mean))
# c2_critical_sd <- mean(c(c2_critical_t1_sd, c2_critical_t2_sd))

# Mean and SD for critical trials
c2_critical_mean <- 4.93
c2_critical_sd <- 2.76
c2_critical_variance <- (c2_critical_sd)^2

# t-test to get t-value
# c2_tvalue <- (c2_control_mean - c2_critical_mean)/
#               ((c2_control_sd - c2_critical_sd)/sqrt(c2_n_pairs))
# 
# c2_cohensd <- c2_tvalue/sqrt(c2_n_pairs)

c2_m_difference <- c2_control_mean-c2_critical_mean #control - critical because lower scores are better
c2_cohensd <- c2_m_difference/sqrt(mean(c(c2_critical_variance,
                                          c2_control_variance)))

#==== 3. Beanland and Pammer (2010) exp. 1A, moving ====

#  Compute effect sizes from mean, sds and N
c3_n_pairs <- 31

# # Control trial 1
# c3_control_t1_mean <- 2.6
# c3_control_t1_sd <- 2.6
# 
# # Control trial 2
# c3_control_t2_mean <- 5.5
# c3_control_t2_sd <- 3.4

# # Combined mean and SD for control trials
# c3_control_mean <- mean(c(c3_control_t1_mean, c3_control_t2_mean))
# c3_control_sd <- mean(c(c3_control_t1_sd, c3_control_t2_sd))

# Mean and SD for control trials
c3_control_mean <- 4.10
c3_control_sd <- 2.76
c3_control_variance <- (c3_control_sd)^2

# # Critical trial 1
# c3_critical_t1_mean <- 2.2
# c3_critical_t1_sd <- 2.9
# 
# # Critical trial 2
# c3_critical_t2_mean <- 3.3
# c3_critical_t2_sd <- 2.6

# # Combined mean and SD for critical trials
# c3_critical_mean <- mean(c(c3_critical_t1_mean, c3_critical_t2_mean))
# c3_critical_sd <- mean(c(c3_critical_t1_sd, c3_critical_t2_sd))

# Mean and SD for critical trials
c3_critical_mean <- 2.76
c3_critical_sd <- 2.49
c3_critical_variance <- (c3_critical_sd)^2

# t-test to get t-value
# c3_tvalue <- (c3_control_mean - c3_critical_mean)/
#               ((c3_control_sd - c3_critical_sd)/sqrt(c3_n_pairs))
# 
# c3_cohensd <- c3_tvalue/sqrt(c3_n_pairs)

c3_m_difference <- c3_control_mean-c3_critical_mean #control - critical because lower scores are better
c3_cohensd <- c3_m_difference/sqrt(mean(c(c3_critical_variance,
                                          c3_control_variance)))


#==== 4. Beanland and Pammer (2010) exp. 2, slow US ====

#  Compute effect sizes from mean, sds and N
c4_n_pairs <- 41

# # Control trial 1
# c4_control_t1_mean <- 0.2
# c4_control_t1_sd <- 1.1
# 
# # Control trial 2
# c4_control_t2_mean <- 0.2
# c4_control_t2_sd <- 0.6

# # Combined mean and SD for control trials
# c4_control_mean <- mean(c(c4_control_t1_mean, c4_control_t2_mean))
# c4_control_sd <- mean(c(c4_control_t1_sd, c4_control_t2_sd))

# Mean and SD for control trials
c4_control_mean <- 0.21
c4_control_sd <- 0.75
c4_control_variance <- (c4_control_sd)^2

# # Critical trial 1
# c4_critical_t1_mean <- 0.1
# c4_critical_t1_sd <- 0.7
# 
# # Critical trial 2
# c4_critical_t2_mean <- -0.2
# c4_critical_t2_sd <- 0.6
# 
# # Combined mean and SD for critical trials
# c4_critical_mean <- mean(c(c4_critical_t1_mean, c4_critical_t2_mean))
# c4_critical_sd <- mean(c(c4_critical_t1_sd, c4_critical_t2_sd))

# Mean and SD for critical trials
c4_critical_mean <- -0.04
c4_critical_sd <- 0.50
c4_critical_variance <- (c4_critical_sd)^2

# t-test to get t-value
# c4_tvalue <- (c4_control_mean - c4_critical_mean)/((c4_control_sd - c4_critical_sd)/sqrt(c4_n_pairs))
# 
# c4_cohensd <- c4_tvalue/sqrt(c4_n_pairs)

c4_m_difference <- c4_control_mean-c4_critical_mean #control - critical because lower scores are better
c4_cohensd <- c4_m_difference/sqrt(mean(c(c4_critical_variance,
                                          c4_control_variance)))

#==== 5. Beanland and Pammer (2010) exp. 2, fast US ====

#  Compute effect sizes from mean, sds and N
c5_n_pairs <- 41

# # Control trial 1
# c5_control_t1_mean <- 0.2
# c5_control_t1_sd <- 0.9
# 
# # Control trial 2
# c5_control_t2_mean <- 0.2
# c5_control_t2_sd <- 0.5

# # Combined mean and SD for control trials
# c5_control_mean <- mean(c(c5_control_t1_mean, c5_control_t2_mean))
# c5_control_sd <- mean(c(c5_control_t1_sd, c5_control_t2_sd))

# Mean and SD for control trials
c5_control_mean <- 0.15
c5_control_sd <- 0.43
c5_control_variance <- (c5_control_sd)^2

# # Critical trial 1
# c5_critical_t1_mean <- 0.1
# c5_critical_t1_sd <- 0.6
# 
# # Critical trial 2
# c5_critical_t2_mean <- -0.3
# c5_critical_t2_sd <- 0.7
# 
# # Combined mean and SD for critical trials
# c5_critical_mean <- mean(c(c5_critical_t1_mean, c5_critical_t2_mean))
# c5_critical_sd <- mean(c(c5_critical_t1_sd, c5_critical_t2_sd))

# Mean and SD for critical trials
c5_critical_mean <- -0.15
c5_critical_sd <- 0.43
c5_critical_variance <- (c5_critical_sd)^2

# t-test to get t-value
#c5_tvalue <- (c5_control_mean - c5_critical_mean)/((c5_control_sd - c5_critical_sd)/sqrt(c5_n_pairs))

#c5_cohensd <- c5_tvalue/sqrt(c5_n_pairs)

c5_m_difference <- c5_control_mean-c5_critical_mean #control - critical because lower scores are better
c5_cohensd <- c5_m_difference/sqrt(mean(c(c5_critical_variance,
                                          c5_control_variance)))

#==== 6. Gabay et al. (2012) - Exp. 1 ====

# Compute effect sizes from F-value and dfs

# Experimental condition: valid
# Control condition: invalid

c6_fvalue <- -6.1 # negative value, because experimental condition is smaller
c6_dfeffect <- 1
c6_dferror <- 28

c6_cohensd <- (c6_fvalue * c6_dfeffect)/(c6_fvalue*c6_dfeffect+c6_dferror)

#==== 7. Gabay et al. (2012) - Exp. 2 ====

# Compute effect sizes from F-value and dfs

# Experimental condition: valid
# Control condition: invalid

c7_fvalue <- -4.4 # negative value, because experimental condition is smaller
c7_dfeffect <- 1
c7_dferror <- 21

c7_cohensd <- (c7_fvalue * c7_dfeffect)/(c7_fvalue*c7_dfeffect+c7_dferror)

#==== 8. Lo e Yeh (2008) - exp. 1 (200 ms) ====

# Cohen's d is given; positive value because the effect is facilitatory

c8_cohensd <- 0.12


#==== 9. Lo e Yeh (2008) - exp. 1 (500 ms) ====

# Cohen's d is given; positive value because the effect is facilitatory

c9_cohensd <- 0.71

#==== 10. Lo e Yeh (2008) - exp. 2 (200 ms) ====

# Cohen's d is given; positive value because the effect is facilitatory

c10_cohensd <- 0.36


#==== 11. Lo e Yeh (2008) - exp. 2 (500 ms) ====

# Cohen's d is given; positive value because the effect is facilitatory

c11_cohensd <- 0.002


#==== 12. Moore and Egeth (1997), exp. 1 ====

# Compute effect size from proportion
c12_pop <- 20
c12_successes <- 85*20/100
c12_pop - c12_successes 

chisquared <- unname(chisq.test(c(c12_successes, c12_failures), 
                                                  p = c(0.5, 0.5))$statistic)

c12_cohensd <- esc_chisq(chisquared, 
                         es.type = "d", 
                         totaln = 20)$es

#==== 13. Moore and Egeth (1997), exp. 3 ====

# Compute effect size from proportion
c13_pop <- 20
c13_successes <- round(95*c13_pop/100)
c13_failures <- c13_pop - c13_successes

chisquared <- unname(chisq.test(c(c13_successes, c13_failures), 
                                p = c(0.5, 0.5))$statistic)

c13_cohensd <- esc_chisq(chisquared, 
                         es.type = "d", 
                         totaln = 20)$es


#==== 14. Moore et al. (2003) ====

# Compute effect sizes from t-value and sample size

c14_tvalue <- 2.72 # positive value, because effect is in the direction of facilitation
c14_n_pairs <- 43

c14_cohensd <- c14_tvalue/sqrt(c14_n_pairs)


#==== 15. Moore et al. (2004) ====

# Compute effect sizes from t-value and sample size

c15_tvalue <- 0.51 # positive value, because effect is in the direction of facilitation
c15_n_pairs <- 25

c15_cohensd <- c15_tvalue/sqrt(c15_n_pairs)


#==== 16. Most et al. (2005) ====

# Compute effect sizes from t-value and sample size

c16_tvalue <- 2.90 # positive value, because effect is in the direction of facilitation
c16_n_pairs <- 181

c16_cohensd <- c16_tvalue/sqrt(c16_n_pairs)


#==== 17. Pitts et al. (2011) ==== FOR NOW, DO NOT USE

# Compute effect sizes from F-value and p-value?
# 
#  Compute effect sizes from mean, sds and N

# c16_1_n <- 16
# c16_2_n <- 16
# 
# # Mean and SD for group 1 (unaware)
# c16_1_mean <- 609
# c16_1_sd <- 49
# 
# # Mean and SD for group 2 (aware)
# c16_2_mean <- 618
# c16_2_sd <- 56
# 
# c16_cohensd <- esc_mean_sd(c16_1_mean, c16_1_sd, c16_1_n,
#                            c16_2_mean, c16_2_sd, c16_2_n,
#                            es.type = "d")$es


#==== 17. Razpurker-Apfeld et al. (2008) - columns/rows, RT ====

# Compute effect sizes from F-value and dfs

c17_fvalue <- 5.41 # negative value, because experimental condition is smaller
c17_dfeffect <- 1
c17_dferror <- 13

c17_cohensd <- (c17_fvalue * c17_dfeffect)/(c17_fvalue*c17_dfeffect+c17_dferror)


#==== 18. Razpurker-Apfeld et al. (2008) - columns/rows, d' ====

# Compute effect sizes from F-value and dfs

c18_fvalue <- 4.98 # positive value, because effect is facilitatory
c18_dfeffect <- 1
c18_dferror <- 13

c18_cohensd <- (c18_fvalue * c18_dfeffect)/(c18_fvalue*c18_dfeffect+c18_dferror)


#==== 19. Razpurker-Apfeld et al. (2008) - triangle/arrow, RT ====

# Compute effect sizes from F-value and dfs

c19_fvalue <- 0.001 # negative value, because experimental condition is smaller
c19_dfeffect <- 1
c19_dferror <- 13

c19_cohensd <- (c19_fvalue * c19_dfeffect)/(c19_fvalue*c19_dfeffect+c19_dferror)


#==== 20. Razpurker-Apfeld et al. (2008) - triangle/arrow, d' ====

# Compute effect sizes from F-value and dfs

c20_fvalue <- 2.3 # positive value, because effect is facilitatory
c20_dfeffect <- 1
c20_dferror <- 13

c20_cohensd <- (c20_fvalue * c20_dfeffect)/(c20_fvalue*c20_dfeffect+c20_dferror)

#==== 21. Richards et al. (2012), tracking task  ====

# Compute effect sizes from proportions
c21_pop <- 25+29
c21_successes <- 25
c21_failures <- c21_pop - c21_successes

c21_chisquared <- unname(chisq.test(c(c21_successes, c21_failures), 
                                    p = c(0.5, 0.5))$statistic)
c21_cohensd <- (-1) * esc_chisq(c21_chisquared, 
                                es.type = "d", 
                                totaln = c21_pop)$es #negative, because effect is not facilitatory


#==== 22. Russell et al. (2008) - exp. 1, acc ====

# Compute effect sizes from F-value and dfs

c22_fvalue <- 23.70 # positive value, because effect is facilitatory
c22_dfeffect <- 1
c22_dferror <- 24

c22_cohensd <- (c22_fvalue * c22_dfeffect)/(c22_fvalue*c22_dfeffect+c22_dferror)

#==== 23. Russell et al. (2008) - exp. 1, RT ====

# Compute effect sizes from F-value and dfs

c23_fvalue <- 3.9 # positive value, because effect is facilitatory
c23_dfeffect <- 1
c23_dferror <- 24

c23_cohensd <- (c23_fvalue * c23_dfeffect)/(c23_fvalue*c23_dfeffect+c23_dferror)


#==== 24. Russell et al. (2008) - exp. 2, acc ====

# Compute effect sizes from F-value and dfs

c24_fvalue <- 39.20 # positive value, because effect is facilitatory
c24_dfeffect <- 1
c24_dferror <- 27

c24_cohensd <- (c24_fvalue * c24_dfeffect)/(c24_fvalue*c24_dfeffect+c24_dferror)


#==== 25. Russell et al. (2008) - exp. 2, RT ====

# Compute effect sizes from F-value and dfs

c25_fvalue <- 2.39 # positive value, because effect is facilitatory
c25_dfeffect <- 1
c25_dferror <- 27

c25_cohensd <- (c25_fvalue * c25_dfeffect)/(c25_fvalue*c25_dfeffect+c25_dferror)


#==== 26. Russell et al. (2008) - exp. 3, acc ====

# Compute effect sizes from F-value and dfs

c26_fvalue <- 7.7 # positive value, because effect is facilitatory
c26_dfeffect <- 1
c26_dferror <- 23

c26_cohensd <- (c26_fvalue * c26_dfeffect)/(c26_fvalue*c26_dfeffect+c26_dferror)

#==== 27. Russell et al. (2008) - exp. 3, RT ====

# Compute effect sizes from F-value and dfs

c27_fvalue <- 3.1 # positive value, because effect is facilitatory
c27_dfeffect <- 1
c27_dferror <- 23

c27_cohensd <- (c27_fvalue * c27_dfeffect)/(c27_fvalue*c27_dfeffect+c27_dferror)


#==== 28. Russell et al. (2008) - exp. 4A, acc ====

# Compute effect sizes from F-value and dfs

c28_fvalue <- 37.10 # positive value, because effect is facilitatory
c28_dfeffect <- 1
c28_dferror <- 19

c28_cohensd <- (c28_fvalue * c28_dfeffect)/(c28_fvalue*c28_dfeffect+c28_dferror)

#==== 29. Russell et al. (2008) - exp. 4A, RT ====

# Compute effect sizes from F-value and dfs

c29_fvalue <- ? # positive value, because effect is facilitatory
c29_dfeffect <- ?
c29_dferror <- ?

c29_cohensd <- NA #(c29_fvalue * c29_dfeffect)/(c29_fvalue*c29_dfeffect+c29_dferror)

#==== 30. Russell et al. (2008) - exp. 4B, acc ====

# Compute effect sizes from F-value and dfs

c30_fvalue <- 34.08 # positive value, because effect is facilitatory
c30_dfeffect <- 1
c30_dferror <- 20

c30_cohensd <- (c30_fvalue * c30_dfeffect)/(c30_fvalue*c30_dfeffect+c30_dferror)


#==== 31. Russell et al. (2008) - exp. 4B, RT ====

# Compute effect sizes from F-value and dfs

c31_fvalue <- ? # positive value, because effect is facilitatory
c31_dfeffect <- ?
c31_dferror <- ?
  
c31_cohensd <- NA #(c31_fvalue * c31_dfeffect)/(c31_fvalue*c31_dfeffect+c31_dferror)


#==== 32. Russell et al. (2008) - exp. 5, acc ====

# Compute effect sizes from F-value and dfs

c32_fvalue <- 4.3 # positive value, because effect is facilitatory
c32_dfeffect <- 1
c32_dferror <- 23

c32_cohensd <- (c32_fvalue * c32_dfeffect)/(c32_fvalue*c32_dfeffect+c32_dferror)


#==== 33. Russell et al. (2008) - exp. 5, RT ====

# Compute effect sizes from F-value and dfs

c33_fvalue <- 1.83 # positive value, because effect is facilitatory
c33_dfeffect <- 1
c33_dferror <- 23
  
c33_cohensd <- (c33_fvalue * c33_dfeffect)/(c33_fvalue*c33_dfeffect+c33_dferror)


#==== 34. Shafto and Pitts (2015) ====

# Compute effect sizes from t-value and sample size

c34_tvalue <- -1.57 #negative value, because the control condition is larger (effect is also non-significant)
c34_n_pairs <- 14

c34_cohensd <- c34_tvalue/sqrt(c34_n_pairs)


#==== 35. Schnuerch et al. (2016) - exp. 1 ====

# Compute effect sizes from t-value and sample size

c35_tvalue <- 3.15 # positive value, because the effect is facilitatory
c35_n_pairs <- 60

c35_cohensd <- c35_tvalue/sqrt(c35_n_pairs)

# Computed Cohen's d is different from reported Cohen's d (0.43)


#==== 36. Schnuerch et al. (2016) - exp. 2 ====

# Compute effect sizes from t-value and sample size

c36_tvalue <- 2.68 # positive value, because the effect is facilitatory
c36_n_pairs <- 57

c36_cohensd <- c36_tvalue/sqrt(c36_n_pairs)


#==== 37. Scholte et al. (2006) ====

#==== 38. Vandenbroucke et al. (2014) ====

#==== 39. Wiemer et al. (2013) ====

# Compute effect sizes from t-value and sample size

c37_tvalue <- 3.27 # positive value, because the effect is facilitatory

c37_m1 <- -0.14
c37_m2 <- -0.71
c37_SD1 <- 0.76
c37_SD2 <- 0.47
c37_n1 <- 60
c37_n2 <- 60

#c37_cohensd <- c37_tvalue*sqrt(c37_n1+c37_n2)/sqrt(c37_n1*c37_n2)
c37_cohensd <- abs(c37_m1 - c37_m2)/sqrt((((c37_n1-1)*c37_SD1**2)+
                                            ((c37_n2-1)*c37_SD2**2))/(c37_n1+c37_n2))


#==== 40. Mack and Rock. exp 1. (2000) ====

#==== 41. Mack and Rock. exp 2. (2000) ====
c41_pop <- 41
c41_successes <- 13
c41_failures <- c41_pop - c41_successes


c41_chisquared <- unname(chisq.test(c(c41_successes, c41_failures), 
                                    p = c(0.5, 0.5))$statistic)

c41_cohensd <- esc_chisq(c41_chisquared,
                         es.type = "d",
                         totaln = c41_pop)$es

#==== 42. Mack and Rock. exp 3. (2000) ====
c42_pop <- 21
c42_successes <- 6
c42_failures <- c42_pop - c42_successes


c42_chisquared <- unname(chisq.test(c(c42_successes, c42_failures), 
                                    p = c(0.5, 0.5))$statistic)

c42_cohensd <- esc_chisq(c42_chisquared,
                         es.type = "d",
                         totaln = c42_pop)$es


#==== 43. Mack and Rock. exp 4 (2000) ====
c43_pop <- 29
c43_successes <- 14
c43_failures <- c43_pop - c43_successes


c43_chisquared <- unname(chisq.test(c(c43_successes, c43_failures), 
                                    p = c(0.5, 0.5))$statistic)

c43_cohensd <- esc_chisq(c43_chisquared,
                         es.type = "d",
                         totaln = c43_pop)$es

#==== 44. Mack and Rock. exp 5 (2000) ====
c44_pop <- 10+9
c44_successes <- 5+3
c44_failures <- c44_pop - c44_successes


c44_chisquared <- unname(chisq.test(c(c44_successes, c44_failures), 
                                    p = c(0.20, 0.80))$statistic)

c44_cohensd <- esc_chisq(c44_chisquared,
                         es.type = "d",
                         totaln = c44_pop)$es

#========================================#
#==== Build vector with effect sizes ====#
#========================================#

cohensd_names <- paste("c", c(1:28, 30, 32:36), "_cohensd",
                 sep = "")

cohensd <- map_dbl(cohensd_names, get)

