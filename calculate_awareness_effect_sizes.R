
library(esc)
library(tidyverse)

# Effect sizes:

# Formulas are taken from Lakens et al.'s sheet for effect sizes


#==== 1. Ariga et al. (2007) exp 2 ====

# Compute effect size from proportion

# c1_successes <- 45*20/100
# c1_pop <- 20
# 
# chisquared <- prop.test(c1_successes, 
#                         c1_pop,
#                         p = 0.5, 
#                         alternative = 'two.sided')$statistic
# 
# c1_cohensd <- esc_chisq(chisquared, 
#                         es.type = "d", 
#                         totaln = 20)$es

#=================================================================#
c1_pop <- 20
c1_successes <- round(45*c1_pop/100)
c1_failures <- c1_pop - c1_successes


c1_chisquared <- unname(chisq.test(c(c1_successes, c1_failures), 
                            p = c(0.5, 0.5))$statistic)

c1_cohensd <- esc_chisq(c1_chisquared,
                        es.type = "d",
                        totaln = c1_pop)$es

#==== 2 Beanland and Pammer (2010) exp. 1A, fixating ====

# Compute effect size from proportion

c2_successes <- 20*20/100
c2_pop <- 72

chisquared <- prop.test(c2_successes, 
                        c2_pop,
                        p = 0.5, 
                        alternative = 'two.sided')$statistic

c2_cohensd <- esc_chisq(chisquared, 
                        es.type = "d", 
                        totaln = 20)$es

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

# Compute effect size from proportion using n of noticers

c6_successes <- 12
c6_pop <- 30

chisquared <- prop.test(c6_successes, 
                        c6_pop,
                        p = 0.5, 
                        alternative = 'two.sided')$statistic

c6_cohensd <- esc_chisq(chisquared, 
                        es.type = "d", 
                        totaln = c6_pop)$es


#==== 7. Gabay et al. (2012) - Exp. 2 ====

# Compute effect size from proportion using n of noticers

c7_successes <- 13
c7_pop <- 23

chisquared <- prop.test(c7_successes, 
                        c7_pop,
                        p = 0.5, 
                        alternative = 'two.sided')$statistic

c7_cohensd <- esc_chisq(chisquared, 
                        es.type = "d", 
                        totaln = c7_pop)$es


#==== 8. Lo e Yeh (2008) - exp. 1 (200 ms) ====

# Compute effect size from proportion using given chi-squared value

c8_pop <- 43
c8_chisquared <- 1.88

c8_cohensd <- esc_chisq(c8_chisquared, 
                        es.type = "d", 
                        totaln = c8_pop)$es


#==== 9. Lo e Yeh (2008) - exp. 1 (500 ms) ====

# Compute effect size from proportion using given chi-squared value

c9_pop <- 41
c9_chisquared <- 0.02

c9_cohensd <- esc_chisq(c9_chisquared, 
                        es.type = "d", 
                        totaln = c9_pop)$es


#==== 10. Lo e Yeh (2008) - exp. 2 (200 ms) ====

# Compute effect size from proportion using chi-squared value
c10_pop <- 23
c10_chisquared <- 0.04

c10_cohensd <- esc_chisq(c10_chisquared, 
                        es.type = "d", 
                        totaln = c10_pop)$es

#==== 11. Lo e Yeh (2008) - exp. 2 (500 ms) ====

# Compute effect size from proportion using chi-squared value
c11_pop <- 25
c11_chisquared <- 0.36

c11_cohensd <- esc_chisq(c11_chisquared, 
                         es.type = "d", 
                         totaln = c11_pop)$es


#==== 12. Moore and Egeth (1997), exp. 1 ====

# Compute effect size from percentage given

c12_pop <- 20
c12_successes <- round(10*c12_pop/100)
c12_failures <- c12_pop - c12_successes


c12_chisquared <- unname(chisq.test(c(c12_successes, c12_failures), 
                                   p = c(0.5, 0.5))$statistic)

c12_cohensd <- esc_chisq(c12_chisquared,
                        es.type = "d",
                        totaln = c12_pop)$es

#==== 13. Moore and Egeth (1997), exp. 3 ====

# Compute effect size from proportion

c13_pop <- 20
c13_successes <- round(0*c13_pop/100)
c13_failures <- c13_pop - c13_successes

c13_chisquared <- unname(chisq.test(c(c13_successes, c13_failures), 
                                    p = c(0.5, 0.5))$statistic)

c13_cohensd <- esc_chisq(c13_chisquared,
                         es.type = "d",
                         totaln = c13_pop)$es

#==== 14. Moore et al. (2003) ====

# Compute effect sizes from reported percentage of noticers

c14_pop <- 44
c14_successes <- round(54.55*c14_pop/100)
c14_failures <- c14_pop - c14_successes

c14_chisquared <- unname(chisq.test(c(c14_successes, c14_failures), 
                                    p = c(0.5, 0.5))$statistic)

c14_cohensd <- esc_chisq(c14_chisquared,
                         es.type = "d",
                         totaln = c14_pop)$es


#==== 15. Moore et al. (2004) ====

# Compute effect sizes from reported percentage of noticers

c15_pop <- 25
c15_successes <- round(16*c15_pop/100)
c15_failures <- c15_pop - c15_successes

c15_chisquared <- unname(chisq.test(c(c15_successes, c15_failures), 
                                    p = c(0.5, 0.5))$statistic)

c15_cohensd <- esc_chisq(c15_chisquared,
                         es.type = "d",
                         totaln = c15_pop)$es

#==== 16. Most et al. (2005) ====

# Compute effect sizes from rates of noticing for individual experiments 1-7 and pool them

# Exp. 1
c16_1_pop <- 65
c16_1_successes <- 16+16
c16_1_failures <- c16_1_pop - c16_1_successes

# Exp. 2
c16_2_pop <- 28
c16_2_successes <- round(7*(c16_2_pop/2)/100) + # condition 1 (attend squares)
  round(86*(c16_2_pop/2)/100) # condition 2 (attend circles)
c16_2_failures <- c16_2_pop - c16_2_successes

# Exp. 3
c16_3_pop <- 102
c16_3_successes <- round(68*25/100) + # caucasian US x caucasian target
  round(40*25/100) + # caucasian US x african american target
  round(56*25/100) + # african american US x caucasian target
  round(81*27/100) # african american US x african american target
c16_3_failures <- c16_3_pop - c16_3_successes

# Exp. 4
c16_4_pop <- 43
c16_4_successes <- round(68*22/100) + # white triangle US
  round(38*21/100) # black triangle US
c16_4_failures <- c16_4_pop - c16_4_successes

# Exp. 5
c16_5_pop <- 44
c16_5_successes <- round(36*22/100) + # gradual onset US
  round(41*21/100) # sudden onset US
c16_5_failures <- c16_5_pop - c16_5_successes

# Exp. 6
c16_6_pop <- 45
c16_6_successes <- round(23*22/100) + # gradual onset US
  round(43*23/100) # sudden onset US
c16_6_failures <- c16_6_pop - c16_6_successes

# Exp. 7
c16_7_pop <- 43
c16_7_successes <- round(67*21/100) + # gradual onset US
  round(50*22/100) # sudden onset US
c16_7_failures <- c16_7_pop - c16_7_successes


# Exps 1-7 pooled
c16_pop <- do.call(sum, mget(paste('c16', c(1:7), 'pop', sep = '_')))
c16_successes <- do.call(sum, mget(paste('c16', c(1:7), 'successes', sep = '_')))
c16_failures <- c16_pop - c16_successes


c16_chisquared <- unname(chisq.test(c(c16_successes, c16_failures), 
                                    p = c(0.5, 0.5))$statistic)

c16_cohensd <- esc_chisq(c16_chisquared,
                         es.type = "d",
                         totaln = c16_pop)$es


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

# Compute effect sizes from reported percentage of noticers
c17_pop <- 7
c17_successes <- 1+ # columns/rows condition
  1 # triangle/arrow condition
c17_failures <- c17_pop - c17_successes

c17_chisquared <- unname(chisq.test(c(c17_successes, c17_failures), 
                                    p = c(0.5, 0.5))$statistic)

c17_cohensd <- esc_chisq(c17_chisquared,
                         es.type = "d",
                         totaln = c17_pop)$es


#==== 18. Razpurker-Apfeld et al. (2008) - columns/rows, d' ====

# Effect size is the same as for RT
c18_cohensd <- c17_cohensd


#==== 19. Razpurker-Apfeld et al. (2008) - triangle/arrow, RT ====

# Compute effect sizes from reported percentage of noticers
c19_pop <- 7
c19_successes <- 1
c19_failures <- c19_pop - c19_successes

c19_chisquared <- unname(chisq.test(c(c19_successes, c19_failures), 
                                    p = c(0.5, 0.5))$statistic)

c19_cohensd <- esc_chisq(c19_chisquared,
                         es.type = "d",
                         totaln = c19_pop)$es

#==== 20. Razpurker-Apfeld et al. (2008) - triangle/arrow, d' ====

# Effect size is the same as for RT
c20_cohensd <- c19_cohensd

#==== 21. Richards et al. (2012), tracking task  ====

# Compute effect sizes from proportions
c21_pop <- 131
c21_successes <- round(47*c21_pop/100)
c21_failures <- c21_pop - c21_successes

c21_chisquared <- unname(chisq.test(c(c21_successes, c21_failures), 
                                    p = c(0.5, 0.5))$statistic)
c21_cohensd <- esc_chisq(c21_chisquared, 
                                es.type = "d", 
                                totaln = c21_pop)$es


#==== 22. Russell et al. (2008) - exp. 1, acc ====

# Compute effect sizes from proportions
c22_pop <- 25
c22_successes <- 12
c22_failures <- c22_pop - c22_successes

c22_chisquared <- unname(chisq.test(c(c22_successes, c22_failures), 
                                    p = c(0.5, 0.5))$statistic)
c22_cohensd <- esc_chisq(c22_chisquared, 
                                es.type = "d", 
                                totaln = c22_pop)$es

#==== 23. Russell et al. (2008) - exp. 1, RT ====

# Effect size is the same as for acc
c23_cohensd <- c22_cohensd


#==== 24. Russell et al. (2008) - exp. 2, acc ====

# Compute effect sizes from proportions
c24_pop <- 28
c24_successes <- 16
c24_failures <- c24_pop - c24_successes

c24_chisquared <- unname(chisq.test(c(c24_successes, c24_failures), 
                                    p = c(0.5, 0.5))$statistic)
c24_cohensd <- esc_chisq(c24_chisquared, 
                         es.type = "d", 
                         totaln = c24_pop)$es


#==== 25. Russell et al. (2008) - exp. 2, RT ====

# Effect size is the same as for acc
c25_cohensd <- c24_cohensd


#==== 26. Russell et al. (2008) - exp. 3, acc ====

# Compute effect sizes from proportions
c26_pop <- 24
c26_successes <- 12
c26_failures <- c26_pop - c26_successes

c26_chisquared <- unname(chisq.test(c(c26_successes, c26_failures), 
                                    p = c(0.5, 0.5))$statistic)
c26_cohensd <- esc_chisq(c26_chisquared, 
                         es.type = "d", 
                         totaln = c26_pop)$es

#==== 27. Russell et al. (2008) - exp. 3, RT ====

# Effect size is the same as for acc
c27_cohensd <- c26_cohensd


#==== 28. Russell et al. (2008) - exp. 4A, acc ====

# Compute effect sizes from proportions
c28_pop <- 20
c28_successes <- 5
c28_failures <- c28_pop - c28_successes

c28_chisquared <- unname(chisq.test(c(c28_successes, c28_failures), 
                                    p = c(0.5, 0.5))$statistic)
c28_cohensd <- esc_chisq(c28_chisquared, 
                         es.type = "d", 
                         totaln = c28_pop)$es


#==== 29. Russell et al. (2008) - exp. 4A, RT ====

# Effect size is the same as for acc
c29_cohensd <- c28_cohensd

#==== 30. Russell et al. (2008) - exp. 4B, acc ====

# Compute effect sizes from proportions
c30_pop <- 20
c30_successes <- 5
c30_failures <- c30_pop - c30_successes

c30_chisquared <- unname(chisq.test(c(c30_successes, c30_failures), 
                                    p = c(0.5, 0.5))$statistic)
c30_cohensd <- esc_chisq(c30_chisquared, 
                         es.type = "d", 
                         totaln = c30_pop)$es


#==== 31. Russell et al. (2008) - exp. 4B, RT ====

# Effect size is the same as for acc
c31_cohensd <- c30_cohensd


#==== 32. Russell et al. (2008) - exp. 5, acc ====

# Compute effect sizes from proportions
c32_pop <- 22
c32_successes <- 18
c32_failures <- c32_pop - c32_successes

c32_chisquared <- unname(chisq.test(c(c32_successes, c32_failures), 
                                    p = c(0.5, 0.5))$statistic)
c32_cohensd <- esc_chisq(c32_chisquared, 
                         es.type = "d", 
                         totaln = c32_pop)$es


#==== 33. Russell et al. (2008) - exp. 5, RT ====

# Effect size is the same as for acc
c33_cohensd <- c32_cohensd


#==== 34. Shafto and Pitts (2015) ====

# Compute effect sizes from proportions
c34_pop <- 30
c34_successes <- 15
c34_failures <- c34_pop - c34_successes

c34_chisquared <- unname(chisq.test(c(c34_successes, c34_failures), 
                                    p = c(0.5, 0.5))$statistic)
c34_cohensd <- esc_chisq(c34_chisquared, 
                         es.type = "d", 
                         totaln = c34_pop)$es


#==== 35. Schnuerch et al. (2016) - exp. 1 ====

# Compute effect sizes from proportions
c35_pop <- 61
c35_successes <- round(8*c21_pop/100)
c35_failures <- c35_pop - c35_successes

c35_chisquared <- unname(chisq.test(c(c35_successes, c35_failures), 
                                    p = c(0.5, 0.5))$statistic)
c35_cohensd <- esc_chisq(c35_chisquared, 
                         es.type = "d", 
                         totaln = c35_pop)$es


#==== 36. Schnuerch et al. (2016) - exp. 2 ====

# Compute effect sizes from proportions
c36_pop <- 58
c36_successes <- round(6*c21_pop/100)
c36_failures <- c36_pop - c36_successes

c36_chisquared <- unname(chisq.test(c(c36_successes, c36_failures), 
                                    p = c(0.5, 0.5))$statistic)
c36_cohensd <- esc_chisq(c36_chisquared, 
                         es.type = "d", 
                         totaln = c36_pop)$es


#==== 37. Scholte et al. (2006) ====

#==== 38. Vandenbroucke et al. (2014) ====

#==== 39. Wiemer et al. (2013) ====

# Compute effect sizes from proportions
c37_pop <- 120
c37_successes <- round(58.3*(c21_pop/2)/100) + # flower us
  round(51.7*(c21_pop/2)/100) # spider us
c37_failures <- c37_pop - c37_successes

c37_chisquared <- unname(chisq.test(c(c37_successes, c37_failures), 
                                    p = c(0.5, 0.5))$statistic)
c37_cohensd <- esc_chisq(c37_chisquared, 
                         es.type = "d", 
                         totaln = c37_pop)$es

#==== 40. Mack and Rock. exp 1 (2000) ====
c40_pop <- 80
c40_successes <- 30
c40_failures <- c40_pop - c40_successes


c40_chisquared <- unname(chisq.test(c(c40_successes, c40_failures), 
                                   p = c(0.5, 0.5))$statistic)

c40_cohensd <- esc_chisq(c40_chisquared,
                        es.type = "d",
                        totaln = c40_pop)$es

#==== 41. Mack and Rock. exp 2 (2000) ====
c41_pop <- 75
c41_successes <- 34
c41_failures <- c41_pop - c41_successes


c41_chisquared <- unname(chisq.test(c(c41_successes, c41_failures), 
                                    p = c(0.5, 0.5))$statistic)

c41_cohensd <- esc_chisq(c41_chisquared,
                         es.type = "d",
                         totaln = c41_pop)$es

#==== 42. Mack and Rock. exp 3 (2000) ====
c42_pop <- 30
c42_successes <- 9
c42_failures <- c42_pop - c42_successes


c42_chisquared <- unname(chisq.test(c(c42_successes, c42_failures), 
                                    p = c(0.5, 0.5))$statistic)

c42_cohensd <- esc_chisq(c42_chisquared,
                         es.type = "d",
                         totaln = c42_pop)$es

#==== 44. Mack and Rock. exp 4 (2000) ====
c44_pop <- 20+20
c44_successes <- 10+11
c44_failures <- c44_pop - c44_successes


c44_chisquared <- unname(chisq.test(c(c44_successes, c44_failures), 
                                    p = c(0.5, 0.5))$statistic)

c44_cohensd <- esc_chisq(c44_chisquared,
                         es.type = "d",
                         totaln = c44_pop)$es

#========================================#
#==== Build vector with effect sizes ====#
#========================================#

awareness_cohensd_names <- paste("c", c(1:28, 30, 32:36, 40:44), "_cohensd",
                       sep = "")

awareness_cohensd <- map_dbl(awareness_cohensd_names, get)

