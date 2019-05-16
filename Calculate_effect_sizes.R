
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

# t-test to get t-value
c2_tvalue <- (c2_control_mean - c2_critical_mean)/
              ((c2_control_sd - c2_critical_sd)/sqrt(c2_n_pairs))

c2_cohensd <- c2_tvalue/sqrt(c2_n_pairs)
  

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

# t-test to get t-value
c3_tvalue <- (c3_control_mean - c3_critical_mean)/
              ((c3_control_sd - c3_critical_sd)/sqrt(c3_n_pairs))

c3_cohensd <- c3_tvalue/sqrt(c3_n_pairs)


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

# t-test to get t-value
c4_tvalue <- (c4_control_mean - c4_critical_mean)/((c4_control_sd - c4_critical_sd)/sqrt(c4_n_pairs))

c4_cohensd <- c4_tvalue/sqrt(c4_n_pairs)

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

# t-test to get t-value
c5_tvalue <- (c5_control_mean - c5_critical_mean)/((c5_control_sd - c5_critical_sd)/sqrt(c5_n_pairs))

c5_cohensd <- c5_tvalue/sqrt(c5_n_pairs)

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

c12_successes <- 85*20/100
c12_pop <- 20

chisquared <- prop.test(c12_successes, c12_pop,p=0.5, alternative = 'two.sided')$statistic

c12_cohensd <- esc_chisq(chisquared, es.type = "d", totaln = 20)$es

#==== 13. Moore and Egeth (1997), exp. 3 ====

# Compute effect size from proportion

c13_successes <- 95*20/100
c13_pop <- 20

chisquared <- prop.test(c13_successes, c13_pop,p=0.5, alternative = 'two.sided')$statistic

c13_cohensd <- esc_chisq(chisquared, es.type = "d", totaln = 20)$es


#==== 14. Moore et al. (2003) ====

# Compute effect sizes from t-value and sample size

c13_tvalue <- 2.72 # positive value, because effect is in the direction of facilitation
c13_n_pairs <- 43

c13_cohensd <- c13_tvalue/sqrt(c13_n_pairs)


#==== 15. Moore et al. (2004) ====

# Compute effect sizes from t-value and sample size

c14_tvalue <- 0.51 # positive value, because effect is in the direction of facilitation
c14_n_pairs <- 24

c14_cohensd <- c14_tvalue/sqrt(c14_n_pairs)


#==== 15. Most et al. (2005) ====

# Compute effect sizes from t-value and sample size

c15_tvalue <- 2.90 # positive value, because effect is in the direction of facilitation
c15_n_pairs <- 181

c15_cohensd <- c15_tvalue/sqrt(c15_n_pairs)


#==== 15. Pitts et al. (2011) ==== FOR NOW, DO NOT USE

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


#==== 16. Razpurker-Apfeld et al. (2008) - columns/rows, RT ====

# Compute effect sizes from F-value and dfs

c16_fvalue <- 5.41 # negative value, because experimental condition is smaller
c16_dfeffect <- 1
c16_dferror <- 13

c16_cohensd <- (c16_fvalue * c16_dfeffect)/(c16_fvalue*c16_dfeffect+c16_dferror)


#==== 17. Razpurker-Apfeld et al. (2008) - columns/rows, d' ====

# Compute effect sizes from F-value and dfs

c17_fvalue <- 4.98 # positive value, because effect is facilitatory
c17_dfeffect <- 1
c17_dferror <- 13

c17_cohensd <- (c17_fvalue * c17_dfeffect)/(c17_fvalue*c17_dfeffect+c17_dferror)


#==== 18. Razpurker-Apfeld et al. (2008) - triangle/arrow, RT ====

# Compute effect sizes from F-value and dfs

c18_fvalue <- 0.001 # negative value, because experimental condition is smaller
c18_dfeffect <- 1
c18_dferror <- 13

c18_cohensd <- (c18_fvalue * c18_dfeffect)/(c18_fvalue*c18_dfeffect+c18_dferror)


#==== 19. Razpurker-Apfeld et al. (2008) - triangle/arrow, d' ====

# Compute effect sizes from F-value and dfs

c19_fvalue <- 2.3 # positive value, because effect is facilitatory
c19_dfeffect <- 1
c19_dferror <- 13

c19_cohensd <- (c19_fvalue * c19_dfeffect)/(c19_fvalue*c19_dfeffect+c19_dferror)

#==== 20. Richards et al. (2012), tracking task  ====

# Compute effect sizes from proportions

c20_successes <- 25
c20_pop <- 25+29

c20_chisquared <- prop.test(c20_successes, c20_pop,p=0.5, alternative = 'greater')$statistic

c20_cohensd <- (-1) * esc_chisq(c20_chisquared, es.type = "d", totaln = 25+29)$es #negative, because effect is not facilitatory


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

cohensd_names <- paste("c", c(1:20, 22:28, 30, 32:37), "_cohensd",
                 sep = "")

cohensd <- map_dbl(cohensd_names, get)
