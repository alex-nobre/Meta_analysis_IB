
library(esc)
library(tidyverse)
library(DescTools)

# Effect sizes:

# Formulas are taken from Lakens et al.'s sheet for effect sizes


#==== 1. Ariga et al. (2007) exp 2 ====

c1_pop <- 20
c1_success_percentage <- 50
c1_success_proportion <- c1_success_percentage/100
c1_base_chance <- 0.5
c1_successes <- round(c1_success_percentage*c1_pop/100)
c1_failures <- c1_pop - c1_successes

c1_chisquared <- unname(chisq.test(c(c1_successes, c1_failures), 
                                   p = c(0.5, 0.5))$statistic)

c1_cohensd <- esc_chisq(c1_chisquared,
                        es.type = "d",
                        totaln = c1_pop)$es

# c1_etasquared <- esc_chisq(c1_chisquared, 
#                             es.type = "eta", 
#                             totaln = c1_pop)$es

c1_phi <- sqrt(c1_chisquared/c1_pop)

c1_r <- c1_phi #esc_phi(phi = c1_phi, totaln = c1_pop, es.type = "r")


#==== 2 Beanland and Pammer (2010) exp. 1A, fixating ====

# Compute effect size from percentage of noticers
c2_pop <- 36
c2_success_percentage <- 25
c2_success_proportion <- c2_success_percentage/100
c2_base_chance <- 0.5
c2_successes <- round(c2_success_percentage*c2_pop/100)
c2_failures <- c2_pop - c2_successes

c2_chisquared <- unname(chisq.test(c(c2_successes, c2_failures), 
                                   p = c(0.5, 0.5))$statistic)

c2_cohensd <- (-1) * esc_chisq(c2_chisquared, # ES is negative
                               es.type = "d",
                               totaln = c2_pop)$es

# c2_etasquared <- esc_chisq(c2_chisquared, 
#                             es.type = "eta", 
#                             totaln = c2_pop)$es

c2_phi <- sqrt(c2_chisquared/c2_pop) * (-1) # effect size is negative

c2_r <- c2_phi #esc_phi(phi = c2_phi, totaln = c2_pop, es.type = "r")


#==== 3. Beanland and Pammer (2010) exp. 1A, moving ====

# Compute effect size from percentage of noticers
c3_pop <- 36
c3_success_percentage <- 14
c3_success_proportion <- c3_success_percentage/100
c3_base_chance <- 0.5
c3_successes <- round(c3_success_percentage*c3_pop/100)
c3_failures <- c3_pop - c3_successes

c3_chisquared <- unname(chisq.test(c(c3_successes, c3_failures), 
                                   p = c(0.5, 0.5))$statistic)

c3_cohensd <- (-1) * esc_chisq(c3_chisquared, # ES is negative
                               es.type = "d",
                               totaln = c3_pop)$es

# c3_etasquared <- esc_chisq(c3_chisquared, 
#                             es.type = "eta", 
#                             totaln = c3_pop)$es

c3_phi <- sqrt(c3_chisquared/c3_pop) * (-1) # effect size is negative

c3_r <- c3_phi #esc_phi(phi = c3_phi, totaln = c3_pop, es.type = "r")


#==== 4. Beanland and Pammer (2010) exp. 2, slow US ====

# Compute effect size from percentage of noticers
c4_pop <- 25
c4_success_percentage <- 14
c4_success_proportion <- c4_success_percentage/100
c4_base_chance <- 0.5
c4_successes <- round(c4_success_percentage*c4_pop/100)
c4_failures <- c4_pop - c4_successes

c4_chisquared <- unname(chisq.test(c(c4_successes, c4_failures), 
                                   p = c(0.5, 0.5))$statistic)


c4_cohensd <- (-1) * esc_chisq(c4_chisquared, # ES is negative
                               es.type = "d",
                               totaln = c4_pop)$es

# c4_etasquared <- esc_chisq(c4_chisquared, 
#                             es.type = "eta", 
#                             totaln = c4_pop)$es

c4_phi <- sqrt(c4_chisquared/c4_pop) * (-1) # effect size is negative

c4_r <- c4_phi #esc_phi(phi = c4_phi, totaln = c4_pop, es.type = "r")


#==== 5. Beanland and Pammer (2010) exp. 2, fast US ====

# Compute effect size from proportion
c5_pop <- 25
c5_success_percentage <- 48
c5_success_proportion <- c5_success_percentage/100
c5_base_chance <- 0.5
c5_successes <- round(c5_success_percentage*c5_pop/100)
c5_failures <- c5_pop - c5_successes


c5_chisquared <- unname(chisq.test(c(c5_successes, c5_failures), 
                                   p = c(0.5, 0.5))$statistic)

c5_cohensd <- (-1) * esc_chisq(c5_chisquared, # ES is negative
                               es.type = "d",
                               totaln = c5_pop)$es

# c5_etasquared <- esc_chisq(c5_chisquared, 
#                             es.type = "eta", 
#                             totaln = c5_pop)$es

c5_phi <- sqrt(c5_chisquared/c5_pop) * (-1) # effect size is negative

c5_r <- c5_phi #esc_phi(phi = c5_phi, totaln = c5_pop, es.type = "r")


#==== 6. Gabay et al. (2012) - Exp. 1 ====

# Compute effect size from proportion using n of noticers
c6_pop <- 30
c6_successes <- 12
c6_failures <- c6_pop - c6_successes
c6_success_percentage <- (c6_successes/c6_pop) * 100
c6_success_proportion <- c6_success_percentage/100
c6_base_chance <- 0.5

c6_chisquared <- unname(chisq.test(c(c6_successes, c6_failures), 
                                   p = c(0.5, 0.5))$statistic)

c6_cohensd <- (-1) * esc_chisq(c6_chisquared, # ES is negative
                               es.type = "d",
                               totaln = c6_pop)$es

# c6_etasquared <- esc_chisq(c6_chisquared, 
#                             es.type = "eta", 
#                             totaln = c6_pop)$es

c6_phi <- sqrt(c6_chisquared/c6_pop) * (-1) # effect size is negative

c6_r <- c6_phi #esc_phi(phi = c6_phi, totaln = c6_pop, es.type = "r")


#==== 7. Gabay et al. (2012) - Exp. 2 ====

# Compute effect size from proportion using n of noticers
c7_pop <- 23
c7_successes <- 13
c7_failures <- c7_pop - c7_successes
c7_success_percentage <- (c7_successes/c7_pop) * 100
c7_success_proportion <- c7_success_percentage/100
c7_base_chance <- 0.5

c7_chisquared <- unname(chisq.test(c(c7_successes, c7_failures), 
                                   p = c(0.5, 0.5))$statistic)

c7_cohensd <- esc_chisq(c7_chisquared,
                        es.type = "d",
                        totaln = c7_pop)$es

# c7_etasquared <- esc_chisq(c7_chisquared, 
#                             es.type = "eta", 
#                             totaln = c7_pop)$es

c7_phi <- sqrt(c7_chisquared/c7_pop)

c7_r <- c7_phi #esc_phi(phi = c7_phi, totaln = c7_pop, es.type = "r")


#==== 8. Lo e Yeh (2008) - exp. 1 (200 ms) ====

# Compute effect size from proportion using given chi-squared value
c8_pop <- 43
c8_success_percentage <- 60
c8_success_proportion <- c8_success_percentage/100
c8_base_chance <- 0.5
c8_successes <- round(c8_success_percentage*c8_pop/100)
c8_failures <- c8_pop - c8_successes

c8_chisquared <- 1.88

c8_cohensd <- esc_chisq(c8_chisquared, 
                        es.type = "d", 
                        totaln = c8_pop)$es

c8_r <- sqrt((c8_cohensd**2)/(c8_cohensd**2 + 4))

# c8_phi <- sqrt(c8_chisquared/c8_pop)
# 
# c8_r <- c8_phi - same as through cohen's d

#==== 9. Lo e Yeh (2008) - exp. 1 (500 ms) ====

# Compute effect size from proportion using given chi-squared value

c9_pop <- 41
c9_success_percentage <- 51
c9_success_proportion <- c9_success_percentage/100
c9_base_chance <- 0.5
c9_successes <- round(c9_success_percentage*c9_pop/100)
c9_failures <- c9_pop - c9_successes

c9_chisquared <- 0.02

c9_cohensd <- esc_chisq(c9_chisquared, 
                        es.type = "d", 
                        totaln = c9_pop)$es

c9_r <- sqrt((c9_cohensd**2)/(c9_cohensd**2 + 4))

#==== 10. Lo e Yeh (2008) - exp. 2 (200 ms) ====

# Compute effect size from proportion using chi-squared value
c10_pop <- 23
c10_success_percentage <- 47.83
c10_success_proportion <- c10_success_percentage/100
c10_base_chance <- 0.5
c10_successes <- round(c10_success_percentage*c10_pop/100)
c10_failures <- c10_pop - c10_successes

c10_chisquared <- 0.04

c10_cohensd <- (-1) * esc_chisq(c10_chisquared,  # ES is negative
                                es.type = "d", 
                                totaln = c10_pop)$es

c10_r <- sqrt((c10_cohensd**2)/(c10_cohensd**2 + 4)) * (-1) # is negative

#==== 11. Lo e Yeh (2008) - exp. 2 (500 ms) ====

# Compute effect size from proportion using chi-squared value
c11_pop <- 25
c11_success_percentage <- 44
c11_success_proportion <- c11_success_percentage/100
c11_base_chance <- 0.5
c11_successes <- round(c11_success_percentage*c11_pop/100)
c11_failures <- c11_pop - c11_successes


c11_chisquared <- 0.36

c11_cohensd <- (-1) * esc_chisq(c11_chisquared, # ES is negative
                                es.type = "d", 
                                totaln = c11_pop)$es

c11_r <- sqrt((c11_cohensd**2)/(c11_cohensd**2 + 4)) * (-1) # is negative


#==== 12. Moore and Egeth (1997), exp. 1 ====

# Compute effect size from percentage of noticers reported

c12_pop <- 20
c12_success_percentage <- 10
c12_success_proportion <- c12_success_percentage/100
c12_base_chance <- 0.5 #yes/no
c12_successes <- round(c12_success_percentage*c12_pop/100)
c12_failures <- c12_pop - c12_successes


c12_chisquared <- unname(chisq.test(c(c12_successes, c12_failures), 
                                    p = c(0.5, 0.5))$statistic)

c12_cohensd <- (-1) * esc_chisq(c12_chisquared, # ES is negative
                                es.type = "d",
                                totaln = c12_pop)$es

# c12_etasquared <- esc_chisq(c12_chisquared, 
#                             es.type = "eta", 
#                             totaln = c12_pop)$es

c12_phi <- sqrt(c12_chisquared/c12_pop) * (-1) #effect size is negative

#c12_r <- c12_phi #esc_phi(phi = c12_phi, totaln = c12_pop, es.type = "r")

c12_z <- z_test(x = c12_successes, n = c12_pop, p = 0.5, alternative = "two.sided")$ts.z

c12_z_ES <- ES.h(p1 = c12_success_proportion, c12_base_chance <- 0.5)

c12_r <- c12_z/sqrt(c12_pop)


#==== 13. Moore and Egeth (1997), exp. 3 ====

# Compute effect size from n of noticers reported

c13_pop <- 20
c13_success_percentage <- 0
c13_success_proportion <- c13_success_percentage/100
c13_base_chance <- 0.5 #yes/no
c13_successes <- round(c13_success_percentage*c13_pop/100)
c13_failures <- c13_pop - c13_successes

c13_chisquared <- unname(chisq.test(c(c13_successes, c13_failures), 
                                    p = c(0.5, 0.5))$statistic)

c13_cohensd <- esc_chisq(c13_chisquared,
                         es.type = "d",
                         totaln = c13_pop)$es

# c13_etasquared <- esc_chisq(c13_chisquared, 
#                             es.type = "eta", 
#                             totaln = c13_pop)$es

c13_phi <- sqrt(c13_chisquared/c13_pop) * (-1) #effect size is negative

#c13_r <- c13_phi #esc_phi(phi = c13_phi, totaln = c13_pop, es.type = "r")


c13_z <- z_test(x = c13_successes, n = c13_pop, p = 0.5, alternative = "two.sided")$ts.z

c13_z_ES <- ES.h(p1 = c13_success_proportion, c13_base_chance <- 0.5)

c13_r <- c13_z/sqrt(c13_pop)


#==== 14. Moore et al. (2003) ====

# Compute effect sizes from reported percentage of noticers

c14_pop <- 44
c14_success_percentage <- 54.55
c14_success_proportion <- c14_success_percentage/100
c14_base_chance <- 0.5 # regular/irregular
c14_successes <- round(c14_success_percentage*c14_pop/100)
c14_failures <- c14_pop - c14_successes

c14_chisquared <- unname(chisq.test(c(c14_successes, c14_failures), 
                                    p = c(0.5, 0.5))$statistic)

c14_cohensd <- esc_chisq(c14_chisquared,
                         es.type = "d",
                         totaln = c14_pop)$es

# c14_etasquared <- esc_chisq(c14_chisquared, 
#                             es.type = "eta", 
#                             totaln = c14_pop)$es

c14_phi <- sqrt(c14_chisquared/c14_pop)

c14_r <- c14_phi #esc_phi(phi = c14_phi, totaln = c14_pop, es.type = "r")


#==== 15. Moore et al. (2004) ====

# Compute effect sizes from reported percentage of noticers

c15_pop <- 25
c15_success_percentage <- 48
c15_success_proportion <- c15_success_percentage/100
c15_base_chance <- 0.5 #left/right
c15_successes <- round(c15_success_percentage*c15_pop/100)
c15_failures <- c15_pop - c15_successes

c15_chisquared <- unname(chisq.test(c(c15_successes, c15_failures), 
                                    p = c(0.5, 0.5))$statistic)

c15_cohensd <- (-1) * esc_chisq(c15_chisquared, # ES is negative
                                es.type = "d",
                                totaln = c15_pop)$es

# c15_etasquared <- esc_chisq(c15_chisquared, 
#                             es.type = "eta", 
#                             totaln = c15_pop)$es

c15_phi <- sqrt(c15_chisquared/c15_pop) * (-1) #effect size is negative

c15_r <- c15_phi #esc_phi(phi = c15_phi, totaln = c15_pop, es.type = "r")


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
c16_success_percentage <- (c16_successes/c16_pop) * 100
c16_success_proportion <- c16_success_percentage/100
c16_base_chance <- 0.5 #yes/no


c16_chisquared <- unname(chisq.test(c(c16_successes, c16_failures), 
                                    p = c(0.5, 0.5))$statistic)

c16_cohensd <- esc_chisq(c16_chisquared,
                         es.type = "d",
                         totaln = c16_pop)$es

# c16_etasquared <- esc_chisq(c16_chisquared, 
#                             es.type = "eta", 
#                             totaln = c16_pop)$es

c16_phi <- sqrt(c16_chisquared/c16_pop)

c16_r <- c16_phi #esc_phi(phi = c16_phi, totaln = c16_pop, es.type = "r")


#==== 17. Razpurker-Apfeld et al. (2008) - columns/rows, RT ====

# Compute effect sizes from reported percentage of noticers
c17_pop <- 7
c17_successes <- 1 #+ # columns/rows condition
#1 # triangle/arrow condition
c17_failures <- c17_pop - c17_successes
c17_success_percentage <- (c17_successes/c17_pop) * 100
c17_success_proportion <- c17_success_percentage/100
c17_base_chance <- 0.5 #yes/no

c17_chisquared <- unname(chisq.test(c(c17_successes, c17_failures), 
                                    p = c(0.5, 0.5))$statistic)

c17_cohensd <- (-1) * esc_chisq(c17_chisquared, # ES is negative
                                es.type = "d",
                                totaln = c17_pop)$es

# c17_etasquared <- esc_chisq(c17_chisquared, 
#                             es.type = "eta", 
#                             totaln = c17_pop)$es

c17_phi <- sqrt(c17_chisquared/c17_pop) * (-1) # effect size is negative

c17_r <- c17_phi #esc_phi(phi = c17_phi, totaln = c17_pop, es.type = "r")


#==== 18. Razpurker-Apfeld et al. (2008) - columns/rows, d' ====

# Effect size is the same as for RT; should not be counted twice.
c18_pop <- c17_pop
c18_successes <- NA#1 #+ # columns/rows condition
c18_failures <- NA#c18_pop - c18_successes
c18_success_percentage <- NA#(c18_successes/c18_pop) * 100
c18_success_proportion <- NA#c18_success_percentage/100
c18_base_chance <- NA#0.5 #yes/no

c18_chisquared <- NA #c17_chisquared
c18_cohensd <- NA #c17_cohensd
c18_r <- NA


#==== 19. Razpurker-Apfeld et al. (2008) - triangle/arrow, RT ====

# Compute effect sizes from reported percentage of noticers
c19_pop <- 7
c19_successes <- 1
c19_failures <- c19_pop - c19_successes
c19_success_percentage <- (c19_successes/c19_pop) * 100
c19_success_proportion <- c19_success_percentage/100
c19_base_chance <- 0.5 #yes/no

c19_chisquared <- unname(chisq.test(c(c19_successes, c19_failures), 
                                    p = c(0.5, 0.5))$statistic)

c19_cohensd <- (-1) * esc_chisq(c19_chisquared, # ES is negative
                                es.type = "d",
                                totaln = c19_pop)$es

# c19_etasquared <- esc_chisq(c19_chisquared, 
#                             es.type = "eta", 
#                             totaln = c19_pop)$es

c19_phi <- sqrt(c19_chisquared/c19_pop) * (-1) # ES is negative

c19_r <- c19_phi #esc_phi(phi = c19_phi, totaln = c19_pop, es.type = "r")


#==== 20. Razpurker-Apfeld et al. (2008) - triangle/arrow, d' ====

# Effect size is the same as for RT; should not be counted twice.
c20_pop <- c19_pop
c20_successes <- NA#1 #+ # columns/rows condition
c20_failures <- NA#c20_pop - c20_successes
c20_success_percentage <- NA#(c20_successes/c20_pop) * 100
c20_success_proportion <- NA#c20_success_percentage/100
c20_base_chance <- NA#0.5 #yes/no


c20_chisquared <- NA #c19_chisquared
c20_cohensd <- NA #c19_cohensd
c20_r <- NA

#==== 21. Richards et al. (2012), tracking task  ====
# 
# # Compute effect sizes from proportions
# c21_pop <- 131
# c21_successes <- round(47*c21_pop/100)
# c21_failures <- c21_pop - c21_successes
# 
# c21_chisquared <- unname(chisq.test(c(c21_successes, c21_failures), 
#                                     p = c(0.5, 0.5))$statistic)
# 
# 
# c21_cohensd <- (-1) * esc_chisq(c21_chisquared, # ES is negative
#                          es.type = "d",
#                          totaln = c21_pop)$es
# 
# # c21_etasquared <- esc_chisq(c21_chisquared, 
# #                             es.type = "eta", 
# #                             totaln = c21_pop)$es
# 
# c21_phi <- sqrt(c21_chisquared/c21_pop) * (-1) # effect sizs is negative
# 
# c21_r <- c21_phi #esc_phi(phi = c21_phi, totaln = c21_pop, es.type = "r")


#==== 22. Russell et al. (2008) - exp. 1, acc ====

# Compute effect sizes from proportions
c22_pop <- 25
c22_successes <- 12
c22_failures <- c22_pop - c22_successes
c22_success_percentage <- (c22_successes/c22_pop) * 100
c22_success_proportion <- c22_success_percentage/100
c22_base_chance <- 0.5 #yes/no

c22_chisquared <- unname(chisq.test(c(c22_successes, c22_failures), 
                                    p = c(0.5, 0.5))$statistic)


c22_cohensd <- (-1) * esc_chisq(c22_chisquared, # ES is negative
                                es.type = "d",
                                totaln = c22_pop)$es

# c22_etasquared <- esc_chisq(c22_chisquared, 
#                             es.type = "eta", 
#                             totaln = c22_pop)$es

c22_phi <- sqrt(c22_chisquared/c22_pop) * (-1) # effect sizs is negative

c22_r <- c22_phi #esc_phi(phi = c22_phi, totaln = c22_pop, es.type = "r")


#==== 23. Russell et al. (2008) - exp. 1, RT ====

# Effect size is the same as for acc; should not be counted twice
c23_pop <- c22_pop
c23_successes <- NA#1 #+ # columns/rows condition
c23_failures <- NA#c23_pop - c23_successes
c23_success_percentage <- NA#(c23_successes/c23_pop) * 100
c23_success_proportion <- NA#c23_success_percentage/100
c23_base_chance <- NA#0.5 #yes/no

c23_chisquared <- NA #c22_chisquared
c23_cohensd <- NA #c22_cohensd

c23_r <- NA


#==== 24. Russell et al. (2008) - exp. 2, acc ====

# Compute effect sizes from proportions
c24_pop <- 28
c24_successes <- 16
c24_failures <- c24_pop - c24_successes
c24_success_percentage <- (c24_successes/c24_pop) * 100
c24_success_proportion <- c24_success_percentage/100
c24_base_chance <- 0.5 #yes/no

c24_chisquared <- unname(chisq.test(c(c24_successes, c24_failures), 
                                    p = c(0.5, 0.5))$statistic)


c24_cohensd <- esc_chisq(c24_chisquared,
                         es.type = "d",
                         totaln = c24_pop)$es

# c24_etasquared <- esc_chisq(c24_chisquared, 
#                             es.type = "eta", 
#                             totaln = c24_pop)$es

c24_phi <- sqrt(c24_chisquared/c24_pop)

c24_r <- c24_phi #esc_phi(phi = c24_phi, totaln = c24_pop, es.type = "r")


#==== 25. Russell et al. (2008) - exp. 2, RT ====

# Effect size is the same as for acc; should not be counted twice
c25_pop <- c24_pop
c25_successes <- NA#1 #+ # columns/rows condition
c25_failures <- NA#c25_pop - c25_successes
c25_success_percentage <- NA#(c25_successes/c25_pop) * 100
c25_success_proportion <- NA#c25_success_percentage/100
c25_base_chance <- NA#0.5 #yes/no

c25_chisquared <- NA #c24_chisquared
c25_cohensd <- NA #c24_cohensd

c25_r <- NA

#==== 26. Russell et al. (2008) - exp. 3, acc ====

# Compute effect sizes from proportions
c26_pop <- 24
c26_successes <- 12
c26_failures <- c26_pop - c26_successes
c26_success_percentage <- (c26_successes/c26_pop) * 100
c26_success_proportion <- c26_success_percentage/100
c26_base_chance <- 0.5 #yes/no

c26_chisquared <- unname(chisq.test(c(c26_successes, c26_failures), 
                                    p = c(0.5, 0.5))$statistic)


c26_cohensd <- esc_chisq(c26_chisquared,
                         es.type = "d",
                         totaln = c26_pop)$es

# c26_etasquared <- esc_chisq(c26_chisquared, 
#                             es.type = "eta", 
#                             totaln = c26_pop)$es

c26_phi <- sqrt(c26_chisquared/c26_pop)

c26_r <- c26_phi #esc_phi(phi = c26_phi, totaln = c26_pop, es.type = "r")


#==== 27. Russell et al. (2008) - exp. 3, RT ====

# Effect size is the same as for acc; should not be counted twice
c27_pop <- c26_pop
c27_successes <- NA#1 #+ # columns/rows condition
c27_failures <- NA#c27_pop - c27_successes
c27_success_percentage <- NA#(c27_successes/c27_pop) * 100
c27_success_proportion <- NA#c27_success_percentage/100
c27_base_chance <- NA#0.5 #yes/no

c27_chisquared <- NA #c26_chisquared
c27_cohensd <- NA #c26_cohensd

c27_r <- NA

#==== 28. Russell et al. (2008) - exp. 4A, acc ====

# Compute effect sizes from proportions
c28_pop <- 20
c28_successes <- 5
c28_failures <- c28_pop - c28_successes
c28_success_percentage <- (c28_successes/c28_pop) * 100
c28_success_proportion <- c28_success_percentage/100
c28_base_chance <- 0.5 #yes/no

c28_chisquared <- unname(chisq.test(c(c28_successes, c28_failures), 
                                    p = c(0.5, 0.5))$statistic)


c28_cohensd <- (-1) * esc_chisq(c28_chisquared, # ES is negative
                                es.type = "d",
                                totaln = c28_pop)$es

# c28_etasquared <- esc_chisq(c28_chisquared, 
#                             es.type = "eta", 
#                             totaln = c28_pop)$es

c28_phi <- sqrt(c28_chisquared/c28_pop) * (-1) # effect size is negative

c28_r <- c28_phi #esc_phi(phi = c28_phi, totaln = c28_pop, es.type = "r")


#==== 29. Russell et al. (2008) - exp. 4A, RT ====

# Effect size is the same as for acc; should not be counted twice
c29_pop <- c28_pop
c29_successes <- NA#1 #+ # columns/rows condition
c29_failures <- NA#c29_pop - c29_successes
c29_success_percentage <- NA#(c29_successes/c29_pop) * 100
c29_success_proportion <- NA#c29_success_percentage/100
c29_base_chance <- NA#0.5 #yes/no

c29_chisquared <- NA #c28_chisquared
c29_cohensd <- NA #c28_cohensd

c29_r <- NA

#==== 30. Russell et al. (2008) - exp. 4B, acc ====

# Compute effect sizes from proportions
c30_pop <- 21
c30_successes <- 10
c30_failures <- c30_pop - c30_successes
c30_success_percentage <- (c30_successes/c30_pop) * 100
c30_success_proportion <- c30_success_percentage/100
c30_base_chance <- 0.5 #yes/no

c30_chisquared <- unname(chisq.test(c(c30_successes, c30_failures), 
                                    p = c(0.5, 0.5))$statistic)

c30_cohensd <- (-1) * esc_chisq(c30_chisquared, # ES is negative
                                es.type = "d",
                                totaln = c30_pop)$es

# c30_etasquared <- esc_chisq(c30_chisquared, 
#                             es.type = "eta", 
#                             totaln = c30_pop)$es

c30_phi <- sqrt(c30_chisquared/c30_pop) * (-1) # effect size is negative

c30_r <- c30_phi #esc_phi(phi = c30_phi, totaln = c30_pop, es.type = "r")


#==== 31. Russell et al. (2008) - exp. 4B, RT ====

# Effect size is the same as for acc; should not be counted twice
c31_pop <- c30_pop
c31_successes <- NA#1 #+ # columns/rows condition
c31_failures <- NA#c31_pop - c31_successes
c31_success_percentage <- NA#(c31_successes/c31_pop) * 100
c31_success_proportion <- NA#c31_success_percentage/100
c31_base_chance <- NA#0.5 #yes/no

c31_chisquared <- NA #c30_chisquared
c31_cohensd <- NA #c30_cohensd
c31_r <- NA

#==== 32. Russell et al. (2008) - exp. 5, acc ====

# Compute effect sizes from proportions
c32_pop <- 24
c32_successes <- 18
c32_failures <- c32_pop - c32_successes
c32_success_percentage <- (c32_successes/c32_pop) * 100
c32_success_proportion <- c32_success_percentage/100
c32_base_chance <- 0.5 #yes/no

c32_chisquared <- unname(chisq.test(c(c32_successes, c32_failures), 
                                    p = c(0.5, 0.5))$statistic)

c32_cohensd <- esc_chisq(c32_chisquared,
                         es.type = "d",
                         totaln = c32_pop)$es

# c32_etasquared <- esc_chisq(c32_chisquared, 
#                             es.type = "eta", 
#                             totaln = c32_pop)$es

c32_phi <- sqrt(c32_chisquared/c32_pop)

c32_r <- c32_phi #esc_phi(phi = c32_phi, totaln = c32_pop, es.type = "r")


#==== 33. Russell et al. (2008) - exp. 5, RT ====

# Effect size is the same as for acc; should not be counted twice
c33_pop <- c32_pop
c33_successes <- NA#1 #+ # columns/rows condition
c33_failures <- NA#c33_pop - c33_successes
c33_success_percentage <- NA#(c33_successes/c33_pop) * 100
c33_success_proportion <- NA#c33_success_percentage/100
c33_base_chance <- NA#0.5 #yes/no

c33_chisquared <- NA #c32_chisquared
c33_cohensd <- NA #c32_cohensd

c33_r <- NA


#==== 34. Shafto and Pitts (2015) ====

# Compute effect sizes from proportions
#c34_pop <- 30
#c34_successes <- 15
#c34_failures <- c34_pop - c34_successes

#c34_chisquared <- unname(chisq.test(c(c34_successes, c34_failures), 
#                                    p = c(0.5, 0.5))$statistic)
#c34_cohensd <- esc_chisq(c34_chisquared, 
#                         es.type = "d", 
#                         totaln = c34_pop)$es


#==== 35. Schnuerch et al. (2016) - exp. 1 ====

# Compute effect sizes from percentages of noticers
c35_pop <- 61
c35_success_percentage <- 8
c35_success_proportion <- c35_success_percentage/100
c35_base_chance <- 0.5
c35_successes <- round(c35_success_percentage*c35_pop/100)
c35_failures <- c35_pop - c35_successes

c35_chisquared <- unname(chisq.test(c(c35_successes, c35_failures), 
                                    p = c(0.5, 0.5))$statistic)


c35_cohensd <- (-1) * esc_chisq(c35_chisquared, # ES is negative
                                es.type = "d",
                                totaln = c35_pop)$es

# c35_etasquared <- esc_chisq(c35_chisquared, 
#                             es.type = "eta", 
#                             totaln = c35_pop)$es

c35_phi <- sqrt(c35_chisquared/c35_pop) * (-1) # effect size is negative

c35_r <- c35_phi #esc_phi(phi = c35_phi, totaln = c35_pop, es.type = "r")



#==== 36. Schnuerch et al. (2016) - exp. 2 ====

# Compute effect sizes from percentages
c36_pop <- 58
c36_success_percentage <- 6
c36_success_proportion <- c36_success_percentage/100
c36_base_chance <- 0.5 #yes/no
c36_successes <- round(c36_success_percentage*c36_pop/100)
c36_failures <- c36_pop - c36_successes

c36_chisquared <- unname(chisq.test(c(c36_successes, c36_failures), 
                                    p = c(0.5, 0.5))$statistic)

c36_cohensd <- (-1) * esc_chisq(c36_chisquared, # ES is negative
                                es.type = "d",
                                totaln = c36_pop)$es

# c36_etasquared <- esc_chisq(c36_chisquared, 
#                             es.type = "eta", 
#                             totaln = c36_pop)$es

c36_phi <- sqrt(c36_chisquared/c36_pop) * (-1) # effect size is negative

c36_r <- c36_phi #esc_phi(phi = c36_phi, totaln = c36_pop, es.type = "r")


#==== 37. Scholte et al. (2006) ====

#==== 38. Wood and Simons (2019), exp. 1 ====

# Compute effect size from proportion
c38_pop <- 58+62+32+23
c38_success_percentage <- 16.5
c38_success_proportion <- c38_success_percentage/100
c38_base_chance <- round(1/6, digits = 2) #6AFC
c38_successes <- round(c38_success_percentage*c38_pop/100) #percentage from strict criterion
c38_failures <- c38_pop - c38_successes

c38_chisquared <- unname(chisq.test(c(c38_successes, c38_failures), 
                                    p = c(0.5, 0.5))$statistic)

c38_cohensd <- (-1) * esc_chisq(c38_chisquared, # ES is negative
                                es.type = "d",
                                totaln = c38_pop)$es
# 
# c38_etasquared <- esc_chisq(c38_chisquared, 
#                             es.type = "eta", 
#                             totaln = c38_pop)$es

c38_phi <- sqrt(c38_chisquared/c38_pop) * (-1) # effect size is negative

c38_r <- c38_phi #esc_phi(phi = c38_phi, totaln = c38_pop, es.type = "r")


#==== 39. Wood and Simons (2019), exp. 2 ====

# Compute effect size from proportion
c39_pop <- 30+29+29+32+35+29+32
c39_success_percentage <- 31.3
c39_success_proportion <- c39_success_percentage/100
c39_base_chance <- 1/5 #5AFC
c39_successes <- round(c39_success_percentage*c39_pop/100) #percentage from lax criterion
c39_failures <- c39_pop - c39_successes

c39_chisquared <- unname(chisq.test(c(c39_successes, c39_failures), 
                                    p = c(c39_base_chance, 1-c39_base_chance))$statistic)

c39_cohensd <- (-1) * esc_chisq(c39_chisquared, # ES is negative
                                es.type = "d",
                                totaln = c39_pop)$es
# 
# c39_etasquared <- esc_chisq(c39_chisquared, 
#                             es.type = "eta", 
#                             totaln = c39_pop)$es

c39_phi <- sqrt(c39_chisquared/c39_pop) * (-1) # effect size is negative

c39_r <- c39_phi #esc_phi(phi = c39_phi, totaln = c39_pop, es.type = "r")

#==== Wiemer====#
# # Compute effect sizes from proportions
# c37_pop <- 120
# c37_successes <- round(58.3*(c21_pop/2)/100) + # flower us
#   round(51.7*(c21_pop/2)/100) # spider us
# c37_failures <- c37_pop - c37_successes
# 
# c37_chisquared <- unname(chisq.test(c(c37_successes, c37_failures), 
#                                     p = c(0.5, 0.5))$statistic)
# c37_cohensd <- esc_chisq(c37_chisquared, 
#                          es.type = "d", 
#                          totaln = c37_pop)$es

#==== 41. Mack and Rock. exp 1 (2000) ====
c41_pop <- 80
c41_successes <- 30
c41_failures <- c41_pop - c41_successes
c41_success_percentage <- (c41_successes/c41_pop) * 100
c41_success_proportion <- c41_success_percentage/100
c41_base_chance <- 0.5 #yes/no

c41_chisquared <- unname(chisq.test(c(c41_successes, c41_failures), 
                                    p = c(0.5, 0.5))$statistic)

c41_cohensd <- (-1) * esc_chisq(c41_chisquared, # ES is negative
                                es.type = "d",
                                totaln = c41_pop)$es
# 
# c41_etasquared <- esc_chisq(c41_chisquared, 
#                             es.type = "eta", 
#                             totaln = c41_pop)$es

c41_phi <- sqrt(c41_chisquared/c41_pop) * (-1) # effect size is negative

c41_r <- c41_phi #esc_phi(phi = c41_phi, totaln = c41_pop, es.type = "r")


#==== 42. Mack and Rock. exp 2 (2000) ====
c42_pop <- 75
c42_successes <- 34
c42_failures <- c42_pop - c42_successes
c42_success_percentage <- (c42_successes/c42_pop) * 100
c42_success_proportion <- c42_success_percentage/100
c42_base_chance <- 0.5 #yes/no


c42_chisquared <- unname(chisq.test(c(c42_successes, c42_failures), 
                                    p = c(0.5, 0.5))$statistic)

c42_cohensd <- (-1) * esc_chisq(c42_chisquared, # ES is negative
                                es.type = "d",
                                totaln = c42_pop)$es
# 
# c42_etasquared <- esc_chisq(c42_chisquared, 
#                             es.type = "eta", 
#                             totaln = c42_pop)$es

c42_phi <- sqrt(c42_chisquared/c42_pop) * (-1) # effect size is negative

c42_r <- c42_phi #esc_phi(phi = c42_phi, totaln = c42_pop, es.type = "r")


#==== 43. Mack and Rock. exp 3 (2000) ====
c43_pop <- 30
c43_successes <- 9
c43_failures <- c43_pop - c43_successes
c43_success_percentage <- (c43_successes/c43_pop) * 100
c43_success_proportion <- c43_success_percentage/100
c43_base_chance <- 0.5 #yes/no


c43_chisquared <- unname(chisq.test(c(c43_successes, c43_failures), 
                                    p = c(0.5, 0.5))$statistic)

c43_cohensd <- (-1) * esc_chisq(c43_chisquared, # ES is negative
                                es.type = "d",
                                totaln = c43_pop)$es
# 
# c43_etasquared <- esc_chisq(c43_chisquared, 
#                             es.type = "eta", 
#                             totaln = c43_pop)$es

c43_phi <- sqrt(c43_chisquared/c43_pop) * (-1) # effect size is negative

c43_r <- c43_phi #esc_phi(phi = c43_phi, totaln = c43_pop, es.type = "r")


#==== 44. Mack and Rock. exp 4 (2000) ====
c44_pop <- 60
c44_successes <- 31
c44_failures <- c44_pop - c44_successes
c44_success_percentage <- (c44_successes/c44_pop) * 100
c44_success_proportion <- c44_success_percentage/100
c44_base_chance <- 0.5 #yes/no


c44_chisquared <- unname(chisq.test(c(c44_successes, c44_failures), 
                                    p = c(0.5, 0.5))$statistic)

c44_cohensd <- esc_chisq(c44_chisquared,
                         es.type = "d",
                         totaln = c44_pop)$es
# 
# c44_etasquared <- esc_chisq(c44_chisquared, 
#                             es.type = "eta", 
#                             totaln = c44_pop)$es

c44_phi <- sqrt(c44_chisquared/c44_pop)

c44_r <- c44_phi #esc_phi(phi = c44_phi, totaln = c44_pop, es.type = "r")

#==== 45. Mack and Rock. exp 5 (2000) ====
c45_pop <- 20+20
c45_successes <- 10+11
c45_failures <- c45_pop - c45_successes
c45_success_percentage <- (c45_successes/c45_pop) * 100
c45_success_proportion <- c45_success_percentage/100
c45_base_chance <- 0.5 #yes/no


c45_chisquared <- unname(chisq.test(c(c45_successes, c45_failures), 
                                    p = c(0.5, 0.5))$statistic)

c45_cohensd <- esc_chisq(c45_chisquared,
                         es.type = "d",
                         totaln = c45_pop)$es
# 
# c45_etasquared <- esc_chisq(c45_chisquared, 
#                             es.type = "eta", 
#                             totaln = c45_pop)$es

c45_phi <- sqrt(c45_chisquared/c45_pop)

c45_r <- c45_phi #esc_phi(phi = c45_phi, totaln = c45_pop, es.type = "r")


#==== 46. Rashal et al. exp 1 RT (2017) ====

# use reported number of noticers for "change" question
c46_pop <- 20
c46_successes <- 11
c46_failures <- c46_pop - c46_successes
c46_success_percentage <- (c46_successes/c46_pop) * 100
c46_success_proportion <- c46_success_percentage/100
c46_base_chance <- 0.5 #yes/no

c46_chisquared <- unname(chisq.test(c(c46_successes, c46_failures), 
                                    p = c(0.5, 0.5))$statistic)

c46_cohensd <- esc_chisq(c46_chisquared,
                         es.type = "d",
                         totaln = c46_pop)$es

# c46_etasquared <- esc_chisq(c46_chisquared, 
#                             es.type = "eta", 
#                             totaln = c46_pop)$es

c46_phi <- sqrt(c46_chisquared/c46_pop)

c46_r <- c46_phi #esc_phi(phi = c46_phi, totaln = c46_pop, es.type = "r")

#==== 47. Rashal et al. exp 2 acc (2017) ====

# use reported number of noticers for "change" question
c47_pop <- 28
c47_successes <- 16
c47_failures <- c47_pop - c47_successes
c47_success_percentage <- (c47_successes/c47_pop) * 100
c47_success_proportion <- c47_success_percentage/100
c47_base_chance <- 0.5 #yes/no

c47_chisquared <- unname(chisq.test(c(c47_successes, c47_failures), 
                                    p = c(0.5, 0.5))$statistic)

c47_cohensd <- esc_chisq(c47_chisquared,
                         es.type = "d",
                         totaln = c47_pop)$es

# c47_etasquared <- esc_chisq(c47_chisquared, 
#                             es.type = "eta", 
#                             totaln = c47_pop)$es

c47_phi <- sqrt(c47_chisquared/c47_pop)

c47_r <- c47_phi #esc_phi(phi = c47_phi, totaln = c47_pop, es.type = "r")

#==== 48. Rashal et al. exp 3 RT (2017) ====

# use reported number of noticers for "change" question
c48_pop <- 18
c48_successes <- 11
c48_failures <- c48_pop - c48_successes
c48_success_percentage <- (c48_successes/c48_pop) * 100
c48_success_proportion <- c48_success_percentage/100
c48_base_chance <- 0.5 #yes/no

c48_chisquared <- unname(chisq.test(c(c48_successes, c48_failures), 
                                    p = c(0.5, 0.5))$statistic)

c48_cohensd <- esc_chisq(c48_chisquared,
                         es.type = "d",
                         totaln = c48_pop)$es

# c48_etasquared <- esc_chisq(c48_chisquared, 
#                             es.type = "eta", 
#                             totaln = c48_pop)$es

c48_phi <- sqrt(c48_chisquared/c48_pop)

c48_r <- c48_phi #esc_phi(phi = c48_phi, totaln = c48_pop, es.type = "r")

#==== 49. Rashal et al. exp 4 acc (2017) ====

# use reported number of noticers for "change" question
c49_pop <- 15
c49_successes <- 7
c49_failures <- c49_pop - c49_successes
c49_success_percentage <- (c49_successes/c49_pop) * 100
c49_success_proportion <- c49_success_percentage/100
c49_base_chance <- 0.5 #yes/no

c49_chisquared <- unname(chisq.test(c(c49_successes, c49_failures), 
                                    p = c(0.5, 0.5))$statistic)

c49_cohensd <- (-1) * esc_chisq(c49_chisquared,
                                es.type = "d",
                                totaln = c49_pop)$es

# c49_etasquared <- esc_chisq(c49_chisquared, 
#                             es.type = "eta", 
#                             totaln = c49_pop)$es

c49_phi <- sqrt(c49_chisquared/c49_pop) * (-1) # effect size is negative

c49_r <- c49_phi #esc_phi(phi = c49_phi, totaln = c49_pop, es.type = "r")

#==== 50. Rashal et al. exp 4 RT (2017) ====

# Effect size is the same as for acc; should not be counted twice
c50_fvalue <- 7.72
c50_pop <- 14
c50_n <- c50_pop
c50_successes <- NA#1 #+ # columns/rows condition
c50_failures <- NA#c50_pop - c50_successes
c50_success_percentage <- NA#(c50_successes/c50_pop) * 100
c50_success_proportion <- NA#c50_success_percentage/100
c50_base_chance <- NA#0.5 #yes/no

c50_partialetasquared <- NA #0.36
c50_chisquared <- NA
c50_cohensd <- NA
c50_r <- NA #0.59

#==== 51. Rashal et al. exp 5 acc (2017) ====

# use reported number of noticers for "change" question
c51_pop <- 18
c51_successes <- 9
c51_failures <- c51_pop - c51_successes
c51_success_percentage <- (c51_successes/c51_pop) * 100
c51_success_proportion <- c51_success_percentage/100
c51_base_chance <- 0.5 #yes/no

c51_chisquared <- unname(chisq.test(c(c51_successes, c51_failures), 
                                    p = c(0.5, 0.5))$statistic)

c51_cohensd <- esc_chisq(c51_chisquared,
                         es.type = "d",
                         totaln = c51_pop)$es

# c51_etasquared <- esc_chisq(c51_chisquared, 
#                             es.type = "eta", 
#                             totaln = c51_pop)$es

c51_phi <- sqrt(c51_chisquared/c51_pop)

c51_r <- c51_phi #esc_phi(phi = c51_phi, totaln = c51_pop, es.type = "r")


#==== 52. Rashal et al. exp 5 RT (2017) ====

# Effect size is the same as for acc; should not be counted twice
c52_fvalue <- 3.56
c52_pop <- 18
c52_n <- c52_pop
c52_successes <- NA#1 #+ # columns/rows condition
c52_failures <- NA#c52_pop - c52_successes
c52_success_percentage <- NA#(c52_successes/c52_pop) * 100
c52_success_proportion <- NA#c52_success_percentage/100
c52_base_chance <- NA#0.5 #yes/no

c52_partialetasquared <- NA #0.17
c52_chisquared <- NA
c52_cohensd <- NA
c52_r <- NA #0.40

#==== 53. Rashal et al. exp 6 acc (2017) ====

# use reported number of noticers for "change" question
c53_pop <- 18
c53_successes <- 11
c53_failures <- c53_pop - c53_successes
c53_success_percentage <- (c53_successes/c53_pop) * 100
c53_success_proportion <- c53_success_percentage/100
c53_base_chance <- 0.5 #yes/no

c53_chisquared <- unname(chisq.test(c(c53_successes, c53_failures), 
                                    p = c(0.5, 0.5))$statistic)

c53_cohensd <- esc_chisq(c53_chisquared,
                         es.type = "d",
                         totaln = c53_pop)$es

# c53_etasquared <- esc_chisq(c53_chisquared, 
#                             es.type = "eta", 
#                             totaln = c53_pop)$es

c53_phi <- sqrt(c53_chisquared/c53_pop)

c53_r <- c53_phi #esc_phi(phi = c53_phi, totaln = c53_pop, es.type = "r")


#==== 54. Rashal et al. exp 6 RT (2017) ====

# Effect size is the same as for acc; should not be counted twice
c54_fvalue <- 1.85
c54_pop <- 18
c54_n <- c54_pop
c54_successes <- NA#1 #+ # columns/rows condition
c54_failures <- NA#c54_pop - c54_successes
c54_success_percentage <- NA#(c54_successes/c54_pop) * 100
c54_success_proportion <- NA#c54_success_percentage/100
c54_base_chance <- NA#0.5 #yes/no


c54_partialetasquared <- NA #0.1
c54_chisquared <- NA
c54_cohensd <- NA
c54_r <- NA#0.31

#==== 55. Kimchi et al. (2004) exp. 1 - Column/row by color similarity, RT ====

# use reported number of noticers for "pattern" question
c55_pop <- 14
c55_success_percentage <- 0
c55_success_proportion <- c55_success_percentage/100
c55_base_chance <- 0.5 #???
c55_successes <- round(c55_success_percentage*c55_pop/100) #percentage from yes or no question
c55_failures <- c55_pop - c55_successes

c55_chisquared <- unname(chisq.test(c(c55_successes, c55_failures), 
                                    p = c(0.5, 0.5))$statistic)

c55_cohensd <- esc_chisq(c55_chisquared,
                         es.type = "d",
                         totaln = c55_pop)$es

# c55_etasquared <- esc_chisq(c55_chisquared, 
#                             es.type = "eta", 
#                             totaln = c55_pop)$es

c55_phi <- sqrt(c55_chisquared/c55_pop)

c55_r <- c55_phi #esc_phi(phi = c55_phi, totaln = c55_pop, es.type = "r")

#==== 56. Kimchi et al. (2004) exp. 1 - triangle/arrow by color similarity, acc ====

# use reported number of noticers for "pattern" question
c56_pop <- 14
c56_success_percentage <- 0
c56_success_proportion <- c56_success_percentage/100
c56_base_chance <- 0.5 #???
c56_successes <- round(c56_success_percentage*c56_pop/100) #percentage from yes or no question
c56_failures <- c56_pop - c56_successes

c56_chisquared <- unname(chisq.test(c(c56_successes, c56_failures), 
                                    p = c(0.5, 0.5))$statistic)

c56_cohensd <- esc_chisq(c56_chisquared,
                         es.type = "d",
                         totaln = c56_pop)$es

# c56_etasquared <- esc_chisq(c56_chisquared, 
#                             es.type = "eta", 
#                             totaln = c56_pop)$es

c56_phi <- sqrt(c56_chisquared/c56_pop)

c56_r <- c56_phi #esc_phi(phi = c56_phi, totaln = c56_pop, es.type = "r")

#==== 57. Kimchi et al. (2004) exp. 1 - triangle/arrow, acc ====

# use reported number of noticers for "pattern" question
c57_pop <- 14
c57_success_percentage <- 7
c57_success_proportion <- c57_success_percentage/100
c57_base_chance <- 0.5 #???
c57_successes <- round(c57_success_percentage*c57_pop/100) #percentage from yes or no question
c57_failures <- c57_pop - c57_successes

c57_chisquared <- unname(chisq.test(c(c57_successes, c57_failures), 
                                    p = c(0.5, 0.5))$statistic)

c57_cohensd <- esc_chisq(c57_chisquared,
                         es.type = "d",
                         totaln = c57_pop)$es

# c57_etasquared <- esc_chisq(c57_chisquared, 
#                             es.type = "eta", 
#                             totaln = c57_pop)$es

c57_phi <- sqrt(c57_chisquared/c57_pop)

c57_r <- c57_phi #esc_phi(phi = c57_phi, totaln = c57_pop, es.type = "r")

#==== 58. Kimchi et al. (2004) exp. 2 - square/cross by color similarity, acc ====

# use reported number of noticers for "pattern" question
c58_pop <- 12
c58_success_percentage <- 0
c58_success_proportion <- c58_success_percentage/100
c58_base_chance <- 0.5 #???
c58_successes <- round(c58_success_percentage*c58_pop/100) #percentage from yes or no question
c58_failures <- c58_pop - c58_successes

c58_chisquared <- unname(chisq.test(c(c58_successes, c58_failures), 
                                    p = c(0.5, 0.5))$statistic)

c58_cohensd <- esc_chisq(c58_chisquared,
                         es.type = "d",
                         totaln = c58_pop)$es

# c58_etasquared <- esc_chisq(c58_chisquared, 
#                             es.type = "eta", 
#                             totaln = c58_pop)$es

c58_phi <- sqrt(c58_chisquared/c58_pop)

c58_r <- c58_phi #esc_phi(phi = c58_phi, totaln = c58_pop, es.type = "r")


#==== 59. Kimchi et al. (2004) exp. 2 - square/cross, RT ====

# use reported number of noticers for "pattern" question
c59_pop <- 12
c59_success_percentage <- 25
c59_success_proportion <- c59_success_percentage/100
c59_base_chance <- 0.5 #???
c59_successes <- round(100*c59_pop/100) #percentage from yes or no question
c59_failures <- c59_pop - c59_successes

c59_chisquared <- unname(chisq.test(c(c59_successes, c59_failures), 
                                    p = c(0.5, 0.5))$statistic)

c59_cohensd <- esc_chisq(c59_chisquared,
                         es.type = "d",
                         totaln = c59_pop)$es

# c59_etasquared <- esc_chisq(c59_chisquared, 
#                             es.type = "eta", 
#                             totaln = c59_pop)$es

c59_phi <- sqrt(c59_chisquared/c59_pop)

c59_r <- c59_phi #esc_phi(phi = c59_phi, totaln = c59_pop, es.type = "r")

#==== 60. Kimchi et al. (2004) exp. 2 - square/cross, acc ====

# Effect size is the same as for RT; should not be counted twice
c60_pop <- 12
c60_success_percentage <- NA#25
c60_success_proportion <- NA#c60_success_percentage/100
c60_base_chance <- NA#0.5 #???
c60_successes <- NA#round(100*c60_pop/100) #percentage from yes or no question
c60_failures <- NA#c60_pop - c60_successes

c60_chisquared <- NA#unname(chisq.test(c(c60_successes, c60_failures), 
#                                    p = c(0.5, 0.5))$statistic)

c60_cohensd <- NA #esc_chisq(c60_chisquared,
#es.type = "d",
#totaln = c60_pop)$es

# c60_etasquared <- esc_chisq(c60_chisquared, 
#                             es.type = "eta", 
#                             totaln = c60_pop)$es

c60_phi <- NA #sqrt(c60_chisquared/c60_pop)

c60_r <- NA #c60_phi #esc_phi(phi = c60_phi, totaln = c60_pop, es.type = "r")

#==== 61. Kimchi et al. (2008) exp. 1 RT ====

# use reported number of noticers for "change" question
c61_pop <- 46
c61_successes <- 21
c61_failures <- c61_pop - c61_successes
c61_success_percentage <- (c61_successes/c61_pop) * 100 #50
c61_success_proportion <- c61_success_percentage/100
c61_base_chance <- 0.5 #yes/no

c61_chisquared <- unname(chisq.test(c(c61_successes, c61_failures), 
                                    p = c(0.5, 0.5))$statistic)

c61_cohensd <- esc_chisq(c61_chisquared,
                         es.type = "d",
                         totaln = c61_pop)$es

# c61_etasquared <- esc_chisq(c61_chisquared, 
#                             es.type = "eta", 
#                             totaln = c61_pop)$es

c61_phi <- sqrt(c61_chisquared/c61_pop)

c61_r <- c61_phi #esc_phi(phi = c61_phi, totaln = c61_pop, es.type = "r")

#==== 62. Kimchi et al. (2008) exp. 1 acc ====

# Effect size is the same as for RT; should not be counted twice
c62_pop <- 46
c62_success_percentage <- NA#100
c62_success_proportion <- NA#c62_success_percentage/100
c62_base_chance <-NA# 0.5 #???
c62_successes <- NA#round(c62_success_percentage*c62_pop/100) #percentage from yes or no question
c62_failures <- NA#c62_pop - c62_successes

c62_chisquared <- NA#unname(chisq.test(c(c62_successes, c62_failures), 
#                                    p = c(0.5, 0.5))$statistic)

c62_cohensd <- NA #esc_chisq(c62_chisquared,
#es.type = "d",
#totaln = c62_pop)$es

# c62_etasquared <- esc_chisq(c62_chisquared, 
#                             es.type = "eta", 
#                             totaln = c62_pop)$es

c62_phi <- NA #sqrt(c62_chisquared/c62_pop)

c62_r <- NA #c62_phi #esc_phi(phi = c62_phi, totaln = c62_pop, es.type = "r")


#==== 63. Lamy et al. (2006) exp. 2  ====

# Effect size is the same as for RT; should not be counted twice
c63_pop <- 16
c63_successes <- 11
c63_failures <- c63_pop - c63_successes
c63_success_percentage <- (c63_successes/c63_pop) * 100
c63_success_proportion <- c63_success_percentage/100
c63_base_chance <- 0.5 #yes/no

c63_chisquared <- unname(chisq.test(c(c63_successes, c63_failures), 
                                    p = c(0.5, 0.5))$statistic)

c63_cohensd <- esc_chisq(c63_chisquared,
                         es.type = "d",
                         totaln = c63_pop)$es

c63_etasquared <- esc_chisq(c63_chisquared,
                            es.type = "eta",
                            totaln = c63_pop)$es

c63_phi <- sqrt(c63_chisquared/c63_pop)

c63_r <- c63_phi #esc_phi(phi = c63_phi, totaln = c63_pop, es.type = "r")


#==== 64. Lamy et al. (2006) exp. 2 different ====
#merged with same; should not be counted twice

c64_pop <- 16
c64_success_percentage <- NA#100
c64_success_proportion <- NA#c64_success_percentage/100
c64_base_chance <-NA# 0.5 #???
c64_successes <- NA#round(c64_success_percentage*c64_pop/100) #percentage from yes or no question
c64_failures <- NA#c64_pop - c64_successes

c64_chisquared <- NA#unname(chisq.test(c(c64_successes, c64_failures), 
#                                    p = c(0.5, 0.5))$statistic)

c64_cohensd <- NA #esc_chisq(c64_chisquared,
#es.type = "d",
#totaln = c64_pop)$es

# c64_etasquared <- esc_chisq(c64_chisquared, 
#                             es.type = "eta", 
#                             totaln = c64_pop)$es

c64_phi <- NA #sqrt(c64_chisquared/c64_pop)

c64_r <- NA #c64_phi #esc_phi(phi = c64_phi, totaln = c64_pop, es.type = "r")


#==== 65. Lamy et al. (2006) exp. 3 RT ====

# Effect size is the same as for RT; should not be counted twice
c65_pop <- 9
c65_successes <- 3
c65_failures <- c65_pop - c65_successes
c65_success_percentage <- (c65_successes/c65_pop) * 100
c65_success_proportion <- c65_success_percentage/100
c65_base_chance <- 0.5 #yes/no

c65_chisquared <- unname(chisq.test(c(c65_successes, c65_failures), 
                                    p = c(0.5, 0.5))$statistic)

c65_cohensd <- esc_chisq(c65_chisquared,
                         es.type = "d",
                         totaln = c65_pop)$es

c65_etasquared <- esc_chisq(c65_chisquared,
                            es.type = "eta",
                            totaln = c65_pop)$es

c65_phi <- sqrt(c65_chisquared/c65_pop)

c65_r <- c65_phi #esc_phi(phi = c65_phi, totaln = c65_pop, es.type = "r")


#==== 66. Lamy et al. (2006) exp. 4 RT ====

# Effect size is the same as for RT; should not be counted twice
c66_pop <- 12
c66_successes <- 2
c66_failures <- c66_pop - c66_successes
c66_success_percentage <- (c66_successes/c66_pop) * 100
c66_success_proportion <- c66_success_percentage/100
c66_base_chance <- 0.5 #yes/no

c66_chisquared <- unname(chisq.test(c(c66_successes, c66_failures), 
                                    p = c(0.5, 0.5))$statistic)

c66_cohensd <- esc_chisq(c66_chisquared,
                         es.type = "d",
                         totaln = c66_pop)$es

c66_etasquared <- esc_chisq(c66_chisquared,
                            es.type = "eta",
                            totaln = c66_pop)$es

c66_phi <- sqrt(c66_chisquared/c66_pop)

c66_r <- c66_phi #esc_phi(phi = c66_phi, totaln = c66_pop, es.type = "r")


#==== 67. Lamy et al. (2006) exp. 5 easy ====

# Effect size is the same as for RT; should not be counted twice
c67_pop <- 13
c67_successes <- 1
c67_failures <- c67_pop - c67_successes
c67_success_percentage <- (c67_successes/c67_pop) * 100
c67_success_proportion <- c67_success_percentage/100
c67_base_chance <- 0.5 #yes/no

c67_chisquared <- unname(chisq.test(c(c67_successes, c67_failures), 
                                    p = c(0.5, 0.5))$statistic)

c67_cohensd <- esc_chisq(c67_chisquared,
                         es.type = "d",
                         totaln = c67_pop)$es

c67_etasquared <- esc_chisq(c67_chisquared,
                            es.type = "eta",
                            totaln = c67_pop)$es

c67_phi <- sqrt(c67_chisquared/c67_pop)

c67_r <- c67_phi #esc_phi(phi = c67_phi, totaln = c67_pop, es.type = "r")

#==== 68. Kimchi et al. (2004) exp. 1 - connected triangle/arrow, RT ====

# use reported number of noticers for "pattern" question
c68_pop <- 14
c68_success_percentage <- 93
c68_success_proportion <- c68_success_percentage/100
c68_base_chance <- 0.5 #???
c68_successes <- round(c68_success_percentage*c68_pop/100) #percentage from yes or no question
c68_failures <- c68_pop - c68_successes

c68_chisquared <- unname(chisq.test(c(c68_successes, c68_failures), 
                                    p = c(0.5, 0.5))$statistic)

c68_cohensd <- esc_chisq(c68_chisquared,
                         es.type = "d",
                         totaln = c68_pop)$es

# c68_etasquared <- esc_chisq(c68_chisquared, 
#                             es.type = "eta", 
#                             totaln = c68_pop)$es

c68_phi <- sqrt(c68_chisquared/c68_pop)

c68_r <- c68_phi #esc_phi(phi = c68_phi, totaln = c68_pop, es.type = "r")


#==== 69. Kimchi et al. (2004) exp. 1 - connected triangle/arrow, accuracy ====

# Effect size is the same as for RT; should not be counted twice
c69_pop <- 14
c69_success_percentage <- NA #93
c69_success_proportion <- NA #c69_success_percentage/100
c69_base_chance <- NA #0.5 #???
c69_successes <- NA #round(c69_success_percentage*c69_pop/100) #percentage from yes or no question
c69_failures <- NA #c69_pop - c69_successes

c69_chisquared <- NA #unname(chisq.test(c(c69_successes, c69_failures), 
#              p = c(0.5, 0.5))$statistic)

c69_cohensd <- NA #esc_chisq(c69_chisquared,
#      es.type = "d",
#     totaln = c69_pop)$es

# c69_etasquared <- esc_chisq(c69_chisquared, 
#                             es.type = "eta", 
#                             totaln = c69_pop)$es

c69_phi <- NA #sqrt(c69_chisquared/c69_pop)

c69_r <- NA #c69_phi #esc_phi(phi = c69_phi, totaln = c69_pop, es.type = "r")


#==== 70. Kimchi et al. (2004) exp. 2 - disconnected square/cross, RT ====

# use reported number of noticers for "pattern" question
c70_pop <- 12
c70_success_percentage <- 25
c70_success_proportion <- c70_success_percentage/100
c70_base_chance <- 0.5 #???
c70_successes <- round(c70_success_percentage*c70_pop/100) #percentage from yes or no question
c70_failures <- c70_pop - c70_successes

c70_chisquared <- unname(chisq.test(c(c70_successes, c70_failures), 
                                    p = c(0.5, 0.5))$statistic)

c70_cohensd <- esc_chisq(c70_chisquared,
                         es.type = "d",
                         totaln = c70_pop)$es

# c70_etasquared <- esc_chisq(c70_chisquared, 
#                             es.type = "eta", 
#                             totaln = c70_pop)$es

c70_phi <- sqrt(c70_chisquared/c70_pop)

c70_r <- c70_phi #esc_phi(phi = c70_phi, totaln = c70_pop, es.type = "r")


#==== 71. Kimchi et al. (2004) exp. 2 - disconnected square/cross, acc ====

# Effect size is the same as for RT; should not be counted twice
c71_pop <- 12
c71_success_percentage <- NA# 92
c71_success_proportion <- NA #c71_success_percentage/100
c71_base_chance <- NA #0.5 #???
c71_successes <- NA #round(c71_success_percentage*c71_pop/100) #percentage from yes or no question
c71_failures <- NA #c71_pop - c71_successes

c71_chisquared <- NA #unname(chisq.test(c(c71_successes, c71_failures), 
#               p = c(0.5, 0.5))$statistic)

c71_cohensd <- NA #esc_chisq(c71_chisquared,
#       es.type = "d",
#      totaln = c71_pop)$es

# c71_etasquared <- esc_chisq(c71_chisquared, 
#                             es.type = "eta", 
#                             totaln = c71_pop)$es

c71_phi <- NA #sqrt(c71_chisquared/c71_pop)

c71_r <- NA #c71_phi #esc_phi(phi = c71_phi, totaln = c71_pop, es.type = "r")

#========================================#
#==== Build vector with effect sizes =====
#========================================#

#=================== Cohen's d ====================
effect_indices <- c(1:11, 41:45, 12:20, 22:28, 30, 32, 33, 35, 36, 
                    38, 39, 46:57, 68, 69, 58:60, 70, 71, 61:67)
awareness_cohensd_names <- paste("c", effect_indices, "_cohensd",
                                 sep = "")

awareness_cohensd <- map_dbl(awareness_cohensd_names, get)

# Replace infinite d values for max d excluding NAs
non_NA_awareness_cohensd <- awareness_cohensd[!is.na(awareness_cohensd)] # remove missings
max_d <- max(non_NA_awareness_cohensd[-which(non_NA_awareness_cohensd==Inf)]) # compute max d without Inf values
awareness_cohensd[which(awareness_cohensd==Inf)] <- max_d # replace Inf values for max d

# Compute hedge's g
awareness_totaln <- map_dbl(paste("c", effect_indices, "_pop",
                                  sep = ""), get)

# awareness_hedgesg_2 <- hedges_g(awareness_cohensd, awareness_totaln)

awareness_chisquare_names <- paste("c", effect_indices, "_chisquared",
                                   sep = "")

awareness_chisquares <- map_dbl(awareness_chisquare_names, get)

# Compute hedge's g
awareness_hedgesg <- esc_chisq(awareness_chisquares,"g", totaln = awareness_totaln)$es

# Replace infinite hedge's g for max g excluding NAs
non_NA_awareness_hedgesg <- awareness_hedgesg[!is.na(awareness_hedgesg)] # remove missings
max_g <- max(non_NA_awareness_hedgesg[-which(non_NA_awareness_hedgesg==Inf)]) # compute max g without Inf values
awareness_hedgesg[which(awareness_hedgesg==Inf)] <- max_g # replace Inf values for max g


# SE of hedge's g
awareness_hedgesg_se <- esc_chisq(awareness_chisquares,"g", totaln = awareness_totaln)$se

# Replace infinite hedge's g SE for max SE excluding NAs
non_NA_awareness_hedgesg_se <- awareness_hedgesg_se[!is.na(awareness_hedgesg_se)] # remove missings
max_g_se <- max(non_NA_awareness_hedgesg_se[-which(non_NA_awareness_hedgesg_se==Inf)]) # compute max SE without Inf values
awareness_hedgesg_se[which(awareness_hedgesg_se==Inf)] <- max_g_se # replace Inf values for max g SE


#==================== r (correlation coefficient) ====================
awareness_r_names <- paste("c", effect_indices, "_r",
                           sep = "")

awareness_rs <- map_dbl(awareness_r_names, get)
#awareness_rs_nonNas <- awareness_rs[-which(is.na(awareness_rs))]

awareness_rs[which(awareness_rs == 1.0)] <- 0.999
awareness_rs[which(awareness_rs == -1.0)] <- -0.999
awareness_rs[which(awareness_rs == 0)] <- 0.001

# Transform to Fisher's z (Viechtbauer and Cheung (2010))
awareness_z_rs <- FisherZ(awareness_rs)
awareness_z_rs_nonNAs <- awareness_z_rs[-which(is.na(awareness_z_rs))]
