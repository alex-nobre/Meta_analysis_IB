
# Effect sizes:

# Experimental condition - control condition


#==== 1. Ariga et al. (2007) exp 2 ====

# Compute effect sizes from t-value and sample size

# Experimental condition: Invalid same-object
# Control condition: Invalid different object

c1_tvalue <- 0.61 # positive value, because experimental condition is larger
c1_n_pairs <- 20

c1_cohensd <- c1_tvalue/sqrt(c1_n_pairs)



#==== 2 Beanland and Pammer 1A fixating ====

#  Compute effect sizes from mean and variance

# Critical trial 1
sub_es_c2_t1 <- c()
  
# Critical trial 2
  
c2 <- c("Beanland and Pammer (2001) - exp 1A, fixating", )


#==== 3. Beanland and Pammer 1A moving ====

# Compute effect sizes from mean and variance

# Critical trial 1
c3 <- c("Beanland and Pammer (2001) - exp 1A, moving", )

# Critical trial 2


#==== 4. Beanland and Pammer 2 ====

#==== 5. Gabay et al. (2012) - Exp. 1 ====

# Compute effect sizes from F-value and dfs

# Experimental condition: valid
# Control condition: invalid

c5_fvalue <- -6.1 # negative value, because experimental condition is smaller
c5_dfeffect <- 1
c5_dferror <- 28

c5_p_eta_sqr <- (c5_fvalue * c5_dfeffect)/(c5_fvalue*c5_dfeffect+c5_dferror)

#==== 6. Gabay et al. (2012) - Exp. 2 ====

# Compute effect sizes from F-value and dfs

# Experimental condition: valid
# Control condition: invalid

c6_fvalue <- -4.4 # negative value, because experimental condition is smaller
c6_dfeffect <- 1
c6_dferror <- 21

c6_p_eta_sqr <- (c6_fvalue * c6_dfeffect)/(c6_fvalue*c6_dfeffect+c6_dferror)

#==== 6. Lo e Yeh (2008) - exp. 1 (200 ms) ====

# Compute effect sizes from t-value and sample size

# Experimental condition: with configuration/illusion present
# Control condition: no configuration/illusion absent

c7_tvalue 


#==== 10. Moore and Egeth (1997) ====

