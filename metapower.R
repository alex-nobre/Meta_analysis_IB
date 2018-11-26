# Adapted from link

es <- sum_es # Enter your summary effect size
as <- av_size  # Average per number per group
mk <- length(power_table$cohensh)  # Number of effect sizes
hg <- 3  # Heterogeniety (".33" for small, "1" for moderate, & "3" for large)

eq1 <- ((as+as)/((as)*(as))) + ((es^2)/(2*(as+as)))
eq2 <- hg*(eq1)
eq3 <- eq2+eq1
eq4 <- eq3/mk
eq5 <- (es/sqrt(eq4))
Power <- (1-pnorm(1.96-eq5)) # Two-tailed
Power