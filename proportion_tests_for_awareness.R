
library(pwr)
library(tidyverse)

# Read spreadsheet
power_table <- read_delim("C:/Users/Biosig/Google Drive/Doutorado/Tese/Meta-analysis/Dados/Analysis/power_awareness.txt", 
                          delim="\t", locale = locale(decimal_mark = ","))

View(power_table)
str(power_table)

# Change percentages to proportion
power_table$prop.noticers <- ifelse(power_table$prop.noticers > 1, 
                                    power_table$prop.noticers/100,
                                    power_table$prop.noticers)


power_table$prop.unnoticers <- ifelse(power_table$prop.unnoticers > 1, 
                                    power_table$prop.unnoticers/100,
                                    power_table$prop.unnoticers)


# Build vectors with values of P-hat, p and probability

successes <- power_table["N noticers"][[1]]
n <- power_table["N per group"][[1]]
probability <- 0.5

# compute value of proportion test if not yet available
chi_square_value <- sapply(successes, 
                        function(x,y) binom.test(x,y,p=0.5,
                                                 alternative = 'two.sided')$p.value, n)

chi_square_value <- vector(mode='numeric', length=length(successes))

chi_square_p_value <- vector(mode='numeric', length=length(successes))
for(i in seq_along(successes)) {
  test <- binom.test(successes[i], n[i], p=0.5, alternative = 'two.sided')
  chi_square_p_value[i] <- test$p.value
}


power_table$`X^2 test` <- ifelse(is.na(power_table$`X^2 test`), 
                                 chi_square_value,
                                 power_table$`X^2 test`)

power_table$p <- ifelse(is.na(power_table$p), 
                                 chi_square_value,
                                 power_table$`X^2 test`)

# Compute Cohen's H
power_table$cohensh <- ES.h(power_table$prop.unnoticers, power_table$prop.noticers)

# Compute summary effect size
sum_es <- sum((power_table$`N per group` * power_table$cohensh))/sum(power_table$`N per group`)

av_size <- mean(power_table$`N per group`)



