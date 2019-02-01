library(pwr)
library(tidyverse)

# Read spreadsheet
power_table <- read_delim("./power_implicit.csv", 
                          delim=",", locale = locale(decimal_mark = "."))

View(power_table)
str(power_table)

# Compute summary effect size
sum_es <- sum((power_table$n_per_group * power_table$cohens_d))/sum(power_table$n_per_group)

av_size <- mean(power_table$n_per_group)



