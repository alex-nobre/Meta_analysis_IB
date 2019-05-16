
# Import packages
library(tidyverse)
library(irr)

# Load data
rater_data <- read_delim("./Data_for_agreement_computation.csv",
                         # The arguments below are formatted for data
                         # created in Excel with the Brazilian format
                         delim = ";",
                         locale = locale(decimal_mark = ","))
rater_data$Rater_1 <- factor(rater_data$Rater_1)
rater_data$Rater_2 <- factor(rater_data$Rater_2)

# Compute Cohen's kappa
kappa2(rater_data[,c(3,4)], "unweighted")

