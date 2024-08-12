# Post-stratification weights #
###############################

# Weighting variables:
# - Sex (2)
# - Graduate/Undergraduate Level (3)
# - Facultad (10)
# - Academic year (5)


install.packages("combinat")
library(combinat)

# Number of combinations of categories for wieghting 
choose(20,4)

# Expected count per cell
nrow(rp_data)/choose(20,4) 


