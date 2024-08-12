################################################################
######################## Data Quality ##########################
################################################################

# Typo in variable AGE
which(rp_data$AGE=="2")

# Typo replaced with NA
rp_data$AGE <- replace(rp_data$AGE, list = c(which(rp_data$AGE=="2")), NA)

# Eliminate those who did not consent to participate
rp_data <- rp_data[-c(which(rp_data$CONSENT=="0")),]
911 - nrow(rp_data) #20

# Eliminate those who were not enrolled when taking the survey
rp_data <- rp_data[-c(which(rp_data$CRD_HRS=="0")),]
891 - nrow(rp_data) #4

# Eliminate rows with completion time of less than 8.5 minutes #
################################################################

# Adding a variable - time to complete survey (TIME)
rp_data$TIME <- difftime(rp_data$END, rp_data$START, units = "mins")
rp_data$TIME <- as.numeric(rp_data$TIME)

# Adding a variable - proportion of missing values per row (PROP_MISS_all)
rp_data <- add_prop_miss(rp_data, label = "PROP_MISS")
summary(rp_data$PROP_MISS_all)

# Exploring proportion of missingness and completion time
# summary(rp_data$PROP_MISS_all*100)
# sd(rp_data$PROP_MISS_all*100)
# hist(rp_data$PROP_MISS_all)
# summary(rp_data$TIME)
# var(rp_data$TIME)

# Plotting completion time to proportion of missingness per case
plot(rp_data$TIME, rp_data$PROP_MISS_all, xlim = c(0,50), ylab = "Proportion of missing items", xlab = "Completion time")
abline(v = 6.25, h = 0.2, col = "red", lwd=2)

# Creating a careless response (CR) dummy variable for either filtering
# or later adjustment
rp_data$CR_ind <- ifelse(rp_data$TIME < 6.25 | rp_data$PROP_MISS_all > 0.2,
                         1, 0)

length(which(rp_data$TIME < 6.25))

# Exploring the response patterns of careless responders in the sample
# cr <- length(which(rp_data$CR_ind == 1)); cr
# cr/nrow(rp_data)*100
# 
# rp_data_fil <- filter(rp_data, CR_ind == 0)
# plot(rp_data_fil$TIME, rp_data_fil$PROP_MISS_all, xlim = c(0,20), ylab = "Proportion of missing items", xlab = "Completion time")
# abline(v = 6.25, h = 0.2, col = "red", lwd=2)
# 
# View(rp_data[which(rp_data$PROP_MISS_all > 0.2),])

# What's left for data cleansing and prepping
# 2) Missing data analysis
# 3) Missing data imputation
# 4) Population characteristics analysis/comparison with institutional data
# 4) Create/calculate sample weights (if necessary)


# **** FOR PURPOSES OF RESEARCH PAPERS ****

write.csv(rp_data, "Clean_rp_data_092023.csv")

