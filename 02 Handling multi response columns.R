###########################################
# Handling multiple response survey items #
###########################################

# Separating multiple answer variables
multi_resp <- c("GOV_ASSIST", "TECH_ACSS", "EDUC_MOD",
                "PREEX_COND", "DEP_TRT", "ANX_TRT",
                "HALUC_TYPE")

for (i in multi_resp) {
  split <- strsplit(rp_data[, i], ";")
  unique_resp <- unique(unlist(split))
  
  indicators <- lapply(unique_resp, function(response) {
    sapply(split, function(row_responses) {
      as.integer(response %in% row_responses)
    })
  })
  
  indicator_df <- as.data.frame(indicators)
  
  colnames(indicator_df) <- unique_resp
  
  rp_data <- cbind(rp_data, indicator_df)
}
View(rp_data)


# Columns resulting from separation of multi-response items
multi_df <- rp_data[, 120:ncol(rp_data)]
head(multi_df)

gov_asst <- multi_df[,1:99]
tech_accs <- multi_df[,100:105]
educ_mod <- multi_df[,106:109]
preex_cond <- multi_df[,110:201]
dep_trt <- multi_df[,202:]
anx_trt
haluc_type

colnames(multi_df)

# What's left for this script
# 1) Verify video game and social media variables are
#    correctly cleaned and labeled
# 2) Create REGION variable using MUNIC

# What's left for data cleansing and prepping
# 1) 01.2 Data Prepping - Eliminating careless responses (completion time method)
# 2) 02 Missing data analysis
# 3) 03 Missing data imputation
# 4) 04 Population characteristics analysis/comparison with institutional data
# 4) 05 Create/calculate sample weights (if necessary)