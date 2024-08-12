# Testing different factor structures for analyzing depression and anxiety
# in university students from UPR-RP using structural equation modeling (SEM)

# Factorial structures tested were
# - Depression and Anxiety
# - Depression, Anxiety and Stress

# Models to be tested
# 1) single factor model
# 2) three-factor model
# 3) correlated factor model
# 4) hierarchical factor model
# 5) Bifactor model


library(readr)
library(MVN)
library(lavaan)
library(lavaanPlot)


Clean_rp_data_092023 <- read_csv("E:/Hernandez et al. 2024 Final Paper/Outputs/Clean_rp_data_092023.csv")
View(Clean_rp_data_092023)

setwd(file.path("E:/Hernandez et al. 2024 Final Paper/Scripts"))

data <- Clean_rp_data_092023[, c(paste0("PHQ", 1:9), 
                                 paste0("GAD", 1:7),
                                 paste0("PSS", 1:10))]

head(data)

# Data cleaning
dat <- apply(dat, MARGIN = 2, as.numeric)
head(dat)

dat <- na.omit(data)

# Checing multivariate normality
phq <- data[, paste0("PHQ", 1:9)]
gad <- data[, paste0("GAD", 1:7)]
pss <- data[, paste0("PSS", 1:10)]

mvn(phq) # -> non-normal
mvn(gad) # -> non-normal
mvn(pss) # -> non-normal

# Depression and Anxiety --------------------------------------------------

# single factor model
m1 <- "f1 =~ PHQ1 + PHQ2 + PHQ3 + PHQ4 + PHQ5 + PHQ6 + PHQ7 + PHQ8 + PHQ9 +
              GAD1 + GAD2 + GAD3 + GAD4 + GAD5 + GAD6 + GAD7"

model1 <- cfa(m1, dat, estimator = "dwls", std.lv = T, ordered = colnames(dat))
summary(model1, fit.measures = T, standardized = T)

# three-factor model
m2 <- "f1 =~ PHQ1 + PHQ2 + PHQ3 + PHQ4 + PHQ5 + PHQ6 + PHQ7 + PHQ8 + PHQ9
        f2 =~ GAD1 + GAD2 + GAD3 + GAD4 + GAD5 + GAD6 + GAD7"

model2 <- cfa(m2, dat, estimator = "dwls", std.lv = T, ordered = colnames(dat))
summary(model2, fit.measures = T, standardized = T)

# correlated factor model
m3 <- "f1 =~ PHQ1 + PHQ2 + PHQ3 + PHQ4 + PHQ5 + PHQ6 + PHQ7 + PHQ8 + PHQ9
        f2 =~ GAD1 + GAD2 + GAD3 + GAD4 + GAD5 + GAD6 + GAD7
        f1 ~~ f2"

model3 <- cfa(m3, dat, estimator = "dwls", std.lv = T, ordered = colnames(dat))
summary(model3, fit.measures = T, standardized = T)

# hierarchical factor model - NOT IDENTIFIED
# m4 <- "f1 =~ PHQ1 + PHQ2 + PHQ3 + PHQ4 + PHQ5 + PHQ6 + PHQ7 + PHQ8 + PHQ9
#         f2 =~ GAD1 + GAD2 + GAD3 + GAD4 + GAD5 + GAD6 + GAD7
#         g =~  f1 + f2"
# 
# model4 <- cfa(m4, dat, estimator = "dwls", std.lv = T)
# summary(model4, fit.measures = T)

# bifactor model - NOT IDENTIFIED
# m5 <- "f1 =~ PHQ1 + PHQ2 + PHQ3 + PHQ4 + PHQ5 + PHQ6 + PHQ7 + PHQ8 + PHQ9
#         f2 =~ GAD1 + GAD2 + GAD3 + GAD4 + GAD5 + GAD6 + GAD7
#         g  =~ PHQ1 + PHQ2 + PHQ3 + PHQ4 + PHQ5 + PHQ6 + PHQ7 + PHQ8 + PHQ9 +
#               GAD1 + GAD2 + GAD3 + GAD4 + GAD5 + GAD6 + GAD7"
# 
# model5 <- cfa(m5, dat, estimator = "dwls", std.lv = T)
# summary(model5, fit.measures = T)

# Fit measures #
fitmeasures(model1, c("chisq","df","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr"))
fitmeasures(model2, c("chisq","df","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr"))
fitmeasures(model3, c("chisq","df","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr"))


# Depression, Anxiety, and Stress -----------------------------------------

# single factor model
mm1 <- "f1 =~ PHQ1 + PHQ2 + PHQ3 + PHQ4 + PHQ5 + PHQ6 + PHQ7 + PHQ8 + PHQ9 +
              GAD1 + GAD2 + GAD3 + GAD4 + GAD5 + GAD6 + GAD7 +
              PSS1 + PSS2 + PSS3 + PSS4 + PSS5 + PSS6 + PSS7 + PSS8 + PSS9 + PSS10"

mod1 <- cfa(mm1, dat, estimator = "dwls", std.lv = T)
summary(mod1, fit.measures = T, standardized = T)

# three-factor model
mm2 <- "f1 =~ PHQ1 + PHQ2 + PHQ3 + PHQ4 + PHQ5 + PHQ6 + PHQ7 + PHQ8 + PHQ9
        f2 =~ GAD1 + GAD2 + GAD3 + GAD4 + GAD5 + GAD6 + GAD7
        f3 =~ PSS1 + PSS2 + PSS3 + PSS4 + PSS5 + PSS6 + PSS7 + PSS8 + PSS9 + PSS10"

mod2 <- cfa(mm2, dat, estimator = "dwls", std.lv = T)
summary(mod2, fit.measures = T, standardized = T)

# correlated factor model
mm3 <- "f1 =~ PHQ1 + PHQ2 + PHQ3 + PHQ4 + PHQ5 + PHQ6 + PHQ7 + PHQ8 + PHQ9
        f2 =~ GAD1 + GAD2 + GAD3 + GAD4 + GAD5 + GAD6 + GAD7
        f3 =~ PSS1 + PSS2 + PSS3 + PSS4 + PSS5 + PSS6 + PSS7 + PSS8 + PSS9 + PSS10
        f1 ~~ f2
        f1 ~~ f3
        f2 ~~ f3"

mod3 <- cfa(mm3, dat, estimator = "dwls", std.lv = T)
summary(mod3, fit.measures = T, standardized = T)

# hierarchical factor model - NOT IDENTIFIED
# mm4 <- "f1 =~ PHQ1 + PHQ2 + PHQ3 + PHQ4 + PHQ5 + PHQ6 + PHQ7 + PHQ8 + PHQ9
#         f2 =~ GAD1 + GAD2 + GAD3 + GAD4 + GAD5 + GAD6 + GAD7
#         f3 =~ PSS1 + PSS2 + PSS3 + PSS4 + PSS5 + PSS6 + PSS7 + PSS8 + PSS9 + PSS10
#         g =~  f1 + f2 + f3"
# 
# mod4 <- cfa(mm4, dat, estimator = "dwls", std.lv = T)
# summary(mod4, fit.measures = T)

# bifactor model - NOT IDENTIFIED
# mm5 <- "f1 =~ PHQ1 + PHQ2 + PHQ3 + PHQ4 + PHQ5 + PHQ6 + PHQ7 + PHQ8 + PHQ9
#         f2 =~ GAD1 + GAD2 + GAD3 + GAD4 + GAD5 + GAD6 + GAD7
#         f3 =~ PSS1 + PSS2 + PSS3 + PSS4 + PSS5 + PSS6 + PSS7 + PSS8 + PSS9 + PSS10
#         g  =~ PHQ1 + PHQ2 + PHQ3 + PHQ4 + PHQ5 + PHQ6 + PHQ7 + PHQ8 + PHQ9 +
#               GAD1 + GAD2 + GAD3 + GAD4 + GAD5 + GAD6 + GAD7 +
#               PSS1 + PSS2 + PSS3 + PSS4 + PSS5 + PSS6 + PSS7 + PSS8 + PSS9 + PSS10"
# 
# mod5 <- cfa(mm5, dat, estimator = "dwls", std.lv = T)
# summary(mod5, fit.measures = T)


fitmeasures(mod1, c("chisq","df","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr"))
fitmeasures(mod2, c("chisq","df","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr"))
fitmeasures(mod3, c("chisq","df","cfi","rmsea","rmsea.ci.lower","rmsea.ci.upper","srmr"))

lavaan::