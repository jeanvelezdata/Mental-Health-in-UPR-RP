#########################################################
# This script preps the final data frame from the       #
# mental health in university students from UPRRP study #
#########################################################

# Load necessary packages
library(readxl)
library(tidyr)
library(dplyr)
library(naniar)
library(lubridate)

# Create file paths
outputsFolder <- file.path("Outputs")
dataFolder <- file.path("Raw Data")

# Importing data 
# rp_data <- read_excel("E:/Hernandez et al. 2024 Final Paper/Raw Data/Salud mental y el uso de la tecnolog?a durante el COVID-19(1-911).xlsx", sheet = "Sheet 1")
# View(rp_data)

# rp_data <- read_excel("E:/Hernandez et al. 2024 Final Paper/Raw Data/Raw_Data_Final_n911_JHT.xlsx")
rp_data <- read_excel(file.path("..", dataFolder, "Raw_Data_Final_n911_JHT.xlsx"))
View(rp_data)

# Renaming columns for ease of use 
colnames(rp_data) <- c("ID", "START", "END", "EMAIL", "NAME", paste0("CON_COMP_", 1:10), "CONSENT", "SEX", "GENDER", "SEX_ORIENT", 
                       "AGE", "HISP_ORIG", "RACE", "MARITAL_STAT", "MUNIC", "INCOME_YR", "EMPLOY", "OTHR_EMPLOY", "WRK_HRS_WK",
                       "LOST_JOB_CV19", "GOV_ASSIST_", "ACA_YR", "CRD_HRS", "FACUL", "ACA_LVL", "GPA_SR", "UNIV_ECON_AST", "TECH_ACSS",
                       "DIFF_ACSS", "PART_DROP", "EDUC_MOD", "PREEX_COND", "DEP_HIST", "DEP_TRT", "ANX_HIST", "ANX_TRT", "CV19_HIST",
                       "VAX_STAT", "VAX_TYPE", "VAX_DOSE", paste0("PHQ", 1:9), "PHQ_FUNC", paste0("GAD", 1:7), paste0("COV", 1:6), 
                       paste0("PSS", 1:10), paste0("TILS", 1:3), "SOC_MED", "SM_TIME_DAY", "SM_TIME_WK_YAM", "SM_REAS", "SM_TIME_DAY_BP",
                       "SM_TIME_W_BP_YAM", "SM_REAS_BP", "SM_IMP_QOL", "VID_GMS", "VG_TYPE", "VG_VIOL", "VG_TIME_DAY", "VG_TIME_WK_YAM",
                       "VG_TIME_DAY_YAM", "VG_TIME_WK_DP_YAM", "VG_TIME_DAY_DP_YAM", "VG_REAS", "VG_TIME_DAY_BP", "VG_REAS_BP", 
                       "VG_IMP_QOL", "EXER_LAST7", "EXER_DAYS_IN_LAST7", "EXER_DURAT_MIN", "SLEEP_HRS", "EAT_OUT", "EAT_FRT", "EAT_VEG",
                       "ALC", "ALC_30", "NICOT", "NICOT_TYPE", "NICOT_30", "CANNA", "CANNA_30", "COCAINE", "COC_TYPE","COC_POWD_30", "CRACK_30",
                       "MED", "MED_TYPE", "MED_30", "HALUC", "HALUC_TYPE", "HALUC_30")

# Replace other answers considered to be NA
na <- c("Prefiero no contestar", "Prefiero no contestar;", "Prefiero no contestar;Nada;",
        "Prefiero no contestar;Cáncer;","No se", "no se", "No sé", "no sé", 
        "Prefiero no contestar;Hipoglucemica;", "Prefiero no contestar;Ninguna;", "",
        "No recuerdo", "No tengo una respuesta al momento", "No estoy segura", 
        "No tengo conocimiento actual", "3.40?", "No sé", ".", "3->")
rp_data <- replace_with_na_all(rp_data, ~.x %in% na)



# Evaluating data structure
str(rp_data)

# Eliminate variables no necessary or containing information not needed for analysis
# This information might be useful for future researchers, but so far we only need CONSENT and CRD_HRS to assess eligibility
rp_data <- rp_data[,-which(colnames(rp_data) %in% c("EMAIL","NAME",paste0("CON_COMP_", 1:10)))]

############################################################################
############################ Feature engineering ###########################
############################################################################

# ID
rp_data$ID <- c(1:nrow(rp_data))

# START
rp_data$START <- as.POSIXct(rp_data$START)

# END
rp_data$END <- as.POSIXct(rp_data$END)

# CONSENT
rp_data$CONSENT <- recode(rp_data$CONSENT, "No consiento a participar en este estudio de investigación." = 0, "Sí, libremente consiento a participar en este estudio de investigación. Al autorizar este consentimiento informado, no he renunciado a ninguno de mis derechos legales." = 1)
rp_data$CONSENT <- as.factor(rp_data$CONSENT)

# SEX
rp_data$SEX <- recode(rp_data$SEX, "Femenino" = "F", "Masculino" = "M", "género fluido" = "Other",
                      "Genero fluido" = "Other", "Género fluído" = "Other", "Bigender" = "Other", 
                      "No-binario(a)" = "Other")
rp_data$SEX <- as.factor(rp_data$SEX)

# GENDER
rp_data$GENDER <- recode(rp_data$GENDER, "Femenino" = "F", "Masculino" = "M", "género fluido" = "Other",
                         "Genero fluido" = "Other", "Género fluído" = "Other", "Bigender" = "Other", 
                         "No-binario(a)" = "Other")
rp_data$GENDER <- relevel(as.factor(rp_data$GENDER), ref = "M")

# SEX_ORIENT
table(rp_data$SEX_ORIENT)
rp_data$SEX_ORIENT <- recode(rp_data$SEX_ORIENT, "Heterosexual" = "Hetero", "Homosexual" = "Non-Hetero", "Pansexual o queer" = "Non-Hetero", "asexual" = "Non-Hetero", "Asexual" = "Non-Hetero", "Bisexual" = "Non-Hetero", "Demisexual" = "Non-Hetero")
rp_data$SEX_ORIENT <- as.factor(rp_data$SEX_ORIENT)

# AGE
rp_data$AGE <- as.numeric(rp_data$AGE)

# Categorical AGE
rp_data$AGE_CAT <- cut(rp_data$AGE, breaks = c(16,18,21,26,58), labels = c("16-17","18-20","21-25","26-57"))
table(rp_data$AGE_CAT)

# HISP_ORIG
table(rp_data$HISP_ORIG, useNA = "always")
rp_data$HISP_ORIG <- recode(rp_data$HISP_ORIG,
                            "No, no soy de origen hispano, latino o español" = "Not Hispanic",
                            "Sí, cubano(a)" = "Cuban",
                            "Sí, dominicano(a)" = "Dominican",
                            "Sí, mexicano(a), mexicanoamericano(a), chicano(a)" = "Mexican",
                            "Sí, otro origen hispano, latino o español" = "Other Hispanic",
                            "Sí, puertorriqueño(a)" = "Puerto Rican")

# HISP_ORIG_BIN
table(rp_data$HISP_ORIG, useNA = "always")
rp_data$HISP_ORIG_BIN <- recode(rp_data$HISP_ORIG,
                            "No, no soy de origen hispano, latino o español" = "No",
                            "Sí, cubano(a)" = "Yes",
                            "Sí, dominicano(a)" = "Yes",
                            "Sí, mexicano(a), mexicanoamericano(a), chicano(a)" = "Yes",
                            "Sí, otro origen hispano, latino o español" = "Yes",
                            "Sí, puertorriqueño(a)" = "Yes")

# RACE
table(rp_data$RACE, useNA = "always")
rp_data$RACE <- recode(rp_data$RACE,
                                "Blanca" = "White",
                                "Negra o afroamericana " = "Black",
                                "China" = "Non-White",
                                "Japonesa" = "Non-White",
                                "Indígena de las Américas o nativa de Alaska" = "Non-White",
                                "Sí, puertorriqueño(a)" = "Yes")

# MARITAL_STAT
table(rp_data$MARITAL_STAT, useNA = "always")
rp_data$MARITAL_STAT <- recode(rp_data$MARITAL_STAT,
                       "Casado(a) o conviviendo" = "Married",
                       "Divorciado(a)" = "Divor/Wid",
                       "Soltero(a)" = "Single",
                       "Viudo(a)" = "Divor/Wid")

# MUNIC
table(rp_data$MUNIC, useNA = "always")
# Region of origin categories
# i.e. EAST, WEST, NORTH, SOUTH, METRO
rp_data$REGION_RES <-
  ifelse(rp_data$MUNIC %in% c(
    "Toa Baja","Toa Alta","Bayamón","Dorado","Vega Alta",
    "Corozal","Orocovis","Barranquitas","Comerío","Naranjito",
    "Cataño"),
    "Bayamón",
    
  ifelse(rp_data$MUNIC %in% c(
    "Trujillo Alto","Carolina","Canóvanas",
    "Loíza", "Guaynabo", "San Juan"), 
    "Metro",
    
  ifelse(rp_data$MUNIC %in% c("Ceiba", "Fajardo",
    "Rio Grande","Vieques","Culebra","Luquillo"),
    "Fajardo",
    
  ifelse(rp_data$MUNIC %in% c("Aguas Buenas",
    "Caguas","Gurabo","Juncos","Las Piedras", 
    "Humacao","Yabucoa","Maunabo","San Lorenzo",
    "Naguabo","Cayey","Aibonito","Cidra"),
    "Caguas",
    
  ifelse(rp_data$MUNIC %in% c("Patillas",
    "Arroyo", "Guayama","Salinas","Coamo",
    "Santa Isabel","Juana Díaz","Villalba",
    "Ponce","Peñuelas","Guayanilla","Yauco",
    "Guánica"),
    "Ponce",
    
  ifelse(rp_data$MUNIC %in% c("Morovis",
    "Vega Baja","Manatí","Ciales","Barceloneta",
    "Florida","Arecibo","Hatillo","Camuy","Lares",
    "Quebradillas","Utuado"),
    "Arecibo",
    
  ifelse(rp_data$MUNIC %in% c("Isabela",
    "San Sebastián","Moca","Aguadilla","Aguada",
    "Añasco","Las Marías","Maricao","Sabana Grande",
    "Lajas","Cabo Rojo","Hormigueros","Mayagüez",
    "San Germán","Rincón"),
    "Mayagüez", rp_data$MUNIC
      
  )))))))

# INCOME_YR
table(rp_data$INCOME_YR, useNA = "always")
rp_data$INCOME_YR <- recode(rp_data$INCOME_YR,
                            "$25,000 - $49,999" = "$25,000+",
                            "$50,000 - $74,999" = "$25,000+",
                            "$75,000 - $99,999" = "$25,000+",
                            "$100,000 - $149,000" = "$25,000+")

rp_data$INCOME_YR <- as.factor(rp_data$INCOME_YR)

# EMPLOY
table(rp_data$EMPLOY)
rp_data$EMPLOY <- recode(rp_data$EMPLOY,
                         "Desempleado(a)" = "No",
                         "Empleado(a)" = "Yes",
                         "Discapacitado(a)" = "No",
                         "Trabajo por cuenta propia" = "SelfEmp",
                         "Retirado(a)" = "No")
rp_data$EMPLOY <- as.factor(rp_data$EMPLOY)

# OTHR_EMPLOY
table(rp_data$OTHR_EMPLOY, useNA = "always")
rp_data$OTHR_EMPLOY <- recode(rp_data$OTHR_EMPLOY,
                              "No aplica" = "No",
                              "Sí" = "Yes")
rp_data$OTHR_EMPLOY <- ifelse(rp_data$EMPLOY == "No" & is.na(rp_data$OTHR_EMPLOY),
                              "No", rp_data$OTHR_EMPLOY)
rp_data$OTHR_EMPLOY <- as.factor(rp_data$OTHR_EMPLOY)


# WRK_HRS_WK
table(rp_data$WRK_HRS_WK, useNA = "always")
rp_data$WRK_HRS_WK <- recode(rp_data$WRK_HRS_WK,
                             "Más de 40" = ">40")
rp_data$WRK_HRS_WK <- ifelse(rp_data$EMPLOY == "No" & is.na(rp_data$WRK_HRS_WK),
                              "No", rp_data$WRK_HRS_WK)
rp_data$WRK_HRS_WK <- as.factor(rp_data$WRK_HRS_WK)

# LOST_JOB_CV19
table(rp_data$LOST_JOB_CV19, useNA = "always")
rp_data$LOST_JOB_CV19 <- recode(rp_data$LOST_JOB_CV19,
                                "Sí" = "Yes")
rp_data$LOST_JOB_CV19 <- as.factor(rp_data$LOST_JOB_CV19)

# GOV_ASSIST
table(rp_data$GOV_ASSIST, useNA = "always")

# ACA_YR
table(rp_data$ACA_YR, useNA = "always")
rp_data$ACA_YR <- recode(rp_data$ACA_YR,
                         "1ero" = "1",
                         "2do" = "2",
                         "3ro" = "3",
                         "4to" = "4",
                         "5to" = "5+",
                         "6to" = "5+",
                         "7mo o más" = "5+")
rp_data$ACA_YR <- as.factor(rp_data$ACA_YR)

# CRD_HRS
table(rp_data$CRD_HRS)
rp_data$CRD_HRS <- recode(rp_data$CRD_HRS, "16 o mas" = "16+",
                          "mas de 21" = "16+",
                          "No estoy matriculado(a) actualmente" = "0")
rp_data$CRD_HRS <- as.ordered(rp_data$CRD_HRS)

# FACUL
table(rp_data$FACUL, useNA = "always")
rp_data$FACUL <- recode(rp_data$FACUL,
                        "Ciencias y Tecnologías de la Información" = "Comunicacion",
                        "Comunicación" = "Comunicacion",
                        "Administración de Empresas" = "Administracion de Empresas",
                        "Planificación" = "Planificacion")
rp_data$FACUL <- as.factor(rp_data$FACUL)

# ACA_LVL
rp_data$ACA_LVL <- recode(rp_data$ACA_LVL, "Bachillerato" = "BA", "Grado asociado" = "ASSOC", "Maestría" = "MAST", "Doctorado" = "DOC", "Juris Doctor" = "DOC")
rp_data$ACA_LVL <- as.factor(rp_data$ACA_LVL)

# ACA_LVL - Binary
rp_data$ACA_LVL_BI <- recode(rp_data$ACA_LVL, "BA" = "SUB", "ASSOC" = "SUB", "MAST" = "GRAD", "DOC" = "GRAD")
rp_data$ACA_LVL_BI <- as.factor(rp_data$ACA_LVL_BI)

# GPA_SR
table(rp_data$GPA_SR)
rp_data$GPA_SR <- recode(rp_data$GPA_SR,
                      "4.00 escolar" = "4.00",
                      "361" = "3.61",
                      "+3.50" = "3.50",
                      "3. 5" = "3.50",
                      "3. 94" = "3.94",
                      "3.95 - 4.00" = "3.98",
                      "4,00" = "4.00",
                      "Current: 4.00 Cumulative: 3.129" = "4.00")

# rp_data$GPA_SR <- replace(rp_data$GPA_SR, list = c(which(rp_data$GPA_SR == "No se")), NA)
rp_data$GPA_SR <- as.numeric(rp_data$GPA_SR)

# UNIV_ECON_AST
table(rp_data$UNIV_ECON_AST, useNA = "always")
rp_data$UNIV_ECON_AST <- recode(rp_data$UNIV_ECON_AST,
                                "Sí" = "Yes")
rp_data$UNIV_ECON_AST <- as.factor(rp_data$UNIV_ECON_AST)

# TECH_ACSS
table(rp_data$TECH_ACSS, useNA = "always")
# Revise: try splitting answers into different columns by ";"
# Have tried various functions but none produce separate columns
# for each answer separated by ";"

# Revise test: is this solution what you would like?
#test <- tidyr::separate(rp_data, 'TECH_ACSS', paste("TECH_ACSS", 1:5, sep="_"), sep=";")


# DIFF_ACSS
table(rp_data$DIFF_ACSS, useNA = "always")
rp_data$DIFF_ACSS <- recode(rp_data$DIFF_ACSS,
                                "Sí" = "Yes")
rp_data$DIFF_ACSS <- as.factor(rp_data$DIFF_ACSS)

# PART_DROP
table(rp_data$PART_DROP, useNA = "always")
rp_data$PART_DROP <- recode(rp_data$PART_DROP,
                            "Sí" = "Yes")
rp_data$PART_DROP <- as.factor(rp_data$PART_DROP)

# EDUC_MOD
table(rp_data$EDUC_MOD, useNA = "always")
# Revise: try splitting answers into different columns by ";"
# Have tried various functions but none produce separate columns
# for each answer separated by ";"

# Revise test: is this solution what you would like?
#test <- tidyr::separate(rp_data, 'EDUC_MOD', paste("EDUC_MOD", 1:3, sep="_"), sep=";")

# PREEX_COND
table(rp_data$PREEX_COND, useNA = "always")
# Revise: try splitting answers into different columns by ";"
# Have tried various functions but none produce separate columns
# for each answer separated by ";"

# Revise test: is this solution what you would like?
# test <- tidyr::separate(rp_data, 'PREEX_COND', paste("PREEX_COND", 1:11, sep="_"), sep=";")

# DEP_HIST
rp_data$DEP_HIST <- recode(rp_data$DEP_HIST, "Sí" = "Yes")
rp_data$DEP_HIST <- as.factor(rp_data$DEP_HIST)

# DEP_TRT
table(rp_data$DEP_TRT, useNA = "always")
# Revise: try splitting answers into different columns by ";"
# Have tried various functions but none produce separate columns
# for each answer separated by ";"

# Revise test: is this solution what you would like?
# test <- tidyr::separate(rp_data, 'DEP_TRT', paste("DEP_TRT", 1:3, sep="_"), sep=";")

# ANX_HIST
rp_data$ANX_HIST <- recode(rp_data$ANX_HIST, "Sí" = "Yes")
rp_data$ANX_HIST <- as.factor(rp_data$ANX_HIST)

# ANX_TRT
table(rp_data$ANX_TRT, useNA = "always")
# Revise: try splitting answers into different columns by ";"
# Have tried various functions but none produce separate columns
# for each answer separated by ";"

# Revise test: is this solution what you would like?
# test <- tidyr::separate(rp_data, 'ANX_TRT', paste("ANX_TRT", 1:3, sep="_"), sep=";")

# CV19_HIST
table(rp_data$CV19_HIST, useNA = "always")
rp_data$CV19_HIST <- recode(rp_data$CV19_HIST, "Sí" = "Yes")
rp_data$CV19_HIST <- as.factor(rp_data$CV19_HIST)

# VAX_STAT
table(rp_data$VAX_STAT, useNA = "always")
rp_data$VAX_STAT <- recode(rp_data$VAX_STAT, "Sí" = "Yes")
rp_data$VAX_STAT <- as.factor(rp_data$VAX_STAT)

# VAX_TYPE
table(rp_data$VAX_TYPE, useNA = "always")
rp_data$VAX_TYPE <- ifelse(rp_data$VAX_STAT == "No", "No Vax", rp_data$VAX_TYPE)
rp_data$VAX_TYPE <- as.factor(rp_data$VAX_TYPE)

# VAX_DOSE
table(rp_data$VAX_DOSE, useNA = "always")
rp_data$VAX_DOSE <- ifelse(rp_data$VAX_STAT == "No", "None", rp_data$VAX_DOSE)
rp_data$VAX_DOSE <- as.factor(rp_data$VAX_DOSE)

# PHQ9
rp_data$PHQ1 <- recode(rp_data$PHQ1, "No, para nada (1)" = 0, "Varios días (2)" = 1, "Durante más de la mitad de los días (3)" = 2, "Casi todos los días (4)" = 3)
rp_data$PHQ2 <- recode(rp_data$PHQ2, "No, para nada (1)" = 0, "Varios días (2)" = 1, "Durante más de la mitad de los días (3)" = 2, "Casi todos los días (4)" = 3)
rp_data$PHQ3 <- recode(rp_data$PHQ3, "No, para nada (1)" = 0, "Varios días (2)" = 1, "Durante más de la mitad de los días (3)" = 2, "Casi todos los días (4)" = 3)
rp_data$PHQ4 <- recode(rp_data$PHQ4, "No, para nada (1)" = 0, "Varios días (2)" = 1, "Durante más de la mitad de los días (3)" = 2, "Casi todos los días (4)" = 3)
rp_data$PHQ5 <- recode(rp_data$PHQ5, "No, para nada (1)" = 0, "Varios días (2)" = 1, "Durante más de la mitad de los días (3)" = 2, "Casi todos los días (4)" = 3)
rp_data$PHQ6 <- recode(rp_data$PHQ6, "No, para nada (1)" = 0, "Varios días (2)" = 1, "Durante más de la mitad de los días (3)" = 2, "Casi todos los días (4)" = 3)
rp_data$PHQ7 <- recode(rp_data$PHQ7, "No, para nada (1)" = 0, "Varios días (2)" = 1, "Durante más de la mitad de los días (3)" = 2, "Casi todos los días (4)" = 3)
rp_data$PHQ8 <- recode(rp_data$PHQ8, "No, para nada (1)" = 0, "Varios días (2)" = 1, "Durante más de la mitad de los días (3)" = 2, "Casi todos los días (4)" = 3)
rp_data$PHQ9 <- recode(rp_data$PHQ9, "No, para nada (1)" = 0, "Varios días (2)" = 1, "Durante más de la mitad de los días (3)" = 2, "Casi todos los días (4)" = 3)

rp_data$PHQ1 <- as.numeric(rp_data$PHQ1)
rp_data$PHQ2 <- as.numeric(rp_data$PHQ2)
rp_data$PHQ3 <- as.numeric(rp_data$PHQ3)
rp_data$PHQ4 <- as.numeric(rp_data$PHQ4)
rp_data$PHQ5 <- as.numeric(rp_data$PHQ5)
rp_data$PHQ6 <- as.numeric(rp_data$PHQ6)
rp_data$PHQ7 <- as.numeric(rp_data$PHQ7)
rp_data$PHQ8 <- as.numeric(rp_data$PHQ8)
rp_data$PHQ9 <- as.numeric(rp_data$PHQ9)

# PHQ_FUNC
table(rp_data$PHQ_FUNC, useNA = "always")
rp_data$PHQ_FUNC <- case_when(rp_data$PHQ_FUNC == rp_data$PHQ_FUNC[4] ~ 1,
            rp_data$PHQ_FUNC == rp_data$PHQ_FUNC[2] ~ 2,
            rp_data$PHQ_FUNC == rp_data$PHQ_FUNC[14] ~ 3,
            rp_data$PHQ_FUNC == rp_data$PHQ_FUNC[28] ~ 4)
rp_data$PHQ_FUNC <- as.factor(rp_data$PHQ_FUNC)
# Revise: This code is a temporary fix. Function is not interpreting the character value correctly.
# This code makes sure the desired value in the column is used for the logical test. A dynamic fix is needed.

# GAD
table(rp_data$GAD1, useNA = "always")
rp_data$GAD1 <- recode(rp_data$GAD1, "Ninguna (0)" = 0, "Por varios días (1)" = 1, "Durante más de la mitad de los días (2)" = 2, "Casi todos los días (3)" = 3)
rp_data$GAD2 <- recode(rp_data$GAD2, "Ninguna (0)" = 0, "Por varios días (1)" = 1, "Durante más de la mitad de los días (2)" = 2, "Casi todos los días (3)" = 3)
rp_data$GAD3 <- recode(rp_data$GAD3, "Ninguna (0)" = 0, "Por varios días (1)" = 1, "Durante más de la mitad de los días (2)" = 2, "Casi todos los días (3)" = 3)
rp_data$GAD4 <- recode(rp_data$GAD4, "Ninguna (0)" = 0, "Por varios días (1)" = 1, "Durante más de la mitad de los días (2)" = 2, "Casi todos los días (3)" = 3)
rp_data$GAD5 <- recode(rp_data$GAD5, "Ninguna (0)" = 0, "Por varios días (1)" = 1, "Durante más de la mitad de los días (2)" = 2, "Casi todos los días (3)" = 3)
rp_data$GAD6 <- recode(rp_data$GAD6, "Ninguna (0)" = 0, "Por varios días (1)" = 1, "Durante más de la mitad de los días (2)" = 2, "Casi todos los días (3)" = 3)
rp_data$GAD7 <- recode(rp_data$GAD7, "Ninguna (0)" = 0, "Por varios días (1)" = 1, "Durante más de la mitad de los días (2)" = 2, "Casi todos los días (3)" = 3)

rp_data$GAD1 <- as.numeric(rp_data$GAD1)
rp_data$GAD2 <- as.numeric(rp_data$GAD2)
rp_data$GAD3 <- as.numeric(rp_data$GAD3)
rp_data$GAD4 <- as.numeric(rp_data$GAD4)
rp_data$GAD5 <- as.numeric(rp_data$GAD5)
rp_data$GAD6 <- as.numeric(rp_data$GAD6)
rp_data$GAD7 <- as.numeric(rp_data$GAD7)

# COV19QoL
table(rp_data$COV1, useNA = "always")
rp_data$COV1 <- recode(rp_data$COV1, "Muy desacuerdo (1)" = 1, "Desacuerdo (2)" = 2, "Ni acuerdo ni desacuerdo (3)" = 3, "De acuerdo (4)" = 4, "Muy de acuerdo (5)" = 5)
rp_data$COV2 <- recode(rp_data$COV2, "Muy desacuerdo (1)" = 1, "Desacuerdo (2)" = 2, "Ni acuerdo ni desacuerdo (3)" = 3, "De acuerdo (4)" = 4, "Muy de acuerdo (5)" = 5)
rp_data$COV3 <- recode(rp_data$COV3, "Muy desacuerdo (1)" = 1, "Desacuerdo (2)" = 2, "Ni acuerdo ni desacuerdo (3)" = 3, "De acuerdo (4)" = 4, "Muy de acuerdo (5)" = 5)
rp_data$COV4 <- recode(rp_data$COV4, "Muy desacuerdo (1)" = 1, "Desacuerdo (2)" = 2, "Ni acuerdo ni desacuerdo (3)" = 3, "De acuerdo (4)" = 4, "Muy de acuerdo (5)" = 5)
rp_data$COV5 <- recode(rp_data$COV5, "Muy desacuerdo (1)" = 1, "Desacuerdo (2)" = 2, "Ni acuerdo ni desacuerdo (3)" = 3, "De acuerdo (4)" = 4, "Muy de acuerdo (5)" = 5)
rp_data$COV6 <- recode(rp_data$COV6, "Muy desacuerdo (1)" = 1, "Desacuerdo (2)" = 2, "Ni acuerdo ni desacuerdo (3)" = 3, "De acuerdo (4)" = 4, "Muy de acuerdo (5)" = 5)

rp_data$COV1 <- as.numeric(rp_data$COV1)
rp_data$COV2 <- as.numeric(rp_data$COV2)
rp_data$COV3 <- as.numeric(rp_data$COV3)
rp_data$COV4 <- as.numeric(rp_data$COV4)
rp_data$COV5 <- as.numeric(rp_data$COV5)
rp_data$COV6 <- as.numeric(rp_data$COV6)

# PSS-10 - positive items reverse coded (4,5,7,8)
table(rp_data$PSS1, useNA = "always")
rp_data$PSS1 <- recode(rp_data$PSS1, "Nunca (1)" = 0, "Casi nunca (2)" = 1, "De vez en cuando (3)" = 2, "A menudo (4)" = 3, "Muy a menudo (5)" = 4)
rp_data$PSS2 <- recode(rp_data$PSS2, "Nunca (1)" = 0, "Casi nunca (2)" = 1, "De vez en cuando (3)" = 2, "A menudo (4)" = 3, "Muy a menudo (5)" = 4)
rp_data$PSS3 <- recode(rp_data$PSS3, "Nunca (1)" = 0, "Casi nunca (2)" = 1, "De vez en cuando (3)" = 2, "A menudo (4)" = 3, "Muy a menudo (5)" = 4)
rp_data$PSS4 <- recode(rp_data$PSS4, "Nunca (1)" = 4, "Casi nunca (2)" = 3, "De vez en cuando (3)" = 2, "A menudo (4)" = 1, "Muy a menudo (5)" = 0)
rp_data$PSS5 <- recode(rp_data$PSS5, "Nunca (1)" = 4, "Casi nunca (2)" = 3, "De vez en cuando (3)" = 2, "A menudo (4)" = 1, "Muy a menudo (5)" = 0)
rp_data$PSS6 <- recode(rp_data$PSS6, "Nunca (1)" = 0, "Casi nunca (2)" = 1, "De vez en cuando (3)" = 2, "A menudo (4)" = 3, "Muy a menudo (5)" = 4)
rp_data$PSS7 <- recode(rp_data$PSS7, "Nunca (1)" = 4, "Casi nunca (2)" = 3, "De vez en cuando (3)" = 2, "A menudo (4)" = 1, "Muy a menudo (5)" = 0)
rp_data$PSS8 <- recode(rp_data$PSS8, "Nunca (1)" = 4, "Casi nunca (2)" = 3, "De vez en cuando (3)" = 2, "A menudo (4)" = 1, "Muy a menudo (5)" = 0)
rp_data$PSS9 <- recode(rp_data$PSS9, "Nunca (1)" = 0, "Casi nunca (2)" = 1, "De vez en cuando (3)" = 2, "A menudo (4)" = 3, "Muy a menudo (5)" = 4)
rp_data$PSS10 <- recode(rp_data$PSS10, "Nunca (1)" = 0, "Casi nunca (2)" = 1, "De vez en cuando (3)" = 2, "A menudo (4)" = 3, "Muy a menudo (5)" = 4)

rp_data$PSS1 <- as.numeric(rp_data$PSS1)
rp_data$PSS2 <- as.numeric(rp_data$PSS2)
rp_data$PSS3 <- as.numeric(rp_data$PSS3)
rp_data$PSS4 <- as.numeric(rp_data$PSS4)
rp_data$PSS5 <- as.numeric(rp_data$PSS5)
rp_data$PSS6 <- as.numeric(rp_data$PSS6)
rp_data$PSS7 <- as.numeric(rp_data$PSS7)
rp_data$PSS8 <- as.numeric(rp_data$PSS8)
rp_data$PSS9 <- as.numeric(rp_data$PSS9)
rp_data$PSS10 <- as.numeric(rp_data$PSS10)

# TILS
table(rp_data$TILS1, useNA = "always")
rp_data$TILS1 <- recode(rp_data$TILS1, "Casi nunca" = "1", "A veces" = "2", "A menudo" = "3")
rp_data$TILS2 <- recode(rp_data$TILS2, "Casi nunca" = "1", "A veces" = "2", "A menudo" = "3")
rp_data$TILS3 <- recode(rp_data$TILS3, "Casi nunca" = "1", "A veces" = "2", "A menudo" = "3")

rp_data$TILS1 <- as.numeric(rp_data$TILS1)
rp_data$TILS2 <- as.numeric(rp_data$TILS2)
rp_data$TILS3 <- as.numeric(rp_data$TILS3)

#SOC_MED 
rp_data$SOC_MED <- recode(rp_data$SOC_MED, "Sí" = "Yes")
rp_data$SOC_MED <- as.factor(rp_data$SOC_MED)

# SM_TIME_DAY
rp_data$SM_TIME_DAY <- ifelse(is.na(rp_data$SM_TIME_DAY) & rp_data$SOC_MED == "No", "0", rp_data$SM_TIME_DAY)
rp_data$SM_TIME_DAY <- recode(rp_data$SM_TIME_DAY, "0" = 0, "Menos de 2 horas" = 1, "2-3 horas" = 2, "4-6 horas" = 3,
                              "7-12 horas" = 4, "Más de 12 horas" = 4)
rp_data$SM_TIME_DAY <- as.ordered(rp_data$SM_TIME_DAY)

# SM_TIME_WK_YAM
table(rp_data$SM_TIME_WK_YAM, useNA = "always")
rp_data$SM_TIME_WK_YAM <- factor(rp_data$SM_TIME_WK_YAM,
                        levels = c("Menos de 2 horas",
                                   "2-3 horas",
                                   "4-6 horas",
                                   "7-12 horas",
                                   "Más de 12 horas"),
                        labels = c("<2","2-3","4-6","7-12",
                                   ">12"), ordered = T)

# This line of code 
rp_data$SM_TIME_WK_YAM <- ifelse(rp_data$SOC_MED == "No" & rp_data$ID >= 148,
                        "0", rp_data$SM_TIME_WK_YAM)

# SM_REAS
rp_data$SM_REAS <- factor(rp_data$SM_REAS,
                          levels = c("Entretenimiento",
                                     "Manejo de emociones negativas",
                                     "Manejo de estrés",
                                     "Para escapar de la realidad",
                                     "Socialización"),
                          labels = c(1:5), ordered = F)
rp_data$SM_REAS <- ifelse(is.na(rp_data$SM_REAS) & rp_data$SOC_MED == "No", "NoSocMed", rp_data$SM_REAS)

# SM_TIME_DAY_BP
table(rp_data$SM_TIME_DAY_BP, useNA = "always")
rp_data$SM_TIME_DAY_BP <- factor(rp_data$SM_TIME_WK_YAM,
                                 levels = c("Menos de 2 horas",
                                            "2-3 horas",
                                            "4-6 horas",
                                            "7-12 horas",
                                            "Más de 12 horas"),
                                 labels = c("<2","2-3","4-6","7-12",
                                            ">12"), ordered = T)
rp_data$SM_TIME_DAY_BP <- ifelse(is.na(rp_data$SM_TIME_DAY_BP) & rp_data$SOC_MED == "No", "0", rp_data$SM_TIME_DAY_BP)


# SM_TIME_W_BP_YAM
rp_data$SM_WK_BP_YAM <- factor(rp_data$SM_TIME_WK_YAM,
                           levels = c("Menos de 2 horas",
                                      "2-3 horas",
                                      "4-6 horas",
                                      "7-12 horas",
                                      "Más de 12 horas"),
                           labels = c("<2","2-3","4-6","7-12",
                                      ">12"), ordered = T)
rp_data$SM_WK_BP_YAM <- ifelse(rp_data$SOC_MED == "No" & rp_data$ID >= 148,
                           "0", rp_data$SM_WK_BP_YAM)

# SM_REAS_BP
rp_data$SM_REAS_BP <- factor(rp_data$SM_REAS_BP,
                             levels = c("Entretenimiento",
                                        "Manejo de emociones negativas",
                                        "Manejo de estrés",
                                        "Para escapar de la realidad",
                                        "Socialización"),
                             labels = c(1:5), ordered = F)
rp_data$SM_REAS_BP <- ifelse(is.na(rp_data$SM_REAS_BP) & rp_data$SOC_MED == "No", "NoSocMed", rp_data$SM_REAS_BP)

# SM_IMP_QOL
rp_data$SM_IMP_QOL <- factor(rp_data$SM_IMP_QOL,
                         levels = c("No, ninguno.",
                                    "Sí, un impacto positivo.",
                                    "Sí, un impacto negativo."),
                         labels = c("None","Pos","Neg"), ordered = F)
rp_data$SM_IMP_QOL <- ifelse(is.na(rp_data$SM_IMP_QOL) & rp_data$SOC_MED == "No", "NoSocMed", rp_data$SM_IMP_QOL)

# VID_GMS
rp_data$VID_GMS <- recode(rp_data$VID_GMS, "Sí" = "Si")
rp_data$VID_GMS <- as.factor(rp_data$VID_GMS)

# VG_TYPE
rp_data$VG_TYPE <- factor(rp_data$VG_TYPE,
                          levels = c("Multijugador en línea ",
                                     "Un solo jugador"),
                          labels = c("Multi","Solo"), ordered = F)
rp_data$VG_TYPE <- ifelse(is.na(rp_data$VG_TYPE) & rp_data$VID_GMS == "No", "NoPlayVG", rp_data$VG_TYPE)

# VG_VIOL
rp_data$VG_VIOL <- factor(rp_data$VG_VIOL,
                          levels = c("Nunca","Casi nunca",
                                     "De vez en cuando",
                                     "A menudo","Muy a menudo"),
                          labels = c(0:4), ordered = T)
rp_data$VG_VIOL <- ifelse(is.na(rp_data$VG_VIOL) & rp_data$VID_GMS == "No", "NoPlayVG", rp_data$VG_VIOL)

# VG_TIME_DAY
rp_data$VG_TIME_DAY <- ifelse(is.na(rp_data$VG_TIME_DAY) & rp_data$VID_GMS == "No", "0", rp_data$VG_TIME_DAY)
rp_data$VG_TIME_DAY <- recode(rp_data$VG_TIME_DAY, "0" = 0 ,"Menos de 2 horas" = 1, "2-3 horas" = 2, "4-6 horas" = 3, "7-12 horas" = 4, "Más de 12 horas" = 4)
rp_data$VG_TIME_DAY <- as.ordered(rp_data$VG_TIME_DAY)

# VG_TIME_WK_YAM
rp_data$VG_TIME_WK_YAM <- factor(rp_data$VG_TIME_WK_YAM,
                        levels = c("1-2 horas",
                                   "3-4 horas",
                                   "5-6 horas",
                                   "7-8 horas",
                                   "9-10 horas",
                                   "11 horas o más"),
                        labels = c("1-2","3-4","5-6","7-8",
                                   "9-10","11+"), ordered = T)

rp_data$VG_TIME_WK_YAM <- ifelse(rp_data$VID_GMS == "No" & rp_data$ID >= 148,
                        "0", rp_data$VG_TIME_WK_YAM)

# VG_TIME_DAY_YAM
rp_data$VG_TIME_DAY_YAM <- factor(rp_data$VG_TIME_DAY_YAM,
                         levels = c("1-2 horas",
                                    "3-4 horas",
                                    "5-6 horas",
                                    "7-8 horas",
                                    "9-10 horas",
                                    "11 horas o más"),
                         labels = c("1-2","3-4","5-6","7-8",
                                    "9-10","11+"), ordered = T)
rp_data$VG_TIME_DAY_YAM <- ifelse(rp_data$VID_GMS == "No" & rp_data$ID >= 148,
                         "0", rp_data$VG_TIME_DAY_YAM)


# VG_TIME_WK_DP_YAM
rp_data$VG_TIME_WK_DP_YAM <- factor(rp_data$VG_TIME_WK_DP_YAM,
                           levels = c("1-2 horas",
                                      "3-4 horas",
                                      "5-6 horas",
                                      "7-8 horas",
                                      "9-10 horas",
                                      "11 horas o más"),
                           labels = c("1-2","3-4","5-6","7-8",
                                      "9-10","11+"), ordered = T)

rp_data$VG_TIME_WK_DP_YAM <- ifelse(rp_data$VID_GMS == "No" & rp_data$ID >= 148,
                           "0", rp_data$VG_TIME_WK_DP_YAM)

# VG_TIME_DAY_DP_YAM
rp_data$VG_TIME_DAY_DP_YAM <- factor(rp_data$VG_TIME_DAY_DP_YAM,
                            levels = c("1-2 horas",
                                       "3-4 horas",
                                       "5-6 horas",
                                       "7-8 horas",
                                       "9-10 horas",
                                       "11 horas o más"),
                            labels = c("1-2","3-4","5-6","7-8",
                                       "9-10","11+"), ordered = T)

rp_data$VG_TIME_DAY_DP_YAM <- ifelse(rp_data$VID_GMS == "No" & rp_data$ID >= 148,
                            "0", rp_data$VG_TIME_DAY_DP_YAM)

# VG_REAS
rp_data$VG_REAS <- factor(rp_data$VG_REAS,
                          levels = c("Entretenimiento",
                                     "Manejo de emociones negativas",
                                     "Manejo de estrés",
                                     "Para escapar de la realidad",
                                     "Socialización"),
                          labels = c(1:5), ordered = F)
rp_data$VG_REAS <- ifelse(is.na(rp_data$VG_REAS) & rp_data$VID_GMS == "No", "NoPlayVG", rp_data$VG_REAS)

# VG_TIME_DAY_BP
rp_data$VG_TIME_DAY_BP <- factor(rp_data$VG_TIME_DAY_BP,
                             levels = c("Menos de 2 horas",
                                        "2-3 horas",
                                        "4-6 horas",
                                        "7-12 horas",
                                        "Más de 12 horas"),
                             labels = c("<2","2-3","4-6","7-12",
                                        ">12"), ordered = T)
rp_data$VG_TIME_DAY_BP <- ifelse(is.na(rp_data$VG_TIME_DAY_BP) & rp_data$VID_GMS == "No",
                             "0", rp_data$VG_TIME_DAY_BP)

# VG_REAS_BP
rp_data$VG_REAS_BP <- factor(rp_data$VG_REAS_BP,
                             levels = c("Entretenimiento",
                                        "Manejo de emociones negativas",
                                        "Manejo de estrés",
                                        "Para escapar de la realidad",
                                        "Socialización"),
                             labels = c(1:5), ordered = F)
rp_data$VG_REAS_BP <- ifelse(is.na(rp_data$VG_REAS_BP) & rp_data$VID_GMS == "No", "NoPlayVG", rp_data$VG_REAS_BP)

# VG_IMP_QOL
rp_data$VG_IMP_QOL <- factor(rp_data$VG_IMP_QOL,
                         levels = c("No, ninguno.",
                                    "Sí, un impacto positivo.",
                                    "Sí, un impacto negativo."),
                         labels = c("No","Pos","Neg"), ordered = F)
rp_data$VG_IMP_QOL <- ifelse(is.na(rp_data$VG_IMP_QOL) & rp_data$VID_GMS == "No", "NoPlayVG", rp_data$VG_REAS_BP)

# EXER_LAST7
table(rp_data$EXER_LAST7, useNA = "always")
rp_data$EXER_LAST7 <- recode(rp_data$EXER_LAST7,
                             "Sí" = "Yes")
rp_data$EXER_LAST7 <- as.factor(rp_data$EXER_LAST7)

# EXER_DAYS_IN_LAST7
table(rp_data$EXER_DAYS_IN_LAST7, useNA = "always")
rp_data$EXER_DAYS_IN_LAST7 <- recode(rp_data$EXER_DAYS_IN_LAST7,
                                     "Todos los días" = "7")
rp_data$EXER_DAYS_IN_LAST7 <- ifelse(rp_data$EXER_LAST7 == "No", "0",
                                     rp_data$EXER_DAYS_IN_LAST7)
rp_data$EXER_DAYS_IN_LAST7 <- as.factor(rp_data$EXER_DAYS_IN_LAST7)

# EXER_DURAT_MIN
table(rp_data$EXER_DURAT_MIN, useNA = "always")
rp_data$EXER_DURAT_MIN <- recode(rp_data$EXER_DURAT_MIN,
                             "Menos de 10 minutos" = "<=20",
                             "11-20 minutos" = "<=20",
                             "21-30 minutos" = "21-30",
                             "31-45 minutos" = "31-45",
                             "46-60 minutos" = "46-60",
                             "Más de 60 minutos" = ">60")
rp_data$EXER_DURAT_MIN <- ifelse(rp_data$EXER_LAST7 == "No", "0",
                                     rp_data$EXER_DURAT_MIN)
rp_data$EXER_DURAT_MIN <- as.factor(rp_data$EXER_DURAT_MIN)

# SLEEP_HRS
table(rp_data$SLEEP_HRS, useNA = "always")
rp_data$SLEEP_HRS <- recode(rp_data$SLEEP_HRS,
                            "1-2" = "1-4",
                            "3-4" = "1-4",
                            "9-10" = "9+",
                            "11+" = "9+")
rp_data$SLEEP_HRS <- as.factor(rp_data$SLEEP_HRS)

# EAT_OUT
table(rp_data$EAT_OUT, useNA = "always")
rp_data$EAT_OUT <- recode(rp_data$EAT_OUT,
                          "Ninguna" = "0",
                          "Más de 21" = "10-21",
                          "10-12" = "10+",
                          "13-15" = "10+",
                          "16-18" = "10+",
                          "19-21" = "10+")
rp_data$EAT_OUT <- as.factor(rp_data$EAT_OUT)

# EAT_FRT
table(rp_data$EAT_FRT, useNA = "always")
rp_data$EAT_FRT <- recode(rp_data$EAT_FRT,
                          "Ninguna" = "0",
                          "5-6" = "5-7",
                          "Todos los días" = "5-7")
rp_data$EAT_FRT <- as.factor(rp_data$EAT_FRT)

# EAT_VEG
table(rp_data$EAT_VEG, useNA = "always")
rp_data$EAT_VEG <- recode(rp_data$EAT_VEG,
                          "Ninguna" = "0",
                          "5-6" = "5-7",
                          "Todos los días" = "5-7")
rp_data$EAT_VEG <- as.factor(rp_data$EAT_VEG)

# ALC
table(rp_data$ALC, useNA = "always")
rp_data$ALC <- recode(rp_data$ALC,
                      "Sí" = "Yes")
rp_data$ALC <- as.factor(rp_data$ALC)

# ALC_30
table(rp_data$ALC_30, useNA = "always")
rp_data$ALC_30 <- recode(rp_data$ALC_30,
                         "Los 30 dias" = "30")
rp_data$ALC_30 <- ifelse(rp_data$ALC == "No", 0, rp_data$ALC_30)
rp_data$ALC_30 <- as.factor(rp_data$ALC_30)

# NICOT
table(rp_data$NICOT, useNA = "always")
rp_data$NICOT <- recode(rp_data$NICOT,
                      "Sí" = "Yes")
rp_data$NICOT <- as.factor(rp_data$NICOT)

# NICOT_TYPE
table(rp_data$NICOT_TYPE, useNA = "always")
rp_data$NICOT_TYPE <- recode(rp_data$NICOT_TYPE,
                             "Cigarillos de cajetilla" = "CIG",
                             "Cigarillos electronicos" = "E-CIG",
                             "Otros productos de tabaco (cigarros, pipas, masticar tabaco, cigarrillos envueltos a mano, y tabaco en polvo" = "OTHER")
rp_data$NICOT_TYPE <- ifelse(rp_data$NICOT == "No", "NONE", rp_data$NICOT_TYPE)
rp_data$NICOT_TYPE <- as.factor(rp_data$NICOT_TYPE)

# NICOT_30
table(rp_data$NICOT_30, useNA = "always")
rp_data$NICOT_30 <- recode(rp_data$NICOT_30,
                         "Los 30 días" = "30")
rp_data$NICOT_30 <- ifelse(rp_data$NICOT == "No", 0, rp_data$NICOT_30)
rp_data$NICOT_30 <- as.factor(rp_data$NICOT_30)

# CANNA
table(rp_data$CANNA, useNA = "always")
rp_data$CANNA <- recode(rp_data$CANNA,
                        "Sí" = "Yes")
rp_data$CANNA <- as.factor(rp_data$CANNA)

# CANNA_30
table(rp_data$CANNA_30, useNA = "always")
rp_data$CANNA_30 <- recode(rp_data$CANNA_30,
                           "Los 30 días" = "30")
rp_data$CANNA_30 <- ifelse(rp_data$CANNA == "No",
                           "0",
                           rp_data$CANNA_30)
rp_data$CANNA_30 <- as.factor(rp_data$CANNA_30)

# COCAINE
table(rp_data$COCAINE, useNA = "always")
rp_data$COCAINE <- recode(rp_data$COCAINE,
                        "Sí" = "Yes")
rp_data$COCAINE <- as.factor(rp_data$COCAINE)

# COC_TYPE
table(rp_data$COC_TYPE, useNA = "always")
rp_data$COC_TYPE <- ifelse(rp_data$COCAINE == "No",
                           "NONE",
                           rp_data$COC_TYPE)
rp_data$COC_TYPE <- as.factor(rp_data$COC_TYPE)

# COC_POWD_30
table(rp_data$COC_POWD_30, useNA = "always")
rp_data$COC_POWD_30 <- ifelse(rp_data$COCAINE == "No","0",
                  rp_data$COC_POWD_30)
rp_data$COC_POWD_30 <- as.factor(rp_data$COC_POWD_30)

# CRACK_30
table(rp_data$CRACK_30, useNA = "always")
rp_data$CRACK_30 <- ifelse(rp_data$COCAINE == "No","0",
                           rp_data$CRACK_30)
rp_data$CRACK_30 <- as.factor(rp_data$CRACK_30)

# MED
table(rp_data$MED, useNA = "always")
rp_data$MED <- recode(rp_data$MED,
                          "Sí" = "Yes")
rp_data$MED <- as.factor(rp_data$MED)

# MED_TYPE
table(rp_data$MED_TYPE, useNA = "always")
rp_data$MED_TYPE <- recode(rp_data$MED_TYPE,
                           "Analgésicos" = "Analges",
                           "Estimulantes" = "Stimul",
                           "Tranquilizantes" = "Tranquil",
                           "Sedantes" = "Sedat")
rp_data$MED_TYPE <- ifelse(rp_data$MED == "No", "NONE",
                           rp_data$MED_TYPE)
rp_data$MED_TYPE <- as.factor(rp_data$MED_TYPE)

# MED_30
table(rp_data$MED_30, useNA = "always")
rp_data$MED_30 <- recode(rp_data$MED_30,
                         "Los 30 días" = "30")
rp_data$MED_30 <- ifelse(rp_data$MED == "No",
                         "0", rp_data$MED_30)
rp_data$MED_30 <- as.factor(rp_data$MED_30)

# HALUC
table(rp_data$HALUC, useNA = "always")
rp_data$HALUC <- recode(rp_data$HALUC,
                          "Sí" = "Yes")
rp_data$HALUC <- as.factor(rp_data$HALUC)

# HALUC_TYPE
table(rp_data$HALUC_TYPE, useNA = "always")
# Revise: try splitting answers into different columns by ";"
# Have tried various functions but none produce separate columns
# for each answer separated by ";"

# HALUC_30
table(rp_data$HALUC_30, useNA = "always")
rp_data$HALUC_30 <- recode(rp_data$HALUC_30,
                         "Los 30 días" = "30")
rp_data$HALUC_30 <- ifelse(rp_data$HALUC == "No",
                         "0", rp_data$HALUC_30)
rp_data$HALUC_30 <- as.factor(rp_data$HALUC_30)

# Score scales # 
phq <- grep("PHQ", colnames(rp_data), ignore.case = T)
rp_data$PHQ_9 <- rowSums(rp_data[,phq[1:9]])

cov <- grep("COV", colnames(rp_data), ignore.case = T)
rp_data$COVQoL <- rowSums(rp_data[,cov[1:6]])

gad <- grep("GAD", colnames(rp_data), ignore.case = T)
rp_data$GAD_7 <- rowSums(rp_data[,gad[1:7]])

pss <- grep("PSS", colnames(rp_data), ignore.case = T)
rp_data$PSS_10 <- rowSums(rp_data[,pss[1:10]])

tils <- grep("TILS", colnames(rp_data), ignore.case = T)
rp_data$TILS <- rowSums(rp_data[,tils[1:3]])

# PHQ-9 Categories #
rp_data$PHQ_CAT <- cut(rp_data$PHQ_9, breaks = c(0,5,10,15,20,28), right=T, labels=c("None", "Mild", "Moderate", "Moderately Severe", "Severe"))
summary(rp_data$PHQ_CAT)

# GAD7 Categories #
rp_data$GAD_CAT <- cut(rp_data$GAD_7, breaks = c(0,5,10,15,22), right=T, labels=c("Minimal", "Mild", "Moderate", "Severe"))
summary(rp_data$GAD_CAT)

# PSS-10 Categories
rp_data$PSS_CAT <- cut(rp_data$PSS_10, breaks = c(0,14,27,41), include.lowest = T, labels=c("Low", "Moderate", "High"))
summary(rp_data$PSS_CAT)

# What's left for this script
# 1) Fix variables with multiple answers (Potentially fixed) 
#    - Check suggested solution
# 2) Implement dynamic fix at line 394

# Commit test

