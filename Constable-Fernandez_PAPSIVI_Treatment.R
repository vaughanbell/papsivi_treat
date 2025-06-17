library(dplyr)
library(tibble)
library(stringr)
library(tidyr)
library(gtsummary)
library(table1)
library(lmtest)
library(fastDummies)
library(multiwayvcov)    

rm(list = ls())

output_dir <- "S:/PAPSIVI_Data/VaughanR/PAPSIVI/paper2/output/"

read_in_minsalud_file <- function(filename) {
  # Read in datafile
  flat_df <- read.csv(filename, header = TRUE, sep = '|', stringsAsFactors = TRUE, encoding = "UTF-8")
  
  # Rename miscoded PersonaID variable label
  flat_df <- flat_df %>%
    rename(personid = X.U.FEFF.PersonaID)
  
  # Convert all the variable names to lowercase
  names(flat_df) <- tolower(names(flat_df))
  
  return(flat_df)
}

ruv_filename = "S:/PAPSIVI_Data/data_files/ExtraccionRUV20211201.txt"
ruv_df <- read_in_minsalud_file(ruv_filename)

# Remove last row (it's empty)
ruv_df <- ruv_df |>
  filter(row_number() <= n() -1)

# Remove ZonaResidencia variable (has 'SIN INFORMACION' for everything)
ruv_df$zonaresidencia <- NULL

# Extract the municipio code from the mpioresidencia and put it into a separate variable municipio_id
ruv_df <- ruv_df |>
  mutate(municipio_id = str_split(mpioresidencia, " - ", simplify = TRUE)[,1])

ruv_df <- ruv_df |>
  mutate(hechovictimizante = case_match(hechovictimizante,
                                        "ABANDONO O DESPOJO FORZADO DE TIERRAS" ~ "despojo_tierras",
                                        "ACTO TERRORISTA / ATENTADOS / COMBATES / ENFRENTAMIENTOS / HOSTIGAMIENTOS" ~ "hostigamientos",
                                        "AMENAZA" ~ "amenaza",
                                        "CONFIMANIENTO" ~ "confinamiento",
                                        "DELITOS CONTRA LA LIBERTAD Y LA INTEGRIDAD SEXUAL EN DESARROLLO DEL CONFLICTO ARMADO" ~ "violenciasexual",
                                        "DESAPARICIÓN FORZADA" ~ "desparacion",
                                        "DESPLAZAMIENTO FORZADO" ~ "desplazamiento",
                                        "HOMICIDIO" ~ "homocidio",
                                        "LESIONES PERSONALES FISICAS" ~ "lesion_fis",
                                        "LESIONES PERSONALES PSICOLOGICAS" ~ "lesion_psic",
                                        "MINAS ANTIPERSONAL, MUNICIÓN SIN EXPLOTAR Y ARTEFACTO EXPLOSIVO IMPROVISADO" ~ "minas",
                                        "PERDIDA DE BIENES MUEBLES O INMUEBLES" ~ "perdida_bienes",
                                        "SECUESTRO" ~ "secuestro",
                                        "TORTURA" ~ "tortura",
                                        "VINCULACIÓN DE NIÑOS NIÑAS Y ADOLESCENTES A ACTIVIDADES RELACIONADAS CON GRUPOS ARMADOS" ~ "reclut_ninos",
                                        "SIN INFORMACIÓN" ~ "no_info"))

# Recode anyone 'older' than 110 to missing
ruv_df <- ruv_df %>%
  mutate(edad = ifelse(edad > 110, NA, edad)) %>%
  mutate(edad = ifelse(edad == 0, NA, edad))

# Recode sex
ruv_df <- ruv_df |>
  mutate(sexo = case_match(sexo,
                           "HOMBRE" ~ "Male",
                           "MUJER" ~ "Female",
                           "LGBTI" ~ "Other",
                           "NO DEFINIDO" ~ NA))

ruv_df$sexo <- factor(ruv_df$sexo, ordered = FALSE)
ruv_df$sexo <- relevel(ruv_df$sexo, ref = "Male")

# Recode ethnicity
ruv_df <- ruv_df |>
  mutate(etnia = case_match(etnia,
                            "1 - INDÍGENA" ~ "Indigenous",
                            "2 - ROM (GITANO)" ~ "Roma",
                            "3 - RAIZAL (SAN ANDRES Y PROVIDENCIA)" ~ "Raizal",
                            "4 - PALENQUERO DE SAN BASILIO" ~ "Palenquero de San Basilio",
                            "5 - NEGRO, MULATO, AFROCOLOMBIANO O AFRODESCENCIENTE" ~ "Afrocolombian",
                            "NO DEFINIDO" ~ "White or Mestiza"))
ruv_df$etnia <- as.factor(ruv_df$etnia)

# Create ethnic minority variable
ruv_df <- ruv_df |>
  mutate(etnia_min = ifelse(etnia == "White or Mestiza", "No", "Yes"))

ruv_df$etnia_min <- factor(ruv_df$etnia_min, ordered = FALSE)
ruv_df$etnia_min <- relevel(ruv_df$etnia_min, ref = "No")

# Recode indicador PAPSIVI to from YES / NO to 1 / 0 and label
ruv_df <- ruv_df |>
  mutate(indicadorpapsivi = ifelse(indicadorpapsivi == "NO", 0, 1))

ruv_df$indicadorpapsivi <- factor(ruv_df$indicadorpapsivi,
                                  levels = c(0, 1),
                                  labels = c("No", "Yes"))

# Recode indicador Discapacidad to from YES / NO to 1 / 0 and label
ruv_df <- ruv_df |>
  mutate(indicadordiscapacidad = ifelse(indicadordiscapacidad == "NO", 0, 1))

ruv_df$indicadordiscapacidad <- factor(ruv_df$indicadordiscapacidad,
                                       levels = c(0, 1),
                                       labels = c("No", "Yes"))

# Load and recode Regimen de salud file
#

regimensal_filename = "S:/PAPSIVI_Data/data_files/ExtraccionRUV_RUAFSalud.txt"
regimensal_df <- read_in_minsalud_file(regimensal_filename)

# Recode healthcare regime
regimensal_df <- regimensal_df |>
  mutate(tipo_regimen = case_match(tipo_regimen,
                                   "S - SUBSIDIADO" ~ "Subsidised",
                                   "C - CONTRIBUTIVO" ~ "Contributive"))

regimensal_df$tipo_regimen <- factor(regimensal_df$tipo_regimen, ordered = FALSE)
regimensal_df$tipo_regimen <- relevel(regimensal_df$tipo_regimen, ref = "Contributive")

# Merge Registro de victimas and Regimen de salud files into new dataframe df
df <- ruv_df %>%
  left_join(regimensal_df, by = "personid", multiple = "first")

# Load and recode CERAC data
#

cerac_filename = "S:/PAPSIVI_Data/data_files/CERAC_Data/CERAC_Data_2000_2012.csv"
cerac_df <- read.csv(cerac_filename, header = TRUE, stringsAsFactors = TRUE, encoding = "UTF-8")

# Change variable names
cerac_df <- cerac_df %>%
  rename(municipio = Municipio) %>%
  rename(exp = Grupo.de.categoría) %>%
  rename(municipio_id = X.U.FEFF.Divipola)

# Label the cat variable conflict exposure
attr(cerac_df$exp, "label") <- "Conflict exposure"

# Quick function to convert municipio_id to the correct format (string with leading zeros)
convert_to_municipio_id_str <- function(num) {
  if (num < 10000) {
    return(sprintf("%05d", num))  # Add leading zeros if less than 1000
  } else {
    return(as.character(num))  # Convert to string without leading zeros
  }
}

# Convert all the municipio ids to the correct format using the above function
cerac_df$municipio_id <- sapply(cerac_df$municipio_id, convert_to_municipio_id_str)

# Recode the conflict exposure category so the highest number is highest exposure
cerac_df <- cerac_df |>
  mutate(exp = case_when(
    exp == "1" ~ as.factor(7),
    exp == "2" ~ as.factor(6),
    exp == "3" ~ as.factor(5),
    exp == "4" ~ as.factor(4),
    exp == "5" ~ as.factor(3),
    exp == "6" ~ as.factor(2),
    exp == "7" ~ as.factor(1)
  ))

# Make sure the conflict exposure category is an ordered factor
cerac_df$exp <- as.ordered(cerac_df$exp)

# Merge CERAC data 
df <- df %>%
  left_join(cerac_df, by = "municipio_id", multiple = "first")

df <- df %>%
  mutate(cerac = ifelse(Intensidad == "alta intensidad", "high", "low")) 

# Load and recode RUV papsivi file (contains info on sessions)
#

ruv_papsivi_filename = "S:/PAPSIVI_Data/data_files/ExtraccionRUV_PAPSIVI.txt"
ruvpap_df <- read_in_minsalud_file(ruv_papsivi_filename)

# Drop indicadorpapsivi as all responses are yes 
ruvpap_df$indicadorpapsivi <- NULL

# Remove tema papsivi as will only consider modalidad 
ruvpap_df$temaatencionpapsivi <- NULL
# Remove last row
ruvpap_df <- ruvpap_df |>
  filter(row_number() <= n() -1)

# Merge 
df <- df %>%
  left_join(ruvpap_df, by = "personid", relationship = "many-to-many")

##### load datasets for previous mental health 

# This is RUV_RIPS atenciones dataset (outpatient data)
RIPS_transtmentales_filename = "S:/PAPSIVI_Data/data_files/ExtraccionRUV_RIPS_AtencTranstMentales.txt"
RIPS_transtmentales <- read_in_minsalud_file(RIPS_transtmentales_filename)

# Remove last row
RIPS_transtmentales <- RIPS_transtmentales |>
  filter(row_number() <= n() -1)

# Remove grupodiagnostico as all the same 
RIPS_transtmentales$grupodiagnostico <- NULL

# This keeps only ICD code label 
RIPS_transtmentales <- RIPS_transtmentales |>
  mutate(diagnostico = substr(diagnostico, 1, 4))

# cleaning variable codes
RIPS_transtmentales <- RIPS_transtmentales |>
  mutate (diagnostico = case_when(
    diagnostico == "F99x" ~ "F999",
    diagnostico == "F89x" ~ "F899",
    diagnostico %in% c("F83X", "F82X") ~ "F899",
    diagnostico %in% c("F59X", "F55X", "F54X") ~ "F599",
    diagnostico == "F39X" ~ "F399",
    diagnostico %in% c("F29X", "F28X", "F24X", "F21X") ~ "F299",
    diagnostico %in% c("F03X", "F04X", "F09X") ~ "F000",
    TRUE ~ diagnostico
  ))

## Now group all diagnoses into the ICD overall categories 
RIPS_transtmentales <- RIPS_transtmentales |> 
  mutate(diagnostico = as.character(diagnostico)) |> 
  mutate(icd_diagnosis = case_when(
    diagnostico >= "F000" & diagnostico <= "F090" ~ 1, 
    diagnostico >= "F100" & diagnostico <= "F199" ~ 2,
    diagnostico >= "F200" & diagnostico <= "F299" ~ 3,
    diagnostico >= "F300" & diagnostico <= "F399" ~ 4,
    diagnostico >= "F400" & diagnostico <= "F499" ~ 5,
    diagnostico >= "F500" & diagnostico <= "F599" ~ 6,
    diagnostico >= "F600" & diagnostico <= "F699" ~ 7,
    diagnostico >= "F700" & diagnostico <= "F799" ~ 8,
    diagnostico >= "F800" & diagnostico <= "F899" ~ 9,
    diagnostico >= "F900" & diagnostico <= "F989" ~ 10,
    diagnostico >= "F999" & diagnostico <= "F999" ~ 11,
    TRUE ~ NA_real_
  ))

RIPS_transtmentales <- RIPS_transtmentales |> 
  mutate(icd_diagnosis = factor(icd_diagnosis, 
                                levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                                labels = c("organic incl dementia", "psychoactive substance abuse", "schizophrenia - delusional", "affective disorders", "neurotic stress-related", "behavioural", "adult personality",  "mental retardation", "psychological development", "childhood onset", "unspecified")))

# Load hospital data 
RIPS_hosp_filename ="S:/PAPSIVI_Data/data_files/ExtraccionRUV_RIPS_HospTranstMentales.txt" 
RIPS_hosptranstmentales <- read_in_minsalud_file(RIPS_hosp_filename)

# Remove last row
RIPS_hosptranstmentales <- RIPS_hosptranstmentales |>
  filter(row_number() <= n() -1)

# Remove grupodiagnostico as all the same 
RIPS_hosptranstmentales$grupodiagnostico <- NULL

# This keeps only ICD code label 
RIPS_hosptranstmentales <- RIPS_hosptranstmentales |>
  mutate(diagnostico = substr(diagnostico, 1, 4))

# Cleaning variable codes
RIPS_hosptranstmentales <- RIPS_hosptranstmentales |>
  mutate (diagnostico = case_when(
    diagnostico == "F99x" ~ "F999",
    diagnostico == "F89x" ~ "F899",
    diagnostico %in% c("F83X", "F82X") ~ "F899",
    diagnostico %in% c("F59X", "F55X", "F54X") ~ "F599",
    diagnostico == "F39X" ~ "F399",
    diagnostico %in% c("F29X", "F28X", "F24X", "F21X") ~ "F299",
    diagnostico %in% c("F03X", "F04X", "F09X") ~ "F000",
    TRUE ~ diagnostico
  ))

# Now group all diagnoses into the ICD overall categories 
RIPS_hosptranstmentales <- RIPS_hosptranstmentales |>
  mutate(diagnostico = as.character(diagnostico)) |> 
  mutate(icd_diagnosis = case_when(
    diagnostico >= "F000" & diagnostico <= "F090" ~ 1, 
    diagnostico >= "F100" & diagnostico <= "F199" ~ 2,
    diagnostico >= "F200" & diagnostico <= "F299" ~ 3,
    diagnostico >= "F300" & diagnostico <= "F399" ~ 4,
    diagnostico >= "F400" & diagnostico <= "F499" ~ 5,
    diagnostico >= "F500" & diagnostico <= "F599" ~ 6,
    diagnostico >= "F600" & diagnostico <= "F699" ~ 7,
    diagnostico >= "F700" & diagnostico <= "F799" ~ 8,
    diagnostico >= "F800" & diagnostico <= "F899" ~ 9,
    diagnostico >= "F900" & diagnostico <= "F989" ~ 10,
    diagnostico >= "F999" & diagnostico <= "F999" ~ 11,
    TRUE ~ NA_real_
  ))

RIPS_hosptranstmentales <- RIPS_hosptranstmentales |>
  mutate(icd_diagnosis = factor(icd_diagnosis, 
                                levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                                labels = c("organic incl dementia", "psychoactive substance abuse", "schizophrenia - delusional", "affective disorders", "neurotic stress-related", "behavioural", "adult personality",  "mental retardation", "psychological development", "childhood onset", "unspecified")))

# Merge both mental health outpatients and hosp dfs together into new df
df_mentalhealth <- RIPS_hosptranstmentales %>%
  full_join(RIPS_transtmentales, by = "personid", multiple = "first", suffix = c("", ".outpatient"))

# Merge to df with RUV and papsivi info
df <- df %>%
  left_join(df_mentalhealth, by = "personid", multiple = "first")

# Rename fecha papsivi variables  
df <- df %>%
  rename(fecha.papsivi = fecha.x)

df <- df %>%
  rename(fecha.hosp = fecha.y)

# Create previous mental health variable - yes if mental contact and no if no mh contact OR if mh contact comes after papsivi contact 
df <- df %>%
  mutate(
    earliest_mh_yr = pmin(fecha.hosp, fecha.outpatient, na.rm = TRUE),
  ) 

df <- df %>%
  mutate(
    mh_prev = case_when(
      !is.na(earliest_mh_yr) & (is.na(fecha.papsivi) | earliest_mh_yr <= fecha.papsivi) ~ "yes",
      TRUE ~ "no"                         
    )
  )

# Create a dataset with only the first registration per participant
first_reg_df <- df |>
  distinct(personid, .keep_all = TRUE)

# Add a reg_count variable to main dataset indicating the count for entries where the same
# personid appears multiple times to use as repeated measures variable if needed
df <- df |>
  group_by(personid) |>
  mutate(df_reg_count = row_number())


########################################################################
#
# Descriptives
#
########################################################################

# Count number of each type of sessions 
summary <- ruvpap_df %>% 
  distinct(personid, modalidaatencionpapsivi) %>%
  group_by(modalidaatencionpapsivi) %>%
  summarise(
    people_count = n_distinct(personid),
    .groups = "drop"
  )
print(summary) 

#### Create a variable people_count to count number of sessions per personid (collapses data by modalidad)
sessions_per_person <- ruvpap_df %>% 
  group_by(personid, modalidaatencionpapsivi)  %>%
  summarise(total_sessions = n(), .groups = "drop")

# This gives breakdown of sessions per type 
attendance_distribution <-  sessions_per_person %>% 
  group_by(modalidaatencionpapsivi, total_sessions) %>%
  summarise(n_patients = n(), .groups= "drop")
print(attendance_distribution) 

# Descriptives for Table 1

label(first_reg_df$sexo) <- "Sex"
label(first_reg_df$edad) <- "Age"
label(first_reg_df$etnia) <- "Ethnicity"
label(first_reg_df$tipo_regimen) <- "Healthcare regime"
label(first_reg_df$indicadordiscapacidad) <- "Disability"

# Generate descriptive statistics tables and write to file
table1(~ sexo + etnia + tipo_regimen + indicadordiscapacidad + edad | indicadorpapsivi, data = first_reg_df)
descrip_table <- table1(~ sexo + etnia + tipo_regimen + indicadordiscapacidad + edad | indicadorpapsivi, data = first_reg_df)
write(descrip_table, file = paste(output_dir, "descrip_table.html", sep = ""))

# Previous mental health diagnosis (only count first reg/unique individuals)
papsivi_yes <- subset(first_reg_df, indicadorpapsivi == "Yes") # for those that attended papsivi
table(papsivi_yes$mh_prev) # number of prev mh in papsivi sample
table(first_reg_df$mh_prev) # number of prev mh in whole sample

prevmh_papsivi <- subset(papsivi_yes, mh_prev == "yes") # subset of only papsivi = yes and previous mh = yes

## Count number of PAPSIVI sessions attended by victimisation type (for future reference)
## this method counts number of unique people per exposure, does not count multiple victimisations

df <- df %>%
  rename(modalidad = modalidaatencionpapsivi)

first_reg_df <- first_reg_df %>%
  rename(modalidad = modalidaatencionpapsivi)

# Make dummy variables
df <- dummy_cols(df, select_columns = c("hechovictimizante"))  
df <- dummy_cols(df, select_columns = c("modalidad"))  

## Count unique number of individuals for sessions by victim type 
# (this method counts number of unique people per victimisation type, 
# allows for multiple victimisation types within sessions)
ind_df <- subset(df, modalidad == "INDIVIDUAL")
group_df <- subset(df, modalidad == "GRUPAL")
family_df <- subset(df, modalidad == "FAMILIAR")
comm_df <- subset(df, modalidad == "COMUNITARIO")

first_reg_grp_df <- group_df |>
  distinct(personid, .keep_all = TRUE)
victim_counts <- first_reg_grp_df %>%
  summarise(across(where(is.numeric), ~sum(.==1, na.rm= TRUE)))

first_reg_ind_df <- ind_df |>
  distinct(personid, .keep_all = TRUE)
victim_counts <- first_reg_ind_df %>%
  summarise(across(where(is.numeric), ~sum(.==1, na.rm= TRUE)))

victimisation_vars <- c("hechovictimizante_tortura", "hechovictimizante_violenciasexual", "hechovictimizante_amenaza", "hechovictimizante_desplazamiento", "hechovictimizante_homocidio", "hechovictimizante_hostigamientos", "hechovictimizante_lesion_fis", "hechovictimizante_despojo_tierras", "hechovictimizante_confinamiento", "hechovictimizante_lesion_psic", "hechovictimizante_minas", "hechovictimizante_perdida_bienes", "hechovictimizante_secuestro", "hechovictimizante_reclut_ninos", "hechovictimizante_desparacion")

# Create variable to count total number of papsivi sessions attended per person of any type
df <- df |>
  group_by(personid) |>
  mutate(total_sessions = if_else(row_number() == 1, sum(!is.na(modalidad)), as.numeric(NA))) %>%
  ungroup() 

# count number of individual sessions
ind_df <- ind_df |>
  group_by(personid) |>
  mutate(ind_sessions = if_else(row_number() == 1, sum(!is.na(modalidad)), as.numeric(NA))) %>%
  ungroup()

# create variable to count number of group sessions
group_df <- group_df |>
  group_by(personid) |>
  mutate(grp_sessions = if_else(row_number() == 1, sum(!is.na(modalidad)), as.numeric(NA))) %>%
  ungroup()

# create variable to count number of family sessions
family_df <- family_df |>
  group_by(personid) |>
  mutate(fam_sessions = if_else(row_number() == 1, sum(!is.na(modalidad)), as.numeric(NA))) %>%
  ungroup()  

# create variable to count number of community sessions 
comm_df <- comm_df |>
  group_by(personid) |>
  mutate(comm_sessions = if_else(row_number() == 1, sum(!is.na(modalidad)), as.numeric(NA))) %>%
  ungroup()


#####################################################################################
#
# Analysis: Treatment assignment - odds of receiving any treatments in the modality
#
#####################################################################################


# run_exp_reg: function to run regression analyses on odds of session type received by each victimisation category
# 
# Parameters:
#  response_var: session type variable
# Returns:
#  dataframe with results
#
run_exp_reg <- function(response_var, adjust = FALSE) {
  
  results_df <- data.frame(
    Variable = character(),
    Coefficient = numeric(),
    OddsRatio = numeric(),
    LowerCI = numeric(),
    UpperCI = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (victim_var in victimisation_vars) {
    
    if(adjust == TRUE) {
      formula <- as.formula(paste(response_var, victim_var, " + edad + sexo + tipo_regimen + etnia + cerac")) 
    } else {
      formula <- as.formula(paste(response_var, victim_var))
    }
    
    # Run regression
    model <- glm(formula, data = df, family = binomial(link = "logit")) 
    
    # Extract model from regression object and print to screen to indicate it's been run
    model_formula <- paste(deparse(formula(model)), collapse = "")
    print(model_formula)
    
    vcov_2way <- cluster.vcov(model, cluster = df$personid)
    robust_results <- coeftest(model, vcov = vcov_2way)
    
    coefs <- robust_results[, "Estimate"]
    ses <- robust_results[, "Std. Error"]
    
    odds_ratio <- exp(coefs)
    lower_95 <- exp(coefs - 1.96 * ses)
    upper_95 <- exp(coefs + 1.96 * ses)
    
    results_df <- rbind(
      results_df,
      data.frame(
        Variable = rownames(robust_results)[2],
        OddsRatio = odds_ratio[2],
        LowerCI = lower_95[2],
        UpperCI = upper_95[2],
        stringsAsFactors = FALSE)
      )
  }
  return(results_df)
}


#
# Run unadjusted regression analyses for odds of session offered by victimisation type
#

# Calculate and store for individual sessions
indresults_unajd_df <- run_exp_reg("modalidad_INDIVIDUAL ~")
write.csv(indresults_unajd_df, paste(output_dir, "modind_unadj_results.csv", sep = ""), row.names = FALSE)

# Calculate and store for group sessions
grpresults_unajd_df <- run_exp_reg("modalidad_GRUPAL ~")
write.csv(grpresults_unajd_df, paste(output_dir, "modgrp_unajd_results.csv", sep = ""), row.names = FALSE)

# Calculate and store for family sessions
famresults_unajd_df <- run_exp_reg("modalidad_FAMILIAR ~")
write.csv(famresults_unajd_df, paste(output_dir, "modfam_unajd_results.csv", sep = ""), row.names = FALSE)

# Calculate and store for community sessions
comresults_unajd_df <- run_exp_reg("modalidad_COMUNITARIO ~")
write.csv(comresults_unajd_df, paste(output_dir, "modcom_unajd_results.csv", sep = ""), row.names = FALSE)

#
# Run adjusted regression analyses for odds of session offered by victimisation type
#

indresults_ad_df <- run_exp_reg("modalidad_INDIVIDUAL ~", adjust = TRUE)
write.csv(indresults_ad_df, paste(output_dir, "modind_ad_results.csv", sep = ""), row.names = FALSE)

grpresults_ad_df <- run_exp_reg("modalidad_GRUPAL ~", adjust = TRUE)
write.csv(grpresults_ad_df, paste(output_dir, "modgrp_ad_results.csv", sep = ""), row.names = FALSE)

famresults_ad_df <- run_exp_reg("modalidad_FAMILIAR ~", adjust = TRUE)
write.csv(famresults_ad_df, paste(output_dir, "modfam_ad_results.csv", sep = ""), row.names = FALSE)

comresults_ad_df <- run_exp_reg("modalidad_COMUNITARIO ~", adjust = TRUE)
write.csv(comresults_ad_df, paste(output_dir, "modcom_ad_results.csv", sep = ""), row.names = FALSE)


#
# Treatment assignment by prev mental health
#

#
# run_prevmh_reg: function to run regression analyses on session type received by presence of previous mental
#            health diagnosis
# 
# Parameters:
#  response_var: session type variable
# Returns:
#  dataframe with results
#
run_prevmh_reg <- function(response_var, adjust = FALSE) {
  
  results_df <- data.frame(
    Variable = character(),
    Coefficient = numeric(),
    OddsRatio = numeric(),
    LowerCI = numeric(),
    UpperCI = numeric(),
    stringsAsFactors = FALSE
  )
  
  if(adjust == TRUE) {
    formula <- as.formula(paste(response_var, "mh_prev", " + edad + sexo + tipo_regimen + etnia + cerac"))
  } else {
    formula <- as.formula(paste(response_var, "mh_prev")) 
  }
  
  # Run regression
  model <- glm(formula, data = df, family = binomial(link = "logit")) 

  # Extract model from regression object and print to screen to indicate it's been run
  model_formula <- paste(deparse(formula(model)), collapse = "")
  print(model_formula)
  
  # Calculate outcomes using cluster robust standard errors, clustering by personid and municipio_id
  vcov_2way <- cluster.vcov(model, cluster = df$personid)
  robust_results <- coeftest(model, vcov = vcov_2way)
  
  coefs <- robust_results[, "Estimate"]
  ses <- robust_results[, "Std. Error"]
  
  odds_ratio <- exp(coefs)
  lower_95 <- exp(coefs - 1.96 * ses)
  upper_95 <- exp(coefs + 1.96 * ses)
  
  # Store in dataframe and return
  results_df <- rbind(
    results_df,
    data.frame(
      Variable = rownames(robust_results)[2],
      OddsRatio = odds_ratio[2],
      LowerCI = lower_95[2],
      UpperCI = upper_95[2],
      stringsAsFactors = FALSE
    )
  )
  return(results_df)
}

#
# Unadjusted regression analyses for odds of session offered by previous mental health diagnosis
#

prevmh_ind_unadj <- run_prevmh_reg("modalidad_INDIVIDUAL ~")
prevmh_grp_unadj <- run_prevmh_reg("modalidad_GRUPAL ~")
prevmh_fam_unadj <- run_prevmh_reg("modalidad_FAMILIAR ~")
prevmh_com_unadj <- run_prevmh_reg("modalidad_COMUNITARIO ~")

#
# Adjusted regression analyses for odds of session offered by previous mental health diagnosis
#

prevmh_sesstype_results_df <- data.frame(
  Coefficient = character(),
  Estimate = numeric(),
  LowerCI = numeric(),
  UpperCI = numeric(),
  session_var  = character()
)

prevmh_ind_ad_df <- run_prevmh_reg("modalidad_INDIVIDUAL ~", adjust = TRUE)
prevmh_ind_ad_df$session_var <- "individual"

prevmh_grp_ad_df <- run_prevmh_reg("modalidad_GRUPAL ~", adjust = TRUE)
prevmh_grp_ad_df$session_var <- "group"

prevmh_fam_ad_df <- run_prevmh_reg("modalidad_FAMILIAR ~", adjust = TRUE)
prevmh_fam_ad_df$session_var <- "family"

prevmh_com_ad_df <- run_prevmh_reg("modalidad_COMUNITARIO ~", adjust = TRUE)
prevmh_com_ad_df$session_var <- "community"

# Bind and tidy up for writing 
prevmh_sesstype_results_df <- rbind(prevmh_sesstype_results_df, prevmh_ind_ad_df, prevmh_grp_ad_df, prevmh_fam_ad_df, prevmh_fam_ad_df, prevmh_com_ad_df)
prevmh_sesstype_results_df <- prevmh_sesstype_results_df %>% relocate(session_var)
write.csv(prevmh_sesstype_results_df, paste(output_dir, "prevmh_sesstype_results_ad.csv", sep = ""), row.names = FALSE)


#######################################################################
#
# Treatment engagement - number of sessions received
#
#######################################################################


# simple_lin_reg: function to run regression analyses on session type received by each victimisation type
# 
# Parameters:
#  formula: regression formula
#  func_df: dataframe with data
# Returns:
#  dataframe with results
#
simple_lin_reg <- function(formula, func_df) {
  model <- glm(formula, data = func_df, family = gaussian) 
  
  vcov_2way <- cluster.vcov(model, cluster = func_df$personid)
  coefs <- coef(model)
  cluster_se <- sqrt(diag(vcov_2way))
  t_crit <- qt(0.975, df = model$df.residual)
  ci_lower <- coefs - t_crit * cluster_se
  ci_upper <- coefs + t_crit * cluster_se
  
  results_df <- data.frame(
    Coefficient = names(coefs)[2],
    Estimate = coefs[2],
    LowerCI = ci_lower[2],
    UpperCI = ci_upper[2],
    stringsAsFactors = FALSE
  )
  return(results_df)
}

#
# Adjusted analysis of total sessions of all types by previous mental health diagnosis
#

prevmh_results_df <- data.frame(
  Coefficient = character(),
  Estimate = numeric(),
  LowerCI = numeric(),
  UpperCI = numeric(),
  session_var  = character()
)

prevmh_tot_df <- simple_lin_reg("total_sessions ~ mh_prev + sexo + edad + tipo_regimen + etnia_min + cerac", df)
prevmh_tot_df$session_var <- "total"

prevmh_ind_df <- simple_lin_reg("ind_sessions ~ mh_prev + sexo + edad + tipo_regimen + etnia_min + cerac", ind_df)
prevmh_ind_df$session_var <- "individual"

prevmh_grp_df <- simple_lin_reg("grp_sessions ~ mh_prev + sexo + edad + tipo_regimen + etnia_min + cerac", group_df)
prevmh_grp_df$session_var <- "group"

prevmh_fam_df <- simple_lin_reg("fam_sessions ~ mh_prev + sexo + edad + tipo_regimen + etnia_min + cerac", family_df)
prevmh_fam_df$session_var <- "family"

prevmh_com_df <- simple_lin_reg("comm_sessions ~ mh_prev + sexo + edad + tipo_regimen + etnia_min + cerac", comm_df)
prevmh_com_df$session_var <- "community"

# Bind and tidy up for writing 
prevmh_results_df <- rbind(prevmh_results_df, prevmh_tot_df, prevmh_ind_df, prevmh_grp_df, prevmh_fam_df, prevmh_com_df)
prevmh_results_df <- prevmh_results_df %>% relocate(session_var)
write.csv(prevmh_results_df, paste(output_dir, "nsessions_prevmh_ad.csv", sep = ""), row.names = FALSE)

#
# Adjusted regressions to examine number of sessions by modality by victimisation type
#

# run_lin_reg: function to run regression analyses on session type received by each victimisation type
# 
# Parameters:
#  response_var: session type variable
# Returns:
#  dataframe with results
#
run_lin_reg <- function(response_var, reg_df, adjust = FALSE) {
  
  results_df <- data.frame(
    Variable = character(),
    Coefficient = numeric(),
    Std.Error = numeric(),
    LowerCI = numeric(),
    UpperCI = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (victim_var in victimisation_vars) {
    
    if(adjust == TRUE) {
      formula <- as.formula(paste(response_var, victim_var, " + edad + sexo + tipo_regimen + etnia + cerac")) 
    } else {
      formula <- as.formula(paste(response_var, victim_var)) 
    }
    
    # Run regression
    model <- glm(formula, data = reg_df, family = gaussian) 
    
    # Extract model from regression object and print to screen to indicate it's been run
    model_formula <- paste(deparse(formula(model)), collapse = "")
    print(model_formula)
    
    # Calculate outcomes using cluster robust standard errors, clustering by personid and municipio_id
    vcov_2way <- cluster.vcov(model, cluster = reg_df$personid)
    
    coefs <- coef(model)
    cluster_se <- sqrt(diag(vcov_2way))
    
    t_crit <- qt(0.975, df = model$df.residual)
    
    ci_lower <- coefs - t_crit * cluster_se
    ci_upper <- coefs + t_crit * cluster_se
    
    # Store in dataframe and return
    results_df <- rbind(
      results_df,
      data.frame(
        Coefficient = names(coefs)[2],
        Estimate = coefs[2],
        LowerCI = ci_lower[2],
        UpperCI = ci_upper[2],
        stringsAsFactors = FALSE
      )
    )
  }
  return(results_df)
}

vic_ind_numses_ad_df <- run_lin_reg("ind_sessions ~", ind_df, adjust = TRUE)
write.csv(vic_ind_numses_ad_df, paste(output_dir, "nsessions_vic_ind_ad.csv", sep = ""), row.names = FALSE)

vic_grp_numses_ad_df <- run_lin_reg("grp_sessions ~", group_df, adjust = TRUE)
write.csv(vic_grp_numses_ad_df, paste(output_dir, "nsessions_vic_grp_ad.csv", sep = ""), row.names = FALSE)

vic_fam_numses_ad_df <- run_lin_reg("fam_sessions ~", family_df, adjust = TRUE)
write.csv(vic_fam_numses_ad_df, paste(output_dir, "nsessions_vic_fam_ad.csv", sep = ""), row.names = FALSE)

vic_com_numses_ad_df <- run_lin_reg("comm_sessions ~", comm_df, adjust = TRUE)
write.csv(vic_com_numses_ad_df, paste(output_dir, "nsessions_vic_com_ad.csv", sep = ""), row.names = FALSE)


#
# Moderators of sessions received in different modalities by sex,
#  ethnic minority status, health insurance
#


# Preamble: No 'Other' individuals in sexo for group_df, so drop this level in that dataframe
group_df$sexo <- droplevels(group_df$sexo)

#
# Sessions moderated by sex
#

sex_results_df <- data.frame(
  Coefficient = character(),
  Estimate = numeric(),
  LowerCI = numeric(),
  UpperCI = numeric(),
  session_var  = character()
)

sex_tot_df <- simple_lin_reg("total_sessions ~ sexo + edad + tipo_regimen + etnia_min + cerac", df)
sex_tot_df$session_var <- "total"

sex_ind_df <- simple_lin_reg("ind_sessions ~ sexo + edad + tipo_regimen + etnia_min + cerac", ind_df)
sex_ind_df$session_var <- "individual"

sex_grp_df <- simple_lin_reg("grp_sessions ~ sexo + edad + tipo_regimen + etnia_min + cerac", group_df)
sex_grp_df$session_var <- "group"

sex_fam_df <- simple_lin_reg("fam_sessions ~ sexo + edad + tipo_regimen + etnia_min + cerac", family_df)
sex_fam_df$session_var <- "family"

sex_com_df <- simple_lin_reg("comm_sessions ~ sexo + edad + tipo_regimen + etnia_min + cerac", comm_df)
sex_com_df$session_var <- "community"

# Bind and tidy up for writing 
sex_results_df <- rbind(sex_results_df, sex_tot_df, sex_ind_df, sex_grp_df, sex_fam_df, sex_com_df)
sex_results_df <- sex_results_df %>% relocate(session_var)
row.names(sex_results_df) <- NULL
write.csv(sex_results_df, paste(output_dir, "nsessions_sex.csv", sep = ""), row.names = FALSE)

#
# Sessions moderated by etnia_min
#

etn_results_df <- data.frame(
  Coefficient = character(),
  Estimate = numeric(),
  LowerCI = numeric(),
  UpperCI = numeric(),
  session_var  = character()
)

etn_tot_df <- simple_lin_reg("total_sessions ~ etnia_min + sexo + edad + tipo_regimen + cerac", df)
etn_tot_df$session_var <- "total"

etn_ind_df <- simple_lin_reg("ind_sessions ~ etnia_min + sexo + edad + tipo_regimen + cerac", ind_df)
etn_ind_df$session_var <- "individual"

etn_grp_df <- simple_lin_reg("grp_sessions ~ etnia_min + sexo + edad + tipo_regimen + cerac", group_df)
etn_grp_df$session_var <- "group"

etn_fam_df <- simple_lin_reg("fam_sessions ~ etnia_min + sexo + edad + tipo_regimen + cerac", family_df)
etn_fam_df$session_var <- "family"

etn_com_df <- simple_lin_reg("comm_sessions ~ etnia_min + sexo + edad + tipo_regimen + cerac", comm_df)
etn_com_df$session_var <- "community"

# Bind and tidy up for writing 
etn_results_df <- rbind(etn_results_df, etn_tot_df, etn_ind_df, etn_grp_df, etn_fam_df, etn_com_df)
etn_results_df <- etn_results_df %>% relocate(session_var)
row.names(etn_results_df) <- NULL
write.csv(etn_results_df, paste(output_dir, "nsessions_etn.csv", sep = ""), row.names = FALSE)

#
# Sessions moderated by tipo_regimen
#

treg_results_df <- data.frame(
  Coefficient = character(),
  Estimate = numeric(),
  LowerCI = numeric(),
  UpperCI = numeric(),
  session_var  = character()
)

treg_tot_df <- simple_lin_reg("total_sessions ~ tipo_regimen + sexo + edad + etnia_min + cerac", df)
treg_tot_df$session_var <- "total"

treg_ind_df <- simple_lin_reg("ind_sessions ~ tipo_regimen + sexo + edad + etnia_min + cerac", ind_df)
treg_ind_df$session_var <- "individual"

treg_grp_df <- simple_lin_reg("grp_sessions ~ tipo_regimen + sexo + edad + etnia_min + cerac", group_df)
treg_grp_df$session_var <- "group"

treg_fam_df <- simple_lin_reg("fam_sessions ~ tipo_regimen + sexo + edad + etnia_min + cerac", family_df)
treg_fam_df$session_var <- "family"

treg_com_df <- simple_lin_reg("comm_sessions ~ tipo_regimen + sexo + edad + etnia_min + cerac", comm_df)
treg_com_df$session_var <- "community"

# Bind and tidy up for writing 
treg_results_df <- rbind(treg_results_df, treg_tot_df, treg_ind_df, treg_grp_df, treg_fam_df, treg_com_df)
treg_results_df <- treg_results_df %>% relocate(session_var)
row.names(treg_results_df) <- NULL
write.csv(treg_results_df, paste(output_dir, "nsessions_treg.csv", sep = ""), row.names = FALSE)


#####################################################################################
#
# Logistic regression: MH diagnosis by icd diagnosis and type of sessions (adjusted)
#
#####################################################################################

# Combine outpatient and inpatient variables 
levels(df$icd_diagnosis) <- levels(df$icd_diagnosis.outpatient) 
df$alldiagnoses <- ifelse(is.na(df$icd_diagnosis.outpatient), as.character(df$icd_diagnosis), as.character(df$icd_diagnosis.outpatient))
df$alldiagnoses <- factor(df$alldiagnoses, levels = levels(df$icd_diagnosis.outpatient))

# Remove diagnoses post-PAPSIVI contact
df <- df %>%
  mutate(
    icd = if_else(
      is.na(fecha.papsivi) | earliest_mh_yr <= fecha.papsivi, 
      alldiagnoses,
      NA_character_                          
    )
  )

# Remove spaces from diagnosis names
df$icd <- gsub(" ", "", df$icd)
df$icd <- gsub("-", "", df$icd)

# Create dummy vars
df <- dummy_cols(df, select_columns = c("icd"))  

# Get list of diagnoses listed in the icd variable and turn into list of dummy column variables
icddx_list <- unique(na.omit(df$icd))
icddx_list <- paste0("icd_", icddx_list)

# run_icd_reg: function to run regression analyses on odds of session type received by each victimisation category
# 
# Parameters:
#  response_var: session type variable
# Returns:
#  dataframe with results
#
run_icd_reg <- function(response_var, adjust = FALSE) {
  
  results_df <- data.frame(
    Variable = character(),
    Coefficient = numeric(),
    OddsRatio = numeric(),
    LowerCI = numeric(),
    UpperCI = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (icd_var in icddx_list) {
    
    if(adjust == TRUE) {
      formula <- as.formula(paste(response_var, icd_var, " + edad + sexo + tipo_regimen + etnia + cerac")) 
    } else {
      formula <- as.formula(paste(response_var, icd_var))
    }
    
    # Run regression
    model <- glm(formula, data = df, family = binomial(link = "logit")) 
    
    # Extract model from regression object and print to screen to indicate it's been run
    model_formula <- paste(deparse(formula(model)), collapse = "")
    print(model_formula)
    
    vcov_2way <- cluster.vcov(model, cluster = df$personid)
    robust_results <- coeftest(model, vcov = vcov_2way)
    
    coefs <- robust_results[, "Estimate"]
    ses <- robust_results[, "Std. Error"]
    
    odds_ratio <- exp(coefs)
    lower_95 <- exp(coefs - 1.96 * ses)
    upper_95 <- exp(coefs + 1.96 * ses)
    
    results_df <- rbind(
      results_df,
      data.frame(
        Variable = rownames(robust_results)[2],
        OddsRatio = odds_ratio[2],
        LowerCI = lower_95[2],
        UpperCI = upper_95[2],
        stringsAsFactors = FALSE)
    )
  }
  return(results_df)
}

mod_ind_icd_df <- run_icd_reg("modalidad_INDIVIDUAL ~", adjust = TRUE)
write.csv(mod_ind_icd_df, paste(output_dir, "mod_ind_icd_results.csv", sep = ""), row.names = FALSE)

mod_grp_icd_df <- run_icd_reg("modalidad_GRUPAL ~", adjust = TRUE)
write.csv(mod_grp_icd_df, paste(output_dir, "mod_grp_icd_results.csv", sep = ""), row.names = FALSE)

mod_fam_icd_df <- run_icd_reg("modalidad_FAMILIAR ~", adjust = TRUE)
write.csv(mod_fam_icd_df, paste(output_dir, "mod_fam_icd_results.csv", sep = ""), row.names = FALSE)

mod_com_icd_df <- run_icd_reg("modalidad_COMUNITARIO ~", adjust = TRUE)
write.csv(mod_com_icd_df, paste(output_dir, "mod_com_icd_results.csv", sep = ""), row.names = FALSE)

#
# Map test code
#

map_df <- df %>%
  group_by(municipio_id) %>%
  summarise(
    total_individuals = n(),
    total_ind_sessions = sum(total_sessions),
    proportion = total_ind_sessions / total_individuals
  )

summary(df$total_sessions)


