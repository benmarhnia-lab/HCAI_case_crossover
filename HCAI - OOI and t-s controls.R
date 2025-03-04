## Process data from HCAI, PDD and EDD
## Select outcome of interest (OOI) and applies case crossover design with time-stratified controls
## Caitlin Jones-Ngo, MS, PhD, 11/14/24

library(tidyverse)
library(readxl)
library(readr)
library(dplyr)
library(data.table)
library(lubridate)

edd_filepath <- #Enter the base file path
edd_prefix <- #Enter the file name prefix
pdd_filepath <- #Enter the base file path
pdd_prefix <- #Enter the file name prefix

years <- #select years of interest

ooi <- "resp" #define ooi
ooi_codes <- resp_codes #define ooi ICD-9 and -10 codes

# Example ICD-10 and -9 codes
resp_codes <- c("J00", "J01", "J02", "J03", "J04", "J05", "J06",
                "J12", "J13", "J14", "J15", "J16", "J17", "J18",
                "J20", "J21", "J22", "J30", "J31", "J33", "J34", "J38", "J39",
                "J40", "J41", "J42", "J43", "J44", "J45", "J46", "J47",
                "J80", "J81", "J82", "J83", "J84", "J85", "J86", "J90", "J92",
                "J94", "J96", "J97", "J98", "J99", "R04", "R05", "R06", "R07", "R09",
                460, 461, 462, 463, 464, 465, 466, 471, 472, 477, 478, 480, 481, 482,
                483, 484, 485, 486, 487, 490, 491, 492, 493, 494, 495, 496, 511, 513,
                514, 515, 516, 517, 518, 519, 786)
renal_codes <- c("N00","N01","N02","N03","N04","N05","N06","N07",
                 "N08","N10","N11","N12","N13","N15","N16",
                 "N17","N18","N19",
                 "N20","N21","N22","N23","N25","N26","N27",
                 "N28","N29","R30","R31","R32","R33","R34",
                 "R35","R36","R37","R39",
                 580,581,582,583,584,585,586,587,588,589,590,
                 591,592,593,594,595,596,597,598,599,788)
cardio_codes <- c("I00","I01","I02","I05","I06","I07","I08",
                  "I09","I10","I11","I12","I13","I14","I15","I16",
                  "I20","I21","I22","I23","I24","I25","I26","I27",
                  "I28","I30","I31","I32","I33","I34","I35","I36",
                  "I37","I38","I39",
                  "I40","I41","I42","I43","I44","I45","I46",
                  "I47","I48","I49","I50","I51","I52","I5A",
                  "I70","I71","I72","I73","I74","I75","I76","I77",
                  "I78","I79",
                  390,391,392,393,394,395,396,397,398,
                  401,402,403,404,405,410,411,412,413,
                  414,415,416,417,420,421,422,423,424,
                  425,426,427,428,429,440,441,442,443,
                  444,445,446,447,448,449)
cerebro_codes <- c("I60","I61","I62","I63","I64","I65","I66","I67",
                   "I68","I69",
                   430,431,432,433,434,435,436,437,438)
psych_codes <- c("F20","F21","F23","F25","F28","F29","F10","F11",
                 "F12","F13","F14","F15","F16","F18","F19","F30",
                 "F31","F32","F33","F06",
                 295, 298, 291, 292, 296, 290, 293)

# Initialize an empty list to store data for each year
data_list <- list()

# Loop over each year to load EDD and PDD files and add `enc_type`
for (year in years) {
  # Load the EDD file
  ed_file <- paste0(edd_filepath, edd_prefix, year, ".csv")
  ed_data <- fread(ed_file)
  ed_data[, enc_type := "EDD"]
  
  # Load the PDD file
  ha_file <- paste0(pdd_filepath, pdd_prefix, year, ".csv")
  ha_data <- fread(ha_file)
  ha_data[, enc_type := "PDD"]  
  
  # Rename columns
  setnames(ed_data, old = c("eth", "faczip", "agecatserv"), new = c("ethncty", "hplzip", "agecat"), skip_absent=TRUE)
  colnames(ha_data)[colnames(ha_data) == "agecatdsch"] <- "agecat"
  ed_data$diag_p <- ed_data$dx_prin
  
  ed_data$serv_dt <- as.Date(ed_data$serv_dt, "%m/%d/%Y")
  ha_data$serv_dt <- as.Date(ha_data$admtdate, "%m/%d/%Y")
  
  # Subset columns you need
  keep <- c("rln", "patzip", "hplzip", "serv_dt", "agecat", 
            "sex", "ethncty", "race_grp","diag_p", "enc_type")
  
  ed_sub <- ed_data[, keep, with = FALSE]
  ha_sub <- ha_data[, keep, with = FALSE]
  
  # Combine EDD and PDD
  data <- rbindlist(list(ed_sub, ha_sub), use.names = TRUE, fill = TRUE)
  
  data <- data[order(data$serv_dt),]
  data[, cc_id := .I]
  data[, patzip := fifelse(is.na(patzip) | patzip == "00000", as.character(hplzip), patzip)]
  
  # Define outcome
  ooi_col_name <- paste0("all_", ooi)
  set(data, j = ooi_col_name, value = substr(data$diag_p, 1, 3) %in% ooi_codes)
  
  data_s <- data[get(ooi_col_name) == 1]
  data_s <- data_s[year(serv_dt) == year] #remove encounters where year does not match the year of data collection
  
  ## Time-stratified controls ##
  data_s[, control_dates := lapply(serv_dt, function(x) {
    possible_dates <- seq.Date(x - 30, x + 30, by = "day")
    valid_controls <- possible_dates[month(possible_dates) == month(x) & 
                                       wday(possible_dates) == wday(x) & 
                                       possible_dates != x]
    return(valid_controls)
  })]
  
  # Expand the data.table to create one row per control date
  controls <- data_s[, .(cc_id, rln, patzip, control_dates)]
  controls <- controls[, .(ctrl_dt = unlist(control_dates)), by = .(cc_id, rln, patzip)]
  
  # Merge control dates back to retain covariate data
  controls <- data_s[controls, on = c("cc_id", "rln", "patzip"), nomatch = 0]
  controls <- data_s[controls, 
                             .(cc_id, rln, patzip, hplzip, ctrl_dt, agecat, sex, ethncty, race_grp, 
                               diag_p, enc_type), 
                             on = c("cc_id", "rln", "patzip")]
  setnames(controls, "ctrl_dt", "serv_dt")
  
  # Add case indicator (controls are 0, cases are 1)
  controls[, case_indicator := 0]
  
  cases <- data_s[, .(cc_id, rln, patzip, hplzip, serv_dt, agecat, sex, ethncty, race_grp, 
                      diag_p, enc_type)]
  cases[, case_indicator := 1]
  
  # Combine cases and controls
  controls[, "case_indicator" := as.character(case_indicator)]
  cases[, "case_indicator" := as.character(case_indicator)]
  
  cases[, serv_dt := as.Date(serv_dt)]
  controls[, serv_dt := as.Date(as.numeric(serv_dt), origin = "1970-01-01")]
  
  # Combine cases and controls
  data_ts <- rbindlist(list(cases, controls), use.names = TRUE, fill = TRUE)
  data_ts <- data_ts[order(data_ts$cc_id),]
  
  data_ts[, cc_id := paste0(cc_id, "_", year)]
  data_list[[as.character(year)]] <- data_ts
  rm(cases, controls, ed_data, ed_sub, ha_data, ha_sub, data, data_s, data_ts)
}

# Combine all years' data into one data.table
final_data <- rbindlist(data_list, use.names = TRUE, fill = TRUE)

start <- min(years)
end <- max(years)
filepath <- # Enter filepath to save
write.csv(final_data, paste0(filepath,ooi,"_",start,"_",end,".csv"))
