# In this R script I replicate figure 2, "Dynamic Effects: Coefficients in Years Before and After 
# Deregulation" from Leblebicioğlu & Weinberger (2020). I then attempt to estimate the same coefficient
# using a local projections DiD estimator, proposed in Dube, Girardi, Jorda, Taylor (2023).

# Author: Ethan Goode
# Date created: 12/14/23
# Last edited: 12/14/23

# Load libraries ----------------------------------------------------------------------------------------
# install.packages(c('data.table','tidyverse','viridis','fixest','haven'))
# Data wrangling functions
library(data.table)
library(haven)
library(lubridate)
library(stringr)
library(readxl)

# Visualization functions
library(ggplot)
library(viridis)

# Estimation library
library(fixest)

# Set run options ----------------------------------------------------------------------------------
cdir <- "C:/Users/ethan/OneDrive/Desktop/Replication-1/Leblebicioğlu_Weinberger_2020" # Own path to project directory
data_dir <- paste0(cdir,"/data")
result_dir <- paste0(cdir,"/results")

# Load data ----------------------------------------------------------------------------------------
file_path <- paste0(data_dir,"/state_industry_replication.dfa")
df <- read_dfa(file_path)
df <- as.data.table(df)

# Process data -------------------------------------------------------------------------------------
df[,indgroup := fcase(
    industryid == 3, 1,
    industryid > 6 & industryid < 11, 2,
    industryid == 11, 3,
    (industryid > 13 & industryid < 36) | industryid == 76, 4,
    industryid > 37 & industryid < 47, 5,
    industryid == 47 | industryid == 48, 6,
    (industryid > 48 & industryid < 57)| industryid == 77, 8,
    (industryid > 56 & industryid < 72)|industryid == 78, 7,
    default=0
)]

df[,laborsh_private := comp_private/gsp_private]
df[industryid!=55 & industryid!=77 & industryid!=52 & industryid!=53 & industryid!=54 & industryid!=56 & industryid!=71 , gsp3 := sum(indgsp),by=c('state_name','year')]
df[industryid!=55 & industryid!=77 & industryid!=52 & industryid!=53 & industryid!=54 & industryid!=56 & industryid!=71 , comp3 := sum(indempcomp),by=c('state_name','year')]
df[industryid!=55 & industryid!=77 & industryid!=52 & industryid!=53 & industryid!=54 & industryid!=56 & industryid!=71 , gropsurplus3 := sum(indgropsurplus),by=c('state_name','year')]
df[,laborsh_3 := comp3/gsp3]
df[, laborsh_selfemp_scaled = (1 + (selfempl/empl))*laborsh_priv]
df[, empstateindgroup := sum(indempl), by = .(state_name, indgroup, year)]
df[, empstate := sum(indempl), by = .(state_name, year)]
df[, indgroupshare_state := empstateindgroup / empstate]
df[indgroup == 4 & year == 1977, manufshare_fixed_t := indgroupshare_state]
df[, manufshare_fixed := mean(manufshare_fixed_t, na.rm = TRUE), by = state_name]

# Dropping temporary variables
df[, c("manufshare_fixed_t", "empstateindgroup", "empstate", "indgroupshare_state") := NULL]

# Collapse data and keep the first value of state_name
df <- df[, lapply(.SD, function(x) if (is.numeric(x)) mean(x, na.rm = TRUE) else x[1L]), 
         by = .(state_abbrev, year), .SDcols = setdiff(names(df), c("state_name"))]
df[, state_name := first(state_name), by = .(state_abbrev, year)]

# Dropping specific states and filtering years
df <- df[!state_abbrev %in% c("AK", "HI", "SD", "DE", "DC"),]
df <- df[year > 1969 & year < 1997,]

# Creating new variables
df[, loan2gsp := allloans_fdic / gsp / 1000]
df[, loan2gsp_perc := loan2gsp * 100]
df[, lnavgloanyield := log(avgloanyield)]
df[, lnherf_deposits := log(1 + herf_deposits)]
df[, lnherf_assets := log(1 + herf_assets)]
df[, lnassetreturns := log(assetreturns)]
df[, lnunemp := log(unemp)]
df[, lnhpi := log(HPI)]
df[, kint := kpwall]
df[, lnkint := log(kint)]
df[, stateid := .GRP, by = state_abbrev]
df[, avgloanyield_perc := avgloanyield * 100]

# Modifying variables
df[, gsp_private := gsp_private / 1000]
df[, comp_private := comp_private / 1000]

# Merging with another data table (assuming banking_law_indicators is a data.table)
df <- merge(df, banking_law_indicators, by = c("state_abbrev", "year"), all.x = TRUE)
#df <- df[!is.na(_merge) & _merge != 2]
#df[, _merge := NULL]

# Creating additional variables
df[, zero := 0]
df[, zero2 := 0]



# Run analysis Leblebicioğlu & Weinberger (2020) ---------------------------------------------------
# Run analysis Dube, Girardi, Jorda, & Taylor (2023) -----------------------------------------------


