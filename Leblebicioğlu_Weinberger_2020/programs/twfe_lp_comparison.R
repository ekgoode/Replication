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
library(dplyr)
library(haven)
library(lubridate)
library(stringr)
library(forcats)
library(readxl)

# Visualization functions
library(ggplot2)
library(viridis)

# Estimation library
library(fixest)

# Set run options ----------------------------------------------------------------------------------
cdir <- "C:/Users/ethan/OneDrive/Desktop/Replication-1/Leblebicioğlu_Weinberger_2020" # Own path to project directory
data_dir <- paste0(cdir,"/data")
result_dir <- paste0(cdir,"/results")

# Load data ----------------------------------------------------------------------------------------
file_path <- paste0(data_dir,"/state_industry_replication.dta")
df <- read_dta(file_path)
df <- as.data.table(df)
blaw_df <- as.data.table(read_dta(paste0(data_dir, "/banking_law_indicators.dta")))

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

# Collapse the data
df <- df %>%
  group_by(state_abbrev, year) %>%
  summarise(
    across(
        c(laborsh_private,unemp,popgrowth,corptax,unionmem,HPI), \(x) mean(x, na.rm = TRUE)
        ),
        state_name = first(state_name)
        )
df <- as.data.table(df)

# Dropping specific states and filtering years
df <- df[!state_abbrev %in% c("AK", "HI", "SD", "DE", "DC"),]
df <- df[year > 1969 & year < 1997,]

# Creating new variables
df[, lnunemp := log(unemp)]
df[, lnhpi := log(HPI)]

# Merging with another data table (assuming banking_law_indicators is a data.table)
blaw_df[, state_name := NULL]
df <- merge(df, blaw_df, by = c("state_abbrev", "year"), all.x = TRUE)

# Creating additional variables
df[, zero := 0]
df[, zero2 := 0]

# Run analysis Leblebicioğlu & Weinberger (2020) ---------------------------------------------------
formula <- as.formula(
    "laborsh_private ~ intbanking_yb89 + intbanking_yb67 + intbanking_yb45 + intbanking_yb23 + 
  zero + intbanking_y0 + intbanking_ya1 + intbanking_ya23 + intbanking_ya45 + 
  intbanking_ya67 + intbanking_ya89 + intbranching_yb89 + intbranching_yb67 + 
  intbranching_yb45 + intbranching_yb23 + zero2 + intbranching_y0 + intbranching_ya1 + 
  intbranching_ya23 + intbranching_ya45 + intbranching_ya67 + intbranching_ya89 + lnunemp + 
  popgrowth + corptax + unionmem + lnhpi | year + statenum"
)

est <- feols(formula, df, vcov = ~statenum)

# Panel A - Inter-state banking
panelA_df <- as.data.frame(summary(est)$coeftable)
panelA_df$variable <- rownames(panelA_df)
rownames(panelA_df) <- NULL
panelA_df <- panelA_df %>% filter(str_detect(variable,"intbanking_"))
panelA_df <- rbind(panelA_df, c(0,0,0,0,"intbanking_yb1"))
panelA_df$Estimate <- as.numeric(panelA_df$Estimate)
panelA_df$`Std. Error` <- as.numeric(panelA_df$`Std. Error`)

panelA_df <- panelA_df %>%
mutate(
    upper95 = Estimate + (1.96 * `Std. Error`),
    lower95 = Estimate - (1.96 * `Std. Error`)
)

panelA_df <- panelA_df %>%
mutate(
    period = case_when(
        variable == "intbanking_yb89" ~ "-8, -9",
        variable == "intbanking_yb67" ~ "-6, -7",
        variable == "intbanking_yb45" ~ "-4, -5",
        variable == "intbanking_yb23" ~ "-2, -3",
        variable == "intbanking_yb1" ~ "-1",
        variable == "intbanking_y0" ~ "0",
        variable == "intbanking_ya1" ~ "1",
        variable == "intbanking_ya89" ~ "8, 9",
        variable == "intbanking_ya67" ~ "6, 7",
        variable == "intbanking_ya45" ~ "4, 5",
        variable == "intbanking_ya23" ~ "2, 3",
        default = NULL
    )
)
panelA_df$period <- factor(
    panelA_df$period,
    levels = c(
        "-8, -9",
        "-6, -7",
        "-4, -5",
        "-2, -3",
        "-1",
        "0",
        "1",
        "2, 3",
        "4, 5",
        "6, 7",
        "8, 9"
    ))

panelA_plt <- ggplot(data = panelA_df, aes(x=period,y=Estimate)) +
geom_pointrange(aes(ymin = lower95, ymax = upper95),color="#21918c", position = position_dodge(width=0.5)) +
geom_hline(aes(yintercept=0),linetype="dashed",linewidth=1, color="#440154") +
labs(x="Years Before and After Reform",y="Coefficients",title="Inter-state Banking, Leblebicioğlu & Weinberger (2020)") + 
theme(
    panel.grid.major = element_line(color = "grey90", linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(linetype = "solid", fill = NA, color = "black"),
    axis.title = element_text(size = 18, face = "bold", color = "black"),
    axis.text = element_text(size = 16, color = "black"),
    plot.title = element_text(size = 18, face="bold",color="black")
)

ggsave(
    paste0(result_dir,"/fig2_panelA.png"),
    panelA_plt,
    units = "in",
    height = 8, width = 12, dpi = 300,
    device = "png"
)

# Panel B - Intra-state Branching
panelB_df <- as.data.frame(summary(est)$coeftable)
panelB_df$variable <- rownames(panelB_df)
rownames(panelB_df) <- NULL
panelB_df <- panelB_df %>% filter(str_detect(variable,"intbranching_"))
panelB_df <- rbind(panelB_df, c(0,0,0,0,"intbranching_yb1"))
panelB_df$Estimate <- as.numeric(panelB_df$Estimate)
panelB_df$`Std. Error` <- as.numeric(panelB_df$`Std. Error`)

panelB_df <- panelB_df %>%
mutate(
    upper95 = Estimate + (1.96 * `Std. Error`),
    lower95 = Estimate - (1.96 * `Std. Error`)
)

panelB_df <- panelB_df %>%
mutate(
    period = case_when(
        variable == "intbranching_yb89" ~ "-8, -9",
        variable == "intbranching_yb67" ~ "-6, -7",
        variable == "intbranching_yb45" ~ "-4, -5",
        variable == "intbranching_yb23" ~ "-2, -3",
        variable == "intbranching_yb1" ~ "-1",
        variable == "intbranching_y0" ~ "0",
        variable == "intbranching_ya1" ~ "1",
        variable == "intbranching_ya89" ~ "8, 9",
        variable == "intbranching_ya67" ~ "6, 7",
        variable == "intbranching_ya45" ~ "4, 5",
        variable == "intbranching_ya23" ~ "2, 3",
        default = NULL
    )
)
panelB_df$period <- factor(
    panelB_df$period,
    levels = c(
        "-8, -9",
        "-6, -7",
        "-4, -5",
        "-2, -3",
        "-1",
        "0",
        "1",
        "2, 3",
        "4, 5",
        "6, 7",
        "8, 9"
    ))

panelB_plt <- ggplot(data = panelB_df, aes(x=period,y=Estimate)) +
geom_pointrange(aes(ymin = lower95, ymax = upper95),color="#21918c", position = position_dodge(width=0.5)) +
geom_hline(aes(yintercept=0),linetype="dashed",linewidth=1, color="#440154") +
labs(x="Years Before and After Reform",y="Coefficients",title="Intra-state Branching, Leblebicioğlu & Weinberger (2020)") + 
theme(
    panel.grid.major = element_line(color = "grey90", linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(linetype = "solid", fill = NA, color = "black"),
    axis.title = element_text(size = 18, face = "bold", color = "black"),
    axis.text = element_text(size = 16, color = "black"),
    plot.title = element_text(size = 18, face="bold",color="black")
)

ggsave(
    paste0(result_dir,"/fig2_panelB.png"),
    panelB_plt,
    units = "in",
    height = 8, width = 12, dpi = 300,
    device = "png"
)

# Run analysis Dube, Girardi, Jorda, & Taylor (2023) -----------------------------------------------
# Set panel
df <- as.data.table(df)
df <- panel(df, ~ year + statenum, time.step="consecutive")

# Make LHS variable
for (i in 0:9) {
    df[,paste0("laborsh_private",i) := shift(laborsh_private,n=i,type="lead") - shift(laborsh_private,n=1,type="lag"),]
    df[,paste0("intbanking_y0T",i) := shift(intbanking_y0,n=i,type="lead"),]
    df[,paste0("intbranching_y0",i) := shift(intbranching_y0,n=i,type="lead"),]

}
for (i in 1:9) {
    df[,paste0("laborsh_private_neg",i) := shift(laborsh_private,n=i,type="lag") - shift(laborsh_private,n=1,type="lag"),]
    df[,paste0("intbanking_y0T_neg",i) := shift(intbanking_y0,n=i,type="lag"),]
    df[,,paste0("intbranching_y0",i) := shift(intbranching_y0,n=i,type="lag"),]

}

# Make "Unclean" control indicator
#for (i in -9:9) {

#    treatment_name <- ifelse(i < 0, paste0("intbanking_y0T_neg", abs(i)), paste0("intbanking_y0T",i))

#    df[,paste0("UC",i) := 1]
#    df[ (intbanking_y0 == 1) | get(treatment_name) == 0, paste0("UC",i) := 0]

#	}

# Estimate LP
for (i in -9:9) {
    LHS_name <- ifelse(i < 0, paste0("`laborsh_private_neg", abs(i), "`"), paste0("`laborsh_private",i,"`"))
    treatment_name <- ifelse(i < 0, paste0("intbanking_y0T_neg",abs(i)), paste0("intbanking_y0T",i))
    formula <- as.formula(
        paste0(
            LHS_name,
            "~ l(", LHS_name," , 1:5)",
            " + intbanking_y0 + intbranching_y0 - 1"
        )
    )

    lpdid <- feols(formula, df[intbanking_y0==1 | get(treatment_name)==0,])

    coefs <- as.data.frame(summary(lpdid)$coeftable)
    coefs$variable <- rownames(coefs)
    rownames(coefs) <- NULL

    coefs <- coefs %>% 
    filter(variable == "intbanking_y0") %>%
    mutate(h = i)

    if (i == -9) {
        coefs_final <- coefs
    } else {
        coefs_final <- rbind(coefs_final,coefs)
    }
}

coefs_final <- coefs_final %>%
mutate(
    upper95 = Estimate + (1.96 * `Std. Error`),
    lower95 = Estimate - (1.96 * `Std. Error`),
    upper90 = Estimate + (1.645 * `Std. Error`),
    lower90 = Estimate - (1.645 * `Std. Error`)
)

ggplot(data = coefs_final) +
    geom_ribbon(aes(x=h,ymin=lower90,ymax=upper90), fill = 'grey70', alpha=0.5) +
    geom_ribbon(aes(x=h,ymin=lower95,ymax=upper95), fill='grey80',alpha=0.5) +
    geom_line(aes(x=h,y=Estimate),color="#21918c", linewidth=1.2) +
    geom_hline(yintercept = 0, linewidth = 1, linetype = "dashed") +
    scale_x_continuous(breaks = seq(-9,9,by=1), limits=c(-9,9)) +
    labs(x="Years Before and After Reform",y="Coefficients",title="Inter-state Banking, Dube, Girardi, Jorda, & Taylor (2023)") + 
    theme(
    panel.grid.major = element_line(color = "grey90", linetype = "solid"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(linetype = "solid", fill = NA, color = "black"),
    axis.title = element_text(size = 18, face = "bold", color = "black"),
    axis.text = element_text(size = 16, color = "black"),
    plot.title = element_text(size = 18, face="bold",color="black")
)