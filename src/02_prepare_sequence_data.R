# libraries
library(data.table) 
library(PST)
library(WeightedCluster)
library(TraMineR)
library(TraMineRextras)
library(xtable)
library(msm)

# set seed to reproduce imputation
set.seed(210013)

# relative directory of the paper
path_paper = "reports/paper-work-lifecourse/"
path_manuscript = "reports/paper-work-lifecourse/manuscript/"

source(paste0(path_paper, "src/utils.R"))
source("src/calendario/utils.R")

# all missing ids
all_missing_ids = c(10016, 10083, 10097, 10248, 20020, 20120, 20191,
    20289, 20298, 30025, 30148, 30159, 40267, 50080,
    50131, 50163, 50242, 50245)

# load covariates
covs = readRDS(paste0(path_paper, "output/data/baseline_covariates.rds"))
cal_covs = readRDS(paste0(path_paper, "output/data/calendar_covs.rds"))
covs = covs[!reg_folio %in% all_missing_ids][, id := 1:.N]
cal_covs = cal_covs[!reg_folio %in% all_missing_ids]
covs = merge(covs, cal_covs, by = "reg_folio")

# read calendar data and pilar's classes
dat = readRDS(paste0(path_paper, "output/data/job_calendar.rds"))
class = fread("data/clases_latentes.csv")
setnames(class, c("FOLIO_2", "predclass3G"),
         c("reg_folio", "class"))

dat = dat[reg_muestra == 1][!reg_folio %in% all_missing_ids]
dat = merge(dat, class[, .(reg_folio, class)],
            by = "reg_folio", x.all = TRUE)

n = length(unique(dat$reg_folio))
print(paste0("Number of valid cases: ", n))

# check 12 months
table(dat[month_index == 12, days_month])
ids = dat[month_index == 12 & days_month == 0, reg_folio]

wave_summary = createWaveSummary(
    path_to_data = "output/bases/base_general_wide.csv")

table(wave_summary[reg_folio %in% ids & wave == 4, sequence_waves])
table(wave_summary[sequence_waves %in% c("001", "011", "101", "111") &
    wave == 4, cum_months] < 11.75)
# 7 dropouts
# 12 before month 12
# 78 with less than 3/4 of the month

# create variables  :::::::::::::::::::::

# jobs
# 1 = cuenta propia informal
# 2 = cuenta propia formal
# 3 = dependiente informal
# 4 = dependiente formal
work_columns = paste0("trabajo", 1:7, "_oc")
# get max of jobs by time
dat[, job := apply(.SD, 1, getMax), .SDcols = work_columns]
table(dat$job)

# job search
dat[, anyjobsearch := getMax(trab_busco_oc), reg_folio]
dat[, jobsearching := job * 10 + trab_busco_oc]
table(dat$jobsearching)
# 1 = 0 = cuenta propia informal
# 2 = 1 = cuenta propia formal
# 3 = 10 = dependiente informal
# 4 = 11 = dependiente formal
dat[jobsearching %in% c(10, 11), jobsearching := 2]
dat[jobsearching %in% c(20, 21), jobsearching := 2]
dat[jobsearching %in% c(30, 31), jobsearching := 3]
dat[jobsearching %in% c(40, 41), jobsearching := 4]

# prison, check filter
table(dat$jst_carcel_dias)
dat[, prison := ifelse(jst_carcel_dias > 0, 1, 0)]
table(dat$prison)
dat[, anyprison := getMax(prison), reg_folio]
prop.table(table(dat[, getMax(prison), reg_folio]$V1))

# crime
crime_vars = c("rb_habitgente_oc", "rb_habitsin_oc", "rb_nohabit_oc",
    "rb_atm_oc", "rb_vehi_oc", "rb_en_vehi_oc", "rb_hurto_oc",
    "rb_sorpresa_oc", "rb_intim_ame_oc", "rb_intim_arma_oc",
    "rb_violencia_oc", "lesion_oc", "homi_oc", "amenazas_oc",
    "drgs_prep_oc", "drgs_ventas_oc", "act_ilegal_oc", "recepta_oc",
    "vif_oc", "vandalismo_oc", "estafas_oc", "armas_oc")
no_income_crime = c("homi_oc", "amenazas_oc", "vif_oc", "vandalismo_oc",
    "armas_oc", "lesion_oc")

crime_vars = crime_vars[!crime_vars %in% no_income_crime]
nc_crime_vars = gsub("_oc", "", crime_vars)

dat[, "crime" := apply(.SD, 1, flag_positive_values),
    .SDcols = crime_vars]

# any job
dat[, anyjob := as.numeric(job > 0)]
table(dat$anyjob)

dat[is.na(days_month), days_month := 0]
length(unique(dat[month_index == 12 & (days_month <= 15), reg_folio])) /
    length(unique(dat[month_index == 12, reg_folio]))

# save data
saveRDS(dat, file = paste0(path_paper, "output/data/data_for_sequence_analysis.rds"))
saveRDS(covs, file = paste0(path_paper, "output/data/baseline_covariates.rds"))