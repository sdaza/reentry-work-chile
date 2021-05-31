#############################################
# covariates to predict cluster membership
# reentry work paper
# author: sebastian daza
#############################################


library(data.table)
library(lubridate)
library(haven)

# relative directory of the paper
path_paper = "reports/paper-work-lifecourse/"
source(paste0(path_paper, "src/utils.R"))

# pre-incarceration job
jobs = data.table(read_stata(paste0(path_paper, "data/empleo_precarcel.dta")))

# 0 = no trabajo
# 1 = cuenta propia informal
# 2 = cuenta propia formal
# 3 = dependiente informal
# 4 = dependiente formal
attr(jobs$trabajo_pc, "labels")
jobs[, prejobs := trabajo_pc]
jobs[trabajo_pc == 1, prejobs := 3]
jobs[trabajo_pc == 2, prejobs := 4]
jobs[trabajo_pc == 3, prejobs := 1]
jobs[trabajo_pc == 4, prejobs := 2]

table(jobs$trabajo_pc)
labs = c("None", "Self-employed U", "Self-employed", "Under-the-table", "Employed")
jobs[, prejobs := factor(prejobs, levels = 0:4, labs)]
table(jobs$prejobs)

# baseline variables
bs = fread('output/bases/base_general.csv')
setnames(bs, names(bs), tolower(names(bs)))
bs = bs[reg_ola == 0 & reg_muestra == 1]
bs = bs[, lapply(.SD, function(x) ifelse(x < 0, NA, x)), .SDcols = names(bs)]

# add  latent classes
lclass  = fread('data/clases_latentes.csv')
nvars = c('reg_folio', 'prob1', 'prob2', 'prob3', 'probmax', 'class')
setnames(lclass, names(lclass), nvars)
lclass = lclass[, .(reg_folio, class)]

setkey(lclass, reg_folio)
setkey(bs, reg_folio)
bs = lclass[bs]
table(bs$class)

# work expectanction
bs[!is.na(eaf_14), work_importance := ifelse(eaf_14 %in% 1:2, 1, 0)]
table(bs$work_importance)
bs[!is.na(spg_7_8), work_hardness := ifelse(spg_7_8 %in% 3:4, 1, 0)]

table(bs$work_importance)
table(bs$work_hardness)

# age
setnames(bs, 'hdv_1', 'age')

# partner before prison
bs[, previous_partner := ifelse(par_6 == 1 | par_4==1 | par_4==2, 1, 0)]
bs[is.na(previous_partner) & par_4 == 0, previous_partner := 0]
table(bs$previous_partner)

# primary school dropout
bs[, only_primary := ifelse(hdv_7 <= 8, 1, 0)]
table(bs$only_primary)

bs[hdv_7 %in% 0:11, h_school := 0]
bs[hdv_7 > 11, h_school := 1]
table(bs$h_school)

bs[hdv_7 == 0, edu := 'none']
bs[hdv_7 %in% 1:7, edu := 'primary incomplete']
bs[hdv_7 == 8, edu := 'primary complete']
bs[hdv_7 %in% 9:11, edu := 'secondary incomplete']
bs[hdv_7 == 12, edu := 'secondary complete']
bs[hdv_7 > 12, edu := 'some terciary']

table(bs$edu)

# self efficacy
self_efficacy_vars = paste0('car_1_', 10:16)

bs[, self_efficacy_vars[c(2, 5)] := lapply(.SD, reverse),
   .SDcols = self_efficacy_vars[c(2, 5)]]

bs[, self_efficacy := scale(apply(.SD, 1, mean, na.rm=TRUE)),
    .SDcols = self_efficacy_vars]

# desire for help
help_vars = paste0('spg_8_', 1:4)

bs[, c(help_vars) := lapply(.SD, reverse),
   .SDcols = help_vars]

bs[, desire_change := scale(apply(.SD, 1, mean, na.rm=TRUE)),
    .SDcols = help_vars]

# work before prison
bs[, any_previous_work := apply(.SD, 1, any_values, 1),
    .SDcols = c('eaf_6', 'eaf_43')]

table(bs$any_previous_work)

# type of crime
table(bs$del_10)
bs[del_10 %in% c(1:6, 8:11, 17:18, 21:22), crime := 'property']
bs[del_10 == 7, crime := 'theft']
bs[del_10 %in% c(12:14, 19:20), crime := 'person']
bs[del_10 %in% c(15:16), crime := 'drug']
bs[del_10 == 23, crime := 'other']
bs[del_10 == 24 & reg_folio %in% c(30039, 30303), crime := 'other']
bs[reg_folio == 50129, crime := 'property']

table(bs$crime)

# previous sentences
setnames(bs, 'del_5_1', 'previous_sentences')
bs[is.na(previous_sentences) & del_4 == 0, previous_sentences := 0]

# time in prison (months)
table(bs$del_6_1) # months
table(bs$del_6_2) # years
table(bs$del_6_3) # days

bs[, del_6_2 := del_6_2 * 12]
bs[, del_6_3 := del_6_3 / 30.5]

bs[, total_previous_months_in_prison := apply(.SD, 1, sum, na.rm=TRUE),
    .SDcols = paste0('del_6_', 1:3)]
table(bs$total_previous_months_in_prison)

# mental health
bs[, mental_health := scale(apply(.SD, 1, mean, na.rm = TRUE)),
    .SDcols = names(bs) %like% '^sal_31']

# hijos
setnames(bs, 'hij_1', 'nchildren')
bs[, any_children := ifelse(nchildren > 0, 1, 0)]

# early crime
bs[, early_crime := ifelse(del_15 <= 15, 1, 0)]
table(bs$early_crime)

# last sentence extension
bs[, del_11_2 := del_11_2 / 12]
bs[, del_11_3 := del_11_3 / 365.25]
bs[, sentence_length := apply(.SD, 1, sum, na.rm = TRUE),
    .SDcols = paste0('del_11_', 1:3)]

bs[sentence_length == 0, sentence_length := NA]

# drug abuse and dependence
# most frequent drug
dep = paste0('dro_6_', c(1, 2, 4, 5, 6, 7))
abuse = paste0('dro_6_', c(8, 9, 10, 11))

bs[, c(dep, abuse) := lapply(.SD, function (x) ifelse(is.na(x), 0, x)),
    .SDcols = c(dep, abuse)]

bs[, dep_1 := apply(.SD, 1, sum, na.rm = TRUE),
    .SDcols = dep]
bs[, abuse_1 := apply(.SD, 1, sum, na.rm = TRUE),
    .SDcols = abuse]

# more problematic drug
dep = paste0('dro_7_', c(1, 2, 4, 5, 6, 7))
abuse = paste0('dro_7_', c(8, 9, 10, 11))

bs[, c(dep, abuse) := lapply(.SD, function (x) ifelse(is.na(x), 0, x)),
    .SDcols = c(dep, abuse)]

bs[, dep_2 := apply(.SD, 1, sum, na.rm = TRUE),
    .SDcols = dep]
bs[, abuse_2 := apply(.SD, 1, sum, na.rm = TRUE),
    .SDcols = abuse]

bs[, drug_dep := apply(.SD, 1, any_values, 3:6),
    .SDcols = names(bs) %like% '^dep_[1-2]$']

bs[, drug_abuse := apply(.SD, 1, any_values, 1:4),
    .SDcols = names(bs) %like% '^abuse_[1-2]$']

bs[, drug_depabuse := apply(.SD, 1, any_values, 1),
    .SDcols = c('drug_dep', 'drug_abuse')]

# family support and conflict
fsupport = paste0('sfa_8_', 1:4)
fconflict = paste0('sfa_8_', 5:7)

bs[, c(fsupport) := lapply(.SD, reverse),
   .SDcols = c(fsupport)]

bs[, c(fconflict) := lapply(.SD, reverse),
   .SDcols = c(fconflict)]

bs[, family_conflict := scale(apply(.SD, 1, mean, na.rm = TRUE)),
    .SDcols = c(fconflict)]

# select columns
bs = bs[, .(reg_folio, class, age, edu,
            only_primary, h_school, any_previous_work, 
            work_importance, work_hardness,
            nchildren, any_children, crime, early_crime,
            self_efficacy, desire_change, previous_partner,
            previous_sentences, mental_health,
            drug_depabuse, sentence_length,
            family_conflict
    )]

# center variables
bs = merge(bs, jobs, by = "reg_folio", x.all = TRUE)
cvars = c('age', 'sentence_length', 'nchildren', 'previous_sentences')
bs[, paste0('c_', cvars) := lapply(.SD, scale, scale=FALSE), .SDcols = cvars]

# save data.table
saveRDS(bs, file = paste0(path_paper, "output/data/baseline_covariates.rds"))


