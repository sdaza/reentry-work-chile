#################################################
# regression models to predict cluster membership
# reentry work paper
# author: sebastian daza
#################################################


# libraries
library(texreg)
library(TraMineR)
library(PST)
library(xtable)
library(data.table)
library(hash)

# relative directory of the paper
path_paper = "reports/paper-work-lifecourse/"
path_manuscript = "reports/paper-work-lifecourse/manuscript/"
source(paste0(path_paper, "src/utils.R"))

# load data
covs = readRDS(paste0(path_paper, "output/data/baseline_covariates.rds"))
n = nrow(covs)
clusters = fread(paste0(path_paper, "output/data/cluster_membership.csv"))
dat = merge(covs, clusters, by = "reg_folio")

# clusters labels
cluster_labels = readRDS(paste0(path_paper, "output/data/cluster_labels.rds"))
cluster_labels_jobs_se = cluster_labels[[1]]
cluster_labels_job_crime = cluster_labels[[2]]

# discrepancy analysis
seq_data_jobs = readRDS(paste0(path_paper, "output/data/seq_data_jobs.rds"))
seq_data_jobs_se = readRDS(paste0(path_paper, "output/data/seq_data_jobs_se.rds"))
seq_data_job_se_distance  = readRDS(paste0(path_paper, "output/data/seq_data_jobs_se_distance.rds"))
seq_data_job_crime = readRDS(paste0(path_paper, "output/data/seq_data_job_crime.rds"))
seq_data_job_crime_distance = readRDS(paste0(path_paper, "output/data/seq_data_job_crime_distance.rds"))

# explore sequences

labs = c("None", "Self-employed U", "Self-employed", "Employed U", "Employed")
exploreSequences(seq_data_jobs, labs)

labs = c("None", "Crime", "Informal", "Informal-Crime", 
    "Formal", "Formal-Crime")
exploreSequences(seq_data_job_crime, labs)

temp = data.table(seq_data_job_crime)
temp[, anyjob := apply(.SD, 1, function (x) any(x %in% c(2, 4))), .SDcols = 1:12]
temp[, anycrime := apply(.SD, 1, function (x) any(x %in% c(1))), .SDcols = 1:12]
temp[, jobcrime := apply(.SD, 1, function (x) any(x %in% c(3, 5))), .SDcols = 1:12]
temp[, anyjobcrime := apply(.SD, 1, sum), .SDcols = c("anyjob", "anycrime") ]
temp[, du := ifelse(anyjobcrime == 2 | jobcrime, 1, 0)]

temp[, anyinformal := apply(.SD, 1, function (x) any(x %in% c(2))), .SDcols = 1:12]
temp[, informalcrime := apply(.SD, 1, function (x) any(x %in% c(3))), .SDcols = 1:12]
temp[, anyinformalcrime := apply(.SD, 1, sum), .SDcols = c("anyinformal", "anycrime") ]

temp[, anyformal := apply(.SD, 1, function (x) any(x %in% c(4))), .SDcols = 1:12]
temp[, formalcrime := apply(.SD, 1, function (x) any(x %in% c(5))), .SDcols = 1:12]
temp[, anyformalcrime := apply(.SD, 1, sum), .SDcols = c("anyinformal", "anycrime") ]

temp[, du := ifelse(anyjobcrime == 2 | jobcrime, 1, 0)]
temp[, duinformal := ifelse(anyinformalcrime == 2 | informalcrime, 1, 0)]
temp[, duformal := ifelse(anyformalcrime == 2 | formalcrime, 1, 0)]

prop.table(table(temp$du))
prop.table(table(temp$duinformal))
prop.table(table(temp$duformal))

temp[du == 1]
prop.table(table(temp$du, temp$duinformal), 1)
prop.table(table(temp$du, temp$duformal), 1)

temp[, cluster := clusters$cluster_job_crime_4]

prop.table(table(temp$cluster, temp$du), 1)

# explore clusters
table(clusters$cluster_job_4)
labs = c("None", "Self-employed", "Under-the-table", "Formal")
exploreCluster(seq_data_jobs_se, 
    selected_cluster = "Formal", 
    cluster_vector = clusters$cluster_job_4, 
    columns = labs, 
    state = "Formal")

table(clusters$cluster_job_crime_4)
labs = c("None", "Crime", "Informal", "Informal-Crime", "Formal", "Formal-Crime")
temp = exploreCluster(seq_data_job_crime, 
    selected_cluster = "Formal", 
    cluster_vector = clusters$cluster_job_crime_4, 
    columns = labs,
    state = "Formal",
    return_table = TRUE)

temp = data.table(seq_data_job_crime)
temp[, anyjob := apply(.SD, 1, function (x) any(x %in% c(2, 4))), .SDcols = 1:12]
temp[, anycrime := apply(.SD, 1, function (x) any(x %in% c(1))), .SDcols = 1:12]
temp[, jobcrime := apply(.SD, 1, function (x) any(x %in% c(3, 5))), .SDcols = 1:12]
temp[, anyjobcrime := apply(.SD, 1, sum), .SDcols = c("anyjob", "anycrime") ]
temp[, du := ifelse(anyjobcrime == 2 | jobcrime, 1, 0)]
temp[, cluster := clusters$cluster_job_crime_4 ]

prop.table(table(temp[cluster == "Formal", du]))

temp[, working_crime := ifelse(Crime > 0 | get("Informal-Crime") > 0 | get("Formal-Crime") > 0, 1, 0)]
prop.table(table(temp$working_crime))
temp[, anycrime := apply(.SD, 1, function(x) ifelse(sum(x) > 0, 1, 0)), 
    .SDcols = names(temp) %like% "Crime"]
prop.table(table(temp$anycrime))
temp[, anyjob := apply(.SD, 1, function(x) ifelse(sum(x) > 0, 1, 0)), 
    .SDcols = names(temp) %like% "Informal|Formal"]
prop.table(table(temp$anyjob))


# jobs
st = seqtree(seq_data_jobs_se ~ age + h_school +
    any_previous_work + nchildren + previous_sentences +
    drug_depabuse + sentence_length + anyprison,
    data = dat, R = 10000, diss = seq_data_jobs_se_distance,
    weight.permutation = "diss",
    min.size = 0.05,
    max.depth = 5)

seqtreedisplay(st, type = "d", border = NA,
    filename = paste0(path_paper, "output/plots/reg_tree_job.png"))
file.copy(paste0(path_paper, "output/plots/reg_tree_job.png"),
    paste0(path_manuscript, "figures/"), recursive = TRUE)

# multifactor table
job.mfac = dissmfacw(seq_data_jobs_se_distance ~ age + h_school +
    any_previous_work + nchildren +
    previous_sentences +
    drug_depabuse + sentence_length + anyprison,
    data = dat, R = 1000)
job.mfac$mfac[order(-job.mfac$mfac$PseudoR2), ]

# multifactor table
job.mfac = dissmfacw(seq_data_job_se_distance ~ age + h_school +
                          any_previous_work + nchildren +
                          previous_sentences +
                          drug_depabuse + sentence_length +
                          anyprison,
                     data = dat, R = 1000)

tt = data.table(job.mfac$mfac[order(-job.mfac$mfac$PseudoR2), ])

total = tt[Variable == "Total"]
rest = tt[Variable != "Total"]

name_vars = c("age", "h_school", "nchildren", "any_previous_work",
               "previous_sentences", "sentence_length", "drug_depabuse", "anyprison")

label_vars = c("Age", "High school", "Number of children", "Worked before prison",
                "Number of previous sentences", "Sentence length in months",
                "Dependence / abuse of drugs", "Prison during follow-up")

dict_vars = hash(name_vars, label_vars)
rest[, Variable := values(dict_vars, rest[["Variable"]])]
tt = rbind(rest, total)
setnames(tt,
       names(tt),
       c("Covariate", "Pseudo F", "Pseudo $R^2$", "p-value"))

caption = paste0("Multi-factor discrepancy analysis for job sequences (N = ",
                 n-3, ")")
label = "tab:discrepancy_job"
ptab = print(xtable(tt, caption = caption, label = label, align = "llccc"),
             include.rownames = FALSE,
             caption.placement = "top",
             table.placement = "htp",
             sanitize.text.function=function(x) {x})

comment = "Estimation based on Hamming distances of sequences."
ptab = gsub("begin\\{table\\}\\[htp\\]\\n",
            "begin\\{table\\}\\[htp\\]\\\n\\\\footnotesize\\\n\\\\setlength\\{\\\\tabcolsep\\}\\{10pt\\}\\\n\\\\renewcommand\\{\\\\arraystretch\\}\\{1.3\\}\\\n\\\\begin\\{threeparttable\\}\\\n",
            ptab)
ptab = gsub("end\\{tabular\\}\\n",
            paste0("end\\{tabular\\}\\\n\\\\begin{tablenotes}\\\n\\\\scriptsize\\\n\\\\item ",
                   comment,
                   "\\\n\\\\end{tablenotes}\\\n\\\\end{threeparttable}\\\n"),
            ptab)
cat(ptab, file = paste0(path_paper, "output/tables/discrepancy_job.tex"))
file.copy(paste0(path_paper, "output/tables/discrepancy_job.tex"),
    paste0(path_manuscript, "tables/"), recursive = TRUE)

# job and crime
st = seqtree(seq_data_job_crime ~ age + h_school +
    any_previous_work + nchildren +
    previous_sentences +
    drug_depabuse +
    sentence_length + anyprison,
    data = dat, R = 10000, diss = seq_data_job_crime_distance,
    weight.permutation = "diss",
    min.size = 0.05,
    max.depth = 5)

seqtreedisplay(st, type = "d", border = NA,
    sortv = "from.start",
    filename = paste0(path_paper, "output/plots/reg_tree_job_crime.png"))
file.copy(paste0(path_paper, "output/plots/reg_tree_job_crime.png"),
    paste0(path_manuscript, "figures/"), recursive = TRUE)

# multifactor table
jobcrime.mfac = dissmfacw(seq_data_job_crime_distance ~ age + h_school +
                          any_previous_work + nchildren +
                          previous_sentences +
                          drug_depabuse + sentence_length +
                          anyprison,
                     data = dat, R = 1000)

tt = data.table(jobcrime.mfac$mfac[order(-jobcrime.mfac$mfac$PseudoR2), ])

total = tt[Variable == "Total"]
rest = tt[Variable != "Total"]

name_vars = c("age", "h_school", "nchildren", "any_previous_work",
               "previous_sentences", "sentence_length", "drug_depabuse", "anyprison")

label_vars = c("Age", "High school", "Number of children", "Worked before prison",
                "Number of previous sentences", "Sentence length in months",
                "Dependence / abuse of drugs", "Prison during follow-up")

dict_vars = hash(name_vars, label_vars)
rest[, Variable := values(dict_vars, rest[["Variable"]])]
tt = rbind(rest, total)
setnames(tt,
       names(tt),
       c("Covariate", "Pseudo F", "Pseudo $R^2$", "p-value"))

caption = paste0("Multi-factor discrepancy analysis for job and crime sequences (N = ",
                 n-3, ")")
label = "tab:discrepancy_job_crime"
ptab = print(xtable(tt, caption = caption, label = label, align = "llccc"),
             include.rownames = FALSE,
             caption.placement = "top",
             table.placement = "htp",
             sanitize.text.function=function(x) {x})

comment = "Estimation based on Hamming distances of sequences."
ptab = gsub("begin\\{table\\}\\[htp\\]\\n",
            "begin\\{table\\}\\[htp\\]\\\n\\\\footnotesize\\\n\\\\setlength\\{\\\\tabcolsep\\}\\{10pt\\}\\\n\\\\renewcommand\\{\\\\arraystretch\\}\\{1.3\\}\\\n\\\\begin\\{threeparttable\\}\\\n",
            ptab)
ptab = gsub("end\\{tabular\\}\\n",
            paste0("end\\{tabular\\}\\\n\\\\begin{tablenotes}\\\n\\\\scriptsize\\\n\\\\item ",
                   comment,
                   "\\\n\\\\end{tablenotes}\\\n\\\\end{threeparttable}\\\n"),
            ptab)
cat(ptab, file = paste0(path_paper, "output/tables/discrepancy_job_crime.tex"))

file.copy(paste0(path_paper, "output/tables/discrepancy_job_crime.tex"),
    paste0(path_manuscript, "tables/"), recursive = TRUE)