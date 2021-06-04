#############################################
# sequence analysis
# reentry work paper
# author: sebastian daza
#############################################


# libraries
library(data.table) 
library(PST)
library(WeightedCluster)
library(TraMineR)
library(TraMineRextras)
library(xtable)


# set seed to reproduce imputation
set.seed(210013)

# relative directory of the paper
path_paper = "reports/paper-work-lifecourse/"
path_manuscript = "reports/paper-work-lifecourse/manuscript/"
source(paste0(path_paper, "src/utils.R"))
source("src/calendario/utils.R")

dat = readRDS(file = paste0(path_paper, "output/data/data_for_sequence_analysis.rds"))
covs = readRDS(file = paste0(path_paper, "output/data/baseline_covariates.rds"))
n = nrow(covs)

cluster_membership = data.table::copy(unique(dat[, .(reg_folio)]))

# colors 
colors5 = c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac")
colors6 = c("#ffff99", "#e31a1c",  "#a6cee3", "#fb9a99", "#1f78b4", "#beaed4")
colors7 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
    "#e31a1c", "#fdbf6f")
colors8 = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
    "#e31a1c", "#fdbf6f", "#ff7f00")

# sequences :::::::::::

# any job :::::::::::::::::
prop.table(table(dat[, getMax(anyjob), reg_folio]$V1))
seq_data_anyjob = create_sequences(
    data = dat,
    seq_variable = "anyjob",
    seq_labels = c("None", "Employed"),
)
seqstatd(seq_data_anyjob)
seqmeant(seq_data_anyjob, prop = TRUE, serr = TRUE)
seqmeant(seq_data_anyjob, prop = FALSE, serr = TRUE)


# all job categories and plots
labs = c("None", "Self-employed U", "Self-employed", "Employed U", "Employed")
seq_data_jobs = create_sequences(
    data = dat,
    seq_variable = "job",
    seq_labels = labs,
    colors = colors5
)
seqstatd(seq_data_jobs)
seqmeant(seq_data_jobs, prop = TRUE, serr = TRUE)

savepdf(paste0(path_paper, "output/plots/seq_dist_jobs"))
    seqdplot(seq_data_jobs,
        with.legend = FALSE, ltext = labs)
        mtext(side = 1, line = 2.3, "Months", font = 1, cex = 1)
        legend(1.1, 1.08, legend = labs, cex = 0.7, pch = 15, col = colors5,
        horiz = TRUE,  bty = "n", inset = c(0, 2), xpd = TRUE,
        text.width=c(0,0.7,1.3,1.5,1.5))
dev.off()
file.copy(paste0(path_paper, "output/plots/seq_dist_jobs.pdf"),
    paste0(path_manuscript, "figures/"), recursive = TRUE)

# redefine job categories
# 1 = cuenta propia informal
# 2 = cuenta propia formal
# 3 = dependiente informal
# 4 = dependiente formal
dat[job == 1, independent_job := 1]
dat[job == 2, independent_job := 3]
dat[job == 3, independent_job := 2]
dat[job == 4, independent_job := 3]
dat[job == 0, independent_job := 0]
table(dat$independent_job)
labs = c("None", "Self-employed", "Under-the-table", "Formal")

seq_data_jobs_se = create_sequences(
    data = dat,
    seq_variable = "independent_job",
    seq_labels = labs,
    colors = colors5[2:5]
)

# create plot time in state by previous jobs
group = as.character(covs$prejobs)
# group[group == "Self-employed U"] = "Self-employed"
labs = c("None", "Self-employed", "Under-the-table", "Formal")
glabs = c("None", "Self-employed", "Under-the-table", "Formal")
time = timeSpentGroup(seq_data_jobs_se, groupv = group, prop = FALSE,
    states = 0:3, sample = 1000, glabels = glabs, slabels = labs)

out = print(xtable(time, 
    caption = "Months in after-release jobs by job declared 6 months before incarceration", 
    label = "tab:prejobs"), 
    include.rownames = FALSE, 
    caption.placement = "top",
    sanitize.text.function = function(x){x}, 
    file = paste0(path_paper, "output/tables/prejobs.tex"))

tab = list()
tab[[1]] =  "
\\begin{table}[htp]
\\renewcommand{\\arraystretch}{1.1}
\\setlength{\\tabcolsep}{6pt}
\\centering
\\scriptsize
\\caption{Months in after-release jobs by occupation declared 6 months before incarceration} 
\\label{tab:prejobs}
\\begin{threeparttable}
\\begin{tabular}{llllll}
 \\hline
 \\addlinespace
 & & \\multicolumn{4}{c}{Job 6 months before incarceration} \\\\
 \\addlinespace
 \\cmidrule(lr){3-6} 
\\addlinespace
 & Total & None & Self-employed & Under-the-table & Formal \\\\
Job after release  & \\multicolumn{1}{l}{(N=207)} & \\multicolumn{1}{l}{(N=105)} & \\multicolumn{1}{l}{(N=62)}  &
\\multicolumn{1}{l}{(N=16)} & \\multicolumn{1}{l}{(N=24)} \\\\
\\addlinespace
  \\hline
 \\addlinespace
"
tab[[2]] = gsub("(.+\\\\\\\\\\s\\n\\s+\\\\hline\\n)(.+)(\\s+\\\\hline\\n.+)", "\\2", out)
tab[[3]] = "
\\addlinespace
   \\hline
\\end{tabular}
\\begin{tablenotes}
\\item 95\\% bootstrapped confidence intervals in parenthesis (1000 samples).
\\end{tablenotes}
\\end{threeparttable}
\\end{table}
"
cat(paste(tab, collapse = ''), file = paste0(path_paper, "output/tables/prejobs.tex"))
file.copy(paste0(path_paper, "output/tables/prejobs.tex"),
    paste0(path_manuscript, "tables/"), recursive = TRUE)

# compare clusters solutions
seq_data_jobs_se_distance = seqdist(seq_data_jobs_se, norm = "auto", method = "HAM")
benchmark_clusters = wcKMedRange(seq_data_jobs_se_distance, 2:6)

# create benchmark table for jobs ::::::::::::::::::
tcl = benchmark_clusters$stats
tcl = tcl[, c("ASW", "HG", "PBC")]
row.names(tcl) = paste0(2:6, " clusters")

caption = paste0("Employement quality measures for cluster solutions (N = ", n, ")")
label = "tab:quality_clusters_job"
comment = "ASW = Average Silhouette width, HG = Hubert's Gamma, PBC = Point Biserial Correlation."

add_notes_table(tcl,
    align = "lccc",
    tabcolsep = 35,
    caption = caption,
    label = label,
    comment = comment,
    filename = paste0(path_paper, "output/tables/cluster_quality_job.tex")
)
file.copy(paste0(path_paper, "output/tables/cluster_quality_job.tex"),
    paste0(path_manuscript, "tables/"), recursive = TRUE)

# 4 clusters seems the best solution
plot(benchmark_clusters, stat = c("ASW", "HG", "PBC"))

# create plots (self-employed = se)
cl_jobs_se_4 = create_clusters(seq_data_jobs_se, nclusters = 4,
    method = "HAM",
    norm_distance = "auto"
)
cluster_vector = cl_jobs_se_4[["c4"]][[1]]

cluster_levels_jobs_se = c("Unemployed", "Self-employed", "Under-the-table", "Formal")
vorder = table(cluster_vector)
cluster_labels_jobs_se = c(NA, NA, NA, NA)
cluster_labels_jobs_se[which(vorder == min(vorder))] = "Formal"
cluster_labels_jobs_se[which(vorder == max(vorder))] = "Unemployed"
torder = vorder[-c(
    which(vorder == min(vorder)), 
    which(vorder == max(vorder)))]
if (torder[1] > torder[2]) {
    cluster_labels_jobs_se[which(vorder %in% torder)] = c("Self-employed", "Under-the-table")
} else {
    cluster_labels_jobs_se[which(vorder %in% torder)] = c("Under-the-table", "Self-employed")
}

cl_jobs_se_4 = create_clusters(seq_data_jobs_se, nclusters = 4,
    method = "HAM",
    norm_distance = "auto",
    cluster_labels = cluster_labels_jobs_se,
    cluster_levels = cluster_levels_jobs_se,
)

cluster_vector = cl_jobs_se_4[["c4"]][[1]]
n_clusters = c(n, table(cluster_vector))

create_plots(seq_data_jobs_se, cl_jobs_se_4[[1]],
    paste0(path_paper, "output/plots/seq_job_se_4_clusters"),
    order = "sql"
)
file.copy(paste0(path_paper, "output/plots/seq_job_se_4_clusters.pdf"),
    paste0(path_manuscript, "figures/"), recursive = TRUE)

cluster_membership[, cluster_job_4 := cluster_vector]

# create table with stats across clusters ::::::::::::::::::::::
cluster_vector = cl_jobs_se_4[["c4"]][[1]]
slabels =  c("None", "Self-employed", "Under-the-table", "Formal")
glabels = c("Unemployed", "Self-employed", "Under-the-table", "Formal")
table(cluster_vector)

des_vars = c("age", "h_school", "nchildren", "drug_depabuse", "early_crime", 
    "previous_sentences", "sentence_length", "any_previous_work", "work_hardness", 
    "anyjobsearch", "anyprison")
des_labs = c("Age", "High school", "Num. of children", "Dependence / drug abuse", 
    "Crime before age 15", "Num. previous sentences", "Sentence length", "Worked before prison", 
    "Expected hardness of finding job",
    "Searched jobs during follow-up", "Prison during follow-up")
descriptives = list()
for (i in seq_along(des_vars)) {
    myformat = ifelse(des_vars[i] == "age", "%#.1f", "%#.2f")
    descriptives[[i]] = ciPropGroup(covs, des_vars[i], varlabel = des_labs[i], groupv = cluster_vector, 
        glabels = glabels, test = TRUE, format = myformat)
}

descriptives = rbindlist(descriptives)
descriptives[, Variable := paste0("\\quad ", Variable)]

time = timeSpentGroup(seq_data_jobs_se, groupv = cluster_vector, prop = FALSE,
    states = 0:3, sample = 1000, glabels = glabels, slabels = slabels)
time[, State := paste0("\\quad ", State)]

tab = list()
dts = list(time, descriptives)
out1 = print(xtable(dts[[1]]), include.rownames = FALSE, 
    sanitize.text.function = function(x){x})
out1 = gsub("(.+\\\\\\\\\\s\\n\\s+\\\\hline\\n)(.+)(\\s+\\\\hline\\n.+)", "\\2", out1)

out2 = print(xtable(dts[[2]]), include.rownames = FALSE, 
    sanitize.text.function = function(x){x})
out2 = gsub("(.+\\\\\\\\\\s\\n\\s+\\\\hline\\n)(.+)(\\s+\\\\hline\\n.+)", "\\2", out2)

addtorow = list()
addtorow$pos = as.list(c(4, 8))
addtorow$command = as.vector(rep("\\addlinespace[12pt] \n", 3))

# table list
tab = list()

tabn = paste0("& \\multicolumn{1}{l}{(N=", n_clusters[1], ")} & \\multicolumn{1}{l}{(N=", n_clusters[2], ")} 
        & \\multicolumn{1}{l}{(N=", n_clusters[3], ")} & \\multicolumn{1}{l}{(N=", n_clusters[4], ")} 
        & \\multicolumn{1}{l}{(N=", n_clusters[5], ")} \\\\"
)

tab[[1]] =  paste0("
\\begin{table}[htp]
\\scriptsize
\\caption{Covariates by job cluster}
\\label{tab:desc_job_clusters}
\\setlength{\\tabcolsep}{5pt}
\\renewcommand{\\arraystretch}{1.3}
\\begin{threeparttable}
\\begin{tabular}{llllll}
\\hline
\\addlinespace[8pt]
& & \\multicolumn{4}{c}{Cluster} \\\\
\\addlinespace
\\cmidrule(lr){3-6} 
\\addlinespace
& \\multicolumn{1}{l}{Total} & \\multicolumn{1}{l}{Unemployed} & \\multicolumn{1}{l}{Self-employed} & \\multicolumn{1}{l}{Under-the-table} & \\multicolumn{1}{l}{Formal} \\\\", 
tabn, 
"\\addlinespace[8pt]
\\hline
\\addlinespace[12pt]
\\multicolumn{6}{l}{\\textbf{Time spent on job (months)}} \\\\
\\addlinespace
")

tab[[2]] = out1
tab[[3]] = "\\addlinespace[12pt]
\\multicolumn{6}{l}{\\textbf{Covariates (average)*}} \\\\
\\addlinespace
"

tab[[4]] = out2

tab[[5]] = "\\addlinespace
\\addlinespace
\\addlinespace
\\hline
\\addlinespace
\\end{tabular}
\\begin{tablenotes}
\\scriptsize
    \\item * Statistically significant differences across clusters (p-value $<$ 0.05). 95\\% bootstrapped confidence intervals in parenthesis (1000 samples).
\\end{tablenotes}
\\end{threeparttable}
\\end{table}
"
cat(paste(tab, collapse = ''), file = paste0(path_paper, "output/tables/tab_cluster_job.tex"))
file.copy(paste0(path_paper, "output/tables/tab_cluster_job.tex"),
  paste0(path_manuscript,  "tables/"), recursive = TRUE)

# transition table
labs = c("None", "Self-employed", "Under-the-table", "Formal")
tmat = transMat(data.table(seq_data_jobs_se), labels = labs, states = 0:3) 
mmat = matrix(tmat$Est, ncol = 4, nrow = 4, byrow = TRUE)
rownames(mmat) = labs
colnames(mmat) = labs
caption = paste0("Transition rates between job categories of women inmates \\newline
    during the first 12 months following their release (N = ", n, " $\\times$ 12)")
label = "tab:transition_rates_job"
comment = "Probability to switch at a given position from state $s_i$ to state $s_j$. 
    \\\\item 95\\\\% bootstrapped confidence intervals in parenthesis (1000 samples)."

add_notes_table(mmat ,
    align = "lcccc",
    tabcolsep = 10,
    caption = caption,
    fontsize = "scriptsize",
    label = label,
    comment = comment,
    filename = paste0(path_paper, "output/tables/transition_rates_job.tex")
)
file.copy(paste0(path_paper, "output/tables/transition_rates_job.tex"),
    paste0(path_manuscript, "tables/"), recursive = TRUE)

# job and crime ::::::::::::

# 1 = 0 = cuenta propia informal
# 2 = 1 = cuenta propia formal
# 3 = 10 = dependiente informal
# 4 = 11 = dependiente formal
dat[job == 1, ijob := 1]
dat[job == 2, ijob := 2]
dat[job == 3, ijob := 1]
dat[job == 4, ijob := 2]
dat[job == 0, ijob := 0]

dat[, work_crime := ijob]
table(dat$crime)
table(dat$work_crime)
dat[, work_crime := work_crime * 10 + crime]
table(dat$work_crime)

dat[work_crime == 1, work_crime := 1]
dat[work_crime == 10, work_crime := 2]
dat[work_crime == 11, work_crime := 3]
dat[work_crime == 20, work_crime := 4]
dat[work_crime == 21, work_crime := 5]
table(dat$work_crime)

labs = c("None", "Crime", "Informal", "Informal-Crime",
    "Formal", "Formal-Crime")
seq_data_job_crime = create_sequences(data = dat,
    seq_variable = "work_crime",
    seq_labels = labs,
    colors = colors8[1:6]
)

savepdf(paste0(path_paper, "output/plots/seq_dist_job_crime"),
    mar = c(3.3,3.6,2.5,1.1), mgp = c(2.7,0.45,0))
    seqdplot(seq_data_job_crime,
        with.legend = FALSE, ltext = labs)
    mtext(side = 1, line = 2.3, "Months", font = 1, cex = 1)
    legend(3, 1.15, legend = labs, cex = 0.7, pch = 15, col = colors8[1:6],
        ncol = 3, bty = "n", inset = c(0, 1), xpd = TRUE,
        text.width = c(0,0.7,0.7,0.7,1.2, 1.2)
        )
dev.off()
file.copy(paste0(path_paper, "output/plots/seq_dist_job_crime.pdf"),
    paste0(path_manuscript, "figures/"), recursive = TRUE)

seqstatf(seq_data_job_crime)
seqstatd(seq_data_job_crime)
seqmeant(seq_data_job_crime)

# compare clusters solutions
seq_data_job_crime_distance = seqdist(seq_data_job_crime,
    norm = "auto", method = "HAM")
benchmark_clusters = wcKMedRange(seq_data_job_crime_distance, 2:6)

# create benchmark table for jobs
tcl = benchmark_clusters$stats
tcl = tcl[, c("ASW", "HG", "PBC")]
row.names(tcl) = paste0(2:6, " clusters")

caption = paste0("Employement and crime quality measures for cluster solutions (N = ", n, ")")
label = "tab:quality_clusters_job_crime"
comment = "ASW = Average Silhouette width, HG = Hubert's Gamma, PBC = Point Biserial Correlation."

add_notes_table(tcl,
                align = "lccc",
                tabcolsep = 35,
                caption = caption,
                label = label,
                comment = comment,
                filename = paste0(path_paper, "output/tables/cluster_quality_job_crime.tex")
                )
file.copy(paste0(path_paper, "output/tables/cluster_quality_job_crime.tex"),
    paste0(path_manuscript, "tables/"), recursive = TRUE)

# 4 clusters seems the best solution
plot(benchmark_clusters, stat = c("ASW", "HG", "PBC"))

# 4-cluster solution
# cluster_labels_job_crime = cluster_levels_job_crime
cluster_levels_job_crime = c("Unemployed", "Offending", "Informal", "Formal")

cl_job_crime_4 = create_clusters(seq_data_job_crime,
    nclusters = 4,
    method = "HAM",
    norm_distance = "auto"
)
cluster_vector = cl_job_crime_4[["c4"]][[1]]

vorder = table(cluster_vector)
cluster_labels_job_crime = c(NA, NA, NA, NA)
cluster_labels_job_crime[which(vorder == min(vorder))] = "Formal"
cluster_labels_job_crime[which(vorder == max(vorder))] = "Unemployed"
torder = vorder[-c(
    which(vorder == min(vorder)), 
    which(vorder == max(vorder)))]
if (torder[1] > torder[2]) {
    cluster_labels_job_crime[which(vorder %in% torder)] = c("Informal", "Offending")
} else {
    cluster_labels_job_crime[which(vorder %in% torder)] = c("Offending", "Informal")
}

cl_job_crime_4 = create_clusters(seq_data_job_crime,
    nclusters = 4,
    method = "HAM",
    norm_distance = "auto", 
    cluster_labels = cluster_labels_job_crime,
    cluster_levels = cluster_levels_job_crime,
)

cluster_vector = cl_job_crime_4[["c4"]][[1]]
n_clusters = c(n, table(cluster_vector))

create_plots(seq_data_job_crime, cl_job_crime_4[[1]],
    paste0(path_paper, "output/plots/seq_job_crime_4_clusters"),
    order = "sql")
file.copy(paste0(path_paper, "output/plots/seq_job_crime_4_clusters.pdf"),
    paste0(path_manuscript, "figures/"), recursive = TRUE)
cluster_membership[, cluster_job_crime_4 := cluster_vector]

# explore clusters 
select_cluster = "Formal"
temp = data.table(
    by(seq_data_job_crime,
        cluster_membership$cluster_job_crime_4, seqistatd)[[select_cluster]]
)

# create table with stats across clusters ::::::::::::::::
cluster_vector = cl_job_crime_4[["c4"]][[1]]
table(cluster_vector)
slabels = c("None", "Crime", "Informal", "Informal-Crime", "Formal", "Formal-Crime")
glabels = c("Unemployed", "Offending", "Informal", "Formal")

descriptives = list()
for (i in seq_along(des_vars)) {
    myformat = ifelse(des_vars[i] == "age", "%#.1f", "%#.2f")
    descriptives[[i]] = ciPropGroup(covs, des_vars[i], varlabel = des_labs[i], groupv = cluster_vector, 
        glabels = glabels, test = TRUE, format = myformat)
}

descriptives = rbindlist(descriptives)
descriptives[, Variable := paste0("\\quad ", Variable)]

time = timeSpentGroup(seq_data_job_crime, groupv = cluster_vector, prop = FALSE,
    states = 0:5, sample = 1000, glabels = glabels, slabels = slabels)
time[, State := paste0("\\quad ", State)]

tab = list()
dts = list(time, descriptives)
out1 = print(xtable(dts[[1]]), include.rownames = FALSE, sanitize.text.function = function(x){x})
out1 = gsub("(.+\\\\\\\\\\s\\n\\s+\\\\hline\\n)(.+)(\\s+\\\\hline\\n.+)", "\\2", out1)

out2 = print(xtable(dts[[2]]), include.rownames = FALSE, sanitize.text.function = function(x){x})
out2 = gsub("(.+\\\\\\\\\\s\\n\\s+\\\\hline\\n)(.+)(\\s+\\\\hline\\n.+)", "\\2", out2)

addtorow = list()
addtorow$pos = as.list(c(6, 12))
addtorow$command = as.vector(rep("\\addlinespace[12pt] \n", 5))

# table list
tab = list()

tabn = paste0("& \\multicolumn{1}{l}{(N=", n_clusters[1], ")} & \\multicolumn{1}{l}{(N=", n_clusters[2], "} 
        & \\multicolumn{1}{l}{(N=", n_clusters[3], ")} & \\multicolumn{1}{l}{(N=", n_clusters[4], ")} 
        & \\multicolumn{1}{l}{(N=", n_clusters[5], ")} \\\\"
)

tab[[1]] =  paste0("
\\renewcommand{\\arraystretch}{0.8}
\\begin{scriptsize}
{\\setlength{\\tabcolsep}{5pt}
\\begin{longtable}{llllll} 
\\caption{Covariates by job-crime cluster}%
 \\label{tab:desc_job_crime_clusters}\\\\
\\hline
\\addlinespace
& & \\multicolumn{4}{c}{Cluster} \\\\
\\addlinespace
\\cmidrule(lr){3-6} 
\\addlinespace
& \\multicolumn{1}{l}{Total} & \\multicolumn{1}{l}{Unemployed} & \\multicolumn{1}{l}{Offenders} & \\multicolumn{1}{l}{Informal} & \\multicolumn{1}{l}{Formal} \\\\", 
tabn, 
"\\addlinespace[8pt]
\\hline
\\addlinespace[12pt]
\\multicolumn{6}{l}{\\textbf{Time spent on job (months)}} \\\\
\\addlinespace"
)

tab[[2]] = out1
tab[[3]] = "\\addlinespace[12pt]
\\multicolumn{6}{l}{\\textbf{Covariates (average)*}} \\\\
\\addlinespace
"

tab[[4]] = out2

tab[[5]] = "\\addlinespace
\\addlinespace
\\addlinespace
\\hline
\\addlinespace
\\multicolumn{6}{l}{* Statistically significant differences across clusters (p-value $<$ 0.05). 95\\% bootstrapped confidence intervals in parenthesis (1000 samples).} \\\\
\\end{longtable}
}
\\end{scriptsize}
"

cat(paste(tab, collapse = ''), file = paste0(path_paper, 
    "output/tables/tab_cluster_job_crime.tex"))
file.copy(paste0(path_paper, "output/tables/tab_cluster_job_crime.tex"),
  paste0(path_manuscript,  "tables/"), recursive = TRUE)

# transition table ::::::::::::::::::::
labs = c("None", "Crime", "Informal", "Informal-Crime", "Formal", "Formal-Crime")
tmat = transMat(data.table(seq_data_job_crime), labels = labs, states = 0:5) 
mmat = matrix(tmat$Est, ncol = 6, nrow = 6, byrow = TRUE)
rownames(mmat) = labs
colnames(mmat) = labs
caption = paste0("Transition rates between job-crime categories of women inmates \\newline
    during the first 12 months following their release (N = ", n, " $\\times$ 12)")
label = "tab:transition_rates_job_crime"
comment = "Probability to switch at a given position from state $s_i$ to state $s_j$. 
    \\\\item 95\\\\% bootstrapped confidence intervals in parenthesis (1000 samples)."

add_notes_table(mmat ,
    align = "lcccccc",
    tabcolsep = 10,
    caption = caption,
    fontsize = "scriptsize",
    label = label,
    comment = comment,
    filename = paste0(path_paper, "output/tables/transition_rates_job_crime.tex")
)
file.copy(paste0(path_paper, "output/tables/transition_rates_job_crime.tex"),
    paste0(path_manuscript, "tables/"), recursive = TRUE)

# transition matrix any job and crime :::::::::::::::::::::::
dat[, anyjob_crime := anyjob * 10 + crime]
dat[anyjob_crime == 1, ianyjob_crime := 1]
dat[anyjob_crime == 10, ianyjob_crime := 2]
dat[anyjob_crime == 11, ianyjob_crime := 3]
dat[anyjob_crime == 0, ianyjob_crime := 0]

table(dat$ianyjob_crime)

labs = c("None", "Crime", "Job", "Job-Crime")
seq_data_anyjobcrime = create_sequences(
    data = dat,
    seq_variable = "ianyjob_crime",
    seq_labels = labs
  )
tmat = transMat(data.table(seq_data_anyjobcrime), labels = labs, states = 0:3) 
mmat = matrix(tmat$Est, ncol = 4, nrow = 4, byrow = TRUE)
rownames(mmat) = labs
colnames(mmat) = labs
caption = paste0("Transition rates between having any job and crime of women inmates \\newline
    during the first 12 months following their release (N = ", n, " $\\times$ 12)")
label = "tab:transition_rates_anyjob_crime"
comment = "Probability to switch at a given position from state $s_i$ to state $s_j$. 
    \\\\item 95\\\\% bootstrapped confidence intervals in parenthesis (1000 samples)."

add_notes_table(mmat ,
    align = "lcccc",
    tabcolsep = 10,
    caption = caption,
    fontsize = "scriptsize",
    label = label,
    comment = comment,
    filename = paste0(path_paper, "output/tables/transition_rates_anyjob_crime.tex")
)
file.copy(paste0(path_paper, "output/tables/transition_rates_anyjob_crime.tex"),
    paste0(path_manuscript, "tables/"), recursive = TRUE)

# save cluster membership
fwrite(cluster_membership,
       file = paste0(path_paper, "output/data/cluster_membership.csv"),
       row.names = FALSE)

# save sequence data
ccovs = c("anyjob", "anyjobsearch" , "anyprison")
saveRDS(list(cluster_levels_jobs_se, cluster_levels_job_crime),
    file = paste0(path_paper, "output/data/cluster_labels.rds"))
saveRDS(dat[, lapply(.SD, getMax), reg_folio, .SDcols = ccovs],
    file = paste0(path_paper, "output/data/calendar_covs.rds"))
saveRDS(seq_data_jobs, file = paste0(path_paper, "output/data/seq_data_jobs.rds"))
saveRDS(seq_data_jobs_se, file = paste0(path_paper, "output/data/seq_data_jobs_se.rds"))
saveRDS(seq_data_jobs_se_distance,
    file = paste0(path_paper, "output/data/seq_data_jobs_se_distance.rds"))
saveRDS(seq_data_job_crime,
    file = paste0(path_paper, "output/data/seq_data_job_crime.rds"))
saveRDS(seq_data_job_crime_distance,
    file = paste0(path_paper, "output/data/seq_data_job_crime_distance.rds"))


