#############################################
# other sequence analyses
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
library(msm)

# set seed to reproduce imputation
set.seed(210013)

# relative directory of the paper
path_paper = "reports/paper-work-lifecourse/"
path_manuscript = "reports/paper-work-lifecourse/manuscript/"

source(paste0(path_paper, "src/utils.R"))
source("src/calendario/utils.R")

dat = readRDS(file = paste0(path_paper, "output/data/data_for_sequence_analysis.rds"))


# prison ::::::::::::::
seq_data_prison = create_sequences(
    data = dat,
    seq_variable = "prison",
    seq_labels = c("No Prison", "Prison")
)
seqstatd(seq_data_prison)
seqmeant(seq_data_prison, prop = FALSE, serr = TRUE)

# job and searching ::::::::::
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
table(dat$jobsearching)

prop.table(table(dat[, any(jobsearching == "Searched"), reg_folio]$V1))
dat[, anyjobsearch := getMax(trab_busco_oc), reg_folio]
labs = c("None", "Searched", "Self-employed", "Employed U", "Employed L")

seq_data_search = create_sequences(
    data = dat,
    seq_variable = "jobsearching",
    seq_labels = labs,
    colors = colors5
)

savepdf(paste0(path_paper, "output/plots/seq_dist_job_search"))
    seqdplot(seq_data_search,
        with.legend = FALSE, ltext = labs)
    mtext(side = 1, line = 2.3, "Months", font = 1, cex = 1)
    legend(1.5, 1.08, legend = labs, cex = 0.7, pch = 15, col = colors5,
        horiz = TRUE,  bty = "n", inset = c(0, 2), xpd = TRUE,
        text.width=c(0, 0.7, 0.9, 1.2, 1.3))
dev.off()

# transition rates table
states_in = paste0("> ", c("None", "Searched", "Self-employed",
    "Employed U", "Employed L"))
states_out = paste0(c("None", "Searched","Self-employed","Employed U",
    "Employed L"), " >")

jobsearched.trate = seqtrate(seq_data_search)
rownames(jobsearched.trate) = states_out
colnames(jobsearched.trate) = states_in

caption = paste0("Transition rates between job search and job categories of women inmates \\newline
    during the first 12 months following their release (N = ", n, " $\\times$ 11)")
label = "tab:transition_rates_job_search"
comment = "Probability to switch at a given position from state $s_i$ to state $s_j$. U = Under-the-table, L = Legitimate."

add_notes_table(jobsearched.trate ,
    align = "lccccc",
    tabcolsep = 10,
    caption = caption,
    label = label,
    comment = comment,
    filename = paste0(path_paper, "output/tables/transition_rates_job_search.tex")
)
file.copy(paste0(path_paper, "output/tables/transition_rates_job_search.tex"),
    paste0(path_manuscript, "tables/"), recursive = TRUE)

t = seqmeant(seq_data_search, prop = TRUE, serr = TRUE)
t