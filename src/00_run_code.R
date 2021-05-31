#############################################
# source file
# reentry work paper
# author: sebastian daza
#############################################

# relative directory of the paper
path_paper = "reports/paper-work-lifecourse/"

source(paste0(path_paper, "src/01_define_baseline_covariates.R"))
source(paste0(path_paper, "src/02_prepare_sequence_data.R"))
source(paste0(path_paper, "src/03_sequence_analysis.R"))
source(paste0(path_paper, "src/04_discrepancy_models.R"))
