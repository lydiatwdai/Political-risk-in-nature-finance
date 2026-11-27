## Reproduces the tables and figures of the cross-country analysis.
## From the repository root:  LC_ALL=en_US.UTF-8 Rscript run_all.R
## Without the ACLED and GEMS variables (see README), only the simple regressions and Figures 3 and 4 are run.

source("R/00_setup.R", encoding = "UTF-8")
scripts <- c("R/01_multiple_regression.R", "R/02_supplementary_tables.R", "R/03_simple_regression.R",
             "R/04_public_finance_figures.R", "R/05_transformation_check.R")
if (!has_restricted) {
  message("ACLED and GEMS variables not found: running the simple regressions and Figures 3 and 4 only.")
  scripts <- scripts[3:4]
}
for (s in scripts) {
  cat("\n>>>", s, "\n")
  source(s, encoding = "UTF-8")
}
writeLines(capture.output(sessionInfo()), "output/session_info.txt")
