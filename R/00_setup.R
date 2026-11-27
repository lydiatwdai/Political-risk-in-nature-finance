## Shared setup: loads the data, prepares the variables and defines the helper functions.
## ACLED and GEMS variables are not redistributed here (see README). If you place your own copy in
## data/restricted/restricted_variables.csv (or point RESTRICTED_FILE to it), they are added to the data.

suppressMessages(library(readxl))
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

ihs <- function(x) log(x + sqrt(x^2 + 1))

df_all <- read_excel("data/MASTER_LMIC_incl_Ethiopia.xlsx", sheet = "LMIC only")

restricted_file <- Sys.getenv("RESTRICTED_FILE", "data/restricted/restricted_variables.csv")
has_restricted <- file.exists(restricted_file)
if (has_restricted) {
  extra <- read.csv(restricted_file, stringsAsFactors = FALSE)
  stopifnot(!anyDuplicated(extra$iso3), all(df_all$iso3 %in% extra$iso3))
  for (v in setdiff(names(extra), "iso3")) df_all[[v]] <- extra[[v]][match(df_all$iso3, extra$iso3)]
}
require_restricted <- function() {
  if (!has_restricted) stop("This script needs the ACLED and GEMS variables, which are not redistributed. See README.md.")
}

prep <- function(d) {
  d$PV     <- d$wgi_political_stability
  d$FX     <- ihs(d$fx_volatility_stdpct_2015_2024)
  d$RQ     <- d$wgi_regulatory_quality
  d$CC     <- d$wgi_control_corruption
  d$NCS    <- ihs(d$ncs_potential_tco2e)
  d$GDP    <- log(d$gdp_per_capita_usd_2024)
  d$Land   <- ihs(d$land_area_sqkm_2023)
  d$Pop    <- ihs(d$population_2023)
  if (has_restricted) {
    d$ACLED  <- ihs(d$acled_danger_value_2025)
    d$SovDef <- d$gems_sovereign_default_rate
  }
  d
}
df_all <- prep(df_all)

flows <- list(
  Public       = list(col = "crdf_nbs_adaptation_musd_2024",      tf = ihs, label = "IHS(Public Finance)"),
  Philanthropy = list(col = "philanthropy_musd_avg2020_2023",     tf = log, label = "log(Philanthropy Finance)"),
  VCM          = list(col = "vrod_retired_credits_2024_forestry", tf = ihs, label = "IHS(VCM Finance)")
)

vif <- function(d, xs) sapply(xs, function(x)
  1 / (1 - summary(lm(reformulate(setdiff(xs, x), x), data = d))$r.squared))

fit_model <- function(df, flow, xs, tf = NULL) {
  f <- flows[[flow]]; tf <- if (is.null(tf)) f$tf else tf
  d <- df[complete.cases(df[, c(f$col, xs)]), ]
  d$y <- tf(d[[f$col]])
  fit <- lm(reformulate(xs, "y"), data = d)
  res <- residuals(fit); n <- nrow(d)
  bp  <- pchisq(n * summary(lm(res^2 ~ ., data = d[, xs]))$r.squared, df = length(xs), lower.tail = FALSE)
  f2  <- fitted(fit)^2
  reset <- summary(lm(y ~ ., data = cbind(d[, xs], y = d$y, f2 = f2)))$coefficients["f2", 4]
  list(fit = fit, d = d, xs = xs, flow = flow, n = n, vif = vif(d, xs),
       sw = shapiro.test(res)$p.value, bp = bp, reset = reset)
}
