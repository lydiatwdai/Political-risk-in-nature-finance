## Outcome transformation check: Shapiro-Wilk and Breusch-Pagan p-values of the Model 1 residuals
## under the inverse hyperbolic sine and the logarithmic transformation of each outcome.
## Needs the ACLED and GEMS variables (see README). Run from the repository root.

suppressMessages(library(readxl))
ihs <- function(x) log(x + sqrt(x^2 + 1))
source("R/00_setup.R", encoding = "UTF-8")
require_restricted()
d <- df_all
d$PV <- d$wgi_political_stability;           d$ACLED <- ihs(d$acled_danger_value_2025)
d$SovDef <- d$gems_sovereign_default_rate;   d$FX <- ihs(d$fx_volatility_stdpct_2015_2024)
d$RQ <- d$wgi_regulatory_quality;            d$CC <- d$wgi_control_corruption
d$NCS <- ihs(d$ncs_potential_tco2e)
m1 <- c("PV", "ACLED", "SovDef", "FX", "RQ", "CC", "NCS")
cols <- c(Public = "crdf_nbs_adaptation_musd_2024", Philanthropy = "philanthropy_musd_avg2020_2023", VCM = "vrod_retired_credits_2024_forestry")
out <- list()
for (fl in names(cols)) {
  x <- d[[cols[[fl]]]]
  for (tfn in c("IHS", "log")) {
    tf <- if (tfn == "IHS") ihs else log
    defined <- !(tfn == "log" && any(x <= 0, na.rm = TRUE))
    if (!defined) { out[[length(out) + 1]] <- data.frame(flow = fl, transformation = tfn, defined = FALSE, n = NA, shapiro_p = NA, bp_p = NA); next }
    dd <- d[complete.cases(d[, c(cols[[fl]], m1)]), ]; dd$y <- tf(dd[[cols[[fl]]]])
    fit <- lm(reformulate(m1, "y"), data = dd); res <- residuals(fit)
    bp <- pchisq(nrow(dd) * summary(lm(res^2 ~ ., data = dd[, m1]))$r.squared, df = length(m1), lower.tail = FALSE)
    out[[length(out) + 1]] <- data.frame(flow = fl, transformation = tfn, defined = TRUE, n = nrow(dd), shapiro_p = shapiro.test(res)$p.value, bp_p = bp)
  }
}
out <- do.call(rbind, out)
write.csv(out, "output/tables/outcome_transformation_check.csv", row.names = FALSE)
print(out)
