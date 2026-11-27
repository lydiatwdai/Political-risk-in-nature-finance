## Multiple linear regressions of three finance flows on country risk (Supplementary Tables 5, 6, 9, 10; Supplementary Figures 1, 2).
## Two specifications per flow, with identical predictors across flows:
##   Model 1: six risk indicators and NCS mitigation potential
##   Model 2: Model 1 plus GDP per capita, land area and population
## Needs the ACLED and GEMS variables (see README). Run from the repository root.

suppressMessages({ library(ggplot2); library(gridExtra); library(grid) })
source("R/00_setup.R", encoding = "UTF-8")
require_restricted()

display <- c(PV = "Political Stability (WGI)", ACLED = "IHS(ACLED Conflict Danger)",
             SovDef = "Sovereign Default Rate (GEMS)", FX = "IHS(FX Volatility)",
             RQ = "Regulatory Quality (WGI)", CC = "Control of Corruption (WGI)",
             NCS = "IHS(NCS Mitigation Potential)", GDP = "log(GDP per Capita)",
             Land = "IHS(Land Area)", Pop = "IHS(Population)")
m1_vars <- c("PV", "ACLED", "SovDef", "FX", "RQ", "CC", "NCS")
m2_vars <- c(m1_vars, "GDP", "Land", "Pop")

run_all <- function(df) {
  out <- list()
  for (fl in names(flows)) {
    out[[paste(fl, "M1")]] <- fit_model(df, fl, m1_vars)
    out[[paste(fl, "M2")]] <- fit_model(df, fl, m2_vars)
  }
  out
}

models <- run_all(df_all)
## ---------- console summary ----------
for (nm in names(models)) {
  m <- models[[nm]]; s <- summary(m$fit)
  cat(sprintf("\n== %s: N=%d  R2=%.3f  adjR2=%.3f  | Shapiro p=%.3f  BP p=%.3f  RESET p=%.3f  | max VIF=%.1f (%s)\n",
              nm, m$n, s$r.squared, s$adj.r.squared, m$sw, m$bp, m$reset, max(m$vif), names(m$vif)[which.max(m$vif)]))
  print(round(s$coefficients[, c(1, 2, 4)], 4))
}

## ---------- machine-readable output ----------
coef_rows <- do.call(rbind, lapply(names(models), function(nm) {
  m <- models[[nm]]; cf <- summary(m$fit)$coefficients
  data.frame(flow = m$flow, model = sub(".* ", "", nm), term = rownames(cf),
             label = c("Constant", unname(display[m$xs])), estimate = cf[, 1], se = cf[, 2], t = cf[, 3], p = cf[, 4],
             vif = c(NA, unname(m$vif[m$xs])), stringsAsFactors = FALSE, row.names = NULL)
}))
fit_rows <- do.call(rbind, lapply(names(models), function(nm) {
  m <- models[[nm]]; s <- summary(m$fit)
  data.frame(flow = m$flow, model = sub(".* ", "", nm), outcome = flows[[m$flow]]$label, n = m$n, r2 = s$r.squared, adj_r2 = s$adj.r.squared,
             sigma = s$sigma, f_stat = unname(s$fstatistic[1]), f_df1 = unname(s$fstatistic[2]), f_df2 = unname(s$fstatistic[3]),
             shapiro_p = m$sw, bp_p = m$bp, reset_p = m$reset, max_vif = max(m$vif), max_vif_term = names(m$vif)[which.max(m$vif)],
             min_vif = min(m$vif), stringsAsFactors = FALSE)
}))
write.csv(coef_rows, "output/tables/mlr_coefficients.csv", row.names = FALSE)
write.csv(fit_rows, "output/tables/mlr_fit.csv", row.names = FALSE)

## ---------- diagnostic plots ----------
theme_diag <- theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", size = 11),
        plot.subtitle = element_text(size = 9, colour = "grey30"))
diag_plots <- function(m) {
  fit <- m$fit
  q <- ggplot(data.frame(r = rstandard(fit)), aes(sample = r)) +
    stat_qq(colour = "#2F5D8A", alpha = 0.8, size = 1.6) + stat_qq_line(colour = "grey25") +
    labs(title = sprintf("%s (N = %d)", m$flow, m$n), subtitle = "Normal Q-Q plot of standardised residuals",
         x = "Theoretical quantiles", y = "Standardised residuals") + theme_diag
  rv <- ggplot(data.frame(fitted = fitted(fit), resid = residuals(fit)), aes(fitted, resid)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_point(colour = "#2F5D8A", alpha = 0.8, size = 1.6) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#C4562B", linewidth = 0.8) +
    labs(subtitle = "Residuals vs fitted values", x = "Fitted values", y = "Residuals") + theme_diag
  list(q = q, rv = rv)
}
make_fig <- function(suffix, title, file) {
  pl <- lapply(names(flows), function(fl) diag_plots(models[[paste(fl, suffix)]]))
  g <- arrangeGrob(grobs = c(lapply(pl, `[[`, "q"), lapply(pl, `[[`, "rv")), ncol = 3,
                   top = textGrob(title, gp = gpar(fontface = "bold", fontsize = 13)))
  ggsave(file, g, width = 12, height = 7.2, dpi = 200)
}
make_fig("M1", "Model 1 (no controls): residual diagnostics", "output/figures/supp_figure1_diagnostics_model1.png")
make_fig("M2", "Model 2 (with controls): residual diagnostics", "output/figures/supp_figure2_diagnostics_model2.png")
cat("\nSaved mlr_coefficients.csv, mlr_fit.csv and Supplementary Figures 1 and 2.\n")
