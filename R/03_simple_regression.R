## Simple regressions of each finance flow on Political Stability, with residual checks (Supplementary Table 4).
## Run from the repository root.

suppressMessages({ library(readxl); library(ggplot2); library(gridExtra); library(grid) })
invisible(capture.output(suppressMessages(suppressWarnings(source("R/00_setup.R", encoding = "UTF-8")))))

df_all$income <- factor(df_all$wb_income_group, levels = c("Low income", "Lower middle income", "Upper middle income"))
## Ethiopia has no 2024 income class in the World Bank file and is set to Low income in the analysis dataset
stopifnot(!any(is.na(df_all$income)))

simple <- list()
for (fl in names(flows)) simple[[fl]] <- fit_model(df_all, fl, "PV")
## fit_model() keeps rows with complete outcome and PV, transforms the outcome, fits y ~ PV and
## computes Shapiro-Wilk, Breusch-Pagan (Koenker) and RESET exactly as for the multiple regressions.

coef_rows <- do.call(rbind, lapply(names(simple), function(fl) {
  m <- simple[[fl]]; cf <- summary(m$fit)$coefficients; ci <- confint(m$fit)
  data.frame(flow = fl, term = rownames(cf), estimate = cf[, 1], se = cf[, 2], t = cf[, 3], p = cf[, 4],
             ci_lo = ci[, 1], ci_hi = ci[, 2], stringsAsFactors = FALSE, row.names = NULL)
}))
fit_rows <- do.call(rbind, lapply(names(simple), function(fl) {
  m <- simple[[fl]]; s <- summary(m$fit)
  r <- cor(m$d$PV, m$d$y)
  data.frame(flow = fl, outcome = flows[[fl]]$label, n = m$n, r = r, r2 = s$r.squared, adj_r2 = s$adj.r.squared, sigma = s$sigma,
             f_stat = unname(s$fstatistic[1]), f_df1 = unname(s$fstatistic[2]), f_df2 = unname(s$fstatistic[3]),
             shapiro_p = m$sw, bp_p = m$bp, reset_p = m$reset,
             n_zero = sum(m$d[[flows[[fl]]$col]] == 0), stringsAsFactors = FALSE)
}))
write.csv(coef_rows, "output/tables/simple_regression_coefficients.csv", row.names = FALSE)
write.csv(fit_rows, "output/tables/simple_regression_fit.csv", row.names = FALSE)
cat("\n== Simple regressions on Political Stability (130-country file) ==\n")
print(merge(coef_rows[coef_rows$term == "PV", ], fit_rows, by = "flow")[, c("flow", "n", "estimate", "se", "ci_lo", "ci_hi", "p", "r", "r2", "shapiro_p", "bp_p", "reset_p", "n_zero")], digits = 4)
cat("\nIntercepts:\n"); print(coef_rows[coef_rows$term == "(Intercept)", c("flow", "estimate", "se", "p")], digits = 4)

## ---------------------------------------------------------------- residual plots
theme_diag2 <- theme_bw(base_size = 11) + theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold", size = 11),
                                                 plot.subtitle = element_text(size = 9, colour = "grey30"))
dg <- lapply(names(flows), function(fl) {
  m <- simple[[fl]]; fit <- m$fit
  q <- ggplot(data.frame(r = rstandard(fit)), aes(sample = r)) + stat_qq(colour = "#2F5D8A", alpha = 0.8, size = 1.6) + stat_qq_line(colour = "grey25") +
    labs(title = sprintf("%s (N = %d)", fl, m$n), subtitle = "Normal Q-Q plot of standardised residuals", x = "Theoretical quantiles", y = "Standardised residuals") + theme_diag2
  rv <- ggplot(data.frame(fitted = fitted(fit), resid = residuals(fit)), aes(fitted, resid)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") + geom_point(colour = "#2F5D8A", alpha = 0.8, size = 1.6) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "#C4562B", linewidth = 0.8) +
    labs(subtitle = "Residuals vs fitted values", x = "Fitted values", y = "Residuals") + theme_diag2
  list(q = q, rv = rv)
})
gd <- arrangeGrob(grobs = c(lapply(dg, `[[`, "q"), lapply(dg, `[[`, "rv")), ncol = 3,
                  top = textGrob("Simple regressions on Political Stability: residual diagnostics", gp = gpar(fontface = "bold", fontsize = 13)))
ggsave("output/figures/simple_regression_diagnostics.png", gd, width = 12, height = 7.2, dpi = 200, device = ragg::agg_png)
cat("\nSaved the simple regression tables and residual plots.\n")
