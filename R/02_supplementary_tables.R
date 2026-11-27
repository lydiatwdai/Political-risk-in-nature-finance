## Supplementary results for the multiple regressions (Supplementary Tables 6, 7, 8; Supplementary Figure 3):
##   overall F-tests, predictor correlations, candidate variables not included, VCM residuals by retirement status.
## Needs the ACLED and GEMS variables (see README). Run from the repository root.

suppressMessages({ library(readxl); library(ggplot2) })
invisible(capture.output(suppressMessages(suppressWarnings(source("R/01_multiple_regression.R", encoding = "UTF-8")))))

## 1. overall F-test p-values
fit_ext <- read.csv("output/tables/mlr_fit.csv")
fit_ext$f_p <- pf(fit_ext$f_stat, fit_ext$f_df1, fit_ext$f_df2, lower.tail = FALSE)
write.csv(fit_ext, "output/tables/mlr_fit_with_f_test.csv", row.names = FALSE)

## 2. predictor correlations (130 LMICs, pairwise complete observations)
cm <- cor(df_all[, m2_vars], use = "pairwise.complete.obs")
rownames(cm) <- colnames(cm) <- m2_vars
write.csv(round(cm, 4), "output/tables/predictor_correlations.csv")
cat("Pairs with |r| >= 0.70 among predictors:\n")
idx <- which(upper.tri(cm) & abs(cm) >= 0.70, arr.ind = TRUE)
print(data.frame(a = rownames(cm)[idx[, 1]], b = colnames(cm)[idx[, 2]], r = round(cm[idx], 2)))

## 3. candidate variables not included
d <- df_all
cand <- data.frame(
  candidate = c("WGI Voice and Accountability", "WGI Government Effectiveness", "WGI Rule of Law",
                "ACLED Index Ranking", "ACLED Deadliness Value", "ACLED Fragmentation Value", "ACLED Diffusion Value",
                "GEMS Private Lending Default Rate", "GEMS Public Lending Default Rate"),
  stringsAsFactors = FALSE)
vals <- list(d$wgi_voice_accountability, d$wgi_govt_effectiveness, d$wgi_rule_of_law,
             d$acled_index_ranking_2025, ihs(d$acled_deadliness_value_2025), ihs(d$acled_fragmentation_value_2025), ihs(d$acled_diffusion_value_2025),
             d$gems_private_avg_default_rate, d$gems_public_avg_default_rate)
inc <- d[, m2_vars]
res <- lapply(vals, function(v) {
  r <- sapply(inc, function(x) suppressWarnings(cor(v, x, use = "pairwise.complete.obs")))
  j <- which.max(abs(r)); data.frame(n = sum(!is.na(v)), partner = names(r)[j], r = unname(r[j]))
})
cand <- cbind(cand, do.call(rbind, res))
cand$partner_label <- unname(display[cand$partner])
write.csv(cand, "output/tables/candidate_variables.csv", row.names = FALSE)
print(cand)

## 4. VCM residuals by retirement status (Model 1 and Model 2)
get_vcm <- function(m, lab) {
  z <- m$d[[flows$VCM$col]] == 0
  data.frame(model = lab, fitted = fitted(m$fit), resid = residuals(m$fit), zero = z)
}
labs <- c("Model 1 (no controls)", "Model 2 (with controls)")
vcm <- rbind(get_vcm(models[["VCM M1"]], labs[1]), get_vcm(models[["VCM M2"]], labs[2]))
vcm$model <- factor(vcm$model, levels = labs)
n0 <- sum(vcm$zero[vcm$model == labs[1]]); n1 <- sum(!vcm$zero[vcm$model == labs[1]])
vcm$grp <- factor(ifelse(vcm$zero, sprintf("No credits retired (n = %d)", n0), sprintf("Credits retired (n = %d)", n1)),
                  levels = c(sprintf("No credits retired (n = %d)", n0), sprintf("Credits retired (n = %d)", n1)))
cat("\nVCM zero check: max |resid + fitted| over zero countries =", max(abs(vcm$resid + vcm$fitted)[vcm$zero]), " | zeros per model:",
    paste(tapply(vcm$zero, vcm$model, sum), collapse = ", "), "\n")
p <- ggplot(vcm, aes(fitted, resid, shape = grp, colour = grp)) +
  geom_abline(slope = -1, intercept = 0, linetype = "dashed", colour = "grey45") +
  geom_point(size = 2.4, alpha = 0.85) +
  scale_shape_manual(values = c(17, 16)) + scale_colour_manual(values = c("#C4562B", "#2F5D8A")) +
  facet_wrap(~model) +
  labs(x = "Fitted values", y = "Residuals", shape = NULL, colour = NULL) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom", strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey93"))
ggsave("output/figures/supp_figure3_vcm_zeros.png", p, width = 10, height = 4.6, dpi = 200)
cat("Saved the supplementary tables and Supplementary Figure 3.\n")
