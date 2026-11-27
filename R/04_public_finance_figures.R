## Public Finance and Political Stability, all countries and by income group (Figures 3 and 4; Supplementary Table 4).
## Requires R/03_simple_regression.R to have been run first. Run from the repository root.

suppressMessages({ library(readxl); library(ggplot2); library(scales); library(ggrepel) })
invisible(capture.output(suppressMessages(suppressWarnings(source("R/00_setup.R", encoding = "UTF-8")))))

col <- flows$Public$col
df_all$income <- factor(df_all$wb_income_group, levels = c("Low income", "Lower middle income", "Upper middle income"))
sub <- df_all[!is.na(df_all$PV) & !is.na(df_all[[col]]) & !is.na(df_all$income), ]
sub$y_raw <- sub[[col]]
sub$y <- ihs(sub$y_raw)
stopifnot(nrow(sub) == 122)

## ---------------------------------------------------------------- statistics
hc3 <- function(m, term = "PV") {                       # heteroscedasticity-robust (HC3) standard error and p-value
  X <- model.matrix(m); e <- residuals(m); h <- hatvalues(m); n <- nrow(X); k <- ncol(X)
  XtXi <- solve(crossprod(X)); Xe <- X * (e / (1 - h))
  V <- XtXi %*% crossprod(Xe) %*% XtXi
  se <- unname(sqrt(V[term, term])); c(se = se, p = unname(2 * pt(-abs(coef(m)[term] / se), df = n - k)))
}
stat_row <- function(d, label) {
  m <- lm(y ~ PV, data = d); s <- summary(m)$coefficients; ct <- cor.test(d$PV, d$y)
  res <- residuals(m); n <- nrow(d)
  bp <- pchisq(n * summary(lm(res^2 ~ d$PV))$r.squared, df = 1, lower.tail = FALSE)
  reset <- summary(lm(y ~ PV + I(fitted(m)^2), data = d))$coefficients[3, 4]
  rb <- hc3(m)
  data.frame(group = label, n = n, r = unname(ct$estimate), r_p = ct$p.value, r2 = summary(m)$r.squared,
             beta = s["PV", 1], se = s["PV", 2], p = s["PV", 4], se_hc3 = unname(rb["se"]), p_hc3 = unname(rb["p"]),
             shapiro_p = shapiro.test(res)$p.value, bp_p = bp, reset_p = reset, stringsAsFactors = FALSE)
}
stats <- rbind(stat_row(sub, "All countries"),
               do.call(rbind, lapply(levels(sub$income), function(g) stat_row(sub[sub$income == g, ], g))))
write.csv(stats, "output/tables/simple_regression_public_by_income.csv", row.names = FALSE)
print(stats, digits = 4)

a <- anova(lm(y ~ PV + income, sub), lm(y ~ PV * income, sub))
inter <- data.frame(f = a$F[2], df1 = a$Df[2], df2 = a$Res.Df[2], p = a$`Pr(>F)`[2])
write.csv(inter, "output/tables/simple_regression_public_interaction.csv", row.names = FALSE)
cat("\nSlope-by-income interaction:\n"); print(inter, digits = 4)

## consistency with R/03_simple_regression.R
chk <- read.csv("output/tables/simple_regression_fit.csv"); chk <- chk[chk$flow == "Public", ]
stopifnot(abs(chk$r2 - stats$r2[1]) < 1e-9, chk$n == stats$n[1])

## ---------------------------------------------------------------- shared plot settings
inv_ihs <- function(x) sinh(x)
ihs_trans <- trans_new("ihs", transform = ihs, inverse = inv_ihs)
income_colors <- c("Low income" = "#C0392B", "Lower middle income" = "#E67E22", "Upper middle income" = "#2E86C1")
y_breaks <- c(0, 1, 10, 100)
fmt_p <- function(p) if (p < 0.001) "p < 0.001" else sprintf("p = %.3f", p)
stat_text <- function(row, sep = "\n") {
  gsub("-", "−", paste(sprintf("r = %.2f", row$r), sprintf("R² = %.3f", row$r2), fmt_p(row$p), sprintf("N = %d", row$n), sep = sep))
}

## ---------------------------------------------------------------- Figure 1: all countries
## labelled countries: extremes plus a spread across the range
label_names <- c("Yemen", "Tuvalu", "India", "Djibouti", "Somalia", "Colombia", "Angola", "Ghana", "Liberia", "Tonga", "Ethiopia")
missing_lab <- setdiff(label_names, sub$country_name)
if (length(missing_lab)) stop("labels not found: ", paste(missing_lab, collapse = ", "))
lab <- sub[sub$country_name %in% label_names, ]

p1 <- ggplot(sub, aes(PV, y_raw)) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "black", fill = "grey80", linewidth = 1) +
  geom_point(aes(fill = income), shape = 21, size = 3, colour = "black", stroke = 0.3, alpha = 0.9) +
  geom_text_repel(data = lab, aes(label = country_name), size = 3.2, max.overlaps = 30, segment.size = 0.3,
                  colour = "grey20", min.segment.length = 0.2, seed = 7) +
  scale_fill_manual(values = income_colors, name = NULL) +
  scale_x_continuous(breaks = seq(20, 90, 10)) +
  scale_y_continuous(trans = ihs_trans, breaks = y_breaks, labels = label_number(accuracy = 1)) +
  annotate("label", x = -Inf, y = Inf, label = stat_text(stats[1, ]), hjust = -0.08, vjust = 1.08, size = 3.8,
           fill = "white", linewidth = 0.25, label.padding = unit(0.2, "lines")) +
  labs(x = "WGI Political Stability Score (0–100)", y = "Public Finance (USD million, IHS scale)") +
  theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "inside", legend.position.inside = c(0.985, 0.985),
        legend.justification = c(1, 1), legend.background = element_rect(colour = "grey50", linewidth = 0.3))
ggsave("output/figures/figure3_public_finance_political_stability.png", p1, width = 9, height = 6.2, dpi = 600, device = ragg::agg_png)

## ---------------------------------------------------------------- Figure 2: by income group
facet_lab <- setNames(vapply(seq_along(levels(sub$income)), function(i) {
  g <- levels(sub$income)[i]; paste0(g, "\n", stat_text(stats[stats$group == g, ], sep = ", "))
}, ""), levels(sub$income))
sub$facet <- factor(facet_lab[as.character(sub$income)], levels = unname(facet_lab))

p2 <- ggplot(sub, aes(PV, y_raw)) +
  geom_smooth(aes(colour = income, fill = income), method = "lm", formula = y ~ x, linewidth = 1, alpha = 0.2) +
  geom_point(aes(fill = income), shape = 21, size = 2.4, colour = "black", stroke = 0.3, alpha = 0.9) +
  scale_fill_manual(values = income_colors, guide = "none") +
  scale_colour_manual(values = income_colors, guide = "none") +
  scale_y_continuous(trans = ihs_trans, breaks = y_breaks, labels = label_number(accuracy = 1)) +
  facet_wrap(~ facet, scales = "free_x") +
  labs(x = "WGI Political Stability Score (0–100)", y = "Public Finance (USD million, IHS scale)") +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold", size = 10.5),
        strip.background = element_rect(fill = "grey90"))
ggsave("output/figures/figure4_public_finance_by_income_group.png", p2, width = 10, height = 4.6, dpi = 600, device = ragg::agg_png)
cat("\nSaved Figures 3 and 4 and two tables.\n")
