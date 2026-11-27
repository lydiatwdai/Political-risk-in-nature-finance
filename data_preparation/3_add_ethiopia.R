## Adds Ethiopia to the 129-country dataset. The World Bank gives no 2024 income class for Ethiopia
## (low income in all other years). Venezuela has no classification for 2020-2024 and stays excluded.
## Provenance only: documents how columns were added to the dataset. Not part of run_all.R.
## Needs the original source files in data_preparation/raw/ (not redistributed in this repository).

suppressMessages({ library(readxl); library(openxlsx) })
eth_dir <- "data_preparation/raw"

A <- read_excel("data_preparation/raw/MASTER_LMIC.xlsx", sheet = "LMIC only")
C_all <- read_excel(file.path(eth_dir, "dataset cleaned", "master_database_NbS.xlsx"), sheet = "All Countries")
e <- C_all[C_all$iso3 == "ETH", intersect(names(A), names(C_all))]
stopifnot(nrow(e) == 1)
e$wb_income_group <- "Low income"

pop_raw <- suppressMessages(read_excel(file.path(eth_dir, "dataset", "world popoulation.xlsx"), sheet = "Estimates", col_names = FALSE))
hdr <- as.character(pop_raw[13, ]); hdr[is.na(hdr)] <- paste0("col", which(is.na(hdr)))
pop <- pop_raw[14:nrow(pop_raw), ]; colnames(pop) <- hdr; pop$Year <- as.numeric(pop$Year)
p <- pop[pop$Type == "Country/Area" & pop$Year == 2023 & pop[["ISO3 Alpha-code"]] == "ETH", ]
land_raw <- suppressMessages(read_excel(file.path(eth_dir, "dataset", "land area.xls"), sheet = "Data", col_names = FALSE))
h2 <- as.character(land_raw[4, ]); land <- land_raw[5:nrow(land_raw), ]; colnames(land) <- h2
l <- land[land[["Country Code"]] == "ETH", ]
g_raw <- suppressMessages(read_excel(file.path(eth_dir, "dataset cleaned", "GDPperCap.xls"), sheet = "Data", col_names = FALSE))
h3 <- as.character(g_raw[4, ]); gg <- g_raw[5:nrow(g_raw), ]; colnames(gg) <- h3
gd <- gg[gg[["Country Code"]] == "ETH", ]

e$population_2023 <- as.numeric(p[["Total Population, as of 1 July (thousands)"]]) * 1000
e$land_area_sqkm_2023 <- as.numeric(l[["2023"]])
e$population_density_2023 <- e$population_2023 / e$land_area_sqkm_2023
e$gdp_per_capita_usd_2024 <- as.numeric(gd[["2024"]])
e <- e[, names(A)]
B <- rbind(as.data.frame(A), as.data.frame(e))
cat("rows:", nrow(B), "| duplicated iso3:", sum(duplicated(B$iso3)), "\n")
cat("Ethiopia row:\n"); print(unlist(B[B$iso3 == "ETH", c("iso3","wb_income_group","crdf_nbs_adaptation_musd_2024","philanthropy_musd_avg2020_2023","vrod_retired_credits_2024_forestry","wgi_political_stability","acled_danger_value_2025","population_2023","land_area_sqkm_2023","gdp_per_capita_usd_2024")]))

B <- B[, !grepl("^(nd_gain|sp_|acled_|gems_)", names(B))]   # unused or not redistributable
wb <- createWorkbook(); addWorksheet(wb, "LMIC only"); writeData(wb, "LMIC only", B)
saveWorkbook(wb, "data/MASTER_LMIC_incl_Ethiopia.xlsx", overwrite = TRUE)
cat("\nSaved MASTER_LMIC_incl_Ethiopia.xlsx\n")
