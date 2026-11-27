## Provenance only: documents how columns were added to the dataset. Not part of run_all.R.
## Needs the original source files in data_preparation/raw/ (not redistributed in this repository).

suppressMessages({
  library(readxl)
  library(openxlsx)
})

eth_dir <- "data_preparation/raw"

master <- read_excel("data_preparation/raw/MASTER_LMIC.xlsx", sheet = "LMIC only")

## ---- Population (UN WPP2024, GEN/F01 compact) ----
pop_raw <- read_excel(file.path(eth_dir, "world popoulation.xlsx"), sheet = "Estimates", col_names = FALSE)
hdr <- as.character(pop_raw[13, ])
hdr[is.na(hdr)] <- paste0("col", which(is.na(hdr)))
pop <- pop_raw[14:nrow(pop_raw), ]
colnames(pop) <- hdr
pop$Year <- as.numeric(pop$Year)
pop_2023 <- pop[pop$Type == "Country/Area" & pop$Year == 2023, ]
pop_2023 <- data.frame(
  iso3 = pop_2023[["ISO3 Alpha-code"]],
  population_2023 = as.numeric(pop_2023[["Total Population, as of 1 July (thousands)"]]) * 1000,
  stringsAsFactors = FALSE
)

## ---- Land area (World Bank WDI, AG.LND.TOTL.K2) ----
land_raw <- read_excel(file.path(eth_dir, "land area.xls"), sheet = "Data", col_names = FALSE)
hdr2 <- as.character(land_raw[4, ])
land <- land_raw[5:nrow(land_raw), ]
colnames(land) <- hdr2
# most recent year with data is 2023 (2024 column is empty in this WDI extract)
land_2023 <- data.frame(
  iso3 = land[["Country Code"]],
  land_area_sqkm_2023 = as.numeric(land[["2023"]]),
  stringsAsFactors = FALSE
)

## ---- Merge into master ----
merged <- merge(master, pop_2023, by = "iso3", all.x = TRUE)
merged <- merge(merged, land_2023, by = "iso3", all.x = TRUE)
merged <- merged[match(master$iso3, merged$iso3), ]  # restore original row order

cat("Population: matched", sum(!is.na(merged$population_2023)), "of", nrow(merged), "countries\n")
cat("Land area: matched", sum(!is.na(merged$land_area_sqkm_2023)), "of", nrow(merged), "countries\n")

missing_pop <- merged$iso3[is.na(merged$population_2023)]
missing_land <- merged$iso3[is.na(merged$land_area_sqkm_2023)]
if (length(missing_pop) > 0) cat("Missing population for:", paste(missing_pop, collapse = ", "), "\n")
if (length(missing_land) > 0) cat("Missing land area for:", paste(missing_land, collapse = ", "), "\n")

## ---- also add population density as a derived control (common in gravity-style models) ----
merged$population_density_2023 <- merged$population_2023 / merged$land_area_sqkm_2023

## ---- save back to MASTER_LMIC.xlsx ----
wb <- createWorkbook()
addWorksheet(wb, "LMIC only")
writeData(wb, "LMIC only", merged)
saveWorkbook(wb, "data_preparation/raw/MASTER_LMIC.xlsx", overwrite = TRUE)
cat("\nSaved MASTER_LMIC.xlsx with population_2023, land_area_sqkm_2023, population_density_2023 added.\n")
cat("New column count:", ncol(merged), "(was", ncol(master), ")\n")
