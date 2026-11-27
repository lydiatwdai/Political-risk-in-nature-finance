## Provenance only: documents how columns were added to the dataset. Not part of run_all.R.
## Needs the original source files in data_preparation/raw/ (not redistributed in this repository).

suppressMessages({
  library(readxl)
  library(openxlsx)
})

gdp_file <- "data_preparation/raw/API_NY.GDP.PCAP.CD_DS2_en_excel_v2_305580.xls"

master <- read_excel("data_preparation/raw/MASTER_LMIC.xlsx", sheet = "LMIC only")

gdp_raw <- read_excel(gdp_file, sheet = "Data", col_names = FALSE)
hdr <- as.character(gdp_raw[4, ])
gdp <- gdp_raw[5:nrow(gdp_raw), ]
colnames(gdp) <- hdr
gdp_2024 <- data.frame(
  iso3 = gdp[["Country Code"]],
  gdp_per_capita_usd_2024 = as.numeric(gdp[["2024"]]),
  stringsAsFactors = FALSE
)

merged <- merge(master, gdp_2024, by = "iso3", all.x = TRUE)
merged <- merged[match(master$iso3, merged$iso3), ]  # restore original row order

cat("GDP per capita: matched", sum(!is.na(merged$gdp_per_capita_usd_2024)), "of", nrow(merged), "countries\n")
missing <- merged$iso3[is.na(merged$gdp_per_capita_usd_2024)]
if (length(missing) > 0) cat("Missing GDP per capita for:", paste(missing, collapse = ", "), "\n")

wb <- createWorkbook()
addWorksheet(wb, "LMIC only")
writeData(wb, "LMIC only", merged)
saveWorkbook(wb, "data_preparation/raw/MASTER_LMIC.xlsx", overwrite = TRUE)
cat("\nSaved MASTER_LMIC.xlsx with gdp_per_capita_usd_2024 added.\n")
cat("New column count:", ncol(merged), "(was", ncol(master), ")\n")
