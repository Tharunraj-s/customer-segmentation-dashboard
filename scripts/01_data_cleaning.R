# =========================
# Customer Segmentation Dashboard
# Step 1: Data Cleaning
# =========================

# -------------------------
# Load libraries
# -------------------------
library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)

# -------------------------
# Create required folders
# -------------------------
dir.create("data", showWarnings = FALSE)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("scripts", showWarnings = FALSE)
dir.create("app", showWarnings = FALSE)
dir.create("www", showWarnings = FALSE)

# -------------------------
# File paths
# -------------------------
input_file  <- "data/raw/Online Retail.xlsx"
output_file <- "data/processed/clean_online_retail.csv"

# -------------------------
# Check input file exists
# -------------------------
if (!file.exists(input_file)) {
  stop("Raw dataset not found at: data/raw/Online Retail.xlsx")
}

# -------------------------
# Read raw data
# -------------------------
raw_data <- read_excel(input_file)

cat("\nRaw data loaded successfully.\n")
cat("Rows:", nrow(raw_data), "\n")
cat("Columns:", ncol(raw_data), "\n\n")

# -------------------------
# Standardize column names
# -------------------------
clean_names <- names(raw_data) %>%
  str_replace_all("\\s+", "") %>%
  str_trim()

names(raw_data) <- clean_names

# -------------------------
# Validate required columns
# -------------------------
required_cols <- c(
  "InvoiceNo", "StockCode", "Description", "Quantity",
  "InvoiceDate", "UnitPrice", "CustomerID", "Country",
  "Profession", "Age", "FamilySize", "MaritalStatus", "Education"
)

missing_cols <- setdiff(required_cols, names(raw_data))

if (length(missing_cols) > 0) {
  stop(
    paste(
      "Missing required columns:",
      paste(missing_cols, collapse = ", ")
    )
  )
}

# -------------------------
# Clean dataset
# -------------------------
clean_data <- raw_data %>%
  mutate(
    InvoiceNo     = as.character(InvoiceNo),
    StockCode     = as.character(StockCode),
    Description   = str_squish(as.character(Description)),
    InvoiceDate   = as.POSIXct(InvoiceDate, tz = "UTC"),
    UnitPrice     = as.numeric(UnitPrice),
    Quantity      = as.numeric(Quantity),
    CustomerID    = as.character(CustomerID),
    Country       = str_squish(as.character(Country)),
    Profession    = str_squish(as.character(Profession)),
    Age           = as.numeric(Age),
    FamilySize    = as.numeric(FamilySize),
    MaritalStatus = str_squish(as.character(MaritalStatus)),
    Education     = str_squish(as.character(Education))
  ) %>%
  filter(
    !is.na(InvoiceNo),
    !is.na(StockCode),
    !is.na(Description),
    Description != "",
    !is.na(InvoiceDate),
    !is.na(UnitPrice),
    !is.na(Quantity),
    !is.na(CustomerID),
    CustomerID != "",
    !is.na(Country),
    Country != "",
    Quantity > 0,
    UnitPrice > 0
  ) %>%
  filter(!str_detect(InvoiceNo, "^C")) %>%   # remove cancelled invoices
  distinct()

# -------------------------
# Optional: normalize text labels
# -------------------------
clean_data <- clean_data %>%
  mutate(
    Country = str_to_title(Country),
    Profession = ifelse(Profession == "" | is.na(Profession), "Unknown", Profession),
    MaritalStatus = ifelse(MaritalStatus == "" | is.na(MaritalStatus), "Unknown", MaritalStatus),
    Education = ifelse(Education == "" | is.na(Education), "Unknown", Education)
  )

# -------------------------
# Basic summary checks
# -------------------------
cat("Cleaning completed.\n\n")
cat("Rows before cleaning :", nrow(raw_data), "\n")
cat("Rows after cleaning  :", nrow(clean_data), "\n")
cat("Rows removed         :", nrow(raw_data) - nrow(clean_data), "\n\n")

cat("Unique customers     :", n_distinct(clean_data$CustomerID), "\n")
cat("Unique invoices      :", n_distinct(clean_data$InvoiceNo), "\n")
cat("Unique products      :", n_distinct(clean_data$StockCode), "\n")
cat("Unique countries     :", n_distinct(clean_data$Country), "\n\n")

cat("Date range:\n")
cat("From:", as.character(min(clean_data$InvoiceDate, na.rm = TRUE)), "\n")
cat("To  :", as.character(max(clean_data$InvoiceDate, na.rm = TRUE)), "\n\n")

# -------------------------
# Save cleaned dataset
# -------------------------
write_csv(clean_data, output_file)

cat("Cleaned dataset saved to:\n")
cat(output_file, "\n")