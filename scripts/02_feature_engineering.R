# -------------------------
# Load libraries
# -------------------------
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

# -------------------------
# File paths
# -------------------------
input_file_feature  <- "data/processed/clean_online_retail.csv"
output_file_feature <- "data/processed/feature_engineered_online_retail.csv"
output_file_invoice <- "data/processed/invoice_summary.csv"

# -------------------------
# Check input file exists
# -------------------------
if (!file.exists(input_file_feature)) {
  stop("Cleaned dataset not found. Run scripts/01_data_cleaning.R first.")
}

# -------------------------
# Read cleaned data
# -------------------------
clean_data <- read_csv(input_file_feature, show_col_types = FALSE)

cat("\nCleaned data loaded successfully.\n")
cat("Rows:", nrow(clean_data), "\n")
cat("Columns:", ncol(clean_data), "\n\n")

# -------------------------
# Ensure correct data types
# -------------------------
feature_data <- clean_data %>%
  mutate(
    InvoiceDate = as.POSIXct(InvoiceDate, tz = "UTC"),
    Quantity = as.numeric(Quantity),
    UnitPrice = as.numeric(UnitPrice),
    Age = as.numeric(Age),
    FamilySize = as.numeric(FamilySize)
  )

# -------------------------
# Create transaction-level features
# -------------------------
feature_data <- feature_data %>%
  mutate(
    TotalPrice = Quantity * UnitPrice,
    InvoiceYear = year(InvoiceDate),
    InvoiceQuarter = quarter(InvoiceDate),
    InvoiceMonth = month(InvoiceDate, label = TRUE, abbr = FALSE),
    InvoiceMonthNum = month(InvoiceDate),
    InvoiceWeek = isoweek(InvoiceDate),
    InvoiceDay = day(InvoiceDate),
    InvoiceWeekday = wday(InvoiceDate, label = TRUE, abbr = FALSE),
    InvoiceHour = hour(InvoiceDate),
    YearMonth = format(InvoiceDate, "%Y-%m"),
    YearQuarter = paste0(year(InvoiceDate), "-Q", quarter(InvoiceDate)),
    WeekendFlag = ifelse(
      wday(InvoiceDate, week_start = 1) >= 6,
      "Weekend",
      "Weekday"
    ),
    TimeOfDay = case_when(
      InvoiceHour >= 5  & InvoiceHour < 12 ~ "Morning",
      InvoiceHour >= 12 & InvoiceHour < 17 ~ "Afternoon",
      InvoiceHour >= 17 & InvoiceHour < 21 ~ "Evening",
      TRUE ~ "Night"
    ),
    BasketType = case_when(
      Quantity == 1 ~ "Single Item",
      Quantity >= 2 & Quantity <= 5 ~ "Small Basket",
      Quantity >= 6 & Quantity <= 20 ~ "Medium Basket",
      TRUE ~ "Large Basket"
    ),
    PriceBand = case_when(
      UnitPrice < 1 ~ "Low",
      UnitPrice >= 1 & UnitPrice < 5 ~ "Mid",
      UnitPrice >= 5 & UnitPrice < 20 ~ "High",
      TRUE ~ "Premium"
    )
  )

# -------------------------
# Build invoice-level summary
# -------------------------
invoice_summary <- feature_data %>%
  group_by(InvoiceNo, CustomerID, Country, InvoiceDate, YearMonth, YearQuarter) %>%
  summarise(
    NumLineItems = n(),
    NumItems = sum(Quantity, na.rm = TRUE),
    NumUniqueProducts = n_distinct(StockCode),
    InvoiceValue = sum(TotalPrice, na.rm = TRUE),
    AvgItemPrice = mean(UnitPrice, na.rm = TRUE),
    AvgCustomerAge = mean(Age, na.rm = TRUE),
    FamilySize = first(FamilySize),
    Profession = first(Profession),
    MaritalStatus = first(MaritalStatus),
    Education = first(Education),
    DominantTimeOfDay = names(sort(table(TimeOfDay), decreasing = TRUE))[1],
    .groups = "drop"
  ) %>%
  mutate(
    BasketValueBand = case_when(
      InvoiceValue < 100 ~ "Low Value",
      InvoiceValue >= 100 & InvoiceValue < 300 ~ "Medium Value",
      InvoiceValue >= 300 & InvoiceValue < 1000 ~ "High Value",
      TRUE ~ "Very High Value"
    )
  )

# -------------------------
# Quality checks
# -------------------------
cat("Feature engineering completed.\n\n")

cat("Feature data rows    :", nrow(feature_data), "\n")
cat("Invoice summary rows :", nrow(invoice_summary), "\n\n")

cat("New transaction-level columns added:\n")
print(c(
  "TotalPrice", "InvoiceYear", "InvoiceQuarter", "InvoiceMonth",
  "InvoiceMonthNum", "InvoiceWeek", "InvoiceDay", "InvoiceWeekday",
  "InvoiceHour", "YearMonth", "YearQuarter", "WeekendFlag",
  "TimeOfDay", "BasketType", "PriceBand"
))

cat("\nInvoice summary columns added:\n")
print(c(
  "NumLineItems", "NumItems", "NumUniqueProducts", "InvoiceValue",
  "AvgItemPrice", "AvgCustomerAge", "DominantTimeOfDay", "BasketValueBand"
))

# -------------------------
# Save outputs
# -------------------------
write_csv(feature_data, output_file_feature)
write_csv(invoice_summary, output_file_invoice)

cat("\nFeature-engineered dataset saved to:\n")
cat(output_file_feature, "\n")

cat("\nInvoice summary dataset saved to:\n")
cat(output_file_invoice, "\n")