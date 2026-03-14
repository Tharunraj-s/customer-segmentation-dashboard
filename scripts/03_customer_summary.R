# -------------------------
# Load libraries
# -------------------------
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

# -------------------------
# File paths
# -------------------------
input_file  <- "data/processed/feature_engineered_online_retail.csv"
output_file <- "data/processed/customer_summary.csv"

# -------------------------
# Check input file exists
# -------------------------
if (!file.exists(input_file)) {
  stop("Feature-engineered dataset not found. Run scripts/02_feature_engineering.R first.")
}

# -------------------------
# Read data
# -------------------------
feature_data <- read_csv(input_file, show_col_types = FALSE)

cat("\nFeature-engineered data loaded successfully.\n")
cat("Rows:", nrow(feature_data), "\n")
cat("Columns:", ncol(feature_data), "\n\n")

# -------------------------
# Ensure correct data types
# -------------------------
feature_data <- feature_data %>%
  mutate(
    InvoiceDate = as.POSIXct(InvoiceDate, tz = "UTC"),
    Quantity = as.numeric(Quantity),
    UnitPrice = as.numeric(UnitPrice),
    TotalPrice = as.numeric(TotalPrice),
    Age = as.numeric(Age),
    FamilySize = as.numeric(FamilySize)
  )

# -------------------------
# Reference date for recency
# -------------------------
reference_date <- max(feature_data$InvoiceDate, na.rm = TRUE) + days(1)

# -------------------------
# Customer-level summary
# -------------------------
customer_summary <- feature_data %>%
  group_by(CustomerID) %>%
  summarise(
    Country = first(Country),
    Profession = first(Profession),
    Age = first(Age),
    FamilySize = first(FamilySize),
    MaritalStatus = first(MaritalStatus),
    Education = first(Education),
    
    FirstPurchaseDate = min(InvoiceDate, na.rm = TRUE),
    LastPurchaseDate = max(InvoiceDate, na.rm = TRUE),
    
    Recency = as.numeric(reference_date - max(InvoiceDate), units = "days"),
    Frequency = n_distinct(InvoiceNo),
    Monetary = sum(TotalPrice, na.rm = TRUE),
    
    TotalQuantity = sum(Quantity, na.rm = TRUE),
    AvgBasketValue = sum(TotalPrice, na.rm = TRUE) / n_distinct(InvoiceNo),
    AvgItemsPerOrder = sum(Quantity, na.rm = TRUE) / n_distinct(InvoiceNo),
    UniqueProductsPurchased = n_distinct(StockCode),
    
    PreferredCountry = first(Country),
    PreferredTimeOfDay = names(sort(table(TimeOfDay), decreasing = TRUE))[1],
    PreferredWeekday = names(sort(table(InvoiceWeekday), decreasing = TRUE))[1],
    PreferredPriceBand = names(sort(table(PriceBand), decreasing = TRUE))[1],
    PreferredBasketType = names(sort(table(BasketType), decreasing = TRUE))[1],
    
    CustomerLifespanDays = as.numeric(
      max(InvoiceDate, na.rm = TRUE) - min(InvoiceDate, na.rm = TRUE),
      units = "days"
    ),
    .groups = "drop"
  )

# -------------------------
# Replace invalid numeric values
# -------------------------
customer_summary <- customer_summary %>%
  mutate(
    AvgBasketValue = ifelse(is.nan(AvgBasketValue) | is.infinite(AvgBasketValue), 0, AvgBasketValue),
    AvgItemsPerOrder = ifelse(is.nan(AvgItemsPerOrder) | is.infinite(AvgItemsPerOrder), 0, AvgItemsPerOrder),
    CustomerLifespanDays = replace_na(CustomerLifespanDays, 0),
    Age = replace_na(Age, median(Age, na.rm = TRUE)),
    FamilySize = replace_na(FamilySize, median(FamilySize, na.rm = TRUE))
  )

# -------------------------
# RFM scoring
# Higher score = better
# -------------------------
customer_summary <- customer_summary %>%
  mutate(
    R_Score = ntile(desc(-Recency), 5),
    F_Score = ntile(Frequency, 5),
    M_Score = ntile(Monetary, 5),
    RFM_Total = R_Score + F_Score + M_Score,
    RFM_Score = paste0(R_Score, F_Score, M_Score)
  )

# -------------------------
# Business-friendly segment labels
# -------------------------
customer_summary <- customer_summary %>%
  mutate(
    Segment = case_when(
      R_Score >= 4 & F_Score >= 4 & M_Score >= 4 ~ "Champions",
      R_Score >= 3 & F_Score >= 4 ~ "Loyal Customers",
      R_Score >= 4 & F_Score >= 2 & M_Score >= 2 ~ "Potential Loyalists",
      R_Score >= 3 & F_Score <= 2 ~ "Promising",
      R_Score <= 2 & F_Score >= 3 & M_Score >= 3 ~ "At Risk",
      R_Score <= 2 & F_Score <= 2 & M_Score >= 3 ~ "Big Spenders Lost",
      R_Score <= 2 & F_Score <= 2 ~ "Lost",
      TRUE ~ "Needs Attention"
    )
  )

# -------------------------
# Additional customer bands
# -------------------------
customer_summary <- customer_summary %>%
  mutate(
    AgeBand = case_when(
      Age < 25 ~ "18-24",
      Age >= 25 & Age < 35 ~ "25-34",
      Age >= 35 & Age < 45 ~ "35-44",
      Age >= 45 & Age < 55 ~ "45-54",
      TRUE ~ "55+"
    ),
    SpendingBand = case_when(
      Monetary < 500 ~ "Low Value",
      Monetary >= 500 & Monetary < 2000 ~ "Medium Value",
      Monetary >= 2000 & Monetary < 5000 ~ "High Value",
      TRUE ~ "Premium Value"
    ),
    FrequencyBand = case_when(
      Frequency == 1 ~ "One-Time",
      Frequency >= 2 & Frequency <= 5 ~ "Occasional",
      Frequency >= 6 & Frequency <= 15 ~ "Frequent",
      TRUE ~ "Highly Frequent"
    )
  )

# -------------------------
# Column order
# -------------------------
customer_summary <- customer_summary %>%
  select(
    CustomerID,
    Country, PreferredCountry,
    Profession, Age, AgeBand, FamilySize, MaritalStatus, Education,
    FirstPurchaseDate, LastPurchaseDate, CustomerLifespanDays,
    Recency, Frequency, Monetary,
    AvgBasketValue, AvgItemsPerOrder, TotalQuantity, UniqueProductsPurchased,
    PreferredTimeOfDay, PreferredWeekday, PreferredPriceBand, PreferredBasketType,
    R_Score, F_Score, M_Score, RFM_Total, RFM_Score,
    Segment, SpendingBand, FrequencyBand
  )

# -------------------------
# Quality checks
# -------------------------
cat("Customer summary creation completed.\n\n")
cat("Rows in customer summary :", nrow(customer_summary), "\n")
cat("Columns in customer summary :", ncol(customer_summary), "\n\n")

cat("Core metric summary:\n")
print(
  customer_summary %>%
    summarise(
      AvgRecency = round(mean(Recency, na.rm = TRUE), 2),
      AvgFrequency = round(mean(Frequency, na.rm = TRUE), 2),
      AvgMonetary = round(mean(Monetary, na.rm = TRUE), 2),
      AvgBasketValue = round(mean(AvgBasketValue, na.rm = TRUE), 2)
    )
)

cat("\nSegment distribution:\n")
print(customer_summary %>% count(Segment, sort = TRUE))

# -------------------------
# Save output
# -------------------------
write_csv(customer_summary, output_file)

cat("\nCustomer summary dataset saved to:\n")
cat(output_file, "\n")