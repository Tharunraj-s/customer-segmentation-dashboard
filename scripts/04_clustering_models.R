# scripts/04_clustering_prep.R

library(readr)
library(dplyr)
library(cluster)
library(tidyr)

input_file           <- "data/processed/customer_summary.csv"
output_cluster_file  <- "data/processed/customer_clusters.csv"
output_profile_file  <- "data/processed/cluster_profiles.csv"
output_metrics_file  <- "data/processed/clustering_metrics.csv"

if (!file.exists(input_file)) {
  stop("Customer summary dataset not found. Run scripts/03_customer_summary.R first.")
}

customer_data <- read_csv(input_file, show_col_types = FALSE)

cluster_base <- customer_data %>%
  select(
    CustomerID,
    Recency,
    Frequency,
    Monetary,
    AvgBasketValue,
    AvgItemsPerOrder,
    TotalQuantity,
    UniqueProductsPurchased,
    CustomerLifespanDays
  ) %>%
  drop_na()

cluster_base <- cluster_base %>%
  mutate(
    Monetary = log1p(Monetary),
    AvgBasketValue = log1p(AvgBasketValue),
    TotalQuantity = log1p(TotalQuantity),
    UniqueProductsPurchased = log1p(UniqueProductsPurchased)
  )

cluster_features <- cluster_base %>%
  select(-CustomerID)

scaled_matrix <- scale(cluster_features)

set.seed(42)
k_final <- 4

kmeans_model <- kmeans(
  scaled_matrix,
  centers = k_final,
  nstart = 50
)

pca_model <- prcomp(scaled_matrix, center = TRUE, scale. = FALSE)

pca_df <- as.data.frame(pca_model$x[, 1:2])
names(pca_df) <- c("PC1", "PC2")

sil <- silhouette(kmeans_model$cluster, dist(scaled_matrix))
avg_silhouette <- summary(sil)$avg.width

clustered_data <- cluster_base %>%
  mutate(
    Cluster = as.factor(kmeans_model$cluster)
  ) %>%
  bind_cols(pca_df)

customer_clusters <- customer_data %>%
  left_join(
    clustered_data %>% select(CustomerID, Cluster, PC1, PC2),
    by = "CustomerID"
  )

cluster_profiles <- customer_clusters %>%
  group_by(Cluster) %>%
  summarise(
    Customers = n(),
    AvgRecency = round(mean(Recency, na.rm = TRUE), 2),
    AvgFrequency = round(mean(Frequency, na.rm = TRUE), 2),
    AvgMonetary = round(mean(Monetary, na.rm = TRUE), 2),
    AvgBasketValue = round(mean(AvgBasketValue, na.rm = TRUE), 2),
    AvgItemsPerOrder = round(mean(AvgItemsPerOrder, na.rm = TRUE), 2),
    AvgTotalQuantity = round(mean(TotalQuantity, na.rm = TRUE), 2),
    AvgUniqueProducts = round(mean(UniqueProductsPurchased, na.rm = TRUE), 2),
    AvgLifespanDays = round(mean(CustomerLifespanDays, na.rm = TRUE), 2),
    TopSegment = names(sort(table(Segment), decreasing = TRUE))[1],
    TopSpendingBand = names(sort(table(SpendingBand), decreasing = TRUE))[1],
    TopFrequencyBand = names(sort(table(FrequencyBand), decreasing = TRUE))[1],
    .groups = "drop"
  )

cluster_ranked <- cluster_profiles %>%
  mutate(
    ClusterLabel = case_when(
      AvgMonetary == max(AvgMonetary) & AvgFrequency == max(AvgFrequency) ~ "High-Value Loyal",
      AvgRecency == min(AvgRecency) & AvgFrequency >= median(AvgFrequency) ~ "Recently Active",
      AvgRecency == max(AvgRecency) & AvgFrequency <= median(AvgFrequency) ~ "Dormant / Churn Risk",
      TRUE ~ "Regular Customers"
    )
  )

customer_clusters <- customer_clusters %>%
  left_join(
    cluster_ranked %>% select(Cluster, ClusterLabel),
    by = "Cluster"
  )

cluster_profiles <- cluster_ranked

clustering_metrics <- tibble(
  Metric = c(
    "Number of Customers Clustered",
    "Number of Clusters",
    "Average Silhouette Score",
    "PCA Variance Explained PC1",
    "PCA Variance Explained PC2"
  ),
  Value = c(
    nrow(clustered_data),
    k_final,
    round(avg_silhouette, 4),
    round(summary(pca_model)$importance[2, 1], 4),
    round(summary(pca_model)$importance[2, 2], 4)
  )
)

write_csv(customer_clusters, output_cluster_file)
write_csv(cluster_profiles, output_profile_file)
write_csv(clustering_metrics, output_metrics_file)

cat("\nCustomer clusters saved to:\n", output_cluster_file, "\n")
cat("\nCluster profiles saved to:\n", output_profile_file, "\n")
cat("\nClustering metrics saved to:\n", output_metrics_file, "\n")