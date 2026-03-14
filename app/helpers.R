# app/helpers.R

# =========================
# Helper Functions
# Customer Segmentation Dashboard
# =========================

library(dplyr)
library(ggplot2)
library(scales)
library(plotly)

# -------------------------
# Number formatting helpers
# -------------------------
format_number <- function(x, digits = 0) {
  comma(round(x, digits))
}

format_currency <- function(x, digits = 0, prefix = "£") {
  paste0(prefix, comma(round(x, digits)))
}

format_percent <- function(x, digits = 1) {
  paste0(round(x * 100, digits), "%")
}

# -------------------------
# KPI card UI helper
# -------------------------
kpi_card <- function(title, value, subtitle = NULL, color_class = "kpi-default") {
  div(
    class = paste("kpi-card", color_class),
    div(class = "kpi-title", title),
    div(class = "kpi-value", value),
    if (!is.null(subtitle)) div(class = "kpi-subtitle", subtitle)
  )
}

# -------------------------
# Plot theme helper
# -------------------------
theme_master <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15, color = "#1f2937"),
      plot.subtitle = element_text(size = 11, color = "#6b7280"),
      axis.title = element_text(face = "bold", color = "#374151"),
      axis.text = element_text(color = "#4b5563"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
}

# -------------------------
# Empty plot helper
# -------------------------
empty_plot <- function(message = "No data available for the selected filters.") {
  ggplot() +
    annotate("text", x = 1, y = 1, label = message, size = 5, color = "#6b7280") +
    xlim(0, 2) +
    ylim(0, 2) +
    theme_void()
}

# -------------------------
# Plotly wrapper
# -------------------------
ggplotly_clean <- function(p, tooltip = c("x", "y")) {
  ggplotly(p, tooltip = tooltip) %>%
    layout(
      legend = list(orientation = "h", x = 0, y = -0.2),
      margin = list(l = 60, r = 20, t = 50, b = 60)
    )
}

# -------------------------
# Segment color palette
# -------------------------
segment_palette <- c(
  "Champions" = "#16a34a",
  "Loyal Customers" = "#0ea5e9",
  "Potential Loyalists" = "#8b5cf6",
  "Promising" = "#f59e0b",
  "At Risk" = "#ef4444",
  "Big Spenders Lost" = "#dc2626",
  "Lost" = "#6b7280",
  "Needs Attention" = "#14b8a6"
)

# -------------------------
# Cluster color palette
# -------------------------
cluster_palette <- c(
  "1" = "#2563eb",
  "2" = "#16a34a",
  "3" = "#f59e0b",
  "4" = "#ef4444",
  "5" = "#8b5cf6",
  "6" = "#14b8a6",
  "7" = "#ec4899",
  "8" = "#6b7280"
)

# -------------------------
# Safe top-N helper
# -------------------------
top_n_safe <- function(data, n = 10, sort_col) {
  data %>%
    arrange(desc({{ sort_col }})) %>%
    slice_head(n = n)
}