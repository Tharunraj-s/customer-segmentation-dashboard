# =========================
# Customer Segmentation Dashboard
# App Launcher
# =========================

# -------------------------
# Load required libraries
# -------------------------
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(readr)
library(lubridate)
library(scales)
library(tidyr)

# -------------------------
# Source app files
# -------------------------
source("app/helpers.R", local = TRUE)
source("app/ui.R", local = TRUE)
source("app/server.R", local = TRUE)

# -------------------------
# Run app
# -------------------------
shinyApp(ui = ui, server = server)