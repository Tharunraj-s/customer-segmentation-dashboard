# =========================
# UI
# Customer Segmentation Dashboard
# =========================

library(shiny)
library(bslib)
library(plotly)
library(DT)

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2563eb",
    secondary = "#14b8a6",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  div(
    class = "app-header",
    div(
      class = "app-title-wrap",
      h1(class = "app-title", "Customer Segmentation & Sales Analytics Dashboard"),
      p(
        class = "app-subtitle",
        "An interactive customer intelligence platform built with R Shiny"
      )
    )
  ),
  
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      
      div(class = "sidebar-section-title", "Filters"),
      
      selectInput(
        inputId = "country",
        label = "Country",
        choices = NULL,
        selected = "All"
      ),
      
      dateRangeInput(
        inputId = "date_range",
        label = "Invoice Date Range",
        start = NULL,
        end = NULL
      ),
      
      sliderInput(
        inputId = "top_n",
        label = "Top N items",
        min = 5,
        max = 20,
        value = 10,
        step = 1
      ),
      
      sliderInput(
        inputId = "clusters",
        label = "Number of clusters",
        min = 2,
        max = 8,
        value = 4,
        step = 1
      ),
      
      selectInput(
        inputId = "cluster_x",
        label = "Cluster X-axis",
        choices = c(
          "Recency",
          "Frequency",
          "Monetary",
          "AvgBasketValue",
          "AvgItemsPerOrder",
          "TotalQuantity",
          "UniqueProductsPurchased",
          "CustomerLifespanDays",
          "PC1",
          "PC2"
        ),
        selected = "PC1"
      ),
      
      selectInput(
        inputId = "cluster_y",
        label = "Cluster Y-axis",
        choices = c(
          "Recency",
          "Frequency",
          "Monetary",
          "AvgBasketValue",
          "AvgItemsPerOrder",
          "TotalQuantity",
          "UniqueProductsPurchased",
          "CustomerLifespanDays",
          "PC1",
          "PC2"
        ),
        selected = "PC2"
      ),
      
      checkboxInput(
        inputId = "show_plotly",
        label = "Enable interactive charts",
        value = TRUE
      ),
      
      hr(),
      
      div(class = "sidebar-section-title", "Quick Notes"),
      tags$ul(
        class = "sidebar-notes",
        tags$li("Use the filters to focus on country and date range."),
        tags$li("Top N updates countries, products and customers charts."),
        tags$li("Cluster plot can use both business metrics and PCA axes.")
      )
    ),
    
    div(
      class = "main-content",
      
      # KPI ROW
      div(
        class = "kpi-grid",
        uiOutput("kpi_total_revenue"),
        uiOutput("kpi_total_orders"),
        uiOutput("kpi_total_customers"),
        uiOutput("kpi_avg_order_value")
      ),
      
      navset_card_tab(
        id = "main_tabs",
        
        nav_panel(
          "Overview",
          div(
            class = "dashboard-grid two-col",
            card(
              class = "dashboard-card",
              card_header("Daily Sales Trend"),
              card_body(
                uiOutput("sales_trend_ui")
              )
            ),
            card(
              class = "dashboard-card",
              card_header("Monthly Sales Trend"),
              card_body(
                uiOutput("monthly_sales_ui")
              )
            ),
            card(
              class = "dashboard-card",
              card_header("Sales by Weekday"),
              card_body(
                uiOutput("weekday_sales_ui")
              )
            ),
            card(
              class = "dashboard-card",
              card_header("Top Countries by Revenue"),
              card_body(
                uiOutput("country_sales_ui")
              )
            )
          )
        ),
        
        nav_panel(
          "Products & Orders",
          div(
            class = "dashboard-grid two-col",
            card(
              class = "dashboard-card",
              card_header("Top Products by Revenue"),
              card_body(
                uiOutput("top_products_ui")
              )
            ),
            card(
              class = "dashboard-card",
              card_header("Top Customers by Revenue"),
              card_body(
                uiOutput("top_customers_ui")
              )
            ),
            card(
              class = "dashboard-card",
              card_header("Basket Value Distribution"),
              card_body(
                uiOutput("basket_value_ui")
              )
            ),
            card(
              class = "dashboard-card",
              card_header("Orders by Time of Day"),
              card_body(
                uiOutput("time_of_day_ui")
              )
            )
          )
        ),
        
        nav_panel(
          "Customers",
          div(
            class = "dashboard-grid two-col",
            card(
              class = "dashboard-card",
              card_header("Customer Monetary Distribution"),
              card_body(
                uiOutput("customer_monetary_ui")
              )
            ),
            card(
              class = "dashboard-card",
              card_header("Customer Frequency Distribution"),
              card_body(
                uiOutput("customer_frequency_ui")
              )
            ),
            card(
              class = "dashboard-card",
              card_header("Customer Segments"),
              card_body(
                uiOutput("segment_distribution_ui")
              )
            ),
            card(
              class = "dashboard-card",
              card_header("Age Distribution"),
              card_body(
                uiOutput("age_distribution_ui")
              )
            ),
            card(
              class = "dashboard-card",
              card_header("Education Profile"),
              card_body(
                uiOutput("education_profile_ui")
              )
            ),
            card(
              class = "dashboard-card",
              card_header("Profession Profile"),
              card_body(
                uiOutput("profession_profile_ui")
              )
            )
          )
        ),
        
        nav_panel(
          "Segmentation",
          div(
            class = "dashboard-grid two-col",
            card(
              class = "dashboard-card",
              card_header("Cluster Explorer"),
              card_body(
                uiOutput("cluster_plot_ui")
              )
            ),
            card(
              class = "dashboard-card",
              card_header("Cluster Profiles"),
              card_body(
                DTOutput("cluster_profiles_table")
              )
            ),
            card(
              class = "dashboard-card",
              card_header("Clustering Metrics"),
              card_body(
                DTOutput("clustering_metrics_table")
              )
            ),
            card(
              class = "dashboard-card",
              card_header("Customer Segment Details"),
              card_body(
                DTOutput("customer_segment_table")
              )
            )
          )
        )
      )
    )
  )
)