# app/server.R

# =========================
# Server
# Customer Segmentation Dashboard
# =========================

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(lubridate)
library(tidyr)

server <- function(input, output, session) {
  
  # -------------------------
  # File paths
  # -------------------------
  sales_file      <- "data/processed/feature_engineered_online_retail.csv"
  invoice_file    <- "data/processed/invoice_summary.csv"
  customer_file   <- "data/processed/customer_summary.csv"
  cluster_file    <- "data/processed/customer_clusters.csv"
  profile_file    <- "data/processed/cluster_profiles.csv"
  metrics_file    <- "data/processed/clustering_metrics.csv"
  
  # -------------------------
  # Check files exist
  # -------------------------
  required_files <- c(
    sales_file, invoice_file, customer_file,
    cluster_file, profile_file, metrics_file
  )
  
  missing_files <- required_files[!file.exists(required_files)]
  
  if (length(missing_files) > 0) {
    stop(
      paste(
        "Missing required processed files:",
        paste(missing_files, collapse = ", ")
      )
    )
  }
  
  # -------------------------
  # Load data
  # -------------------------
  sales_data <- read_csv(sales_file, show_col_types = FALSE)
  invoice_data <- read_csv(invoice_file, show_col_types = FALSE)
  customer_data <- read_csv(customer_file, show_col_types = FALSE)
  customer_clusters <- read_csv(cluster_file, show_col_types = FALSE)
  cluster_profiles <- read_csv(profile_file, show_col_types = FALSE)
  clustering_metrics <- read_csv(metrics_file, show_col_types = FALSE)
  
  # -------------------------
  # Fix types
  # -------------------------
  sales_data <- sales_data %>%
    mutate(
      InvoiceDate = as.POSIXct(InvoiceDate, tz = "UTC"),
      InvoiceMonth = factor(InvoiceMonth, levels = month.name, ordered = TRUE),
      InvoiceWeekday = factor(
        InvoiceWeekday,
        levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
        ordered = TRUE
      )
    )
  
  invoice_data <- invoice_data %>%
    mutate(
      InvoiceDate = as.POSIXct(InvoiceDate, tz = "UTC")
    )
  
  customer_data <- customer_data %>%
    mutate(
      FirstPurchaseDate = as.POSIXct(FirstPurchaseDate, tz = "UTC"),
      LastPurchaseDate = as.POSIXct(LastPurchaseDate, tz = "UTC")
    )
  
  customer_clusters <- customer_clusters %>%
    mutate(
      FirstPurchaseDate = as.POSIXct(FirstPurchaseDate, tz = "UTC"),
      LastPurchaseDate = as.POSIXct(LastPurchaseDate, tz = "UTC"),
      Cluster = as.factor(Cluster)
    )
  
  # -------------------------
  # Initialize filter inputs
  # -------------------------
  observe({
    updateSelectInput(
      session,
      "country",
      choices = c("All", sort(unique(sales_data$Country))),
      selected = "All"
    )
    
    updateDateRangeInput(
      session,
      "date_range",
      start = min(as.Date(sales_data$InvoiceDate), na.rm = TRUE),
      end = max(as.Date(sales_data$InvoiceDate), na.rm = TRUE),
      min = min(as.Date(sales_data$InvoiceDate), na.rm = TRUE),
      max = max(as.Date(sales_data$InvoiceDate), na.rm = TRUE)
    )
  })
  
  # -------------------------
  # Filtered sales data
  # -------------------------
  filtered_sales <- reactive({
    req(input$date_range)
    
    data <- sales_data %>%
      filter(
        as.Date(InvoiceDate) >= input$date_range[1],
        as.Date(InvoiceDate) <= input$date_range[2]
      )
    
    if (!is.null(input$country) && input$country != "All") {
      data <- data %>% filter(Country == input$country)
    }
    
    data
  })
  
  # -------------------------
  # Filtered invoice data
  # -------------------------
  filtered_invoices <- reactive({
    req(input$date_range)
    
    data <- invoice_data %>%
      filter(
        as.Date(InvoiceDate) >= input$date_range[1],
        as.Date(InvoiceDate) <= input$date_range[2]
      )
    
    if (!is.null(input$country) && input$country != "All") {
      data <- data %>% filter(Country == input$country)
    }
    
    data
  })
  
  # -------------------------
  # Filtered customers
  # based on filtered sales customer IDs
  # -------------------------
  filtered_customers <- reactive({
    valid_ids <- filtered_sales() %>%
      distinct(CustomerID) %>%
      pull(CustomerID)
    
    customer_data %>%
      filter(CustomerID %in% valid_ids)
  })
  
  # -------------------------
  # Filtered clustered customers
  # -------------------------
  filtered_clustered_customers <- reactive({
    valid_ids <- filtered_sales() %>%
      distinct(CustomerID) %>%
      pull(CustomerID)
    
    customer_clusters %>%
      filter(CustomerID %in% valid_ids)
  })
  
  # -------------------------
  # Dynamic cluster recompute for chosen k
  # -------------------------
  dynamic_clustered_data <- reactive({
    df <- filtered_clustered_customers() %>%
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
    
    req(nrow(df) >= input$clusters)
    
    model_df <- df %>%
      mutate(
        Monetary = log1p(Monetary),
        AvgBasketValue = log1p(AvgBasketValue),
        TotalQuantity = log1p(TotalQuantity),
        UniqueProductsPurchased = log1p(UniqueProductsPurchased)
      )
    
    x <- model_df %>% select(-CustomerID)
    x_scaled <- scale(x)
    
    set.seed(42)
    km <- kmeans(x_scaled, centers = input$clusters, nstart = 25)
    pca <- prcomp(x_scaled, center = TRUE, scale. = FALSE)
    
    pca_df <- as.data.frame(pca$x[, 1:2])
    names(pca_df) <- c("PC1", "PC2")
    
    out <- df %>%
      mutate(Cluster = as.factor(km$cluster)) %>%
      bind_cols(pca_df)
    
    out
  })
  
  # -------------------------
  # KPI cards
  # -------------------------
  output$kpi_total_revenue <- renderUI({
    value <- sum(filtered_sales()$TotalPrice, na.rm = TRUE)
    kpi_card("Total Revenue", format_currency(value), "Filtered sales revenue", "kpi-blue")
  })
  
  output$kpi_total_orders <- renderUI({
    value <- n_distinct(filtered_sales()$InvoiceNo)
    kpi_card("Total Orders", format_number(value), "Unique invoices", "kpi-green")
  })
  
  output$kpi_total_customers <- renderUI({
    value <- n_distinct(filtered_sales()$CustomerID)
    kpi_card("Unique Customers", format_number(value), "Customers in selected view", "kpi-purple")
  })
  
  output$kpi_avg_order_value <- renderUI({
    inv <- filtered_invoices()
    value <- mean(inv$InvoiceValue, na.rm = TRUE)
    kpi_card("Average Order Value", format_currency(value, 2), "Mean invoice value", "kpi-orange")
  })
  
  # -------------------------
  # Reusable chart renderer
  # -------------------------
  render_chart_ui <- function(plot_id, plotly_id) {
    renderUI({
      if (isTRUE(input$show_plotly)) {
        plotlyOutput(plotly_id, height = "340px")
      } else {
        plotOutput(plot_id, height = "340px")
      }
    })
  }
  
  # -------------------------
  # Overview chart UIs
  # -------------------------
  output$sales_trend_ui <- render_chart_ui("sales_trend_plot", "sales_trend_plotly")
  output$monthly_sales_ui <- render_chart_ui("monthly_sales_plot", "monthly_sales_plotly")
  output$weekday_sales_ui <- render_chart_ui("weekday_sales_plot", "weekday_sales_plotly")
  output$country_sales_ui <- render_chart_ui("country_sales_plot", "country_sales_plotly")
  
  # -------------------------
  # Product/order chart UIs
  # -------------------------
  output$top_products_ui <- render_chart_ui("top_products_plot", "top_products_plotly")
  output$top_customers_ui <- render_chart_ui("top_customers_plot", "top_customers_plotly")
  output$basket_value_ui <- render_chart_ui("basket_value_plot", "basket_value_plotly")
  output$time_of_day_ui <- render_chart_ui("time_of_day_plot", "time_of_day_plotly")
  
  # -------------------------
  # Customer chart UIs
  # -------------------------
  output$customer_monetary_ui <- render_chart_ui("customer_monetary_plot", "customer_monetary_plotly")
  output$customer_frequency_ui <- render_chart_ui("customer_frequency_plot", "customer_frequency_plotly")
  output$segment_distribution_ui <- render_chart_ui("segment_distribution_plot", "segment_distribution_plotly")
  output$age_distribution_ui <- render_chart_ui("age_distribution_plot", "age_distribution_plotly")
  output$education_profile_ui <- render_chart_ui("education_profile_plot", "education_profile_plotly")
  output$profession_profile_ui <- render_chart_ui("profession_profile_plot", "profession_profile_plotly")
  
  # -------------------------
  # Segmentation chart UI
  # -------------------------
  output$cluster_plot_ui <- render_chart_ui("cluster_plot", "cluster_plotly")
  
  # -------------------------
  # Daily sales trend
  # -------------------------
  sales_trend_gg <- reactive({
    plot_data <- filtered_sales() %>%
      group_by(Date = as.Date(InvoiceDate)) %>%
      summarise(Sales = sum(TotalPrice, na.rm = TRUE), .groups = "drop")
    
    if (nrow(plot_data) == 0) return(empty_plot())
    
    ggplot(plot_data, aes(Date, Sales, text = paste("Date:", Date, "<br>Sales:", comma(round(Sales, 2))))) +
      geom_line(linewidth = 1, color = "#2563eb") +
      labs(title = "Daily Sales Trend", x = "Date", y = "Revenue") +
      scale_y_continuous(labels = comma) +
      theme_master()
  })
  
  output$sales_trend_plot <- renderPlot({ sales_trend_gg() })
  output$sales_trend_plotly <- renderPlotly({
    ggplotly_clean(sales_trend_gg(), tooltip = "text")
  })
  
  # -------------------------
  # Monthly sales
  # -------------------------
  monthly_sales_gg <- reactive({
    plot_data <- filtered_sales() %>%
      mutate(YearMonth = format(InvoiceDate, "%Y-%m")) %>%
      group_by(YearMonth) %>%
      summarise(Sales = sum(TotalPrice, na.rm = TRUE), .groups = "drop")
    
    if (nrow(plot_data) == 0) return(empty_plot())
    
    ggplot(plot_data, aes(YearMonth, Sales, group = 1, text = paste("Month:", YearMonth, "<br>Sales:", comma(round(Sales, 2))))) +
      geom_line(linewidth = 1, color = "#14b8a6") +
      geom_point(size = 2.2, color = "#14b8a6") +
      labs(title = "Monthly Sales Trend", x = "Month", y = "Revenue") +
      scale_y_continuous(labels = comma) +
      theme_master() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$monthly_sales_plot <- renderPlot({ monthly_sales_gg() })
  output$monthly_sales_plotly <- renderPlotly({
    ggplotly_clean(monthly_sales_gg(), tooltip = "text")
  })
  
  # -------------------------
  # Weekday sales
  # -------------------------
  weekday_sales_gg <- reactive({
    plot_data <- filtered_sales() %>%
      group_by(InvoiceWeekday) %>%
      summarise(Sales = sum(TotalPrice, na.rm = TRUE), .groups = "drop")
    
    if (nrow(plot_data) == 0) return(empty_plot())
    
    ggplot(plot_data, aes(InvoiceWeekday, Sales, text = paste("Weekday:", InvoiceWeekday, "<br>Sales:", comma(round(Sales, 2))))) +
      geom_col(fill = "#8b5cf6") +
      labs(title = "Sales by Weekday", x = "Weekday", y = "Revenue") +
      scale_y_continuous(labels = comma) +
      theme_master()
  })
  
  output$weekday_sales_plot <- renderPlot({ weekday_sales_gg() })
  output$weekday_sales_plotly <- renderPlotly({
    ggplotly_clean(weekday_sales_gg(), tooltip = "text")
  })
  
  # -------------------------
  # Country sales
  # -------------------------
  country_sales_gg <- reactive({
    plot_data <- filtered_sales() %>%
      group_by(Country) %>%
      summarise(Sales = sum(TotalPrice, na.rm = TRUE), .groups = "drop") %>%
      top_n_safe(input$top_n, Sales)
    
    if (nrow(plot_data) == 0) return(empty_plot())
    
    ggplot(plot_data, aes(reorder(Country, Sales), Sales, text = paste("Country:", Country, "<br>Sales:", comma(round(Sales, 2))))) +
      geom_col(fill = "#0ea5e9") +
      coord_flip() +
      labs(title = paste("Top", input$top_n, "Countries by Revenue"), x = "Country", y = "Revenue") +
      scale_y_continuous(labels = comma) +
      theme_master()
  })
  
  output$country_sales_plot <- renderPlot({ country_sales_gg() })
  output$country_sales_plotly <- renderPlotly({
    ggplotly_clean(country_sales_gg(), tooltip = "text")
  })
  
  # -------------------------
  # Top products
  # -------------------------
  top_products_gg <- reactive({
    plot_data <- filtered_sales() %>%
      group_by(Description) %>%
      summarise(Sales = sum(TotalPrice, na.rm = TRUE), .groups = "drop") %>%
      top_n_safe(input$top_n, Sales)
    
    if (nrow(plot_data) == 0) return(empty_plot())
    
    ggplot(plot_data, aes(reorder(Description, Sales), Sales, text = paste("Product:", Description, "<br>Sales:", comma(round(Sales, 2))))) +
      geom_col(fill = "#16a34a") +
      coord_flip() +
      labs(title = paste("Top", input$top_n, "Products by Revenue"), x = "Product", y = "Revenue") +
      scale_y_continuous(labels = comma) +
      theme_master()
  })
  
  output$top_products_plot <- renderPlot({ top_products_gg() })
  output$top_products_plotly <- renderPlotly({
    ggplotly_clean(top_products_gg(), tooltip = "text")
  })
  
  # -------------------------
  # Top customers
  # -------------------------
  top_customers_gg <- reactive({
    plot_data <- filtered_sales() %>%
      group_by(CustomerID) %>%
      summarise(Sales = sum(TotalPrice, na.rm = TRUE), .groups = "drop") %>%
      top_n_safe(input$top_n, Sales)
    
    if (nrow(plot_data) == 0) return(empty_plot())
    
    ggplot(plot_data, aes(reorder(as.character(CustomerID), Sales), Sales, text = paste("Customer:", CustomerID, "<br>Revenue:", comma(round(Sales, 2))))) +
      geom_col(fill = "#f59e0b") +
      coord_flip() +
      labs(title = paste("Top", input$top_n, "Customers by Revenue"), x = "Customer ID", y = "Revenue") +
      scale_y_continuous(labels = comma) +
      theme_master()
  })
  
  output$top_customers_plot <- renderPlot({ top_customers_gg() })
  output$top_customers_plotly <- renderPlotly({
    ggplotly_clean(top_customers_gg(), tooltip = "text")
  })
  
  # -------------------------
  # Basket value distribution
  # -------------------------
  basket_value_gg <- reactive({
    plot_data <- filtered_invoices()
    
    if (nrow(plot_data) == 0) return(empty_plot())
    
    ggplot(plot_data, aes(InvoiceValue, text = paste("Invoice Value:", comma(round(InvoiceValue, 2))))) +
      geom_histogram(bins = 35, fill = "#2563eb", alpha = 0.85) +
      labs(title = "Basket Value Distribution", x = "Invoice Value", y = "Count") +
      scale_x_continuous(labels = comma) +
      theme_master()
  })
  
  output$basket_value_plot <- renderPlot({ basket_value_gg() })
  output$basket_value_plotly <- renderPlotly({
    ggplotly_clean(basket_value_gg(), tooltip = "text")
  })
  
  # -------------------------
  # Orders by time of day
  # -------------------------
  time_of_day_gg <- reactive({
    plot_data <- filtered_sales() %>%
      group_by(TimeOfDay) %>%
      summarise(Orders = n_distinct(InvoiceNo), .groups = "drop")
    
    if (nrow(plot_data) == 0) return(empty_plot())
    
    ggplot(plot_data, aes(TimeOfDay, Orders, text = paste("Time:", TimeOfDay, "<br>Orders:", Orders))) +
      geom_col(fill = "#14b8a6") +
      labs(title = "Orders by Time of Day", x = "Time of Day", y = "Orders") +
      theme_master()
  })
  
  output$time_of_day_plot <- renderPlot({ time_of_day_gg() })
  output$time_of_day_plotly <- renderPlotly({
    ggplotly_clean(time_of_day_gg(), tooltip = "text")
  })
  
  # -------------------------
  # Customer monetary distribution
  # -------------------------
  customer_monetary_gg <- reactive({
    plot_data <- filtered_customers()
    
    if (nrow(plot_data) == 0) return(empty_plot())
    
    ggplot(plot_data, aes(Monetary, text = paste("Monetary:", comma(round(Monetary, 2))))) +
      geom_histogram(bins = 35, fill = "#0ea5e9", alpha = 0.9) +
      labs(title = "Customer Monetary Distribution", x = "Monetary Value", y = "Count") +
      scale_x_continuous(labels = comma) +
      theme_master()
  })
  
  output$customer_monetary_plot <- renderPlot({ customer_monetary_gg() })
  output$customer_monetary_plotly <- renderPlotly({
    ggplotly_clean(customer_monetary_gg(), tooltip = "text")
  })
  
  # -------------------------
  # Customer frequency distribution
  # -------------------------
  customer_frequency_gg <- reactive({
    plot_data <- filtered_customers()
    
    if (nrow(plot_data) == 0) return(empty_plot())
    
    ggplot(plot_data, aes(Frequency, text = paste("Frequency:", Frequency))) +
      geom_histogram(bins = 30, fill = "#8b5cf6", alpha = 0.9) +
      labs(title = "Customer Frequency Distribution", x = "Purchase Frequency", y = "Count") +
      theme_master()
  })
  
  output$customer_frequency_plot <- renderPlot({ customer_frequency_gg() })
  output$customer_frequency_plotly <- renderPlotly({
    ggplotly_clean(customer_frequency_gg(), tooltip = "text")
  })
  
  # -------------------------
  # Segment distribution
  # -------------------------
  segment_distribution_gg <- reactive({
    plot_data <- filtered_customers() %>%
      count(Segment, sort = TRUE)
    
    if (nrow(plot_data) == 0) return(empty_plot())
    
    ggplot(plot_data, aes(reorder(Segment, n), n, fill = Segment, text = paste("Segment:", Segment, "<br>Customers:", n))) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = segment_palette, guide = "none") +
      labs(title = "Customer Segments", x = "Segment", y = "Customers") +
      theme_master()
  })
  
  output$segment_distribution_plot <- renderPlot({ segment_distribution_gg() })
  output$segment_distribution_plotly <- renderPlotly({
    ggplotly_clean(segment_distribution_gg(), tooltip = "text")
  })
  
  # -------------------------
  # Age distribution
  # -------------------------
  age_distribution_gg <- reactive({
    plot_data <- filtered_customers() %>% filter(!is.na(Age))
    
    if (nrow(plot_data) == 0) return(empty_plot("No age data available."))
    
    ggplot(plot_data, aes(Age, text = paste("Age:", Age))) +
      geom_histogram(binwidth = 5, fill = "#f59e0b", alpha = 0.9) +
      labs(title = "Age Distribution", x = "Age", y = "Count") +
      theme_master()
  })
  
  output$age_distribution_plot <- renderPlot({ age_distribution_gg() })
  output$age_distribution_plotly <- renderPlotly({
    ggplotly_clean(age_distribution_gg(), tooltip = "text")
  })
  
  # -------------------------
  # Education profile
  # -------------------------
  education_profile_gg <- reactive({
    plot_data <- filtered_customers() %>%
      filter(!is.na(Education), Education != "") %>%
      count(Education, sort = TRUE)
    
    if (nrow(plot_data) == 0) return(empty_plot("No education data available."))
    
    ggplot(plot_data, aes(reorder(Education, n), n, text = paste("Education:", Education, "<br>Customers:", n))) +
      geom_col(fill = "#16a34a") +
      coord_flip() +
      labs(title = "Education Profile", x = "Education", y = "Customers") +
      theme_master()
  })
  
  output$education_profile_plot <- renderPlot({ education_profile_gg() })
  output$education_profile_plotly <- renderPlotly({
    ggplotly_clean(education_profile_gg(), tooltip = "text")
  })
  
  # -------------------------
  # Profession profile
  # -------------------------
  profession_profile_gg <- reactive({
    plot_data <- filtered_customers() %>%
      filter(!is.na(Profession), Profession != "") %>%
      count(Profession, sort = TRUE) %>%
      slice_head(n = input$top_n)
    
    if (nrow(plot_data) == 0) return(empty_plot("No profession data available."))
    
    ggplot(plot_data, aes(reorder(Profession, n), n, text = paste("Profession:", Profession, "<br>Customers:", n))) +
      geom_col(fill = "#ef4444") +
      coord_flip() +
      labs(title = "Profession Profile", x = "Profession", y = "Customers") +
      theme_master()
  })
  
  output$profession_profile_plot <- renderPlot({ profession_profile_gg() })
  output$profession_profile_plotly <- renderPlotly({
    ggplotly_clean(profession_profile_gg(), tooltip = "text")
  })
  
  # -------------------------
  # Dynamic cluster plot
  # -------------------------
  cluster_plot_gg <- reactive({
    df <- dynamic_clustered_data()
    
    req(input$cluster_x, input$cluster_y)
    
    if (!(input$cluster_x %in% names(df)) || !(input$cluster_y %in% names(df))) {
      return(empty_plot("Selected cluster axes are not available."))
    }
    
    ggplot(
      df,
      aes_string(
        x = input$cluster_x,
        y = input$cluster_y,
        color = "Cluster",
        text = "paste('Customer:', CustomerID,
                      '<br>Cluster:', Cluster,
                      '<br>X:', round(get(input$cluster_x), 2),
                      '<br>Y:', round(get(input$cluster_y), 2))"
      )
    ) +
      geom_point(alpha = 0.75, size = 2.5) +
      scale_color_manual(values = cluster_palette) +
      labs(
        title = "Dynamic Customer Cluster Explorer",
        x = input$cluster_x,
        y = input$cluster_y,
        color = "Cluster"
      ) +
      theme_master()
  })
  
  output$cluster_plot <- renderPlot({ cluster_plot_gg() })
  output$cluster_plotly <- renderPlotly({
    ggplotly_clean(cluster_plot_gg(), tooltip = "text")
  })
  
  # -------------------------
  # Cluster profiles table
  # -------------------------
  output$cluster_profiles_table <- renderDT({
    datatable(
      cluster_profiles,
      rownames = FALSE,
      options = list(pageLength = 8, scrollX = TRUE),
      class = "compact stripe hover"
    )
  })
  
  # -------------------------
  # Clustering metrics table
  # -------------------------
  output$clustering_metrics_table <- renderDT({
    datatable(
      clustering_metrics,
      rownames = FALSE,
      options = list(dom = "t", scrollX = TRUE),
      class = "compact stripe hover"
    )
  })
  
  # -------------------------
  # Customer segment details table
  # -------------------------
  output$customer_segment_table <- renderDT({
    df <- filtered_clustered_customers() %>%
      select(
        CustomerID, Segment, Country, Age, Profession,
        Recency, Frequency, Monetary, AvgBasketValue,
        Cluster, ClusterLabel
      ) %>%
      arrange(desc(Monetary))
    
    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE),
      class = "compact stripe hover"
    )
  })
  
}