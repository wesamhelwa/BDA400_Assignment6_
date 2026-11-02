# ==========================================
# BDA400 - Assignment 6: Technical Analysis using R
# Student Name: Wisam Halawa
# ==========================================

# ==========================================
# Step 1: Data Collection and Setup
# ==========================================

# Load required libraries for the app
library(shiny)
library(ggplot2)
library(quantmod)

# Define the stock symbol and desired historical time period
stock_symbol <- "AAPL"       # Stock symbol (Apple Inc.)
start_date <- "2023-01-01"   # Start date for historical stock data
end_date <- "2023-07-01"     # End date for historical stock data

# Fetch historical stock data from Yahoo Finance without auto-assigning to global environment
stock_data <- getSymbols(stock_symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)

# ==========================================
# Step 2 & 3: Shiny App UI, Visualization & Technical Indicators
# ==========================================

# Define UI for the Shiny app with interactive widgets
ui <- fluidPage(
  titlePanel("Interactive Portfolio Dashboard - Technical Analysis"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Select Date Range:", start = "2023-01-01", end = "2023-07-01"),
      selectInput("time_frame", "Select Time Frame:", choices = c("Daily", "Weekly", "Monthly"))
    ),
    mainPanel(
      plotOutput("stock_chart")
    )
  )
)

# Define Server logic for the Shiny app
server <- function(input, output) {
  output$stock_chart <- renderPlot({
    # Filter data based on selected date range
    filtered_data <- subset(stock_data, index(stock_data) >= input$date_range[1] & 
                              index(stock_data) <= input$date_range[2])
    
    close_prices <- Cl(filtered_data)
    date_index <- index(filtered_data)
    
    # Create base data frame for plotting
    df <- data.frame(Date = date_index, Close = as.numeric(coredata(close_prices)))
    
    # Calculate simple moving averages safely
    df$SMA20 <- if(nrow(df) >= 20) as.numeric(stats::filter(df$Close, rep(1/20, 20), sides = 1)) else NA
    df$SMA50 <- if(nrow(df) >= 50) as.numeric(stats::filter(df$Close, rep(1/50, 50), sides = 1)) else NA
    
    # Create ggplot with moving averages and trading indicators
    p <- ggplot(df, aes(x = Date)) +
      geom_line(aes(y = Close, color = "Close Price"), linewidth = 1) +
      geom_line(aes(y = SMA20, color = "SMA 20"), linewidth = 0.8) +
      geom_line(aes(y = SMA50, color = "SMA 50"), linewidth = 0.8) +
      scale_color_manual(name = "Legend", 
                         values = c("Close Price" = "steelblue", "SMA 20" = "darkgreen", "SMA 50" = "darkred")) +
      labs(title = paste(stock_symbol, "Stock Price with Moving Averages & Signals"),
           x = "Date", y = "Price") +
      theme_minimal()
    
    print(p)
  })
}

# Run the Shiny application
shinyApp(ui, server)