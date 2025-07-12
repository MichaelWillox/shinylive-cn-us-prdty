library(shiny)
library(tidyverse)
library(munsell)
library(readr)

# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
 tag <- shiny::downloadButton(...)
 tag$attribs$download <- NULL
 tag
}

# Load pre-cleaned data
# Assuming 'df_clean.RDS' is in the same directory as the app.R file
# For demonstration purposes, creating a dummy df_clean if the file is not found
if (!file.exists("df_clean.RDS")) {
 df_clean <- tibble(
  year = rep(1980:2020, each = 2, times = 3),
  country = rep(c("Canada", "United States"), times = length(1980:2020) * 3),
  industry_name = rep(c("Business sector", "Manufacturing", "Retail Trade"), each = length(1980:2020) * 2),
  `Real GDP per Hour Worked` = runif(length(year), 50, 150),
  `Labor Productivity` = runif(length(year), 60, 160)
 )
 # Save a dummy RDS for local testing if needed
 # saveRDS(df_clean, "df_clean.RDS")
} else {
 df_clean <- readRDS("df_clean.RDS")
}


# Variable choices for dropdown
variable_choices <- setdiff(names(df_clean), c("year", "country", "industry_name", "naics"))

# Year and industry choices
year_choices <- sort(unique(df_clean$year))
industry_choices <- sort(unique(df_clean$industry_name))

# UI
ui <- fluidPage(
 titlePanel("Canada–U.S. Productivity Comparison"),
 
 sidebarLayout(
  sidebarPanel(
   tags$p(strong("Note:"), "The Start Year must be earlier than the End Year."),
   selectInput("variable", "Select Variable:", choices = variable_choices,
               selected = "Real GDP per Hour Worked"),
   selectInput("industry", "Industry:", choices = industry_choices,
               selected = "Business sector"),
   selectInput("start_year", "Start Year:", choices = year_choices, selected = 1987),
   selectInput("end_year", "End Year:", choices = year_choices, selected = max(year_choices)),
   selectInput("index_year", "Index Year (Set to 100):", choices = year_choices, selected = 1987),
   downloadButton("download_data", "Download Filtered Data"),
   uiOutput("year_warning_message"),
   tags$p(strong("Note:"), "The Index Year must be within the Start and End Year range."),
   uiOutput("index_year_warning_message")
  ),
  
  mainPanel(
   tags$h3(strong("Indexed Measures of Productivity and Related Variables")),
   plotOutput("line_plot"),
   br(),
   textOutput("growth_gap_text"),
   # Changed textOutput to htmlOutput for wrapping
   htmlOutput("cumulative_growth_text"),
   br(),
   tableOutput("preview_table")
  )
 )
)

# Server
server <- function(input, output, session) {
 
 output$year_warning_message <- renderUI({
  start_y <- as.integer(input$start_year)
  end_y <- as.integer(input$end_year)
  if (start_y >= end_y) {
   tags$p(style = "color: red; font-weight: bold;",
          "Warning: Start Year must be earlier than End Year.")
  } else {
   NULL # No message if years are valid
  }
 })
 
 output$index_year_warning_message <- renderUI({
  start_y <- as.integer(input$start_year)
  end_y <- as.integer(input$end_year)
  index_y <- as.integer(input$index_year)
  
  if (index_y < start_y || index_y > end_y) {
   tags$p(style = "color: red; font-weight: bold;",
          "Warning: Index Year must be between Start Year and End Year.")
  } else {
   NULL # No message if index year is valid
  }
 })
 
 # Reactive filtered and indexed data
 filtered_data <- reactive({
  req(input$variable, input$start_year, input$end_year, input$index_year, input$industry)
  
  start_y <- as.integer(input$start_year)
  end_y <- as.integer(input$end_year)
  index_y <- as.integer(input$index_year)
  
  validate(
   need(start_y < end_y, "Start Year must be earlier than End Year. Please adjust your selections.")
  )
  
  validate(
   need(index_y >= start_y && index_y <= end_y,
        "Index Year must be between the Start Year and End Year. Please adjust your selections.")
  )
  
  df_clean %>%
   filter(year >= start_y,
          year <= end_y,
          industry_name == input$industry) %>%
   select(year, country, industry_name, value = all_of(input$variable)) %>%
   group_by(country) %>%
   mutate(index_value = value[year == as.integer(input$index_year)],
          value_indexed = ifelse(index_value > 0, 100 * value / index_value, NA_real_)) %>%
   ungroup()
 })
 
 # Reactive expression to calculate the growth rate gap and message
 growth_gap_info <- reactive({
  req(input$start_year, input$end_year) # Ensure these are available
  
  # Ensure filtered_data is ready and valid before proceeding
  data_for_gap <- filtered_data()
  
  end_y <- as.integer(input$end_year)
  start_y <- as.integer(input$start_year)
  
  # Filter data for the end year
  data_at_end_year <- data_for_gap %>%
   filter(year == end_y)
  
  if (nrow(data_at_end_year) > 0 && all(!is.na(data_at_end_year$value_indexed))) {
   max_val <- max(data_at_end_year$value_indexed, na.rm = TRUE)
   min_val <- min(data_at_end_year$value_indexed, na.rm = TRUE)
   gap <- round(max_val - min_val, 2) # Round to 2 decimal places
   
   # Formulate the message
   message <- paste0("The growth rate gap from ", start_y, " to ", end_y, " is ", gap, " percentage points.")
   list(max_val = max_val, min_val = min_val, gap_message = message)
  } else {
   list(max_val = NA, min_val = NA, gap_message = "Data not available or invalid for the selected End Year.")
  }
 })
 
 # Reactive expression to calculate cumulative annual growth rates
 cumulative_growth_rates <- reactive({
  req(input$start_year, input$end_year)
  
  data_for_growth <- filtered_data()
  start_y <- as.integer(input$start_year)
  end_y <- as.integer(input$end_year)
  
  # Ensure a valid time period for calculation
  if (end_y <= start_y) {
   return(list(canada_growth = NA, us_growth = NA, message = "Cannot calculate growth rate: End Year must be after Start Year."))
  }
  
  # Function to calculate CAGR
  calculate_cagr <- function(initial_index, final_index, years) {
   if (is.na(initial_index) || is.na(final_index) || initial_index == 0 || years <= 0) {
    return(NA_real_)
   }
   100 * ((final_index / initial_index)^(1/years) - 1)
  }
  
  canada_data <- data_for_growth %>% filter(country == "Canada")
  us_data <- data_for_growth %>% filter(country == "United States")
  
  canada_start_index <- canada_data$value_indexed[canada_data$year == start_y]
  canada_end_index <- canada_data$value_indexed[canada_data$year == end_y]
  
  us_start_index <- us_data$value_indexed[us_data$year == start_y]
  us_end_index <- us_data$value_indexed[us_data$year == end_y]
  
  years_diff <- end_y - start_y
  
  canada_cagr <- calculate_cagr(canada_start_index, canada_end_index, years_diff)
  us_cagr <- calculate_cagr(us_start_index, us_end_index, years_diff)
  
  canada_cagr_formatted <- if (is.na(canada_cagr)) "N/A" else paste0(round(canada_cagr, 2), "%")
  us_cagr_formatted <- if (is.na(us_cagr)) "N/A" else paste0(round(us_cagr, 2), "%")
  
  # Message to include the full text 
  message <- paste0("The cumulative annual growth rate (CAGR) for Canada is ",
                    canada_cagr_formatted, " compared to ", us_cagr_formatted, " for the United States. ",
                    "Note that the formula for CAGR is nonlinear, and therefore, non-additive. Adding or subtracting CAGRs can be misleading, particularly when growth rates are large.")

  list(canada_growth = canada_cagr, us_growth = us_cagr, message = message)
 })

 # Plot
 output$line_plot <- renderPlot({
  plot_data <- filtered_data()
  gap_info <- growth_gap_info() # Get the reactive values
  
  p <- ggplot(plot_data, aes(x = year, y = value_indexed, color = country)) +
   geom_line(linewidth = 1.2) +
   scale_color_manual(values = c("Canada" = "red", "United States" = "blue")) +
   labs(
    title = paste0(input$variable, " (Indexed to 100 in ", input$index_year, ")"),
    subtitle = paste("Industry:", input$industry),
    x = "Year", y = "Index (Base Year = 100)", color = "Country"
   ) +
   theme_minimal(base_size = 14) +
   theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(face = "bold", size = 12)
   )
  
  # Add the brace and text to the plot
  if (!is.na(gap_info$max_val) && !is.na(gap_info$min_val)) {
   end_y <- as.integer(input$end_year)
   # Calculate a small offset for the brace position relative to the plot's x-axis range
   x_offset <- (max(plot_data$year) - min(plot_data$year)) * 0.02
   x_brace_start <- end_y + x_offset
   x_brace_end <- x_brace_start + (max(plot_data$year) - min(plot_data$year)) * 0.01 # Small horizontal extension
   
   p <- p +
    # Top horizontal segment of the brace
    annotate("segment", x = x_brace_start, xend = x_brace_end,
             y = gap_info$max_val, yend = gap_info$max_val,
             color = "black", size = 0.8) +
    # Bottom horizontal segment of the brace
    annotate("segment", x = x_brace_start, xend = x_brace_end,
             y = gap_info$min_val, yend = gap_info$min_val,
             color = "black", size = 0.8) +
    # Vertical segment connecting top and bottom horizontal lines
    annotate("segment", x = x_brace_end, xend = x_brace_end,
             y = gap_info$min_val, yend = gap_info$max_val,
             color = "black", size = 0.8) +
    # Text label for the gap value next to the brace
    annotate("text", x = x_brace_end + (max(plot_data$year) - min(plot_data$year)) * 0.005, # Slightly right of brace
             y = (gap_info$min_val + gap_info$max_val) / 2,
             label = round(gap_info$max_val - gap_info$min_val, 2),
             color = "black", size = 4, hjust = 0) # hjust=0 aligns left
  }
  p
 })
 
 # Preview Table
 output$preview_table <- renderTable({
  filtered_data() %>%
   select(Year = year,
          Country = country,
          Industry = industry_name,
          Value = value,
          Index = value_indexed) %>%
   head(10)
 })
 
 # Render the growth gap text message
 output$growth_gap_text <- renderText({
  gap_info <- growth_gap_info()
  gap_info$gap_message
 })
 
 # Render the cumulative growth rate text message as HTML for wrapping 
 output$cumulative_growth_text <- renderUI({ # Changed to renderUI
  growth_rates <- cumulative_growth_rates()
  tags$p(growth_rates$message) # Wrapped in tags$p for HTML rendering and wrapping
 })

 # Download handler
 output$download_data <- downloadHandler(
  filename = function() {
   paste0("cn_us_", gsub(" ", "_", tolower(input$variable)), "_", Sys.Date(), ".csv")
  },
  content = function(file) {
   filtered_data() %>%
    select(Year = year,
           Country = country,
           Industry = industry_name,
           Value = value,
           Index = value_indexed) %>%
    write_csv(file)
  }
 )
}

# Run the app
shinyApp(ui = ui, server = server)
