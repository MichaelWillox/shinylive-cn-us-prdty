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
df_clean <- readRDS("df_clean.RDS")

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
   selectInput("variable", "Select Variable:", choices = variable_choices,
               selected = "Real GDP per Hour Worked"),
   selectInput("industry", "Industry:", choices = industry_choices,
               selected = "Business sector"),
   selectInput("start_year", "Start Year:", choices = year_choices, selected = 1987),
   selectInput("end_year", "End Year:", choices = year_choices, selected = max(year_choices)),
   selectInput("index_year", "Index Year (Set to 100):", choices = year_choices, selected = 1987),
   downloadButton("download_data", "Download Filtered Data")
  ),
  
  mainPanel(
   tags$h3(strong("Indexed Measures of Productivity")),
   plotOutput("line_plot"),
   br(),
   tableOutput("preview_table")
  )
 )
)

# Server
server <- function(input, output, session) {
 
 # Reactive filtered and indexed data
 filtered_data <- reactive({
  req(input$variable, input$start_year, input$end_year, input$index_year, input$industry)
  
  df_clean %>%
   filter(year >= as.integer(input$start_year),
          year <= as.integer(input$end_year),
          industry_name == input$industry) %>%
   select(year, country, industry_name, value = all_of(input$variable)) %>%
   group_by(country) %>%
   mutate(index_value = value[year == as.integer(input$index_year)],
          value_indexed = ifelse(index_value > 0, 100 * value / index_value, NA_real_)) %>%
   ungroup()
 })
 
 # Plot
 output$line_plot <- renderPlot({
  ggplot(filtered_data(), aes(x = year, y = value_indexed, color = country)) +
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
