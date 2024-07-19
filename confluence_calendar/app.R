library(shiny)
library(dplyr)
library(purrr)
library(writexl)
library(stringr)
library(lubridate)
library(tidyr)
library(readr)

source("functions.R")

# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

ui <- fluidPage(
  titlePanel("Confluence Calendar Aggregation Tool"),

  sidebarLayout(
    sidebarPanel(
      numericInput(
        "year_to_filter",
        "Year to aggregate",
        2024,
        min = 2020,
        max = NA,
        step = 1
      ),
      fileInput("file1", "Upload Confluence iCalendar export", accept = ".ics"),
      fileInput("file2", "Optionally upload names mapping", accept = ".csv"),
    ),

    mainPanel(tableOutput("contents"),
              downloadButton("dl", "Download"))
  ))

server <- function(input, output) {
  output$contents <- renderTable({
    file <- input$file1
    ext <<- tools::file_ext(file$datapath)

    req(file)
    validate(need(ext == "ics", "Please upload an ics file"))

    parsed_ics <<- parse_ics( input$file1$datapath, filter_year = input$year_to_filter, names_mapping_file = input$file2$datapath)
    parsed_ics
  })
  output$dl <- downloadHandler(
    filename = function() { "aggregation.xlsx"},
    content = function(file) {write_xlsx(parse_ics( input$file1$datapath, filter_year = input$year_to_filter, names_mapping_file = input$file2$datapath), path = file)}
  )
}

shinyApp(ui = ui, server = server)
