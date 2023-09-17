library(tidyverse)
library(shiny)
d = readr::read_csv(here::here("data/weather.csv"))
theme_set(theme_bw())
avail_choices = d |> 
                  select(city) |>
                  unique() |>
                  pull()

shinyApp(
  ui = fluidPage(
    titlePanel("Temperature Forecasts"),
    sidebarLayout(
      sidebarPanel(
        selectInput("city", label = h3("Select a city"), 
                    choices = avail_choices, 
                    selected = 1),
        hr(),
        fluidRow(column(3, verbatimTextOutput("value")))
      ),
      mainPanel( plotOutput("plot") )
    )
  ),
  server = function(input, output, session) {
    output$plot = renderPlot({
      d |>
        filter(city %in% input$city) |>
        ggplot(aes(x=time, y=temp, color=city)) +
        geom_line()
    })
  }
)
