library(tidyverse)
library(shiny)
d = readr::read_csv(here::here("data/weather.csv"))
avail_choices = d |> 
                  select(city) |>
                  unique() |>
                  pull()
d_vars = d |>
  select(where(is.numeric)) |>
  names()

shinyApp(
  ui = fluidPage(
    titlePanel("Weather Forecasts"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "city", "Select a city",
          choices = avail_choices
        ),
        selectInput(
          "var", "Select a variable",
          choices = d_vars, selected = "temp"
        )
      ),
      mainPanel( 
        plotOutput("plot"),
        tableOutput("minmax")
      )
    )
  ),
  server = function(input, output, session) {
    output$plot = renderPlot({
      d |>
        filter(city %in% input$city) |>
        ggplot(aes(x=time, y=.data[[input$var]])) +
        ggtitle(input$var) +
        geom_line()
    })
    
    output$minmax = renderTable({
      minmax_names = c("min", "max")
      names(minmax_names) = c(
        "min" = paste("min", input$var, sep = "_"), 
        "max" = paste("max", input$var, sep = "_")
      )
      
      d |>
        filter(city %in% input$city) |>
        mutate(
          day = lubridate::wday(time, label = TRUE, abbr = FALSE),
          date = as.character(lubridate::date(time))
        ) |>
        group_by(date, day) |>
        summarize(
          `min` = min(.data[[input$var]]),
          `max` = max(.data[[input$var]]),
          .groups = "drop"
        ) |> 
        rename(!!!minmax_names)
      
    })
  }
)
