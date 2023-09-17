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
        radioButtons(
          "city", "Select a city",
          choices = c(avail_choices, "San Antonio")
        ) 
      ),
      mainPanel( 
        plotOutput("plot")
      )
    )
  ),
  server = function(input, output, session) {
    output$plot = renderPlot({
      # 
      stopifnot(input$city %in% d$city)
      d |>
        filter(city %in% input$city) |>
        ggplot(aes(x=time, y=temp, color=city)) +
        geom_line()
    })
  }
)



