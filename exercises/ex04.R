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
    titlePanel("Weather Forecasts"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "city", "Select a city",
          choices = avail_choices
        ),
        selectInput(
          "var", "Select a variable",
          choices = c()
          # , 
          # selected = "temp"
        )
      ),
      mainPanel( 
        plotOutput("plot")
      )
    )
  ),
  server = function(input, output, session) {
    
    d_city = reactive({
      d |>
        filter(city %in% input$city)
    })
    
    observe({
      updateSelectInput(inputId = "var", choices = d_vars())
    })
    
    d_vars = reactive({
      d |>
        select(where(is.numeric)) |>
        select(where(function(x) var(x) != 0)) |>
        names()      
    })  

    output$plot = renderPlot({
      d_city() |>
        ggplot(aes(x=time, y=.data[[input$var]], color=city)) +
        ggtitle(input$var) +
        geom_line()
    })
    
  }
)
