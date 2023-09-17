library(tidyverse)
library(shiny)
d = readr::read_csv(here::here("data/weather.csv"))
theme_set(theme_bw())
avail_choices = d |> 
                  select(city) |>
                  unique() |>
                  pull()
mydat = d |> select(city, time, temp, source) |> 
  filter(source == "obs") |>
  mutate(weekday = lubridate::wday(time, label = TRUE)) |>
  mutate(date = lubridate::date(time)) |>
  group_by(city, weekday) |>
  summarize(min = min(temp), max = max(temp)) |> 
  ungroup()
# reactlog::reactlog_enable()

shinyApp(
  ui = fluidPage(
    titlePanel("Temperature Forecasts"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "city", "Select a city",
          choices = avail_choices
        ),
        checkboxInput("forecast", "Highlight forecasted data", value = FALSE)
      ),
      mainPanel( plotOutput("plot") ,
                 tableOutput("table"))
    )
  ),
  server = function(input, output, session) {
    output$plot = renderPlot({
      
      d_city = filter(d, city %in% input$city)
      
      if (input$forecast) {
        ggplot(d_city, aes(x=time, y=temp, color=source)) +
          geom_line() +
          scale_color_manual(values = c("red","black"))
      } else {
        ggplot(d_city, aes(x=time, y=temp), color=1) +
          geom_line()
      }
    })
    
    output$table = renderTable({
      filter(mydat, city %in% input$city) |> 
        select(-city)
    })
    
  }
)

# reactlog::reactlog_disable()
