suppressMessages({
  library(shiny)
})

shinyUI(fluidPage(
  
  titlePanel("TV Show Explorer"),
  
  fluidRow(
    column(
      12,
      wellPanel(
        selectInput(
          inputId = "plotType",
          label = "Select a plot type:",
          choices = list(
            Genre = c(
              "Count of Shows" = "genre_count",
              "Median Rating" = "genre_rating",
              "Median Number of Years" = "genre_years"
            ),
            Network = c(
              "Count of Shows" = "network_count",
              "Median Rating" = "network_rating",
              "Median Number of Years" = "network_years"
            )
          )
          )
        )       
      )# end column

  ), # end row
  fluidRow(
    column(
      7,
      plotOutput(
        "facetPlot",
        height = "600px",
        brush = brushOpts(
          id = "facetPlot_brush",
          resetOnNew = TRUE
        )
      )# end plot
    ),# end column
    column(
      5,
      plotOutput("detailPlot", height = "600px")
    )
  )# end row
))