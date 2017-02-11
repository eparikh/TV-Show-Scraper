suppressMessages({
library(shiny)
})

source("helpers.R")

facetPlots <- getFacetPlots()

shinyServer(function(input, output){
  
  thePlot <- reactive({facetPlots[[input$plotType]]})
  
  # plot the master scatter plot w/ facet wrap
  output$facetPlot <- renderPlot(thePlot())
  
  output$detailPlot <- renderPlot({
    if(!is.na(detailPlot$title)){
      getDetailPlot(
        detailPlot$data,
        detailPlot$title,
        detailPlot$xAxis,
        detailPlot$yAxis,
        detailPlot$xRange,
        detailPlot$yRange
      )
    }
  })
  
  # get chosen values from master plot
  detailPlot <- reactiveValues(
    xAxis = NA,
    yAxis = NA,
    xRange = NA,
    yRange = NA,
    title = NA,
    data = NA
  )
  
  observe({
    brush <- input$facetPlot_brush
    if (!is.null(brush)) {
      detailPlot$xAxis = brush$mapping$x
      detailPlot$yAxis = brush$mapping$y
      detailPlot$xRange = c(brush$xmin, brush$xmax)
      detailPlot$yRange = c(brush$ymin, brush$ymax)
      
      facet <- brush$mapping$panelvar1
      ofType <- brush$panelvar1
      
      detailPlot$title = paste(ofType, ": ", detailPlot$yAxis, " by ", detailPlot$xAxis)
      detailPlot$data = thePlot()$data[thePlot()$data[facet]==ofType,]
      
    } else {
      detailPlot$xAxis = NA
      detailPlot$yAxis = NA
      detailPlot$xRange = NA
      detailPlot$yRange = NA
      detailPlot$title = NA
      detailPlot$data = NA
    }
  })
  

})