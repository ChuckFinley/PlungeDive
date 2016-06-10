library(dplyr)
library(shiny)
library(oce)

source('TdrHelper.R')

shinyServer(function(input, output) {
  # For now, plot the first event from deployment 388
  fastlog <- fetch.fastlog(388, 1)
  output$dive.plot <- renderPlot(plot.fastlog(fastlog))
  
  # Display x,y coordinates of last-clicked point
  output$click.point <- renderText({
    # Get near poins
    click.point <- nearPoints(fastlog,
                              input$click.dive,
                              maxpoints = 1,
                              threshold = 10)
    
    # If no nearby points have been clicked, display the empty string
    if(nrow(click.point) == 0) return('')
    
    # Otherwise, display x and y coordinates
    sprintf('(%s,%s)', 
            format(click.point$UTC, '%b %e %H:%M:%OS1'), 
            format(click.point$Pressure, digits = 3))
  })
})