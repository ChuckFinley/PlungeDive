library(dplyr)
library(shiny)

shinyUI(fluidPage(
  titlePanel('PlungeDive'),
  
  sidebarLayout(position = 'right',
                sidebarPanel('Phase Palette',
                             textOutput('click.point')),
                mainPanel('Dive Plot',
                          plotOutput('dive.plot',
                                     click = 'click.dive'))
  )
))
