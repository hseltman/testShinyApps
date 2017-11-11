library(shiny)
fluidPage(
sidebarLayout(
sidebarPanel(
sliderInput('n', 'n', min=0, max=10, value=5),
),
mainPanel(p('Empty Main Panel'))))

