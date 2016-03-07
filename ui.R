shinyUI(pageWithSidebar(
  headerPanel("The World in Food"),
  sidebarPanel(
    sliderInput('slideYear', 'Select the Year',value = 1990, min = 1970, max = 2011, step = 1,)
  ),
  mainPanel(
    plotOutput('newplot')
  )
))
