library (shiny)  
shinyUI (pageWithSidebar (
  headerPanel ("Example plot"),
  sidebarPanel (
    sliderInput ('mu', 'Guessat the mean',value =  70, min = 62 , max =  74, step =  0.05 ,)
  ),
  mainPanel (
    plotOutput ('new Hist ')
  )
))