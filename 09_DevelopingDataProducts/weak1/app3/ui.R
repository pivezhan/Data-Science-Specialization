shinyUI(pageWithSidebar(
headerPanel ("Example plot"),
sidebarPanel (
sliderInput ('mu', 'Guessat the mean',value = 70, min = 62, max =  74, step = 0.05,)
,textInput(inputId= "text1" ,  label =  "The Mean Value" )
,submitButton('Submit')),

mainPanel (
p('Manual Mean'),
textOutput('text1'),
plotOutput ('newHist')
)
))