library(shiny)
shinyUI(pageWithSidebar(
        headerPanel("CLT Theorem Evaluation"),
        sidebarPanel(
numericInput('id1', 'Enter Number of Samples', 50, min = 1, max = 1000, step = 1),
                sliderInput('mu', 'Guess at the mean',value = 3.5, min = 1, max = 6, step = 0.001,)
        ),
        mainPanel(
                h2('MSE'),
                verbatimTextOutput("oid2"),
                plotOutput('newHist')

        )
))
