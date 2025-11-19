library(shiny)
library(UsingR)
n=10
shinyServer(
function(input, output) {
output$newHist <- renderPlot({
n <- input$id1
set.seed(123);
temp <- matrix(sample(1 : 6, n * 10000, replace = TRUE), ncol = n)
temp <- apply(temp, 1, mean)
hist(temp, xlab='Mean Values', 
col='lightblue',main='Histogram')
mu <- input$mu
lines(c(mu, mu), c(0, 2000),col="red",lwd=5)
mse <- mean((temp - mu)^2)
output$oid2 <- renderPrint(mse)
        })
})
