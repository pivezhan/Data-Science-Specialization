setwd(file.path("D:", "sbu", "Rlearning",
                "developing data product", "final project", "App"))

library(shiny)
runApp()
n<-80
temp <- matrix(sample(1 : 80, n * 10000, replace = TRUE), ncol = n)
temp <- apply(temp, 1, mean)
hist(temp)
function(input, output) {
        output$newHist <- renderPlot({
                hist(temp, xlab='child height', col='lightblue',main='Histogram')
                mu <- input$mu
                lines(c(mu, mu), c(0, 200),col="red",lwd=5)
                mse <- mean((temp - mu)^2)
                text(63, 150, paste("mu = ", mu))
                text(63, 140, paste("MSE = ", round(mse, 2)))
        })
}
### this procedure should follow exactly
setwd(file.path("C:", "Users", "lenovo", "Desktop", "Rlearning",
                "developing data product", "final project", "presentation"))
library(devtools)
library(slidify)
options(rpubs.upload.method = "internal")
options(RCurlOptions = list(verbose = FALSE, capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))
install_github('slidify', 'ramnathv')
install_github('slidifyLibraries', 'ramnathv')
