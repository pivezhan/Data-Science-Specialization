setwd("C:\\Users\\lenovo\\Desktop\\Rlearning\\data product\\week2")
library(UsingR)
install.packages("devtools")
library(devtools)
install_github('slidify','ramnathv')
install_github('slidifyLibraries','ramnathv')
library(slidify)

author('test')
slidify('index.Rmd')
library(knitr)
browseURL("index.html")

###RSTUDIO Presentation