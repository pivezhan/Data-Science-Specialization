columnmean <- function(y, removeNA=TRUE) {
        nc<-ncol(y)
        means<-numeric(nc)
        for(i in 1:nc){
                means[i]<-mean(y[,i],na.rm=removeNA)
        }
        means
}

make.power<-function(n) {
        pow<-function(x){
                x^2
        }
        pow
}

y<-10
f<-function(x){
        y<-2
        y^2+g(x)  
}
g<-function(x){
        x+y
}

make.NegLogLik <- function(data, fixed=c(FALSE,FALSE)) {
        params <- fixed
        function(p) {
                params[!fixed] <- p
                mu <- params[1]
                sigma <- params[2]
                a <- -0.5*length(data)*log(2*pi*sigma^2)
                b <- -0.5*sum((data-mu)^2) / (sigma^2)
                -(a + b)
        } 
}
nLL <- make.NegLogLik(normals, c(1, FALSE))
x <- seq(0, 1.9, len = 100)
y <- sapply(x, nLL)
plot(x, exp(-(y - min(y))), type = "l")
