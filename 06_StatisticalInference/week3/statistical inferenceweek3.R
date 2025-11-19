
## normal distribution with different degree of freedom
library(ggplot2); library(manipulate)
k <- 1000
xvals <- seq(-5, 5, length = k)
myplot <- function(df){
        d <- data.frame(y = c(dnorm(xvals), dt(xvals, df)),
                        x = xvals,
                        dist = factor(rep(c("Normal", "T"), c(k,k))))
        g <- ggplot(d, aes(x = x, y = y)) 
        g <- g + geom_line(size = 2, aes(colour = dist))
        g
}
manipulate(myplot(mu), mu = slider(1, 20, step = 1))  

## evaluation of quantiles
library(ggplot2); library(manipulate)

pvals <- seq(.5, .99, by = .01)
myplot2 <- function(df){
        d <- data.frame(n= qnorm(pvals),t=qt(pvals, df),
                        p = pvals)
        g <- ggplot(d, aes(x= n, y = t))
        g <- g + geom_abline(size = 2, col = "lightblue")
        g <- g + geom_line(size = 2, col = "black")
        g <- g + geom_vline(xintercept = qnorm(0.975))
        g <- g + geom_hline(yintercept = qt(0.975, df))
        g
}
manipulate(myplot2(df), df = slider(1, 20, step = 1))

##sleep data
data(sleep)
head(sleep)

##plotting the data
library(ggplot2)
g <- ggplot(sleep, aes(x = group, y = extra, group = factor(ID)))
g <- g + geom_line(size = 1, aes(colour = ID)) + geom_point(size =10, pch = 21, fill = "salmon", alpha = .5)
g

##results
g1 <- sleep$extra[1 : 10]; g2 <- sleep$extra[11 : 20]
difference <- g2 - g1
mn <- mean(difference); s <- sd(difference); n <- 10

mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)##c(-1,1) means +/- equation
t.test(difference)
t.test(g2, g1, paired = TRUE)
t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)

rbind(
        mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n),
        as.vector(t.test(difference)$conf.int),
        as.vector(t.test(g2, g1, paired = TRUE)$conf.int),
        as.vector(t.test(extra ~ I(relevel(group, 2)), paired = TRUE, data = sleep)$conf.int)
)

##example
sp <- sqrt((7 * 15.34^2 + 20 * 18.23^2) / (8 + 21 - 2))
132.86 - 127.44 + c(-1, 1) * qt(.975, 27) * sp * (1 / 8 + 1 / 21)^.5

# Mistakenly treating the sleep data as grouped
g1 <- sleep$extra[1 : 10]; g2 <- sleep$extra[11 : 20]
n1 <- length(g1); n2 <- length(g2)
sp <- sqrt( ((n1 - 1) * sd(g1)^2 + (n2-1) * sd(g2)^2) / (n1 + n2-2))
md <- mean(g2) - mean(g1)
semd <- sp * sqrt(1 / n1 + 1/n2)
rbind(
        md + c(-1, 1) * qt(.975, n1 + n2 - 2) * semd,  
        t.test(g2, g1, paired = FALSE, var.equal = TRUE)$conf,
        t.test(g2, g1, paired = TRUE)$conf
)

# Grouped versus independent
library(ggplot2)
g <- ggplot(sleep, aes(x = group, y = extra, group = factor(ID)))
g <- g + geom_line(size = 1, aes(colour = ID)) + geom_point(size =10, pch = 21, fill = "salmon", alpha = .5)
g

## ChickWeight data in R
library(datasets); data(ChickWeight); library(reshape2)

##define weight gain or loss
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
names(wideCW)[-(1 : 2)] <- paste("time", names(wideCW)[-(1 : 2)], sep = "")
library(dplyr)
wideCW <- mutate(wideCW,
                 gain = time21 - time0)

# Plotting the raw data
g <- ggplot(ChickWeight, aes(x = Time, y = weight, colour = Diet, group = Chick))
g <- g + geom_line()
g <- g + stat_summary(aes(group = 1), geom = "line", fun.y = mean, size = 1, col = "black")
g <- g + facet_grid(. ~ Diet)
g

# Weight gain by diet

g <- ggplot(wideCW, aes(x = factor(Diet), y = gain, fill = factor(Diet)))
g <- g + geom_violin(col = "black", size = 2)
g


## t interval
wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
rbind(
        t.test(gain ~ Diet, paired = FALSE, var.equal = TRUE, data = wideCW14)$conf,
        t.test(gain ~ Diet, paired = FALSE, var.equal = FALSE, data = wideCW14)$conf
)

# T test in R
library(UsingR); data(father.son)
t.test(father.son$sheight - father.son$fheight)


# chickWeight data
# Recall that we reformatted this data
library(datasets); data(ChickWeight); library(reshape2)

##define weight gain or loss
wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
names(wideCW)[-(1 : 2)] <- paste("time", names(wideCW)[-(1 : 2)], sep = "")
library(dplyr)
wideCW <- mutate(wideCW,
                 gain = time21 - time0
)

# Unequal variance T test comparing diets 1 and 4
wideCW14 <- subset(wideCW, Diet %in% c(1, 4))
t.test(gain ~ Diet, paired = FALSE, 
       var.equal = TRUE, data = wideCW14)

# Revisiting an earlier example
choose(8, 7) * .5 ^ 8 + choose(8, 8) * .5 ^ 8 
pbinom(6, size = 8, prob = .5, lower.tail = FALSE)

##poison example
ppois(9, 5, lower.tail = FALSE)
