install.packages("ggplot2")
library(ggplot2)
#qqplot compare the underlying distribution of the two variables

#normal distribution
set.seed(42)
x <- rnorm(100) #N(0,1)
qqnorm(x)
qqline(x)

y <- rgamma(100,1)
qqnorm(y)
qqline(y)

#Chiquared distribution

z <- rchisq(500, df=3)
qqplot(qchisq(ppoints(500),df=3),z, main=expression("Q-Q plot for chisq distribution"))
qqline(y, distribution = function(p)qchisq(p,df=3), prob = c(0.1,0.6), col=2)

install.packages("ISwR")
library(ISwR)
data(thuesen)
attach(thuesen)

options(na.action = na.exclude())
qqnorm(short.velocity)
qqline(short.velocity)
velcoity.lm <- lm(short.velocity~blood.glucose)
summary(velcoity.lm)

install.packages("UsingR")
library(UsingR)
install.packages("reshape")
library(reshape)

data("galton")
long <- melt(galton)
g <- ggplot(long, aes(x=value, fill=variable))
g <- g + geom_histogram(colour = "black", binwidth = 1)
g <- g + facet_grid(.~variable)
g

#least square
install.packages("manipulate")
library(manipulate)
myHis <- function(mu){
  mse <- mean(galton$child - mu)^2
   g <- ggplot(galton, aes(x=child) + geom_histogram(fill="salmon", colour ="black", binwidth = 1))
   g <- g + geom_vline(xintercept = mu, size =3)
   g <- g + ggtitle(paste("mu=", mu, " ","MSE= ", round(mse,2), sep = ""))
   g
}   
   h <- manipulate(myHist(a), a =slider(62,74, step=0.5))

#dotplot
ggplot(galton, aes(x=parent, y=child)) + geom_point()

freqqData <- as.data.frame(table(galton))
#size of the dots to represent the number
Plot(as.numeric(as.vector(frewData$parent)),as.numeric(as.vector(freqData$child)), pch=21, col="black", bg = "lightblue", cex = .05*freqData$freq, xlab ="parent", ylab = "child"

myPlot <- function(beta){
  y <- galton$child-mean(galton$child)
  x <- galton$parent - mean(galton$parent)
  
  plot(
    as.numeric(as.vector(freqData$parent)),
    as.numeric(as.vector(freqData$child)),
    pch=21, col = "black", bg = "lightblue", cex =0.05* freqData$freq,
    xlab = "parent"
    ylab = "child"
  )
  abline(0,beta, lwd = 3)
  points(0,0, cex =2, pch =19)
  mse <-mean((y-beta*x)^2)
  title(paste("beta =", beta, "mse = ", round))
  
}


model <- lm(I(child - mean(child)) ~ I(parent - mean))-1, data=galton)

  )
}