source('/media/paco/myhdd/soft/dev/R/sourceDir.R')
sourceDir('/media/paco/myhdd/soft/dev/R/Rfunctions')

n <- 10
set.seed(8)
library(spatstat)
x <- psp(x0 = runif(n),
	      y0 = runif(n),
	      x1 = runif(n),
	      y1 = runif(n), owin())
plotSegments(x$ends)
plot(distmap(x), add = T)
lim <- c(0.1,0.9)
abline(h=lim, v=lim, col = 'gray')
pr <- percolate(x=x$ends, xlim=lim, ylim=lim)

plotSegments(x$ends[which(pr$g == 1), ],
	     add = T, segPar=list(col = "green"))

P <- selfcrossing.psp(x)
plot(P, add=TRUE, col="green")

