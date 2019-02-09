confelli <-function(mu, Sigma,n, df, level = 0.95, xlab = "", ylab = "", add=T, prec=51){
    d <- sqrt(diag(Sigma))
    dfvec <- c(2, df)
    phase <- acos(Sigma[1, 2]/(d[1] * d[2]))
    angles <- seq( - (pi), pi, len = prec)
    cte<- (dfvec[1]*(n-1))/(dfvec[2]*n)
    mult <- sqrt(cte * qf(level, dfvec[1], dfvec[2]))
    xpts <- mu[1] + d[1] * mult * cos(angles)
    ypts <- mu[2] + d[2] * mult * cos(angles + phase)
    if(add) lines(xpts, ypts)
    else plot(xpts, ypts, type = "l", xlab = xlab, ylab = ylab)
}
mu=c(-9.36, 13.27)
sigma=matrix(c(199.26,88.38,88.38,418.61), nc=2, nr=2, byrow=T)
df=9
n=11
confelli(mu, sigma, n, df, level=0.95, xlab="", ylab="", add=F)
points(mu[1],mu[2], pch=19)
points(0,0, col=2, pch=19)
grid(10, 10, lwd = 2)
