### Ejemplo 6.3 
confelli2 <-function(mu, Sigma,n, df, level = 0.95, xlab = "", ylab = "", add=T, prec=51){
  d <- sqrt(diag(Sigma))
  dfvec <- c(2, df)
  phase <- acos(Sigma[1, 2]/(d[1] * d[2]))
  angles <- seq( - (pi), pi, len = prec)
  cte<- ((1/50)+(1/50))*(dfvec[1]*(n-2))/(dfvec[2])
  mult <- sqrt(cte * qf(level, dfvec[1], dfvec[2]))
  xpts <- mu[1] + d[1] * mult * cos(angles)
  ypts <- mu[2] + d[2] * mult * cos(angles + phase)
  if(add) lines(xpts, ypts)
  else plot(xpts, ypts, type = "l", xlab = xlab, ylab = ylab)
}
xbar<-c(-1.9,0.2)
Spooled<-matrix(c(2,1,1,5), nr=2, nc=2)
VP<- eigen(Spooled)
e1<-VP$vectors[,1]
e2<-VP$vectors[,2]
lambda1<-VP$values[1]
lambda2<-VP$values[2]
{
  e.ma<-(xbar)-sqrt(lambda1)*sqrt(((1/50)+(1/50))*(2*98/97)*qf(0.95,2,97))*e1
  e.me<-(xbar)-sqrt(lambda2)*sqrt(((1/50)+(1/50))*(2*98/97)*qf(0.95,2,97))*e2
  e.ma1<-(xbar)+sqrt(lambda1)*sqrt(((1/50)+(1/50))*(2*98/97)*qf(0.95,2,97))*e1
  e.me1<-(xbar)+sqrt(lambda2)*sqrt(((1/50)+(1/50))*(2*98/97)*qf(0.95,2,97))*e2
}
plot(xbar[1],xbar[2], xlim=c(-2.5,1), ylim=c(-1,2), pch=19)
#points(c(e.ma[1],e.me[1],e.ma1[1],e.me1[1]),c(e.ma[2],e.me[2],e.ma1[2],e.me1[2]))
lines(c(e.me[1],e.me1[1]),c(e.me[2],e.me1[2]), lty =1)
lines(c(e.ma[1],e.ma1[1]),c(e.ma[2],e.ma1[2]), lty =1)
confelli2(xbar, Spooled, 100, 97, level=0.95, xlab="", ylab="", add=T)
abline(h=0, lty=2)
abline(v=0, lty=2)
