### Ejercicio 5.9
{ # datos
  xbar<-c(95.52,164.58,55.69,93.39,17.98,31.13)
  s1<-c(3266.46, 1343.97,731.54, 1175.50, 162.58, 238.37)
  s2<-c(1343.97,721.91,324.25,537.35,80.17,117.73)
  s3<-c(731.54,324.25,179.28,281.17,39.15,56.80)
  s4<-c(1175.50,537.35,281.17,474.98,63.73,94.85)
  s5<-c(162.68,80.17,39.15,63.73,9.95,13.88)
  s6<-c(238.37,117.73,56.80,94.85,13.88,21.26)
  S<-matrix(c(s1,s2,s3,s4,s5,s6), nr=6, nc=6, byrow = T )
  n=61
  alpha=0.05
}
{ # Punto A: IC simultaneos T^2 para muestra grande
  li<-NA
  ls<-NA
  for(i in 1:6){
    li[i]<-xbar[i]-sqrt(qchisq(0.95,6))*sqrt(S[i,i]/n)
    ls[i]<-xbar[i]+sqrt(qchisq(0.95,6))*sqrt(S[i,i]/n)
  }
  round(rbind(li,ls),2)
}
{ # Punto C: IC Bonferroni
  liB<-NA
  lsB<-NA
  for(i in 1:6){
    liB[i]<-xbar[i]-qt(1-alpha/(2*6),60)*sqrt(S[i,i]/n)
    lsB[i]<-xbar[i]+qt(1-alpha/(2*6),60)*sqrt(S[i,i]/n)
  }
  round(rbind(liB,lsB),2)
}
{ # Punto E: IC Bonferroni para una combinación lineal
  a<-c(0,0,0,0,-1,1)
  lc<-NA
  lc[1]<-a%*%xbar- qt(1-alpha/(2*7),60)*sqrt(t(a)%*%S%*%a/n)
  lc[2]<-a%*%xbar+ qt(1-alpha/(2*7),60)*sqrt(t(a)%*%S%*%a/n)
  round(lc,2)
}
{ # Punto B: Elipse del 95% de confianza muestra grande
  S.b<-matrix(c(S[1,1],S[1,4],S[4,1],S[4,4]), nr=2, nc=2, byrow=T)
  solve(S.b)
  VP<- eigen(S.b)
  e1<-VP$vectors[,1]
  e2<-VP$vectors[,2]
  lambda1<-VP$values[1]
  lambda2<-VP$values[2]
  e.ma<-c(xbar[1],xbar[4])-sqrt(lambda1)*sqrt(qchisq(0.95,2)/n)*e1
  e.me<-c(xbar[1],xbar[4])-sqrt(lambda2)*sqrt(qchisq(0.95,2)/n)*e2
  e.ma1<-c(xbar[1],xbar[4])+sqrt(lambda1)*sqrt(qchisq(0.95,2)/n)*e1
  e.me1<-c(xbar[1],xbar[4])+sqrt(lambda2)*sqrt(qchisq(0.95,2)/n)*e2
  plot(xbar[1],xbar[4], xlim=c(70,120), ylim=c(85,105))
  points(c(e.ma[1],e.me[1],e.ma1[1],e.me1[1]),c(e.ma[2],e.me[2],e.ma1[2],e.me1[2]))
  lines(c(e.me[1],e.me1[1]),c(e.me[2],e.me1[2]), lty =1)
  lines(c(e.ma[1],e.ma1[1]),c(e.ma[2],e.ma1[2]), lty =1)
  
  mu1<-c(e.ma[1],e.me[1],e.ma1[1],e.me1[1])
  mu4<-c(e.ma[2],e.me[2],e.ma1[2],e.me1[2])
  rbind(mu1, mu4)
  n*t(c(95.52-mu1[4],93.39-mu4[4]))%*%solve(S.b)%*%c(95.52-mu1[4],93.39-mu4[4])
  
  lines(seq(from=75, to=117, length.out=1000),rep(liB[4],1000), lty=2)
  lines(seq(from=75, to=117, length.out=1000),rep(lsB[4],1000), lty=2)
  lines(rep(liB[1],1000),seq(from=80, to=105, length.out=1000), lty=2)
  lines(rep(lsB[1],1000),seq(from=80, to=105, length.out=1000), lty=2)
  
  rbind(liB[c(1,4)],lsB[c(1,4)])
  rbind(li[c(1,4)],ls[c(1,4)])     
}
