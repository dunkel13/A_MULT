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
