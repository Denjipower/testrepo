G<-function(x){
  return((x^2)*exp(-x))
}

G1<-function(x){
  return((x^2)*(1-x)^3*exp(-x^2))
}

montecarlo<-function(G,a,b,M){
  s<-0
  for(i in 1:M){
    s<- s+G(a+(b-a)*runif(1,0,1))
  }
  return(((b-a)/M)*s)
}

montecarloinf<-function(G,a,b,M){
  s<-0
  for(i in 1:M){
    s<- s+G(a+(b-a)*rnorm(1))
  }
  return(((b-a)/M)*s)
}

