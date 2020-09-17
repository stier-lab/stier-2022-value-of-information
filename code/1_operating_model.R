# Reparameterized logistic model with Allee effect and resource removal (Eq 1):

dbdt.fun <- function(B,K,A,r,F){
  r*B*(1-B/K)*(B/K-A/K)-F
}

B.vec <- seq(0, 100, by=1)

K = 100
r = 1.25
A.vec <- c(-10,0,10,20,30,500)

par(mfrow=c(3,2))

for (i in 1:length(A.vec)){
  A <- A.vec[i]
  Bmsy <- 70
  K <- -(3*Bmsy^2 - 2*A*Bmsy)/(A - 2*Bmsy)
  print(Bmsy)
  MSY <- 20
  r <- MSY/(Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K))
  Fmsy <- MSY/Bmsy
  
  dbdt<-sapply(B.vec, FUN=dbdt.fun, K=K, A=A, r=r, F=0)
  
  plot(B.vec, dbdt, type="l")
  lines(B.vec, Fmsy*B.vec, col="red")
}


par(mfrow=c(1,1))
A  <- 0
Bmsy <- 70
MSY <- 20
K <- -(3*Bmsy^2 - 2*A*Bmsy)/(A - 2*Bmsy)
r <- MSY/(Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K))
Fmsy <- MSY/Bmsy

# MCS: Analytical solution for equilibrium biomass?
b.star.fun <- function(F,K,A,r){
  max(0,(A*r + K*r + (-r*(4*F*K^2 - A^2*r - K^2*r + 2*A*K*r))^(1/2))/(2*r))
  }

F.vec <- seq(0, 2*Fmsy, length.out = 100)

b.star.vec <- sapply(F.vec, FUN=b.star.fun, K=K, A=A, r=r)
b.star.vec <- replace(b.star.vec,which(b.star.vec=="NaN"),0)


p <- 1 # price
c <- 10 # cost of fishing

plot(F.vec, p*F.vec*b.star.vec, 
     type="l", lwd=2, col="blue",
     xlab="Exploitation Rate",
     ylab="Catch Value or Cost")

lines(F.vec, c*F.vec, type="l", lwd=2, col="red")
