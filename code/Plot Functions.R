dbdt.fun<-function(B,K,A,r,F) r*B*(1-B/K)*(B/K-A/K)-F

Blist<-seq(0,100,by=1)

K=100
r=1.25
A.list<-c(-10,0,10,20,30,500)
par(mfrow=c(3,2))

for (i in 1:length(A.list)){
  A=A.list[i]
  Bmsy<- 70
  K<--(3*Bmsy^2 - 2*A*Bmsy)/(A - 2*Bmsy)
  print(Bmsy)
  MSY<-20
  r<-MSY/(Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K))
  Fmsy<-MSY/Bmsy
  
  dbdt<-sapply(Blist,FUN=dbdt.fun,K=K,A=A,r=r,F=0)
  
  plot(Blist,dbdt,type="l")
  lines(Blist,Fmsy*Blist,col="red")
}


par(mfrow=c(1,1))
A=0
Bmsy=70
MSY<-20
K<--(3*Bmsy^2 - 2*A*Bmsy)/(A - 2*Bmsy)
r<-MSY/(Bmsy*(1-Bmsy/K)*(Bmsy/K-A/K))
Fmsy<-MSY/Bmsy

b.star.fun<-function(F,K,A,r) max(0,(A*r + K*r + (-r*(4*F*K^2 - A^2*r - K^2*r + 2*A*K*r))^(1/2))/(2*r))
F.list<-seq(0,2*Fmsy,length.out=100)

b.star.vec<-sapply(F.list,FUN=b.star.fun,K=K,A=A,r=r)
b.star.vec<-replace(b.star.vec,which(b.star.vec=="NaN"),0)

plot(F.list,p*F.list*b.star.vec,type="l",lwd=2,col="blue",xlab="Exploitation Rate",ylab="Catch Value or Cost")
lines(F.list,c*F.list,type="l",lwd=2,col="red")



