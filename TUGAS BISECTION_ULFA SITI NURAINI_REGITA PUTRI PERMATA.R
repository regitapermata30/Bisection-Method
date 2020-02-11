
xr=c(); xbb=c(); xba=c(); fxbb=c(); fxr=c();kali=c();err=c()
bisec <- function(x) -12-21*x+18*x^2-2.7*x^3 #function
xbb[2] <- -1 #initial bawah
xba[2] <- 0  #initial atas
e <- 10^-6   #error
i<- 2
xr[1]=0;xr[1]
err[1]=0; err[2]=0

repeat {
  xr[i]<-(xbb[i]+xba[i])/2
  fxr[i]<-bisec(xr[i])
  fxbb[i]<-bisec(xbb[i])
  kali[i]<-fxbb[i]*fxr[i]
  if (kali[i] > 0){
    xbb [i+1] <- xr [i]
    xba [i+1] <- xba[i]} 
  else {
    xba[i+1]<-xr[i]
    xbb[i+1]<- xbb[i]}
  err[i]=abs(xr[i]-xr[i-1])
  if(err[i]< e){break}
  i<- i+1
}
df = cbind(xr,err)
df = df[-1,]; df
print("solution for equation is ") ;round(xr[i-1],4)
