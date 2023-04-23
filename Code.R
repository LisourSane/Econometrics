a=c()
for(i in 1:10){
  x=rnorm(10)
  a=append(a,x)
}
mean(x)
funkc=function(x){
  mean(x)
  return(mean(x))
}