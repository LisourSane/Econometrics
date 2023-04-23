dane=data10
attach(data10)
summary(dane)
co=data.frame(dane)
View(co)
colnames(co)=c(seq(1,11,by=1))
cor(co,use = 'complete.obs')

for(i in 1:10){
  cat('Korelacja', colnames(co)[1], 'i', colnames(co)[i])
  print(cor(co[1],dane[i],use = 'complete.obs'))
}
total_score=c()
for(i in 1:11){
  total_score=append(total_score,cor(dane[1],dane[i],use = 'complete.obs'))
}

X=data.frame(colnames(dane),total_score)


