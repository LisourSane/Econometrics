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
cor(dane[1],dane[2])
cor(total.sum.of.scores.from.6.problems.,Population..total)

