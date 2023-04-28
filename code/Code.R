data=read.csv("data/data10.csv",header = TRUE,sep = ';',row.names = 1,dec = ',')
install.packages(lmtest)
install.packages('car')
install.packages("tseries")
library(car)
library(tseries)
library(lmtest)
colnames(data)=c('total','population','GDP','GDP.per.cap','mortality','internet','area','life.expt','secondary','primary','sex')
attach(data)
summary(data)
View(data)
cor(data,use = 'pairwise.complete.obs')

par(mfrow=c(1,2)) #1000x500
plot(area,total)
plot(GDP,total)


model1=lm(total~population+GDP+GDP.per.cap+mortality+internet+area+life.expt+secondary+primary+sex)
summary(model1)
summary(model1)$r.squared
plot(model1)
par(mfrow=c(1,3))
plot(model1,which = c(1,2,3)) #1200x400 scale=0.515
          
#liniowość modelu
harvtest(model1)
raintest(model1)
resettest(model1)

#normalność błędów
shapiro.test(model1$residuals)
jarque.bera.test(model1$residuals)

#jednorodność wariancji
bptest(model1)
gqtest(model1)
hmctest(model1)
#niezależność reszt
dwtest(model1)
bgtest(model1)
#brak współniniowości
vif(model1)
