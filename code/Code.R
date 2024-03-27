data=read.csv("Świętochowski1_II.csv",header = TRUE,sep = ';',row.names = 1,dec = ',')
install.packages('lmtest')
install.packages('car')
install.packages("tseries")
install.packages('MASS')
install.packages('stats')
library(MASS)
library(car)
library(tseries)
library(lmtest)
library(stats)
colnames(data)=c('total','population','GDP','GDP.per.cap','mortality','internet','area','life.expt','secondary','primary','sex')
attach(data)
summary(data)
View(data)
data['Kosovo',]$internet = NA

cor(data,use = 'pairwise.complete.obs')

par(mfrow=c(1,2)) #1000x500
plot(area,total)
plot(GDP,total)


model1=lm(total~population+GDP+GDP.per.cap+mortality+internet+area+life.expt+secondary+primary+sex,data=data,na.action =na.exclude)
summary(model1)
plot(model1)
summary(model1)$adj.r.squared
par(mfrow=c(1,3))
plot(model1,which = c(1,2,3)) #1200x400 scale=0.515

#liniowość modelu
harvtest(model1)
resettest(model1)
raintest(model1)

#normalność błędów
shapiro.test(model1$residuals)
jarque.bera.test(model1$residuals)

#stała wariancja
gqtest(model1)
hmctest(model1)
#niezależność reszt
dwtest(model1)
bgtest(model1)
#brak współniniowości
vif(model1)
#odstające obserwacje
par(mfrow=c(1,2))
plot(model1,which = c(4,5))
x=data[c(99,77,31),]
View(x)
2*10/104
#jak wygląda model1
plot(data[,c(1)],fitted(model1),xlab = 'Wyniki na IMO',ylab = 'Wyniki wskazane przez model')
mod1=lm(fitted(model1)~data[,c(1)])
abline(mod1)
#model bez Indii
data=data[-31,]
attach(data)
model2=lm(total~population+GDP+GDP.per.cap+mortality+internet+area+life.expt+secondary+primary+sex,data=data,na.action = na.exclude)
summary(model2)

#zmiana wyniku Tanzanii
data['Tanzania',]$total=0.0001
model3=lm(total~population+GDP+GDP.per.cap+mortality+internet+area+life.expt+secondary+primary+sex,data=data,na.action = na.exclude)
summary(model2)
model2$coefficients-model3$coefficients

#transformacja logatyrmiczna
lgt3=logtrans(model3)
alpha=lgt3$x[which.max(lgt3$y)]
model5=lm(log(total+alpha)~population+GDP+GDP.per.cap+mortality+internet+area+life.expt+secondary+primary+sex,data=data,na.action = na.exclude)
summary(model5)
plot(model5,which = c(1,2,3,4))

#liniowość modelu
harvtest(model5)
resettest(model5)
raintest(model5)

#normalność błędów
shapiro.test(model5$residuals)
jarque.bera.test(model5$residuals)

#stała wariancja
gqtest(model5)
hmctest(model5)
#niezależność reszt
dwtest(model5)
bgtest(model5)
#brak współniniowości
vif(model5)


#transformacja box-cox

bcm3=boxcox(model3)
lambda=bcm3$x[which.max(bcm3$y)]
print(lambda)

model4=lm((total^lambda-1)/lambda~I(log(population))+GDP+GDP.per.cap+mortality+internet+area+life.expt+secondary+primary+sex,data=data,na.action = na.exclude)
summary(model4)

plot(model4,which=c(1,2,3,4))
#testowanie transformowanego modelu
#liniowość modelu
harvtest(model4)
resettest(model4)
raintest(model4)

#normalność błędów
shapiro.test(model4$residuals)
jarque.bera.test(model4$residuals)

#stała wariancja
gqtest(model4)
hmctest(model4)
#niezależność reszt
dwtest(model4)
bgtest(model4)
#brak współniniowości
vif(model4)

#transformacje zmiennej population
model6=lm((total^lambda-1)/lambda~I(log(population))+GDP+GDP.per.cap+mortality+internet+area+life.expt+secondary+primary+sex,data=data,na.action = na.exclude)
summary(model6)
plot(model6,which = c(1,2,3,4))

#liniowość modelu
harvtest(model6)
resettest(model6)
raintest(model6)

#normalność błędów
shapiro.test(model6$residuals)
jarque.bera.test(model6$residuals)

#stała wariancja
gqtest(model6)
hmctest(model6)
#niezależność reszt
dwtest(model6)
bgtest(model6)
#brak współniniowości
vif(model6)

model6=lm((total^lambda-1)/lambda~I(log(population))+GDP+GDP.per.cap+mortality+internet+area+life.expt+secondary+primary+sex,data=na.omit(data),na.action = na.omit)
step(model6,k=log(103))


#końcowy model
model8=lm((total^lambda - 1)/lambda ~I(log(population)) + area + life.expt + primary + sex, data = na.omit(data))
summary(model8)
summary(model8)$adj.r.squared
plot(model8,which = c(1,2,3,4))


#liniowość modelu
harvtest(model8)
resettest(model8)
raintest(model8)

#normalność błędów
shapiro.test(model8$residuals)
jarque.bera.test(model8$residuals)

#stała wariancja
gqtest(model8)
hmctest(model8)
#niezależność reszt
dwtest(model8)
bgtest(model8)
#brak współniniowości
vif(model8)

#porównanie modeli
#res vs fitted
par(mfrow=c(1,2))
plot(model1,which = c(1))
plot(model8,which = c(1))
#qq plot
par(mfrow=c(1,2))
plot(model1,which = c(2))
plot(model8,which = c(2))
#scale location
par(mfrow=c(1,2))
plot(model1,which = c(3))
plot(model8,which = c(3))
#cooks distance
par(mfrow=c(1,2))
plot(model1,which = c(4))
plot(model8,which = c(4))

