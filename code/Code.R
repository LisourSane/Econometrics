read.csv()
install.packages(lmtest)
library(lmtest)
data=data10
colnames(data)=c('total','population','GDP','GDP.per.cap','mortality','internet','area','life.expt','secondary','primary','sex')
attach(data)
View(data)
summary(data)
cor(data,use = 'pairwise.complete.obs')

par(mfrow=c(1,2))
plot(area,total)
plot(GDP,total)
a=c()
for(i in 1:9){
  a=append(a,summary(model1)$coefficient[i])
}
a
b=c('intercept','population','GDP','mortality','internet','area','life.expt','secondary','primary')
x=data.frame(b,a)
x


model1=lm(total~population+GDP+mortality+internet+area+life.expt+secondary+primary)
summary(model1)
summary(model1)$r.squared
plot(model1)
par(mfrow=c(1,2))

harvtest(model1)
resettest(model1)

shapiro.test(model1$residuals)
          
plot(model1,which = c(1,2))
par(mfrow=c(1,2))
plot(model1,which = c(3,5)) #800/400

hmctest(model1)



#jednorodność wariancji
bptest(model1)
gqtest(model1)
hmctest(model1)
#niezależność reszt
dwtest(model1)
bgtest(model1)

model2=lm(total~GDP+area+life.expt+primary)
summary(model2)
anova(model1,model2)
plot(model1)
plot(model2)