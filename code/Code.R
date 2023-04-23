dane=data10
data=dane
View(data)
colnames(data)=c('total','population','GDP','GDP.per.cap','mortality','internet','area','life.expt','secondary','primary','sex')
attach(data)
View(data)
cor(data,use = 'pairwise.complete.obs')
summary(data)

par(mfrow=c(2,2))
plot(area,total)
plot(GDP,total)
plot(sex,total)
plot(GDP.per.cap,total)

par(mfrow=c(1,2))
plot(area,total)
plot(GDP,total)


model1=lm(total.sum.of.scores.from.6.problems.~Land.area..sq..km.+GDP..current.US..+Sex.ratio.at.birth..male.births.per.female.+GDP.per.capita..current.US..+Individuals.using.the.Internet....of.population.+Mortality.rate..under.5..per.1.000.+Primary.education..duration..years.+Lower.secondary.school.starting.age..year.+Life.expectancy.at.birth..total..years.+Population..total)
summary(model1)
plot(x)
model2=lm(total.sum.of.scores.from.6.problems.~GDP..current.US..+Land.area..sq..km.+Life.expectancy.at.birth..total..years.+Primary.education..duration..years.)
summary(model2)
model3=lm(total.sum.of.scores.from.6.problems.~GDP..current.US..+Land.area..sq..km.+Life.expectancy.at.birth..total..years.+Primary.education..duration..years.-1)
summary(model3)
anova(model3,model2)
plot(model2)


abline(lm(total.sum.of.scores.from.6.problems.~Land.area..sq..km.))
abline(lm(total.sum.of.scores.from.6.problems.~GDP..current.US..))
