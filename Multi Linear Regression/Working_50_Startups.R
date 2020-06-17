#Prepare a prediction model for profit of 50_startups data.
#Do transformations for getting better predictions of profit and make a table containing R^2 value for each prepared model.
#R&D Spend -- Research and devolop spend in the past few years
#Administration -- spend on administration in the past few years
#Marketing Spend -- spend on Marketing in the past few years
#State -- states from which data is collected
#Profit  -- profit of each state in the past few years

#Y= Profir
#X1 = R.D.Spend, X2 = Administration, X3 = Marketing.Spend, X4 = State

startups_data <- read.csv(choose.files())

summary(startups_data)
str(startups_data)
sum(is.na(startups_data))

attach(startups_data)
library(moments)

hist(R.D.Spend)
boxplot(R.D.Spend)
skewness(R.D.Spend)
kurtosis(R.D.Spend)

hist(Administration)
boxplot(Administration)
skewness(Administration)
kurtosis(Administration)

hist(Marketing.Spend)
boxplot(Marketing.Spend)
skewness(Marketing.Spend)
kurtosis(Marketing.Spend)

hist(Profit)
boxplot(Profit)
skewness(Profit)
kurtosis(Profit)

pairs(startups_data)
cor(startups_data[-4])
cor2pcor(cor(startups_data[-4]))

cor(Marketing.Spend,R.D.Spend)

model1<-lm(Profit~R.D.Spend+Administration+Marketing.Spend)
summary(model1)
vif(model1)
avPlots(model1,id.n=3,id.cex=1)
influencePlot(model1,id.n=3)

startups_data_1<-startups_data[-c(49,47,50),]
model2<-lm(startups_data_1$Profit~startups_data_1$R.D.Spend+startups_data_1$Marketing.Spend)
summary(model2)
vif(model2)
avPlots(model2,id.n=3,id.cex=1)
influenceIndexPlot(model2,id.n=3)
influencePlot(model2,id.n=3)

plot(model2)
qqPlot(model2, id.n=5)
hist(residuals(model2))
shapiro.test(residuals(model2))
