#https://quizlet.com/197426976/linear-models-and-r-flash-cards/?new
#https://www.youtube.com/watch?v=P-WYkSZp9lY 

data <-read.csv("GPA By STRM MAJ LEVEL.csv")
str(data)
data$STRM <- as.factor(data$STRM)
linerModel <- lm(avg ~ n,data=data)
linerModel
predict(linerModel, interval = "prediction")

airquality
plot(Ozone ~ Wind, airquality)
model1 <- lm(Ozone ~ Wind, airquality)
plot(model1)
coef(model1)

model2 <- glm(Ozone ~ Wind, airquality, family = poisson())

plot(model2)
coef(model2)

library(nlme)
model3 <- gls(Ozone ~ Wind, airquality, na.action = na.exclude)
coef(model3)
airquality$Date =as.Date(paste(1973, airquality$Month, airquality$Day,sep="-"))

library(lattice)
xyplot(Ozone ~ Date, airquality)
model4 <- gls(Ozone ~ Wind*Date, airquality,na.action = na.exclude)
air2 =subset(airquality, complete.cases(Ozone))
model5 <- gls(Ozone ~ Wind*Date, air2)
plot(ACF(model5,form=~Date), alpha = 0.05)

model6 =update(model5,correlation=corAR1())

library(MuMIn)

AICc(model5,model6)
summary(model6)
