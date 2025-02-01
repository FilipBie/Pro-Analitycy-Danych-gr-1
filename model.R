# model liniowy regresji 
dane_apartmentsRent <- Imputed_Data_Combined

model <-lm(formula = log(price)~  squareMeters + floor + buildYear, data = dane_apartmentsRent)

summary(model)

#wykres reszt modelu
plot(model$residuals)


#testowanie współlinowości 
library(car)
vif(model)

sample_residuals <- sample(model$residuals, 5000)
shapiro.test(sample_residuals)

library(sandwich)
library(lmtest)
coeftest(model, vcov = vcovHC(model, type = "HC1"))

qqnorm(model$residuals)
qqline(model$residuals, col = "red")

library(lmtest)
bptest(model)





