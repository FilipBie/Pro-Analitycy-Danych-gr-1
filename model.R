# model liniowy regresji 
dane_apartmentsRent <- Imputed_Data_Combined

model <-lm(formula = log(price)~  squareMeters + floor + buildYear, data = dane_apartmentsRent)

summary(model)

#model 2

library(dplyr)

city_avg_prices <- Imputed_Data_Combined %>%
  group_by(city) %>%
  summarise(avg_price = mean(price, na.rm = TRUE))

Imputed_Data_Combined$avg_price <- city_avg_prices$avg_price[match(Imputed_Data_Combined$city, city_avg_prices$city)]

model_2 <- lm(formula = log(price) ~ avg_price + type + squareMeters + rooms + buildYear + centreDistance + poiCount + schoolDistance + hasElevator, data = Imputed_Data_Combined)
summary(model_2)

plot(model_2$residuals)

library(car)
vif(model_2)

bptest(model_2)

qqnorm(residuals(model_2))
qqline(residuals(model_2), col = "red")


