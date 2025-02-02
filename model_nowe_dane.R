dane_apartmentsRent <- Imputed_Data_Combined


# Twoje dane miast
cities <- data.frame(
  city = c("Białystok", "Bydgoszcz", "Częstochowa", "Gdańsk", "Gdynia", "Katowice", "Kraków", "Łódź", "Lublin", "Poznań", "Radom", "Rzeszów", "Szczecin", "Warszawa", "Wrocław"),
  lon = c(23.1667, 18.0000, 19.1167, 18.6333, 18.5333, 19.0000, 19.9500, 19.4667, 
          22.5667, 16.9167, 21.1667, 22.0167, 14.5667, 21.0333, 17.0333),
  lat  = c(53.1333, 53.1167, 50.8000, 54.3667, 54.5333, 50.2500, 50.0500, 51.7833, 
           51.2333, 52.4167, 51.4000, 50.0500, 53.4333, 52.2000, 51.1167)
)

# Funkcja do przypisania miasta
przypisz_miasto <- function(longitude, latitude, cities) {
  # Oblicz odległość euklidesową
  odleglosci <- sqrt((cities$lon - longitude)^2 + (cities$lat - latitude)^2)
  
  # Znajdź miasto z najmniejszą odległością
  najblizsze <- cities$city[which.min(odleglosci)]
  
  return(najblizsze)
}

# Przypisz miasto dla każdego mieszkania
dane_apartmentsRent$miasto <- mapply(przypisz_miasto, dane_apartmentsRent$longitude, dane_apartmentsRent$latitude, MoreArgs = list(cities = cities))


# Załóżmy, że Twoje dane mają kolumnę `price`
dane_apartmentsRent$czy_wynajem_pokoju <- ifelse(dane_apartmentsRent$price < 1000, "Tak", "Nie")

# Załóżmy, że Twoje dane mają kolumnę `squareMeters`
dane_apartmentsRent$czy_kawalerka <- ifelse(dane_apartmentsRent$squareMeters <= 30, "Tak", "Nie")

# Załóżmy, że Twoje dane mają kolumnę `price`
dane_apartmentsRent$czy_premium <- ifelse(dane_apartmentsRent$price > 5000, "Tak", "Nie")


model <-lm(formula = log(price)~  squareMeters + city + czy_wynajem_pokoju + czy_premium, data = dane_apartmentsRent)

summary(model)

#wykres reszt modelu
plot(model$residuals)


#testowanie współlinowości 
library(car)
vif(model)

sample_residuals <- sample(model$residuals, 5000)
shapiro.test(sample_residuals)

Slibrary(sandwich)
library(lmtest)
coeftest(model, vcov = vcovHC(model, type = "HC1"))

qqnorm(model$residuals)
qqline(model$residuals, col = "red")

library(lmtest)
bptest(model)

