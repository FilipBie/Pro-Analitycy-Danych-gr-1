# wykres 1 - średnie ceny wynajmu w Polsce na mapie 

table (Imputed_Data_Combined$city)

# srednie ceny miesięcznego czynszu
library(dplyr)
srednie_ceny <- Imputed_Data_Combined %>%
  group_by(city) %>%
  summarise(srednie_ceny = mean(price))
print(srednie_ceny)

install.packages(c("maps", "mapdata", "ggplot2"))

library(maps)
library(mapdata)
library(ggplot2)

# mapa polski 
poland_map <- map_data("worldHires", region = "Poland")

# dane
cities <- data.frame(
  city = c("Białystok", "Bydgoszcz", "Częstochowa", "Gdańsk", "Gdynia", "Katowice", "Kraków", "Łódź", "Lublin", "Poznań", "Radom", "Rzeszów", "Szczecin", "Warszawa", "Wrocław"),
  lon = c(23.1667, 18.0000, 19.1167, 18.6333, 18.5333, 19.0000, 19.9500, 19.4667, 
          22.5667, 16.9167, 21.1667, 22.0167, 14.5667, 21.0333, 17.0333),
  lat  = c(53.1333, 53.1167, 50.8000, 54.3667, 54.5333, 50.2500, 50.0500, 51.7833, 
          51.2333, 52.4167, 51.4000, 50.0500, 53.4333, 52.2000, 51.1167),
  price = c(1928, 1987, 1915, 3213, 3317, 2411, 3317, 2110, 2570, 2593, 1851, 2521, 2627, 5090, 3172) 
)

# mapa 
ggplot() +
  geom_polygon(data = poland_map, aes(x = long, y = lat, group = group), fill = "lightgray", color = "black") +
  geom_point(data = cities, aes(x = lon, y = lat, size = price), alpha = 0.7, color = "darkmagenta") +  
  scale_size_continuous(range = c(3, 10)) +  
  geom_text(data = cities, aes(x = lon, y = lat, label = city), vjust = -1, size = 3, color = "black") +  
  labs(title = "Średnie ceny wynajmu w Polsce", size = "Cena [PLN]") +
  theme_minimal()

# wykres 2 - Cena wynajmu w zależności od wielkości mieszkania 

  ggplot(Imputed_Data_Combined, aes(x = squareMeters, y = price)) +
    geom_point(alpha = 0.7, size = 3, color = "maroon4") +  
    labs(title = "Cena wynajmu w zależności od wielkości mieszkania",
         x = "Wielkość mieszkania [m²]", y = "Cena wynajmu [PLN]") +
    theme_minimal()
  


  

