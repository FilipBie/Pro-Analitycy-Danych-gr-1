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
  

# wykres 3 - rozkład roku wybudowania mieszkań 
  
  ggplot(Imputed_Data_Combined, aes(x = buildYear)) +
    geom_histogram(binwidth = 5, fill = "violetred3", color = "black", alpha = 0.7) +
    labs(title = "Rozkład roku wybudowania mieszkań",
         x = "Rok wybudowania", y = "Liczba mieszkań") +
    theme_minimal()
  
  
# wykres 4 - rozkład metrażu mieszkań 
  
  ggplot(Imputed_Data_Combined, aes(x = squareMeters)) +
    geom_histogram(binwidth = 5, fill = "orchid4", color = "black", alpha = 0.7) +
    labs(title = "Rozkład metrażu mieszkań",
         x = "metraż", y = "Liczba mieszkań") +
    theme_minimal()
  
# wykres 5 - typy budynków 
  
  ggplot(Imputed_Data_Combined, aes(x = type)) +
    geom_bar(fill = "pink3", color = "black", alpha = 0.7) +
    labs(title = "Liczba mieszkań w zależności od typu budynku",
         x = "Typy budynków", y = "Liczba mieszkań") +
    theme_minimal()
  
# wykres 6 - Rozkład cen w zależności od typu mieszkania 
  
  ggplot(Imputed_Data_Combined, aes(x = type, y = price)) +
    geom_jitter(width = 0.2, alpha = 0.5, color = "maroon") +
    labs(title = "Ceny mieszkań w zależności od typu budynku",
         x = "Typ mieszkania", y = "Cena (PLN)") +
    theme_minimal()
  
  
  # wykres 7 - rozkład cen mieszkań 
  
  ggplot(Imputed_Data_Combined, aes(x = price)) +
    geom_histogram(binwidth = 500, fill = "violetred3", color = "black", alpha = 0.7) +
    labs(title = "Rozkład cen mieszkań",
         x = "Cena (PLN)", y = "Liczba mieszkań") +
    theme_minimal()
  
  
  #wykres 8 - średnie ceny wynajmu mieszkań w miastach Polski (wykres słupkowy)
  
  ggplot(srednie_ceny, aes(x = reorder(city, srednie_ceny), y = srednie_ceny, fill = city)) +
    geom_bar(stat = "identity", fill = "mediumorchid4",  alpha = 0.7,) +
    geom_text(aes(label = round(srednie_ceny, 0)),  
              vjust = -0.3, size = 3.5) +
    labs(title = "Średnie ceny wynajmu mieszkań w miastach Polski",
         x = "Miasta", y = "Średnia cena wynajmu (PLN)") +
    coord_flip() +  
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 14, hjust = 0.5)
    )
  
  
  
  
  
  
  
  
