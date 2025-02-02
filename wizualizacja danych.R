#MAPA

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


  

#1. ROZKŁADY
  
# wykres 2 - rozkład cen mieszkań 
  
  ggplot(Imputed_Data_Combined, aes(x = price)) +
    geom_histogram(binwidth = 500, fill = "violetred3", color = "black", alpha = 0.7) +
    labs(title = "Rozkład cen mieszkań",
         x = "Cena (PLN)", y = "Liczba mieszkań") +
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
  
  
#wykres 5 - rozkład liczby pokoi
  
  ggplot(Imputed_Data_Combined, aes(x = rooms)) +
    geom_histogram(binwidth = 1, fill = "violetred3", color = "black", alpha = 0.7) +
    labs(title = "Rozkład liczby pokoi",
         x = "Liczba pokoi", y = "Liczba mieszkań") +
    theme_minimal()
  
#WYKRESY KOŁOWE  
  
# wykres 6 -  procentowy udział mieszkań w zależności od typów budynków 
  
  procentowy_udzial <- Imputed_Data_Combined %>%
    group_by(type) %>%
    summarise(count = n()) %>%
    mutate(percent = round((count / sum(count)) * 100, 1))  
  
  ggplot(procentowy_udzial, aes(x = "", y = percent, fill = type)) +
    geom_bar(stat = "identity", width = 1, color = "black") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(percent, "%")), position = position_stack(vjust = 0.5), size = 4) +
    labs(title = "Procentowy udział mieszkań w zależności od typów budynków",
         x = NULL, y = NULL) +
    scale_fill_manual(values = c(
      "apartmentBuilding" = "mistyrose3",
      "blockOfFlats" = "plum3",
      "tenement" = "hotpink3")) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      legend.position = "top"
    )
  
# wykres 7 - Podsumowanie liczby mieszkań z balkonem i bez balkonu
  
  procentowy_udzial <- Imputed_Data_Combined %>%
    group_by(hasBalcony) %>%
    summarise(count = n()) %>%
    mutate(percent = round((count / sum(count)) * 100, 1))
  
  procentowy_udzial$hasBalcony <- ifelse(procentowy_udzial$hasBalcony == "yes", "Z balkonem", "Bez balkonu")

  ggplot(procentowy_udzial, aes(x = "", y = percent, fill = hasBalcony)) +
    geom_bar(stat = "identity", width = 1, color = "black") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(percent, "%")), position = position_stack(vjust = 0.5), size = 4) +
    labs(title = "Procentowy udział mieszkań z balkonem vs. bez",
         fill = NULL) +
    scale_fill_manual(values = c("Z balkonem" = "plum3", "Bez balkonu" = "hotpink3")) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      legend.position = "top"
    )
  
  # wykres 8 - Podsumowanie liczby mieszkań z parkingiem i bez

  procentowy_udzial <- Imputed_Data_Combined %>%
    group_by(hasParkingSpace) %>%
    summarise(count = n()) %>%
    mutate(percent = round((count / sum(count)) * 100, 1))
  
  procentowy_udzial$hasParkingSpace <- ifelse(procentowy_udzial$hasParkingSpace == "yes", "Z parkingiem", "Bez parkingu")
  
  ggplot(procentowy_udzial, aes(x = "", y = percent, fill = hasParkingSpace)) +
    geom_bar(stat = "identity", width = 1, color = "black") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(percent, "%")), position = position_stack(vjust = 0.5), size = 4) +
    labs(title = "Procentowy udział mieszkań z parkingiem vs. bez",
         fill = NULL) +
    scale_fill_manual(values = c("Z parkingiem" = "plum3", "Bez parkingu" = "hotpink3")) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      legend.position = "top"
    )
  
# wykres 9 - Podsumowanie liczby mieszkań z ochroną i bez

  procentowy_udzial <- Imputed_Data_Combined %>%
    group_by(hasSecurity) %>%
    summarise(count = n()) %>%
    mutate(percent = round((count / sum(count)) * 100, 1))
  
  procentowy_udzial$hasSecurity <- ifelse(procentowy_udzial$hasSecurity == "yes", "Z ochroną", "Bez ochrony")

  ggplot(procentowy_udzial, aes(x = "", y = percent, fill = hasSecurity)) +
    geom_bar(stat = "identity", width = 1, color = "black") +
    coord_polar("y", start = 0) +
    geom_text(aes(label = paste0(percent, "%")), position = position_stack(vjust = 0.5), size = 4) +
    labs(title = "Procentowy udział mieszkań z ochroną vs. bez",
         fill = NULL) +
    scale_fill_manual(values = c("Z ochroną" = "plum3", "Bez ochrony" = "hotpink3")) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      legend.position = "top"
    )
  

  

#ZALEŻNOŚCI Z PODZIAŁEM NA MIASTA 
  
#wykres 10 - średnie ceny wynajmu mieszkań w miastach Polski (wykres słupkowy)
  
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
  
  # wykres 11 - Liczba mieszkań w poszczególnych miastach (wykres słupkowy)
  liczba_mieszkan <- Imputed_Data_Combined %>%
    group_by(city) %>%
    summarise(count = n())
  
  
  ggplot(liczba_mieszkan, aes(x = reorder(city, count), y = count, fill = city)) +
    geom_bar(stat = "identity", fill = "mediumorchid4", alpha = 0.7) +
    geom_text(aes(label = count),  
              vjust = -0.3, size = 3.5) +
    labs(title = "Liczba mieszkań w poszczególnych miastach",
         x = "Miasta", y = "Liczba mieszkań") +
    coord_flip() +  
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 14, hjust = 0.5)
    )
  
  
# wykres 12 - liczba mieszkań z ochroną i bez ochrony w różnych miastach 

  liczba_mieszkan_z_ochrona <- Imputed_Data_Combined %>%
    group_by(city, hasSecurity) %>%
    summarise(count = n())
  
  ggplot(liczba_mieszkan_z_ochrona, aes(x = reorder(city, count), y = count, fill = hasSecurity)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +  
    labs(title = "Liczba mieszkań z ochroną i bez ochrony w różnych miastach",
         x = "Miasto", y = "Liczba mieszkań") +
    scale_fill_manual(values = c("peachpuff", "lightpink1")) +  
    coord_flip() + 
    theme_minimal() +
    theme(
      legend.position = "top",  
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, hjust = 0.5)
    )
  
# wykres 13 - liczba mieszkań z windą i bez windy w różnych miastach 
  
  liczba_mieszkan_z_winda <- Imputed_Data_Combined %>%
    group_by(city, hasElevator) %>%
    summarise(count = n())
  
  ggplot(liczba_mieszkan_z_winda, aes(x = reorder(city, count), y = count, fill = hasElevator)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +  
    labs(title = "Liczba mieszkań z windą i bez windy w różnych miastach",
         x = "Miasto", y = "Liczba mieszkań") +
    scale_fill_manual(values = c("thistle", "deeppink4")) +  
    coord_flip() + 
    theme_minimal() +
    theme(
      legend.position = "top",  
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, hjust = 0.5)
    )

#ZALEŻNOŚCI CENY OD RÓŻNYCH ZMIENNYCH
  
  
# wykres 14 - Rozkład cen w zależności od typu mieszkania 
  
  ggplot(Imputed_Data_Combined, aes(x = type, y = price)) +
    geom_jitter(width = 0.2, alpha = 0.5, color = "maroon") +
    labs(title = "Ceny mieszkań w zależności od typu budynku",
         x = "Typ mieszkania", y = "Cena (PLN)") +
    theme_minimal()
  
  
# wykres 15 - Cena mieszkań w zależności od obecności balkonu
  
  ggplot(Imputed_Data_Combined, aes(x = hasBalcony, y = price)) +
    geom_jitter(width = 0.2, alpha = 0.6, color = "orchid4") +
    labs(title = "Cena mieszkań w zależności od obecności balkonu",
         x = "Obecność balkonu", y = "Cena wynajmu (PLN)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
  
  
# wykres 16 - Cena wynajmu w zależności od wielkości mieszkania
  
  ggplot(Imputed_Data_Combined, aes(x = squareMeters, y = price)) +
    geom_point(alpha = 0.7, size = 3, color = "maroon4") +  
    geom_smooth(method = "lm", color = "black", se = TRUE, fill = "gray70") +  # Dodana regresja
    labs(title = "Cena wynajmu w zależności od wielkości mieszkania",
         x = "Wielkość mieszkania [m²]", y = "Cena wynajmu [PLN]") +
    theme_minimal()


  
# wykres 17 - cena mieszkania w zależności od roku wybudowania 
  
  ggplot(Imputed_Data_Combined, aes(x = buildYear, y = price)) +
    geom_point(alpha = 0.7, size = 3, color = "hotpink4") +  
    geom_smooth(method = "lm", color = "black", se = TRUE, fill = "gray70") +  # Dodana regresja
    labs(title = "Cena wynajmu w zależności od roku wybudowania",
         x = "Rok wybudowania", y = "Cena wynajmu [PLN]") +
    theme_minimal()
  

  
  # wykres 18 - Cena wynajmu vs. Liczba pokoi (scatter plot - jitter plot)
  ggplot(Imputed_Data_Combined, aes(x = as.numeric(rooms), y = price)) +  # Konwersja rooms na liczby
    geom_jitter(width = 0.2, alpha = 0.5, color = "maroon") +
    geom_smooth(method = "lm", color = "black", se = TRUE, fill = "gray70") +  # Dodana regresja
    labs(title = "Cena mieszkań w zależności od liczby pokoi",
         x = "Liczba pokoi", y = "Cena wynajmu [PLN]") +
    theme_minimal()
  
  

  
  
#ANALIZA CENY W ZALEŻNOŚCI OD DOSTĘPNOŚCI INFRASTRUKTÓRY
  


  
  # wykres 19 - Cena wynajmu vs. Odległość od centrum (scatter plot + regresja)
  ggplot(Imputed_Data_Combined, aes(x = centreDistance, y = price)) +
    geom_point(alpha = 0.5, color = "orchid4") +  # Punkty na wykresie
    geom_smooth(method = "lm", color = "black", se = TRUE, fill = "gray70") +  # Linia regresji z przedziałem ufności
    labs(title = "Cena wynajmu vs. Odległość od centrum",
         x = "Odległość od centrum [km]",
         y = "Cena wynajmu [PLN]") +
    theme_minimal()
  
  
  # wykres 20 - Wpływ odległości do szkół na cenę mieszkań (scatter plot + regresja)
  ggplot(Imputed_Data_Combined, aes(x = schoolDistance, y = price)) +
    geom_point(alpha = 0.5, color = "orchid4") +  # Punkty na wykresie
    geom_smooth(method = "lm", color = "black", se = TRUE, fill = "gray70") +  # Linia regresji z przedziałem ufności
    labs(title = "Odległość do szkoły vs. Cena wynajmu",
         x = "Odległość do szkoły [km]",
         y = "Cena wynajmu [PLN]") +
    theme_minimal()

  
  # wykres 21 - Wpływ odległości do klinik na cenę mieszkań (scatter plot + regresja)
  ggplot(Imputed_Data_Combined, aes(x = clinicDistance, y = price)) +
    geom_point(alpha = 0.5, color = "orchid4") +  # Punkty na wykresie
    geom_smooth(method = "lm", color = "black", se = TRUE, fill = "gray70") +  # Linia regresji z przedziałem ufności
    labs(title = "Odległość do kliniki vs. Cena wynajmu",
         x = "Odległość do kliniki [km]",
         y = "Cena wynajmu [PLN]") +
    theme_minimal()

  
  # wykres 22 - Wpływ odległości do uczelni na cenę mieszkań (scatter plot + regresja)
  ggplot(Imputed_Data_Combined, aes(x = collegeDistance, y = price)) +
    geom_point(alpha = 0.5, color = "orchid4") +  # Punkty na wykresie
    geom_smooth(method = "lm", color = "black", se = TRUE, fill = "gray70") +  # Linia regresji z przedziałem ufności
    labs(title = "Odległość do uczelni vs. Cena wynajmu",
         x = "Odległość do uczelni [km]",
         y = "Cena wynajmu [PLN]") +
    theme_minimal()

  # wykres 23 - Cena wynajmu vs. Liczba pokoi (scatter plot - jitter plot)
  ggplot(Imputed_Data_Combined, aes(x = poiCount, y = price)) +
    geom_point(alpha = 0.5, color = "orchid4") +  # Punkty na wykresie
    geom_smooth(method = "lm", color = "black", se = TRUE, fill = "gray70") +  # Linia regresji z przedziałem ufności
    labs(title = "Liczba punktów POI vs. Cena wynajmu",
         x = "Liczba punktów POI",
         y = "Cena wynajmu [PLN]") +
    theme_minimal()
  
  
  #WYKRESY PUDEŁKOWE
  
  
  # wykres 24 - Wpływ obecności windy na ceny mieszkań (boxplot)
  df_winda <- Imputed_Data_Combined %>%
    mutate(hasElevator = ifelse(hasElevator == "yes", "Z windą", "Bez windy"))
  
  ggplot(df_winda, aes(x = hasElevator, y = price, fill = hasElevator)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = "Wpływ obecności windy na ceny mieszkań",
         x = "Obecność windy",
         y = "Cena wynajmu [PLN]") +
    scale_fill_manual(values = c("Z windą" = "plum3", "Bez windy" = "hotpink3")) +
    theme_minimal() +
    theme(legend.position = "none")
  
  
  
  # wykres 25 - Wpływ obecności ochrony na ceny mieszkań (boxplot)
  df_ochrona <- Imputed_Data_Combined %>%
    mutate(hasSecurity = ifelse(hasSecurity == "yes", "Z ochroną", "Bez ochrony"))
  
  ggplot(df_ochrona, aes(x = hasSecurity, y = price, fill = hasSecurity)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = "Wpływ obecności ochrony na ceny mieszkań",
         x = "Obecność ochrony",
         y = "Cena wynajmu [PLN]") +
    scale_fill_manual(values = c("Z ochroną" = "plum3", "Bez ochrony" = "hotpink3")) +
    theme_minimal() +
    theme(legend.position = "none")
  
  
  # wykres 26 - Wpływ obecności parkingu na ceny mieszkań (boxplot)
  df_parking <- Imputed_Data_Combined %>%
    mutate(hasParkingSpace = ifelse(hasParkingSpace == "yes", "Z parkingiem", "Bez parkingu"))
  
  ggplot(df_parking, aes(x = hasParkingSpace, y = price, fill = hasParkingSpace)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = "Wpływ obecności parkingu na ceny mieszkań",
         x = "Obecność parkingu",
         y = "Cena wynajmu [PLN]") +
    scale_fill_manual(values = c("Z parkingiem" = "plum3", "Bez parkingu" = "hotpink3")) +
    theme_minimal() +
    theme(legend.position = "none")
  
  

  

  
  
  
  
  

  

  
  

  
  
  