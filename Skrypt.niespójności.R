




# Funkcja zamieniająca wszystkie puste wartości na NA
apartments_rent_pl_2024_06[apartments_rent_pl_2024_06 == ""] <- NA

# Wyświetlenie zaktualizowanego apartments_rent_pl_2024_06frame
print(apartments_rent_pl_2024_06)


library(openxlsx)
write.xlsx(apartments_rent_pl_2024_06, file = "NaszeDane.xlsx")

# Wczytanie danych
library(readxl)



# Podpunkt 3: Walidacja zakresów i wartości

# 3.1 Sprawdzenie, czy 'squareMeters' ma wartości dodatnie
squareMeters_check <- subset(apartments_rent_pl_2024_06, squareMeters <= 0)

# 3.2 Sprawdzenie, czy 'rooms' ma wartości całkowite i większe od zera
rooms_check <- subset(apartments_rent_pl_2024_06, rooms <= 0 | rooms %% 1 != 0)

# 3.3 Sprawdzenie, czy 'floor' jest mniejsze lub równe 'floorCount'
floor_check <- subset(apartments_rent_pl_2024_06, floor > floorCount)

# 3.5 Sprawdzenie, czy 'price' jest liczbą dodatnią
price_check <- subset(apartments_rent_pl_2024_06, as.numeric(price) <= 0)

# Wyświetlenie wyników walidacji
list(
  squareMeters_check = squareMeters_check,
  rooms_check = rooms_check,
  floor_check = floor_check,
  buildYear_check = buildYear_check,
  price_check = price_check
)

# Podpunkt 4: Sprawdzenie unikalnych wartości dla kolumn kategorycznych

# 4.1 Sprawdzenie unikalnych wartości w kolumnie 'type'
unique_types <- unique(apartments_rent_pl_2024_06$type)

# 4.2 Sprawdzenie unikalnych wartości w kolumnie 'ownership'
unique_ownership <- unique(apartments_rent_pl_2024_06$ownership)

# 4.3 Sprawdzenie unikalnych wartości w kolumnie 'condition'
unique_condition <- unique(apartments_rent_pl_2024_06$condition)

# 4.4 Sprawdzenie unikalnych wartości w kolumnie 'buildingMaterial'
unique_buildingMaterial <- unique(apartments_rent_pl_2024_06$buildingMaterial)

# Wyświetlenie wyników
list(
  unique_types = unique_types,
  unique_ownership = unique_ownership,
  unique_condition = unique_condition,
  unique_buildingMaterial = unique_buildingMaterial
)


