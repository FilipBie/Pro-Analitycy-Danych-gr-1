# Wczytaj bibliotekę i dane
library(readxl)
library(mice)
library(VIM)


# Funkcja zamieniająca wszystkie puste wartości na NA
apartments_rent_pl_2024_06[apartments_rent_pl_2024_06 == ""] <- NA

# Wczytaj dane z pliku

data <- apartments_rent_pl_2024_06

# Sprawdź dane
str(data)
summary(data)

# Dodanie nowych kolumn do imputacji liczbowej
num_cols <- c("squareMeters", "floor", "floorCount", "buildYear", "latitude", "longitude", 
              "pharmacyDistance", "clinicDistance", "postOfficeDistance", 
              "kindergartenDistance", "restaurantDistance", "collegeDistance")

# Imputacja danych liczbowych przy użyciu MICE
mice_result <- mice(data[num_cols], m = 5, method = 'pmm', maxit = 5, seed = 123)


# Dodanie imputowanych kolumn liczbowych do danych
data[num_cols] <- complete(mice_result)

# Kolumny kategoryczne do imputacji Hotdeck
cat_cols <- c("type", "ownership", "buildingMaterial", "condition", "hasParkingSpace", 
              "hasBalcony", "hasElevator", "hasSecurity", "hasStorageRoom")

# Imputacja danych kategorycznych przy użyciu Hotdeck
hotdeck_result <- hotdeck(data, variable = cat_cols)

# Dodanie imputowanych kolumn kategorycznych do danych
data[cat_cols] <- hotdeck_result[cat_cols]

# Sprawdź wynikowe dane
summary(data)

# Funkcja do sprawdzania braków danych (NA) w każdej kolumnie
check_na <- function(data) {
  na_summary <- data.frame(
    Column = colnames(data),
    MissingCount = sapply(data, function(x) sum(is.na(x))),
    TotalRows = nrow(data),
    MissingPercentage = sapply(data, function(x) round(sum(is.na(x)) / nrow(data) * 100, 2))
  )
  return(na_summary)
}


# Wywołanie funkcji dla Twojego zestawu danych
na_summary <- check_na(data)

# Wyświetlenie wyników
print(na_summary)

# Zapisz dane do pliku wynikowego
write.csv(data, "Imputed_Data_Combined.csv", row.names = FALSE)


