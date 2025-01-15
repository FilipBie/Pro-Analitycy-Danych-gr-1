# ANALIZA OPISOWA :).

library(dplyr)
library(summarytools)
library(corrplot)


# Podstawowe statystyki opisowe dla kluczowych zmiennych
key_vars <- c("price", "squareMeters", "rooms", "centreDistance", 
              "schoolDistance", "clinicDistance", "buildYear")
stats <- Imputed_Data_Combined %>%
  select(all_of(key_vars)) %>%
  summarise_all(list(
    Min = ~ min(., na.rm = TRUE),
    Max = ~ max(., na.rm = TRUE),
    Mean = ~ round(mean(., na.rm = TRUE), 2),
    Median = ~ round(median(., na.rm = TRUE), 2),
    SD = ~ round(sd(., na.rm = TRUE), 2),
    Q1 = ~ quantile(., 0.25, na.rm = TRUE),
    Q3 = ~ quantile(., 0.75, na.rm = TRUE),
    IQR = ~ IQR(., na.rm = TRUE)
  ))
# Wyświetl wyniki
print(stats)

# Opcjonalnie: Tabela w formacie HTML z summarytools
view(dfSummary(Imputed_Data_Combined %>% select(all_of(key_vars))))




install.packages("summarytools")
library(dplyr)
library(summarytools)
library(corrplot)


# Cena w zależności od liczby pokoi
ggplot(Imputed_Data_Combined, aes(x = as.factor(rooms), y = price)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(title = "Cena w zależności od liczby pokoi", x = "Liczba pokoi", y = "Cena") +
  theme_minimal()



# Cena a odległość od centrum
ggplot(Imputed_Data_Combined, aes(x = centreDistance, y = price)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Cena a odległość od centrum", x = "Odległość od centrum (km)", y = "Cena") +
  theme_minimal()




# Obliczenie korelacji dla zmiennych ilościowych
numeric_vars <- Imputed_Data_Combined %>%
  select(where(is.numeric)) %>%
  na.omit()

cor_matrix <- cor(numeric_vars)
cor_matrix

# Wizualizacja macierzy korelacji zmiennych ilościowych 
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, addCoef.col = "black")
