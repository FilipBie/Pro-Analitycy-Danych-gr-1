# Instalacja i załadowanie pakietu ggstatsplot, jeśli nie jest zainstalowany
if (!requireNamespace("ggstatsplot", quietly = TRUE)) {
  install.packages("ggstatsplot")
}
library(ggstatsplot)

# Test Kruskala-Wallisa dla wszystkich zmiennych numerycznych
if ("group" %in% colnames(imputed_data_combined)) {
  for (var in colnames(imputed_data_combined_numeric)) {
    print(paste("Test Kruskala-Wallisa dla zmiennej:", var))
    result <- kruskal.test(imputed_data_combined[[var]] ~ imputed_data_combined$group)
    print(result)
  }
}

# Wizualizacja wyników testu Kruskala-Wallisa dla wybranej zmiennej (np. 'squareMeters')
ggbetweenstats(
  data = imputed_data_combined,
  x = group,                   # Zmienna grupująca
  y = squareMeters,            # Zmienna numeryczna
  type = "nonparametric",      # Test nieparametryczny
  title = "Porównanie zmiennej squareMeters w różnych grupach",
  xlab = "Grupa",
  ylab = "Powierzchnia (m²)",
  results.subtitle = TRUE,     # Wyświetlenie wyników testu na wykresie
  mean.plotting = TRUE         # Wyświetlenie średniej na wykresie
)
