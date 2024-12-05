data <- apartments_rent_pl_2024_06
colSums(is.na(data))
library(naniar)
vis_miss(data)
install.packages("Patterns")
library(Patterns)
Missing_Pattern(data)
