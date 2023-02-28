install.packages("UsingR")
paises <- read.table("paises-8.csv", header = TRUE, sep = ";", stringsAsFactors = T)

dim(paises)
summary(paises)
names(paises)
head(paises)

# Ejercicio 3
barplot(paises$Urbana, col = rgb(0.92, 0.7, 0.65), main = "Porcentaje de población urbana")