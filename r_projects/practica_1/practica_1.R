########## 1R (librerías e importación del dataframe):
install.packages("UsingR")
install.packages("ggplot2")
library(UsingR)
library(ggplot2)
paises.8 <- read.table("paises-8.csv", header = T, sep = ";",stringsAsFactors = T, fileEncoding = "ASCII")



########## 2R:
dim(paises.8)
summary(paises.8)
names(paises.8)
str(paises.8)



########## 3R:

## % de población urbana
ggplot(paises.8, aes(Urbana)) + 
  geom_histogram(bins = 9, color = "#FFDB4B", fill = "#FCE99E") + 
  theme_bw() + 
  labs(x = "% de población urbana", y = "Frecuencia", title = "Porcentaje de población urbana por país")

## Religión mayoritaria
ggplot(paises.8, aes(Religion)) + 
  geom_bar(bins = 9, color = "purple", fill = "#E0AAFF") + 
  theme_bw() +
  labs(x = "Religión", y = "Ocurrencias", title = "Religión mayoritaria en cada país")

table(paises.8$Religion)

## Esperanza de vida femenina
ggplot(paises.8, aes(Esp.vida.Fem)) + 
  geom_histogram(bins = 9, color = "#FF48EE", fill = "#FFCDFA") + 
  theme_bw() +
  labs(x = "Esperanza de vida (años)", y = "Frecuencia", title = "Esperanza de vida femenina por país")

paises.8$Pais[paises.8$Esp.vida.Fem < 55]
mean(paises.8$Esp.vida.Fem)

## Esperanza de vida masculina
ggplot(paises.8, aes(Esp.vida.Masc)) + 
  geom_histogram(bins = 9, color = "#EFA330", fill = "#EEF3A0") + 
  theme_bw() + 
  labs(x = "Esperanza de vida (años)", y = "Frecuencia", title = "Esperanza de vida masculina por país")
mean(paises.8$Esp.vida.Masc)

## % de alfabetización
ggplot(paises.8, aes(Alfabet)) +
  geom_histogram(bins = 9, color = "#EF305A", fill = "#F39DB0") + 
  theme_bw() + 
  labs(x = "% de alfbetización", y = "Frecuencia", title = "Porcentaje de alfabetización por país")

paises.8$Pais[paises.8$Alfabet==100]
paises.8$Pais[paises.8$Alfabet<30]

## Mortalidad infantil
ggplot(paises.8, aes(Mort.inf)) + 
  geom_histogram(bins = 9, color = "#1382FF", fill = "#96C8FF") + 
  theme_bw() + 
  labs(x = "Muertes por cada 1000 nacidos", y = "Frecuencia", title = "Mortalidad infantil por país")

paises.8$Pais[paises.8$Mort.inf > 90 & paises.8$Mort.inf < 200]

## PIB per cápita
ggplot(paises.8, aes(PIB.CAP)) +
  geom_histogram(bins = 9, color = "#52FF3A", fill = "#A9FF9D") + 
  theme_bw() + 
  labs(x = "PIB per cápita", y = "Frecuencia", title = "Producto interior bruto per cápita")

paises.8$Pais[paises.8$PIB.CAP == 0]

## Calorías
ggplot(paises.8, aes(Calorias)) + 
  geom_histogram(bins = 9, color = "#FF2DC2", fill = "#FFAEE8") + 
  theme_bw() + 
  labs(x = "Calorías", y = "Frecuencia", title = "Calorías promedio diarias")

paises.8$Pais[paises.8$Calorias > 3000 & paises.8$Calorias < 3700]
paises.8$Pais[paises.8$Calorias < 1700]

## Hijos por mujer
ggplot(paises.8, aes(Hijpromedio)) + 
  geom_histogram(bins = 9, color = "#8716FF", fill = "#DFC0FF") + 
  theme_bw() +
  labs(x = "Hijos por mujer", y = "Frecuencia", title = "Número de hijos promedio por mujer")

paises.8$Pais[paises.8$Hijpromedio >= 6]

## % de alfabetización masculina
ggplot(paises.8, aes(Alfabmasc)) + 
  geom_histogram(bins = 9, color = "#125EFF", fill = "#B6CDFF") + 
  theme_bw() +
  labs(x = "% de alfabetización", y = "Frecuencia", title = "Porcentaje de alfabetización masculina")

## % de alfabetización femenina
ggplot(paises.8, aes(Alfabfem)) + 
  geom_histogram(bins = 9, color = "#E124B3", fill = "#EABADE") + 
  theme_bw() + 
  labs(x = "% de alfabetización", y = "Frecuencia", title = "Porcentaje de alfabetización femenina")



########## 4R:
typeof(paises.8$Esp.vida.Fem)
typeof(paises.8$Esp.vida.Masc)

summary(paises.8$Esp.vida.Fem)
summary(paises.8$Esp.vida.Masc)

par(mfrow = c(1, 2))
hist(paises.8$Esp.vida.Masc, xlab = 'Años', ylab = 'Frecuencia', main = 'Hist. Hombres', col = 5) 
hist(paises.8$Esp.vida.Fem, xlab = 'Años', ylab = 'Frecuencia', main = 'Hist. Mujeres', col = 4)



########## 5R:
## Religión
table(paises.8$Religion)

ggplot(paises.8, aes(Religion)) + 
  geom_bar(fill = "yellow", bins = 8) + 
  theme_bw() + 
  labs(x = "Religiones", y = NULL, title = "Frec. Religiones")

plot(paises.8$Religion, paises.8$Esp.vida.Fem, col = 7) 

plot(paises.8$Religion, paises.8$Pais, col = 7)
  
plot(paises.8$Esp.vida.Fem, paises.8$Religion, col = paises.8$Pais, main = "Esp.Vida.Fem. - Relig. - Países(color)", xlab = "Esperanza de vida femenina", ylab = "Religión")


## Num. hijos
plot(paises.8$Hijpromedio, paises.8$Esp.vida.Fem, xlab = 'Núm. Hijos', ylab = 'Esp.Vida.Fem', main = 'N.Hijos - Esp.Vida.Fem', col = 5)

plot(paises.8$Hijpromedio, paises.8$Pais, xlab = 'Núm. Hijos', ylab = 'País', main = 'N.Hijos - País', col = 5)

par(mfrow = c(1,2))

plot(paises.8$Hijpromedio, paises.8$Pais, xlab = 'N.Hijos', ylab = 'País', main = 'N.Hijos - País', col = 5)

plot(paises.8$Esp.vida.Fem, paises.8$Pais, xlab = 'Esp.vida.Fem', ylab = 'País', main = 'Esp.Vida.Fem - País', col = 5) 

par(mfrow = c(1, 1))


## Alfab.
plot(paises.8$Esp.vida.Fem, paises.8$Alfabet, xlab = 'Mujeres', ylab = 'Alfabet', main = 'Esp.Vida.Fem - Alfabet', col = 4)

par(mfrow = c(1,3))

plot(paises.8$Alfabet, paises.8$Esp.vida.Fem, ylab = 'Esp.Vida.Fem', xlab = 'Alfabet', main = 'Alfabet - Esp.Vida.Fem', col = 4)

plot(paises.8$Alfabet, paises.8$Pais, ylab = 'País', xlab = 'Alfabet', main = 'Alfabet - País', col = 4)

plot(paises.8$Esp.vida.Fem, paises.8$Pais, xlab = 'Esp.Vida.Fem', ylab = 'País', main = 'Esp.Vida.Fem - País', col = 4)

par(mfrow = c(1, 1))


## Calorías
plot(paises.8$Esp.vida.Fem, paises.8$Calorias, xlab = 'Mujeres', ylab = 'Calorias', main = 'Esp.Vida.Fem - Calorias', col = 3)

par(mfrow = c(1,3))

plot(paises.8$Calorias, paises.8$Esp.vida.Fem, ylab = 'Esp.Vida.Fem', xlab = 'Calorías', main = 'Calorías - Esp.Vida.Fem', col = 3)

plot(paises.8$Calorias, paises.8$Pais, ylab = 'País', xlab = 'Calorías', main = 'Calorías - País', col = 3)

plot(paises.8$Esp.vida.Fem, paises.8$Pais, xlab = 'Esp.Vida.Fem', ylab = 'País', main = 'Esp.Vida.Fem - País', col = 3)

par(mfrow = c(1, 1))


## PIB per cápita
plot(paises.8$Esp.vida.Fem, paises.8$PIB.CAP, xlab = 'Mujeres', ylab = 'PIB.CAP', main = 'Esp.Vida.Fem - PIB.CAP', col = 2)

plot(paises.8$PIB.CAP, paises.8$Pais, xlab = 'PIB.CAP', ylab = 'Pais', main = 'PIB.CAP - Pais', col = 2)

par(mfrow = c(1,3))

plot(paises.8$PIB.CAP, paises.8$Pais, xlab = 'PIB.CAP', ylab = 'Pais', main = 'PIB.CAP - Pais', col = 2)

plot(y = paises.8$PIB.CAP, x = paises.8$Esp.vida.Fem, xlab = 'Esp.Vida.Fem', ylab = 'PIB.CAP', main = 'Esp.Vida.Fem - PIB.CAP - País (color)', col = paises.8$Pais)

plot(paises.8$Esp.vida.Fem, paises.8$Pais, xlab = 'Esp.Vida.Fem', ylab = 'País', main = 'Esp.Vida.Fem - País', col = 2)

par(mfrow = c(1, 1))



########## 6R:

cov(paises.8$Esp.vida.Fem, paises.8$Alfabet, use = "complete.obs")#212.991

cor(paises.8$Esp.vida.Fem, paises.8$Alfabet, use = "complete.obs")#0.8833315 

summary(lm(paises.8$Esp.vida.Fem ~ paises.8$Alfabet))#78.03% 


#Recta de Regresión;
ggplot(paises.8, aes(Alfabet, Esp.vida.Fem)) + ggtitle("Recta de Regresión:") + xlab("Alfabetización") + ylab("Esperanza de vida femenina") + geom_point(colour = "pink") + geom_smooth(method = "lm", se = FALSE, colour = "black") 



########## 7R:

table(paises.8$Religion)#28 países.
paises.8Cat <- paises.8[paises.8$Religion == "Catolica",]
head(paises.8Cat)

cov(paises.8Cat$Esp.vida.Fem, paises.8Cat$Alfabet, use = "complete.obs")#148,8675

cor(paises.8Cat$Esp.vida.Fem, paises.8Cat$Alfabet, use = "complete.obs")#0.907876 

summary(lm(paises.8Cat$Esp.vida.Fem ~ paises.8Cat$Alfabet))#0.8242


#Recta de Regresión:
ggplot(paises.8Cat, aes(Alfabet, Esp.vida.Fem)) + ggtitle("Recta de Regresión de los países mayoritariamente Católicos:") + xlab("Alfabetización") + ylab("Esperanza de Vida Femenina") + geom_point(colour = "gold") + geom_smooth(method = "lm", se = FALSE, colour = "black") + theme(plot.title = element_text(size = 10))


plot(paises.8$Alfabet, paises.8$Esp.vida.Fem, cex.main = "0.8", main = "Comparativa entre todos los países (rosa) y los Católicos (dorada):", xlab = "Alfabetización", ylab =  "Esperanza de Vida Femenina", col = "red")
abline(lm(paises.8$Esp.vida.Fem ~ paises.8$Alfabet), col = "pink")
abline(lm(paises.8Cat$Esp.vida.Fem ~ paises.8Cat$Alfabet), col = "gold")
points(paisesCat$Alfabet, paisesCat$Esp.vida.Fem, col = "gold")
