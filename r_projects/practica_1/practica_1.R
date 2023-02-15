install.packages("UsingR")
paises <- read.table("paises-8.csv", header = TRUE, sep = ";", stringsAsFactors = T)

dimension_paises <- dim(paises)
resumen_paises <- summary(paises)
variables_paises <- names(paises)
head(paises)
variables_paises

# paises$Pais[which(paises$Religion == "Animista")]
freq_religion <- table(paises$Religion)
barplot(freq_religion)
