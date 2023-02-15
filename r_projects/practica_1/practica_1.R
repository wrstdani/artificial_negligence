install.packages("UsingR")
paises <- read.table("paises-8.csv", header = TRUE, sep = ";", stringsAsFactors = T)

dimension_paises <- dim(paises)
resumen_paises <- summary(paises)
variables_paises <- names(paises)

which(paises$Religion == "Animista")
paises[12,]
