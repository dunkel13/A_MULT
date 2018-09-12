library(readr)
airP <- read_table2("airP.csv")
View(airP)

########### PCA usando la matriz de covarianzas
sigma <- cov(airP)
# valores y vectores propios de sigma
vp <- eigen(sigma)
vp$vectors
# los vectores propios estás normalizados (norma igual a 1)
v1 <- as.matrix(vp$vectors[,1])
sqrt(t(v1)%*%v1)
# proporción de la varianza explicada por la componente 1
vp$values[1]/sum(vp$values)

# Comparación con lo obtenido usando princomp
PCa <- princomp(airP)
# se necesitan 5 componentes para explicar el 95% de la varianza
summary(PCa)
# la componente 1 se deja llevar por la contribución de la variable radiation
# dado que es la de mayor dispersión 
PCa$loadings


########## PCA usando la matriz de correlación
R <- cor(airP)
# valores y vectores propios de R
vpr <- eigen(R)
v1r <- vpr$vectors; v1r

# Comparación con lo obtenido usando princomp
PC <- princomp(airP, cor=1)
# se necesitan 5 componentes para explicar el 87% de la varianza
summary(PC)
#Y La contribuciones de las variables a las componentes no se dejan llevar por la varianza de alguna de las variables
PC$loadings
