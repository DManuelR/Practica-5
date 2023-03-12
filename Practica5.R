numArtefactos <- c(17, 54, 10, 34, 90, 33, 49, 82, 12, 23, 56, 78, 44, 102, 10, 53, 4, 28, 37, 95)
numArtefactos

numArtefactos_int <- as.integer(numArtefactos)

typeof(numArtefactos_int)

mean(numArtefactos_int)

median(numArtefactos_int) #Es el número intermedio de un grupo de números.  La mitad de los números son superiores a la mediana y la mitad de los números tienen valores menores que la mediana.

moda <- function(x) 
{u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]} 
moda(numArtefactos_int)

tabulate(numArtefactos_int, nbins = 102) #El valor correspondiente de la moda se repite 2 veces

quantile(numArtefactos_int)

rango_intercuartílico <- IQR(numArtefactos_int)
rango_intercuartílico


rango_artefactos <- diff(range(numArtefactos_int))
rango_artefactos

var_val <- var(numArtefactos_int)
var_val
var_val2 <- sum((numArtefactos_int - mean(numArtefactos_int))^2)/(length(numArtefactos_int) - 1)
var_val2

desviacion_estandar <- sd(numArtefactos_int)
desviacion_estandar
desviacion_estandar2 <- sqrt(sum((numArtefactos_int - mean(numArtefactos_int))^2)/(length(numArtefactos_int) - 1))
desviacion_estandar2

#11, la principal diferencia entre la desviación estándar y la varianza es que la desviación estándar se expresa en las mismas unidades que los datos originales, mientras que la varianza se expresa en unidades cuadradas. La desviación estándar es una medida más intuitiva de la dispersión y es más comúnmente utilizada en la práctica.

library(ggplot2)
boxplot(numArtefactos_int, horizontal = TRUE)

vector3 <- c(21, 45, 33, 98, 34, 90, 67, 87, 45, 11, 73, 38, 28, 15, 50, 57, 12, 87, 29, 1)

cv1 <- sd(numArtefactos_int)/mean(numArtefactos_int) * 100
cv1
cv2 <- sd(vector3)/mean(vector3) * 100
cv2
coef_var <- function(x) {
  cv <- sd(x) / mean(x) * 100
  return(cv)
}
coef_var(numArtefactos_int)
coef_var(vector3)


