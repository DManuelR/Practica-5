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

#11, ...

library(ggplot2)

Grafica_dispersion <- plot(numArtefactos_int, y=rep(0, length(numArtefactos_int)), pch=19, xlim=c(min(numArtefactos_int) - 1, max(numArtefactos_int) + 1), xlab="Valor", main="Dispersión de numArtefactos_int")
abline(v=mean(numArtefactos_int), col="red", lty=2)

