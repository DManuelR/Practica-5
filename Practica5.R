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

tabulate(numArtefactos_int, nbins = 102)

quantile(numArtefactos_int)


