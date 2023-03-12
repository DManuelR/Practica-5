#1- Creamos un vector
numArtefactos <- c(17, 54, 10, 34, 90, 33, 49, 82, 12, 23, 56, 78, 44, 102, 10, 53, 4, 28, 37, 95)
numArtefactos

#Si se introduce un número entero sin un punto decimal, R lo almacenará como un número entero (integer). Si se introduce un número con un punto decimal, R lo almacenará como un número double.
#Transformo el numArtefacto a número entero mediante as.integer(), que se utiliza para convertir un objeto de cualquier tipo de datos en un objeto de tipo entero.
numArtefactos_int <- as.integer(numArtefactos)

#2- Calculo la media mediante mean(), es una medida de tendencia central utilizada en estadística para describir el valor central de un conjunto de datos numéricos.
mean(numArtefactos_int)

#3- Calculo la mediana mediante median(), es el número intermedio de un grupo de números. La mitad de los números son superiores a la mediana y la mitad de los números tienen valores menores que la mediana.
median(numArtefactos_int) 

#4- Para calcular la moda creo una función. Dentro de esta encontramos la función unique() para obtener una lista de valores únicos en el vector original, la función tabulate() para contar cuántas veces aparece cada valor único en el vector original, la función match() para encontrar la posición de cada valor en la lista de valores únicos, y se selecciona el valor o los valores únicos que aparecen con mayor frecuencia utilizando la función max().
moda <- function(x) 
{u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]} 
moda(numArtefactos_int)

#5- Con la función tabulate podremos observar la cantidad de veces que se repiten los valores en el vector correspondiente. En este caso el valor correspondiente de la moda se repite 2 veces.
tabulate(numArtefactos_int, nbins = 102)

#6- Para calcular los cuartiles utilizamos la función quantile(). 
quantile(numArtefactos_int)

#7- El resultado del rango intercuartílico es 40, lo que nos indica que los datos están muy dispersos, ya que la mayoría de los valores se encuentran en un rango amplio.
rango_intercuartílico <- IQR(numArtefactos_int)
rango_intercuartílico

#8- Calculamos y almacenamos el rango de numArtefactos_int.
rango_artefactos <- diff(range(numArtefactos_int))
rango_artefactos

#9- Calculamos la varianza de dos maneras. La primera mediante la función var(), y la segunda en la que sum((x - mean(x))^2) calcula la suma de los cuadrados de las diferencias entre cada valor de x y la media de x. Luego, se divide esta suma entre length(x) - 1 (el tamaño de x menos 1) para obtener la varianza muestral.
var_val <- var(numArtefactos_int)
var_val
var_val2 <- sum((numArtefactos_int - mean(numArtefactos_int))^2)/(length(numArtefactos_int) - 1)
var_val2

#10- Para calcular la desviación estándar utilizamos dos funciones. La primera sd(), y en la segunda utilizamos la función sqrt() dentro de la cual encontramos la función utilizada en el ejercicio anterior. 
desviacion_estandar <- sd(numArtefactos_int)
desviacion_estandar
desviacion_estandar2 <- sqrt(sum((numArtefactos_int - mean(numArtefactos_int))^2)/(length(numArtefactos_int) - 1))
desviacion_estandar2

#11- La principal diferencia entre la desviación estándar y la varianza es que la desviación estándar se expresa en las mismas unidades que los datos originales, mientras que la varianza se expresa en unidades cuadradas. La desviación estándar es una medida más intuitiva de la dispersión y es más comúnmente utilizada en la práctica.

#12- Por lo general, para crear gráficas de dispersión es necesario dos objetos. Pero si se quiere realizar una de un solo objeto podemos crear una gráfica de caja y bigotes. 
library(ggplot2)
boxplot(numArtefactos_int, horizontal = TRUE)

#13- Creamos un nuevo vector.
vector3 <- c(21, 45, 33, 98, 34, 90, 67, 87, 45, 11, 73, 38, 28, 15, 50, 57, 12, 87, 29, 1)

#14- Para calcular el coeficiente de variación usamos dos funciones diferentes. Para interpretar los resultados dependeriamos del contexto de los datos, pero con la comparación podemos observar que el resultado de numArtefactos_int es mayor, por lo que tiene una variabilidad relativa mayor.
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

#15- Creo una función en la que indico los estadísticos descriptivos que quiero que aparezcan en mi tabla-resumen, y les doy un nombre para su identificación. Antes de esto se debe cargar library("psych") para que la función describe() funcione.
library(psych)
tabla_resumen <- function(x) {
  resumen <- describe(x)
  resumen_seleccionado <- cbind(resumen$mean, resumen$sd, resumen$min, resumen$max, resumen$median, resumen$range, IQR(x), coef_var(x), var(x))
  colnames(resumen_seleccionado) <- c("Media", "SD", "Min", "Max", "Mediana", "Rango", "Rango Intercuartil", "Coeficiente Variacion", "Var")
  return(resumen_seleccionado)
}
tabla_resumen(numArtefactos_int)

#16- Primero instalo el paquete e1071 para utilizar la función skewness() que me dará el coeficiente de asimetría. El resultado indica que hay una ligera asimetría positiva en los datos. Una distribución simétrica daría lugar si el coeficiente fuera 0. El signo del coeficiente de asimetría indica la dirección de la asimetría. Si es negativo indica que la cola de la distribución se extiende hacia la izquierda, lo que significa que hay valores extremos en la parte inferior de la distribución. Si es positivo indica que la cola de la distribución se extiende hacia la derecha, lo que significa que hay valores extremos en la parte superior de la distribucion.(Ej: -0.3232, 0, 0.3123) 
install.packages("e1071")
library(e1071)
skewness(vector3) 

#17- Tiene una curtosis puntiaguda (distribucion leptocurtica). Tiene una mayor proporción de valores extremos en comparación con una distribución normal. En otras palabras, la distribución tiene valores que se agrupan en torno a la media y una mayor frecuencia de valores extremos en comparación con una distribución normal. Puede tener un pico alto y delgado en su forma, lo que indica que los datos se agrupan en una estrecha gama de valores.
install.packages("moments")
library(moments)
kurtosis(vector3)
