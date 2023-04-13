##Ej 1. Las Variables MAT, CON, LOO, PEG son nominales. La variable COND es ordinal. La variable DATE es interval. Las variables MAXLE, SOCLE, MAXWI, UPSOC, LOSOC, MAWIT, WEIGHT son ratio.
##Ej 2. 
##Ej 3. Tabla importada
##Ej 4. 
library(stats)
names(spearheads)[names(spearheads) == "Mat"] <- "Materiales"
names(spearheads)[names(spearheads) == "Con"] <- "Contexto"
names(spearheads)[names(spearheads) == "Cond"] <- "Conservacion"
names(spearheads)[names(spearheads) == "Peg"] <- "Remache"
names(spearheads)[names(spearheads) == "Maxle"] <- "Longitud_max"
names(spearheads)[names(spearheads) == "Loo"] <- "Loop"
names(spearheads)[names(spearheads) == "Date"] <- "Fecha"
names(spearheads)[names(spearheads) == "Socle"] <- "Longitud_encaje"
names(spearheads)[names(spearheads) == "Maxwi"] <- "Ancho_max"
names(spearheads)[names(spearheads) == "Upsoc"] <- "Ancho_encaje"
names(spearheads)[names(spearheads) == "Mawit"] <- "Ancho_max_encaje"
names(spearheads)[names(spearheads) == "Weight"] <- "Peso"
names(spearheads)[names(spearheads) == "Losoc"] <- "Maximo_encaje"

##Ej 5. 
spearheads$Contexto <- ordered(spearheads$Contexto, levels <- c(1,2,3), labels <- c("Aislado", "Habitacional", "Funerario"))
spearheads$Conservacion <- ordered(spearheads$Conservacion, levels <- c(1,2,3,4), labels <- c("Excelente", "Bueno", "Regular", "Malo"))
spearheads$Remache <- ordered(spearheads$Remache, levels <- c(1,2), labels <- c("Si", "No"))
spearheads$Materiales <- ordered(spearheads$Materiales, levels <- c(1,2), labels <- c("Bronce", "Hierro"))
spearheads$Loop <- ordered(spearheads$Loop, levels <- c(1,2), labels <- c("No", "Si"))

##Ej 6. 
Tab_Materiales <- table(spearheads$Materiales)
Tab_Materiales
Tab_Contexto <- table(spearheads$Contexto)
Tab_Contexto
Tab_Conservacion <- table(spearheads$Conservacion)
Tab_Conservacion

##Ej 7.
Tab_Materiales_Cont <- xtabs(~Materiales+Contexto, spearheads)
Tab_Materiales_Cont
Tab_Materiales_Cons <- xtabs(~Materiales+Conservacion, spearheads)
Tab_Materiales_Cons

##Ej 8.
Porc_Mat <- prop.table(Tab_Materiales) * 100
Porc_Mat
Porc_Cont <- prop.table(Tab_Contexto) * 100
Porc_Cont
Porc_Conservacion <- prop.table(Tab_Conservacion) * 100
Porc_Conservacion

##Ej 9.
Porc_Mat_Cont <- prop.table(Tab_Materiales_Cont) * 100
Porc_Mat_Cont
Porc_Mat_Cons <- prop.table(Tab_Materiales_Cons) * 100
Porc_Mat_Cons

##Ej 10.
Graf_Cons <- plot(x = spearheads$Conservacion, main = "Conservacion", col = c("red", "green", "blue", "yellow3"), ylab = "Frecuencia", xlab = "Estado de Conservacion")

Graf_Cont <- plot(x = spearheads$Contexto, main = "Contexto", col = c("red", "green", "blue"), ylab = "Frecuencia", xlab = "Tipos de Contexto")

Graf_Mat <- plot(x = spearheads$Materiales, main = "Materiales", col = c("blue", "yellow3"), xlab = "Tipo de Material", ylab = "Frecuencia")

Graf_Cons_Horiz <- plot(x = spearheads$Conservacion, main = "Conservacion", col = c("red", "green", "blue", "yellow3"), xlab = "Frecuencia", ylab = "Estado de Conservacion", horiz = TRUE)

Graf_Cont_Horiz <- plot(x = spearheads$Contexto, main = "Contexto", col = c("red", "green", "blue"), xlab = "Frecuencia", ylab = "Tipos de Contexto", horiz = TRUE)

Graf_Mat_Horiz <- plot(x = spearheads$Materiales, main = "Materiales", col = c("blue", "yellow3"), ylab = "Tipo de Material", xlab = "Frecuencia", horiz = TRUE)

##Ej 11.
Tab_Cons_Materiales <- xtabs(~Conservacion+Materiales, spearheads)
Tab_Cons_Materiales
barp <- barplot(Tab_Cons_Materiales, main = "Gráfico de barras agrupado", xlab = "Tipo de Materiales", ylab = "Cantidad de Materiales", col = c("red", "green", "blue", "yellow3"), ylim = c(0, 15), beside = TRUE) 
text(barp, Tab_Cons_Materiales + 0.5, labels = Tab_Cons_Materiales)
legend("top", c("Excelente", "Bueno", "Regular", "Malo"), title = "Conservacion", fill = c("red", "green", "blue", "yellow3"))

##Ej 12.
etiqueta <- paste0(Tab_Conservacion, " = ", round(100 * Tab_Conservacion/sum(Tab_Conservacion), 2), "%")
pie(Tab_Conservacion, clockwise = TRUE, col = c("red", "green", "blue", "yellow3"), labels = etiqueta)
legend("bottomleft", c("Excelente", "Bueno", "Regular", "Malo"), title = "Conservacion", fill = c("red", "green", "blue", "yellow3"))

##Ej 13. Linea negra densidad de Kernel, linea roja distribución normal.
spearheads_2 <- na.omit(spearheads)
spearheads_2
DLM <- density(spearheads_2$Longitud_max)
DLE <- density(spearheads_2$Longitud_encaje)
DAM <- density(spearheads_2$Ancho_max)
DAE <- density(spearheads_2$Ancho_encaje)
DME <- density(spearheads_2$Maximo_encaje)
DAME <- density(spearheads_2$Ancho_max_encaje)
DP <- density(spearheads_2$Peso)

hist(spearheads_2$Longitud_max, main = "Histograma de Probabilidad de la Longitud Max", xlab = "Longitud Maxima", ylab = "Densidad", ylim = c(0.00, 0.07), col = "green", prob = T)
lines(DLM, lwd =2, col = "black")
media <- mean(spearheads_2$Longitud_max)
sd <- sd(spearheads_2$Longitud_max)
x <- seq(min(spearheads_2$Longitud_max), max(spearheads_2$Longitud_max), length.out = 1000)
pdf <- dnorm(x, mean = media, sd = sd)
curve(dnorm(x, mean = mean(spearheads_2$Longitud_max), sd = sd(spearheads_2$Longitud_max)), add = TRUE, col = "red", lwd = 2)

hist(spearheads_2$Longitud_encaje, main = "Histograma de Probabilidad de la Longitud Encaje", xlab = "Longitud Encaje", ylab = "Densidad", ylim = c(0.00, 0.2), col = "blue", prob = T)
lines(DLE, lwd =2, col = "black")
media <- mean(spearheads_2$Longitud_encaje)
sd <- sd(spearheads_2$Longitud_encaje)
x <- seq(min(spearheads_2$Longitud_encaje), max(spearheads_2$Longitud_encaje), length.out = 1000)
pdf <- dnorm(x, mean = media, sd = sd)
curve(dnorm(x, mean = mean(spearheads_2$Longitud_encaje), sd = sd(spearheads_2$Longitud_encaje)), add = TRUE, col = "red", lwd = 2)

hist(spearheads_2$Ancho_max, main = "Histograma de Probabilidad del Ancho Maximo", xlab = "Ancho Maximo", ylab = "Densidad", ylim = c(0.00, 0.4), col = "pink4", prob = T)
lines(DAM, lwd =2, col = "black")
media <- mean(spearheads_2$Ancho_max)
sd <- sd(spearheads_2$Ancho_max)
x <- seq(min(spearheads_2$Ancho_max), max(spearheads_2$Ancho_max), length.out = 1000)
pdf <- dnorm(x, mean = media, sd = sd)
curve(dnorm(x, mean = mean(spearheads_2$Ancho_max), sd = sd(spearheads_2$Ancho_max)), add = TRUE, col = "red", lwd = 2)

hist(spearheads_2$Ancho_encaje, main = "Histograma de Probabilidad del Ancho Encaje", xlab = "Ancho Encaje", ylab = "Densidad", ylim = c(0.00, 2), col = "red4", prob = T)
lines(DAE, lwd =2, col = "black")
media <- mean(spearheads_2$Ancho_encaje)
sd <- sd(spearheads_2$Ancho_encaje)
x <- seq(min(spearheads_2$Ancho_encaje), max(spearheads_2$Ancho_encaje), length.out = 1000)
pdf <- dnorm(x, mean = media, sd = sd)
curve(dnorm(x, mean = mean(spearheads_2$Ancho_encaje), sd = sd(spearheads_2$Ancho_encaje)), add = TRUE, col = "red", lwd = 2)


hist(spearheads_2$Maximo_encaje, main = "Histograma de Probabilidad del Maximo Encaje", xlab = "Maximo Encaje", ylab = "Densidad", ylim = c(0.00, 1.5), col = "yellow4", prob = T)
lines(DME, lwd =2, col = "black")
media <- mean(spearheads_2$Maximo_encaje)
sd <- sd(spearheads_2$Maximo_encaje)
x <- seq(min(spearheads_2$Maximo_encaje), max(spearheads_2$Maximo_encaje), length.out = 1000)
pdf <- dnorm(x, mean = media, sd = sd)
curve(dnorm(x, mean = mean(spearheads_2$Maximo_encaje), sd = sd(spearheads_2$Maximo_encaje)), add = TRUE, col = "red", lwd = 2)


hist(spearheads_2$Ancho_max_encaje, main = "Histograma de Probabilidad del Ancho Maximo Encaje", xlab = "Ancho Maximo Encaje", ylab = "Densidad", ylim = c(0.00, 0.15), col = "orange", prob = T)
lines(DAME, lwd =2, col = "black")
media <- mean(spearheads_2$Ancho_max_encaje)
sd <- sd(spearheads_2$Ancho_max_encaje)
x <- seq(min(spearheads_2$Ancho_max_encaje), max(spearheads_2$Ancho_max_encaje), length.out = 1000)
pdf <- dnorm(x, mean = media, sd = sd)
curve(dnorm(x, mean = mean(spearheads_2$Ancho_max_encaje), sd = sd(spearheads_2$Ancho_max_encaje)), add = TRUE, col = "red", lwd = 2)


hist(spearheads_2$Peso, main = "Histograma de Probabilidad de Peso", xlab = "Peso", ylab = "Densidad", ylim = c(0.00, 0.002), col = "purple4", prob = T)
lines(DP, lwd =2, col = "black")
media <- mean(spearheads_2$Peso)
sd <- sd(spearheads_2$Peso)
x <- seq(min(spearheads_2$Peso), max(spearheads_2$Peso), length.out = 1000)
pdf <- dnorm(x, mean = media, sd = sd)
curve(dnorm(x, mean = mean(spearheads_2$Peso), sd = sd(spearheads_2$Peso)), add = TRUE, col = "red", lwd = 2)

##Ej 14.
boxplot(spearheads_2$Longitud_max, spearheads_2$Longitud_encaje, spearheads_2$Ancho_max, spearheads_2$Ancho_encaje, spearheads_2$Maximo_encaje, spearheads_2$Ancho_max_encaje, spearheads_2$Peso, main = "Gráfico de caja y bigote de las variables cuantitativas",
        xlab = "Variables", ylab = "Valores")

##Ej 15.
library(reshape2)
library(ggplot2)
spearheads_cuantitativo <- data.frame(spearheads_2$Longitud_max, spearheads_2$Longitud_encaje, spearheads_2$Ancho_max, spearheads_2$Ancho_encaje, spearheads_2$Maximo_encaje, spearheads_2$Ancho_max_encaje, spearheads_2$Peso)

datos_largos <- reshape2::melt(spearheads_cuantitativo)

ggplot(datos_largos, aes(x = variable, y = value)) +
  geom_violin() +
  labs(title = "Gráfico de violín de las variables cuantitativas", x = "Variables", y = "Valores")


