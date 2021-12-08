#INTEGRANTES: Sofía Castro     -> 20.055.286-5
#             Sebastián Astete -> 18.562.196-0
#             Bastián Loyola   -> 20.552.001-5
#             Bryan Salas      -> 19.316.410-2
library(dplyr)
library(pwr)
#Se establecen las muestras respecto al cáncer oral por alcohol
alcohol<-c('0','1-9','10-44','45 o más')
cancer_oral <- c(43,89,109,242)
controles <- c(108,141,91,107)
incidencia_A <- data.frame(alcohol,cancer_oral,controles)
#Se establecen las muestras respecto al cáncer oral por tabaco
tabaco<-c('0','1-9','20-39','40 o más')
cancer_oral <- c(26,66,248,143)
controles <- c(85,97,197,68)
incidencia_T <- data.frame(alcohol,cancer_oral,controles)

#PREGUNTA 1
#Estudios previos habían determinado que la incidencia de cáncer oral en la 
#población general que bebe regularmente entre 10 y 44 ml de alcohol era de 60%.
#¿Respaldan estos datos tal estimación? 

#Dado que se busca respaldar, que la incidencia de cáncer oral en la población general que bebe entre 10 y 44 ml de alcohol
#Se utilizará el método de Wald para una proporción
# primero se calcula el intervalo de confianza mediante los valores de la probabilidad de éxito y el error estadístico
# y luego se realizará una prueba de hipotesis, para demostrar si se puede rechazar o no la hipotesis nula

#Estableciendo las Hipótesis
#H0: p = 0.6, la incidencia de cáncer oral que bebe regularmente entre 10 y 44 ml de alcohol es igual a 0.6
#HA: p != 0.6, la incidencia de cáncer oral que bebe regularmente entra 10 y 44 ml de alcohol es distinto a 0.6
#Supuesto: distribución normal para la proporción
#Se extraen los datos respecto a la incidencia para el caso en que se beba entre 10 y 44 ml de alcohol
casos <- incidencia_A[3,2]
controles <- incidencia_A[3,3]
#Se establece el valor nulo
valor_nulo <- 0.6
#Se calcula la probabilidad de éxito, dividiendo los casos en la suma de casos más controles
suma <- casos+controles
p_exito <- casos/suma
#Se establece el nivel de significación
alfa=0.01
#Se calcula el error estadístico
error_est <- sqrt((p_exito * (1 - p_exito)) / suma)
#Se determina el valor de z para nuestro nivel de significación
Z_critico <- qnorm(alfa / 2, lower.tail = FALSE)
#Se determinan los extremos del intervalo de confianza
inferior <- p_exito - Z_critico * error_est
superior <- p_exito + Z_critico * error_est
print(inferior)
print(superior)
# Prueba de hipótesis.
# Se calcula el valor del error estadístico para nuestro valor nulo
error_est_hip <- sqrt((valor_nulo * (1 - valor_nulo)) / suma)
# Se determina el valor p acorde a un valor de una distribución normal y un valor z
Z <- (p_exito - valor_nulo) / error_est_hip
p <- pnorm(Z, lower.tail = FALSE)
cat("Hipótesis alternativa bilateral\n")
cat("Z =", Z, "\n")
cat("p =", p)
#Resultado: Con los valores obtenidos, 
#se determina que no es posible rechazar la hipótesis nula, dado que
# p>alfa, por lo que los datos respaldan la estimación estipulada.

#PREGUNTA 2
#Según estos datos, ¿da lo mismo beber de 10 a 44 ml de alcohol diariamente 
#que hacerlo con 45 o más ml? 
#Supuesto 1: Por cada rango de edad, este se considera una población
#Supuesto 2: Distribución normal para la proporción
#pa: proporción de 10-44 de alcohol por día.
#pa2: proporción de 45-50 de alcohol por día.

# Dado que se busca respaldar que la incidencia de cáncer oral es indistinta para las proporciones pa y pa2
# Se utilizará el método de Wald para dos proporciones
# Primero se calculará el intervalo de confianza mediante los valores de la probabilidad de éxito y el error estadístico de cada proporción
# Luego se realizará la prueba de hipotesis, para demostrar si se puede rechazar o no la hipotesis nula

#H0: pa-pa2=0, #La diferencia entre las incidencias es igual a 0
#HA: pa-pa2!=0 #La diferencia entre las incidencias es distinto de 0
#Se extraen los datos respecto a la incidencia para el caso en que se beba 45 o más ml de alcohol
casos2 <- incidencia_A[4,2]
controles2 <- incidencia_A[4,3]
#Se establece el valor nulo
valor_nulo2 <- 0
#Se calcula la probabilidad de éxito, dividiendo los casos en la suma de casos más controles
suma2 <- casos2+controles2
p_exito2 <- casos2/suma2
#Se restan las probabilidades de éxito, de ambos casos siendo p_exito de aquellas que beben entre 10 y 44 ml, obtenida desde la pregunta 1 y p_exito2 pertenece a aquellas que beben 45 o más ml de alcohol
#Para estimar la diferencia
resta <- p_exito-p_exito2
#Se calcula el valor estadístico en base a los errores estadísticos de las dos cantidades de alcohol estudiadas
error1 <- (p_exito2 * (1 - p_exito2)) / suma2
error2 <- (p_exito* (1 - p_exito)) / suma
error_est <- sqrt(error1 + error2)
#Se calcula el valor de z, de una distribución normal
Z_critico <- qnorm(alfa / 2, lower.tail = FALSE)
#Se determina el intervalo de confianza
inferior <- resta - Z_critico * error_est
superior <- resta + Z_critico * error_est
#Prueba de hipotesis
#Se calcula la probabilidad de éxito agrupada
p_agrupada <- (p_exito2 + p_exito) / (suma2 + suma) 
#Se determina el valor del error estadístico, con la probabilidad agrupada respecto a los valores de cada uno los de intervalos de ml estudiados
error1 <- (p_agrupada * (1 - p_agrupada)) / suma2
error2 <- (p_agrupada * (1 - p_agrupada)) / suma
error_est_hip <- sqrt(error1 + error2)
#Se determina el valor p, acorde a una distribución normal y un valor z
Z2 <- (resta - valor_nulo) / error_est_hip 
p2 <- 2 * pnorm(Z, lower.tail = FALSE)
cat("Hipótesis alternativa bilateral\n")
cat("Z =", Z2, "\n")
cat("p =", p2)
#Resultado: Con los valores obtenidos, 
#se determina que no es posible rechazar la hipótesis nula, dado que
# p>alfa, por lo que los datos respaldan la estimación estipulada
#con un 99% de confianza.

#PREGUNTA 3
# Suponiendo que la diferencia en la proporción de personas que desarrollan la enfermedad entre quienes
# beben de 10 a 44 ml de alcohol por día y aquellos que beben 45 o más ml al día es de 0.20. ¿Cuánta
# gente deberíamos monitorear para obtener un intervalo de confianza del 99% y poder estadístico de 85%? 
# si se intente mantener aproximadamente la misma proporción de gente estudiada en cada caso.

#Supuesto: Por cada rango de edad, este se considera una población y de igual cantidad.

# Dado que queremos conocer la cantidad de personas que necesitamos monitorear para obtener un intervalo de confianza 
# del 99% y un poder estadístico del 85%, sabiendo que la diferencia de proporción es del 0.20 y aplicando el 
# supuesto anteriormente señalado. Hacemos uso de la función pwr.2p.test, la cual recibe cuatro de los
# argumentos y al restante se le asigna el valor NULL. Como resultado, retorna un objeto que incluye el
# valor calculado para el argumento faltante, que para este caso es "n", es decir,
# la cantidad de personas que necesitamos monitorear

resultado <- pwr.2p.test(h=0.20,n=NULL, sig.level=0.01, power=0.85, 
                         alternative="two.sided")

print(ceiling(resultado$n))
# Como resultado de la función anterior, esta nos dice que el valor de n es de 652.4221, pero como siempre
# se debe aproximar al siguiente número entero, hacemos uso de la función ceiling,
# por lo que la cantidad de personas debe ser 653 para cada caso.
