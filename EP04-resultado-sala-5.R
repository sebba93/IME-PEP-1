#INTEGRANTES: Sofía Castro     -> 20.055.286-5
#             Sebastián Astete -> 18.562.196-0
#             Bastián Loyola   -> 20.552.001-5
#             Bryan Salas      -> 19.316.410-2
#PREGUNTA 1
#H0 (hipótesis nula) =  El tiempo de activación promedio para los los sistemas 
#ásperos de prevención no sobrepasa los 25 segundos
#µ0 = 25
#HA (hipótesis alternativa) = El tiempo de activación  promedio para los los sistemas 
#ásperos de prevención sobrepasa los 25 segundos
#µA > 25
library(dplyr)
library(TeachingDemos)
library(ggpubr)
muestra_bomberos <- c(27,41,22,27,23,35,30,33,24,27,28,23,24)

#Se determinan los datos ya conocidos
n <- length(muestra_bomberos)
grados_libertad <- n-1
valor_nulo <- 25

#Debido a que sólo existe un grupo de muestras, se consideró realizar una prueba z pero dado que la cantidad de observaciones era insuficiente se decantó por una prueba T student para una muestra
#Esta prueba consta de dos condiciones
#1- Que las observaciones son independientes entre sí
#2- Que las observaciones provengan de una distribución cercana  a la normal

#Para comprobar estás condiciones analizaremos los datos
#Dado que las observaciones son elegidas aleatoriamente podemos decir que estas son independientes, cumpliendo la primera condición

#Para comprobar que provengan de una dsitribucion cercana a la normal se puede revisar mediante un grafico Q-Q
#En el cual no deben existir muchos puntos fuera del sector marcado que representa la distribución normal
g <- ggqqplot ( data = data.frame ( muestra_bomberos ) ,
                x = "muestra_bomberos",
                color = " steelblue ",
                xlab = "Teórico",
                ylab = "Muestra",
                title = "Gráfico Q-Q muestra v/s distr . normal ")

print(g)

#como se puede apreciar el gráfico los puntos se encuentran dentro del sector aceptado, por lo tanto es una distribución cercana a la normal
#A continuación se realizará la prueba T

#nivel de significación
significancia_P1 <-0.01


# Se aplica la prueba t de Student
prueba <- t.test ( muestra_bomberos ,
                       alternative = "greater",
                       mu = valor_nulo ,
                       conf.level = 1 - significancia_P1 )

print ( prueba )

#Dado que nuestro valor de signifacion es menor al valor de p, no sé puede rechazar la hipotesis nula


#PREGUNTA 2: 
#Este ejercicio posee muestras pareadas, dado que tienen correspondencia en una observación.
#Ademas, la elección de los elementos de la muestra es al azar.
#up= promedio de posdestete
#ul= promedio de lactancia
#H0 (hipótesis nula) = La diferencia entre el contenido total de minerales en los huesos 
#del cuerpo durante el posdestete y la etapa de lactancia es igual 30.
#µp-ul = 30
#HA (hipótesis alternativa) = La diferencia entre el contenido total de minerales en los huesos
#del cuerpo durante el posdestete excede el de la etapa de lactancia por más de 30
#up-ul > 30
lactancia <- c(2875,1893,1978,2599,1974,2671,2164,2225,2591,1678)
posdestete <- c(2895,2006,2126,2885,1942,2626,2164,2184,2627,1750)
resta_P2<- posdestete-lactancia
shapiro_P2<- shapiro.test(resta_P2)
print(shapiro_P2)

g21 <- ggqqplot(lactancia, x = NULL, color = "steelblue",
               xlab = "Teórico", ylab = "Muestra",
               title = "lactancia - Gráfico Q-Q muestra v/s distr. normal"
)
print(g21)

g22 <- ggqqplot(posdestete, x = NULL, color = "steelblue",
               xlab = "Teórico", ylab = "Muestra",
               title = "posdestete - Gráfico Q-Q muestra v/s distr. normal"
)

print(g22)

# La prueba de Shapiro nos indica que no se debe rechazar la hipótesis nula, 
#pues el valor de p es mayor al nivel de significancia estipulado
significancia_P2 <-0.01
prueba_t<-t.test(resta_P2 ,
                 alternative = "greater",
                 mu = 30, conf.level = 1 - significancia_P2)
print(prueba_t)
#El valor de t se acerca a 0 y el valor de p es mayor al nivel de significancia, 
#por lo que no se rechaza la hipotesis nula, sino que se puede concluir con un 99% 
#de confianza que  el contenido total de minerales en los huesos 
#del cuerpo durante el posdestete no excede el de la etapa de lactancia por más de 30.


#PREGUNTA 3
#H0 = No hay diferencia en eficiencia entre el suplemento de "caseína" (uC) y el de "soya" (uS)
#H0 = uC = uS
#HA = Hay diferencia en eficiencia entre el suplemento de "caseína" (uC) y el de "soya" (uS)
#HA = ??C =! ??S
library(dplyr)
pollitos <- chickwts
pollitos_c <- filter(pollitos, feed == "casein")
pollitos_s <- filter(pollitos, feed == "soybean")
pollitos_c_peso <- pollitos_c %>% pull(weight)
pollitos_s_peso <- pollitos_s %>% pull(weight)

plot(chickwts$weight~chickwts$feed)
hist(chickwts$weight)

g0 <- ggqqplot(chickwts, x = "weight", color = "steelblue",
               xlab = "Teórico", ylab = "Muestra",
               title = "chickwts - Gráfico Q-Q muestra v/s distr. normal"
)
print(g0)

#Usaremos la prueba "T Student" para dos muestras independientes
#(1)Tanto las muestras como las observaciónes son independientes, ya que los pollitos
#fueron seleccionados de manera azar, así como el suplemento a usar.
#(2)Para verificar la normalidad de la muestra usaremos el test de Shapiro-Wilk

#Usando un nivel de significación de 0.01, hacemos el test.

normalidad_pollitos_c_peso <- shapiro.test(pollitos_c_peso)
print(normalidad_pollitos_c_peso)

normalidad_pollitos_s_peso <- shapiro.test(pollitos_s_peso)
print(normalidad_pollitos_s_peso)


p_casein <- subset(chickwts, chickwts$feed == "casein")
p_soybean <- subset(chickwts, chickwts$feed == "soybean")

hist(p_casein$weight)
hist(p_soybean$weight)

g1 <- ggqqplot(p_casein, x = "weight", color = "steelblue",
               xlab = "Teórico", ylab = "Muestra",
               title = "Casein - Gráfico Q-Q muestra v/s distr. normal"
)

g2 <- ggqqplot(p_soybean, x = "weight", color = "steelblue",
               xlab = "Teórico", ylab = "Muestra",
               title = "Soybean - Gráfico Q-Q muestra v/s distr. normal"
)

print(g1)
print(g2)


#Los valores calculados en "P" son mayores que nuestro nivel de 
#significanción y nos permiten concluir que la distribución de ambas 
#es de tipo normal por lo que aplicaremos el "T student"

significacion <- 0.01

prueba_t <- t.test(x = pollitos_c_peso ,
                   y = pollitos_s_peso ,
                   paired = FALSE ,
                   alternative="two.sided" ,
                   mu = 0 ,
                   conf.level = 1 - significacion)

print(prueba_t)

#Al aplicar el test podemos ver que en primer lugar que SÍ existe una diferencia 
#entre las eficiencias entre los suplementos, por lo que se descarta la H0 y se
#acepta la HA
