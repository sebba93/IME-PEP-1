#Diego Alvarado 20.283.543-0
#Benjamín Jorquera 19.182.719-8
#Sebastián Astete 18.562.196-0
#Joaquín Torres 19.091.702-9

if (!require(ggpubr)){
  install.packages("ggpubr", dependencies = TRUE )
  require (ggpubr)
}

if (!require(tidyverse)){
  install.packages("tidyverse", dependencies = TRUE )
  require (tidyverse)
}

if (!require(dplyr)){
  install.packages("dplyr", dependencies = TRUE )
  require (dplyr)
}

if (!require(car)){
  install.packages("car", dependencies = TRUE )
  require (car)
}

data <- read.csv(file.choose(), head = TRUE, sep=" ")
#1.-Definir la semilla a utilizar, que corresponde a los últimos cuatro dígitos 
#del RUN (sin considerar el dígito verificador) del integrante de menor edad 
#del equipo.
set.seed(3543)

#2.-Seleccionar una muestra de 50 mujeres (si la semilla es un número par) o 
#50 hombres (si la semilla es impar).
data <- filter(data, Gender>0)
muestra <- sample_n(data, size= 50)


#3. Seleccionar de forma aleatoria ocho posibles variables predictoras.
#se guardan todas las variables en un dataframe 
variables <- data.frame(c("Biacromial.diameter", "Biiliac.diameter", "Bitrochanteric.diameter", "Chest.depth", "Chest.diameter", "Elbows.diameter", "Wrists.diameter",
                  "Knees.diameter", "Ankles.diameter","Shoulder.Girth", "Chest.Girth", "Waist.Girth", "Navel.Girth", "Hip.Girth", "Thigh.Girth", "Bicep.Girth",
                  "Forearm.Girth", "Knee.Girth", "Calf.Maximum.Girth", "Ankle.Minimum.Girth", "Wrist.Minimum.Girth", "Age", "Height"))
colnames(variables) <- c("variable")
#se escogen 8 aleatoriamente
vp8 <- sample_n(variables, size = 8)
print(vp8)

# 1
# Age
# 2
# Bitrochanteric.diameter
# 3
# Knee.Girth
# 4
# Thigh.Girth
# 5
# Shoulder.Girth
# 6
# Biacromial.diameter
# 7
# Chest.diameter
# 8
# Wrist.Minimum.Girth

#4. Seleccionar, de las otras variables, una que el equipo considere que podría ser útil para predecir la variable Peso, justificando bien esta selección.
#se escoge la altura, bajo la postura de que mayor altura implica mayor volumen variablePredictora = "Height"


#5.-Usando el entorno R, construir un modelo de regresión lineal simple con el 
#predictor seleccionado en el paso anterior.

regresionSimple <- lm(muestra$Weight~muestra$Height,data = muestra)
summary(regresionSimple)
plot(regresionSimple)


#6.- Usando herramientas para la exploración de modelos del entorno R, buscar 
#entre dos y cinco predictores de entre las variables seleccionadas al azar en 
#el punto 3, para agregar al modelo de regresión lineal simple obtenido en el paso 5.

#se escogen: Age, Thigh.Girth, Shoulder.Girth
regresionMultiple <- update(regresionSimple, . ~ . + muestra$Age + muestra$Thigh.Girth + muestra$Shoulder.Girth)
summary(regresionMultiple)
plot(regresionMultiple)


# 7. Evaluar los modelos y “arreglarlos” en caso de que tengan algún problema con las condiciones que deben cumplir.



#condiciones para la regresión lineal simple
# 1. Los datos deben presentar una relación lineal: revisando el valor de R = 0.2658 se posee una correlación débil positiva

# 2. La distribución de los residuos debe ser cercana a la normal: analizando el gráfico Normal Q-Q podemos observar que si cumple

# 3. La variabilidad de los puntos en torno a la línea de mínimos cuadrados debe ser aproximadamente
# constante: analizando el grafico Residuals vs Fitted vemos que se obtiene puntos en torno a la línea de mínimos cuadrados 
#     aproximadamente constantes






#condiciones para la regresión lineal multiple
# 1. Las variables predictoras deben ser cuantitativas o dicotómicas (de ahí la necesidad de variables indicadoras para manejar más de dos niveles).
#    Las variables predictoras son todas cuantitativas

# 2. La variable de respuesta debe ser cuantitativa y continua, sin restricciones para su variabilidad.
#    La variable de respuesta (peso) es cuantitativa y continua

# 3. Los predictores deben tener algún grado de variabilidad (su varianza no debe ser igual a cero). En otras
# palabras, no pueden ser constantes.

varHeight <- var(muestra$Height)
varAge <- var(muestra$Age)
varThighGirth <- var(muestra$Thigh.Girth)
varShoulderGirth <- var(muestra$Shoulder.Girth)

print(varHeight)
print(varAge)
print(varThighGirth)
print(varShoulderGirth)
#revisando los resultados, cumplen con la condición, ninguna es igual a 0


# 4. No debe existir multicolinealidad. Esto significa que no deben existir relaciones lineales fuertes entre
# dos o más predictores (coeficientes de correlación altos).
vifs <- vif(regresionMultiple)
cat ("\nVerificar la multicolinealidad :\n")
cat ("-VIFs :\n")
print(vifs)
#observando los valores vemos que si cumple la condición


# 5. Los residuos deben ser homocedásticos (con varianzas similares) para cada nivel de los predictores.
pruebaNcv <- ncvTest(regresionMultiple)
print(pruebaNcv)
#obteniendo un valor de p = 0.33862 se cumple el supuesto de homocedasticidad

# 6. Los residuos deben seguir una distribución cercana a la normal centrada en cero.
print(shapiro.test(regresionMultiple$residuals))
#obteniendo un valor de p = 0.8776 se cumple el supuesto de normalidad



# 7. Los valores de la variable de respuesta son independientes entre sí.
pruebaDW <- durbinWatsonTest(regresionMultiple)
print(pruebaDW)
#obteniendo un valor de p = 0.064 y considerando alfa = 0.05 podemos concluir que los residuos son, en efecto, independientes.


# 8. Cada predictor se relaciona linealmente con la variable de respuesta.
a <- lm(muestra$Weight~muestra$Age,data = muestra)
print("\n*************\n")
print(summary(a))

b <- lm(muestra$Weight~muestra$Thigh.Girth,data = muestra)
print("\n*************\n")
print(summary(b))

c <- lm(muestra$Weight~muestra$Shoulder.Girth,data = muestra)
print("\n*************\n")
print(summary(c))

#para el caso de Age es bastante baja la relacion, con respecto a las variables Thigh.Girth y Shoulder.Girth existe relacion 
#lineal pero esta no es fuerte



#8. Evaluar el poder predictivo del modelo en datos no utilizados para construirlo (o utilizando validación cruzada).

prediccionModSimple <- predict(regresionSimple, muestra)
errorModSimple <- muestra[["Height"]] - prediccionModSimple
mse_ModSimple <- mean(errorModSimple**2)
cat(" MSE para el conjunto de prueba :", mse_ModSimple)






















