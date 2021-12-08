basename <- "EP-03 Datos Casen 2017.csv"
file <- file.path("IME", basename)
población <- read.csv(file = file)
tamaño <- nrow(población)
ingreso <- as.numeric(población[["ytot"]])
poda <- 0.2
q20 <- quantile(ingreso, poda)
q80 <- quantile(ingreso, 1 - poda)
ingreso.podado <- ingreso[ingreso > q20 & ingreso < q80]
tamaño.podado <- length(ingreso.podado)
media.ingreso <- mean(ingreso.podado)
sd.ingreso <- sqrt (sum((ingreso.podado - media.ingreso)^2) / tamaño.podado )
set.seed(2001)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)


z <- (ingreso.normal - media.ingreso)/sd.ingreso


libertad_seis <- sample(z,6)
libertad_doce <- sample(z,12)

libertad_seis <- libertad_seis^2
libertad_doce <- libertad_doce^2

chi_Cuadrado_seis <-sum(libertad_seis)
chi_Cuadrado_doce <-sum(libertad_doce)

f<- (chi_Cuadrado_seis/6)/(chi_Cuadrado_doce/12) 




set.seed(2001)
n.repeticiones <- 30
ensayo <- function(x)
  ifelse(sample(población[["sexo"]], 1) == "Mujer", 1, 0)
treinta.repeticiones <- sapply(1:n.repeticiones, ensayo)

#Calculo frecuencias de 1
frecuencia_1 <- table(treinta.repeticiones)[names(table(treinta.repeticiones)) == 1]
#Calculo de la frecuencia
p <- frecuencia_1/n.repeticiones

#Calculo distribución binomial
binomial = (factorial(n.repeticiones)/(factorial(treinta.repeticiones)*factorial(n.repeticiones- treinta.repeticiones)))*(p^treinta.repeticiones)*(1-p)^(n.repeticiones - treinta.repeticiones)
  
binomial

dbinom(treinta.repeticiones,30,p)
