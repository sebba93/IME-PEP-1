library (ggpubr)

basename <- "Casen 2017.csv"
file <- file.path(getwd(), basename)
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
set.seed(69420)
ingreso.normal <- rnorm(5000, mean = media.ingreso, sd = sd.ingreso)
set.seed(69420)
a <- rnorm(5000)

ingreso.estandar <- (ingreso.normal - media.ingreso) / sd.ingreso

hist(ingreso.normal, prob=TRUE)
lines(density(ingreso.normal), col="blue", lwd=2) # add a density estimate with defaults
lines(density(ingreso.normal, adjust=2), col="green", lwd=2)

hist(ingreso.estandar, prob=TRUE)
lines(density(ingreso.estandar), col="blue", lwd=2) # add a density estimate with defaults
lines(density(ingreso.estandar, adjust=2), col="green", lwd=2)

chi <- (((ingreso.estandar - 14)^2) / 14)
chis <- rchisq(5000, 14)

hist(chi2, 
     breaks = 'Scott', 
     freq = FALSE, 
     xlim = c(0,5), 
     ylim = c(0,1),
     xlab = '',
     ylab = '',
     main = "Distribución Chi 1",
     cex.main=0.9)

chi2 <- (((ingreso.estandar - 4)^2) / 4)
chis2 <- rchisq(5000, 4)

hist(chi2, 
     breaks = 'Scott', 
     freq = FALSE, 
     xlim = c(0,5), 
     ylim = c(0,1),
     xlab = '',
     ylab = '',
     main = "Distribución Chi 2",
     cex.main=0.9)


# gráfico distribución F
x <- rf(5000, df1 = 4, df2 = 14)

hist(x, 
     breaks = 'Scott', 
     freq = FALSE, 
     xlim = c(0,5), 
     ylim = c(0,1),
     xlab = '',
     ylab = '',
     main = "Distribución F",
     cex.main=0.9)

curve(df(x, df1 = 4, df2 = 14), col= 'blue', lwd=2, add = T)


hist(chi)
hist(chi2)

##############################################################

