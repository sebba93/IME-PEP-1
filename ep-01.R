setwd("C:\\CosasdeSeba\\Usach\\08Semestre\\Inferencia y Modelos Estadisticos\\EP-01")
datos <- read.csv("ejercicioPractico.csv",sep = ",",stringsAsFactors = FALSE)

#Demostar que los rangos corresponden
datos[["X2021.03.01"]]
datos[[365]]

datos[["X2021.08.31"]]
datos[[548]]

#Marzo
rowSums(datos[9,365:395])
#Abril
rowSums(datos[9,396:425])
#Mayo
rowSums(datos[9,426:456])
#Junio
rowSums(datos[9,457:486])
#Julio
rowSums(datos[9,487:517])
#Agosto
rowSums(datos[9,518:548])

