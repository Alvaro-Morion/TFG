
# Tráfico Barcelona -------------------------------------------------------


b19 = read.csv("./traficoCat/B19.csv", sep = ",", na.strings = "Mesura no disponible");
b19 = b19[b19$Mes >=3 & b19$Mes <= 6, ]
b19$Codi_tipus_dia = as.factor(b19$Codi_tipus_dia)
b19$Valor_IMD = as.numeric(b19$Valor_IMD)
ref = tapply(b19$Valor_IMD, b19$Codi_tipus_dia, mean, na.rm = T)
med19 = (ref[1] + 3*ref[2] + ref[3] + ref[4] + ref[5])/7
b20 = read.csv("./traficoCat/B20.csv", sep = ",", na.strings = "Mesura no disponible");
b20 = b20[b20$Mes >=3 & b20$Mes<= 6, ]
b20$Codi_tipus_dia = as.factor(b20$Codi_tipus_dia)
b20$Valor_IMD = as.numeric(b20$Valor_IMD)
conf = tapply(b20$Valor_IMD, b20$Codi_tipus_dia, mean, na.rm = T)
med20 = (conf[1] + 3*conf[2] + conf[3] + conf[4] + conf[5])/7
(med20-med19)/med19*100

# Sabadell se ha hecho a mano al no ser posible descargar los datos de la fuente. 