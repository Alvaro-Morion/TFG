library(lubridate)
library(lmtest)
library(ggplot2)
library(GGally)
library(dplyr)

# carga y limpieza de datos meteorológicos

rm = c("indicativo", "provincia", "altitud", "horatmin", "horatmax", "horaracha"
       , "horaPresMax", "horaPresMin");
# Bilbao Ap.
bil = read.csv("./meteo/bil1216.csv", sep = "")
bil = rbind(bil, read.csv("./meteo/bil1720.csv", sep = ""))
bil = bil[,!(names(bil) %in% rm)]
bil$fecha = as.Date(bil$fecha);
bil = bil[as.numeric(format(bil$fecha, '%m%d')) >= 315 & as.numeric(format(bil$fecha, '%m%d')) <=620,]
for (i in 3:length(bil))
{
  bil[,i] = as.numeric(bil[,i])
}
bil = bil[,!names(bil) == 'nombre'];
bil$dir[bil$dir == 99] = NA

#Punta gaena (2020 para viento fuera del valle)

pg = read.csv("./meteo/pg1216.csv", sep = "")
pg = rbind(pg, read.csv("./meteo/pg1720.csv", sep = ""))
pg = pg[,!(names(pg) %in% rm)]
pg$fecha = as.Date(pg$fecha);
pg = pg[as.numeric(format(pg$fecha, '%m%d')) >= 315 & as.numeric(format(pg$fecha, '%m%d')) <=620,]
for (i in 3:length(pg))
{
  pg[,i] = as.numeric(pg[,i])
}
pg = pg[,!names(pg) == 'nombre'];
pg$dir[pg$dir == 99] = NA;
#Vitoria Ap.
vit = read.csv("./meteo/vit1216.csv", sep ="");
vit = rbind(vit, read.csv("./meteo/vit1720.csv", sep = ""));
vit = vit[,!(names(vit) %in% rm)];
vit$fecha = as.Date(vit$fecha);
vit = vit[as.numeric(format(vit$fecha, '%m%d')) >= 315 & as.numeric(format(vit$fecha, '%m%d')) <=620,]
for (i in 3:length(vit))
{
  vit[,i] = as.numeric(vit[,i])
}
vit = vit[,!names(vit) == 'nombre'];

# Barcelona
bar = read.csv("./meteo/brna1115.csv", sep = "")
bar = rbind(bar, read.csv("./meteo/brna1619.csv", sep = ""))
bar = rbind(bar, read.csv("./meteo/brna20.csv", sep = ""))
bar = bar[,!(names(bar) %in% rm)]
bar$fecha = as.Date(bar$fecha);
bar = bar[as.numeric(format(bar$fecha, '%m%d')) >= 315 & as.numeric(format(bar$fecha, '%m%d')) <=620,]
for (i in 3:length(bar))
{
  bar[,i] = as.numeric(bar[,i])
}
bar = bar[,!names(bar) == 'nombre'];
remove(rm, i)

# Sabadell
sabadell = read.csv("./meteo/Dades_meteorol_giques_de_la_XEMA.csv", sep = ",")
sabadell = sabadell[sabadell$CODI_ESTAT == "V", names(sabadell) %in% c("CODI_ESTACIO", "CODI_VARIABLE", "DATA_LECTURA", "VALOR_LECTURA")]
sabadell$DATA_LECTURA = as.Date(sabadell$DATA_LECTURA, format = '%d/%m/%Y ')
velmedia = sabadell[sabadell$CODI_VARIABLE == 30, c(3,4)]
velmedia = variable_daily_mean(velmedia, "velmedia")
dir = sabadell[sabadell$CODI_VARIABLE == 31, c(3,4)]
dir = variable_daily_mean(dir, "dir");
tmed = sabadell[sabadell$CODI_VARIABLE == 32, c(3,4)]
tmed = variable_daily_mean(tmed, "tmed");
prec = sabadell[sabadell$CODI_VARIABLE == 35, c(3,4)]
prec = variable_daily_sum(prec, "prec")
tmax = sabadell[sabadell$CODI_VARIABLE == 40, c(3,4)]
tmax = variable_daily_max(tmax, "tmax")
tmin = sabadell[sabadell$CODI_VARIABLE == 42, c(3,4)]
tmin = variable_daily_min(tmin, "tmin")
remove(sabadell)

sab = merge(tmed, prec, by = "fecha", all = TRUE)
sab = merge(sab, tmin, by = "fecha", all = TRUE)
sab = merge(sab, tmax, by = "fecha", all = TRUE)
sab = merge(sab, dir, by = "fecha", all = TRUE)
sab = merge(sab, velmedia, by = "fecha", all = TRUE)
remove(dir, prec, tmax, tmin, tmed, velmedia)

colnames(sab)<-c("fecha", "tmed", "prec", "tmin", "tmax", "dir", "velmedia")
sab$fecha = as.Date(sab$fecha, origin = "1970-01-01");
sab = sab[as.numeric(format(sab$fecha, '%m%d')) >= 315 & as.numeric(format(sab$fecha, '%m%d')) <=620,]



# Anomalías de las variables meteorológicas respecto a 2011/2012 (prec en procentaje)
prec_anomaly <- function (x)
{
  value = sum(x$prec[as.numeric(format(x$fecha, '%Y')) == 2012], na.rm = T)
  for (i in 2013:2020){
    value = rbind(value, sum(x$prec[as.numeric(format(x$fecha, '%Y')) == i], na.rm = T))
  }
  ref = median(value[1:length(value) - 1])
  cat("Anomalía: ", value[length(value)]-ref, " %: ", value[length(value)]/ref*100)
}

cat("Barcelona:\n") 
  prec_anomaly(bar)
cat("Sabadell:\n")
  prec_anomaly(sab);
cat("Bilbao:\n")
  prec_anomaly(bil);
cat("Vitoria:\n")
  prec_anomaly(vit);

# Anomalías de estancamientos en Bilbao
data = c(2012, length(pg$dir[pg$fecha < "2013-01-01" & (pg$dir < 9 | (pg$dir > 18 & pg$dir < 27))]))
for (i in 2013:2020){
  data = rbind(data, c(i,length(pg$dir[as.numeric(format(pg$fecha, '%Y')) == i & (pg$dir < 9 | (pg$dir > 18 & pg$dir < 27))])))
}
data = as.data.frame(data)
names(data) = c("year", "days")
ref = mean(data$days[data$year < 2020]); ref
sd(data$days[data$year < 2020])
cat("Anomalía de días de estancamiento:")
# comprobación del valor del 2020
anom = data$days[data$year == 2020] - mean(data$days[data$year != 2020]); anom
anom/sd(data$days[data$year != 2020])
# Incorporación de concentraciones de NO2 ---------------------------------

cbar = read.csv("./meteo/bar.csv"); cbar=cbar[,!names(cbar) == 'X']; 
cbar$fecha =as.Date(cbar$fecha)
bar = merge(bar, cbar, by = 'fecha');
csab = read.csv("./meteo/sab.csv"); csab=csab[,!names(csab) =='X'];
csab$fecha =as.Date(csab$fecha)
sab = merge(sab, csab, by = 'fecha');
cbil = read.csv("./meteo/Maz.csv"); cbil=cbil[,!names(cbil) =='X'];
cbil$fecha =as.Date(cbil$fecha)
bil = merge(bil, cbil, by = 'fecha');
cvit = read.csv("./meteo/Gas.csv"); cvit=cvit[,!names(cvit) == 'X'];
cvit$fecha =as.Date(cvit$fecha)
vit= merge(vit, cvit, by = 'fecha');

# Episodios concretos ------------------------

# Estancamineto en el valle

bil[bil$fecha >= "2020-03-26" & bil$fecha <= "2020-03-31", ]
pg[pg$fecha >= "2020-03-26" & pg$fecha <= "2020-03-31", ]