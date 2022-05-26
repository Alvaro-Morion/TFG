source("./normal.R")

# Carga y filtro de datos (tarda un rato)--------------------------------------

cat = read.csv("Qualitat_de_l_aire_als_punts_de_mesurament_autom_tics_de_la_Xarxa_de_Vigil_ncia_i_Previsi__de_la_Contaminaci__Atmosf_rica.csv")
estaciones = levels(as.factor(cat$NOM.ESTACIO[cat$TIPUS.ESTACIO == "traffic" & cat$AREA.URBANA == "urban"]))
cat_data = cat[cat$NOM.ESTACIO == estaciones[1], ];# Selección de estaciones de tipo tráfico ubranas
for (i in 2:length(estaciones))
{
  cat_data = rbind(cat_data, cat[cat$NOM.ESTACIO == estaciones[i], ]);
}
remove(cat);
cat_data = cat_data[cat_data$CONTAMINANT == "NO2", ] # Selección de contaminante
cat_data$DATA = as.Date(cat_data$DATA, '%d/%m/%Y');
cat_data = cat_data[(as.numeric(format(cat_data$DATA, '%m')) > 3 & as.numeric(format(cat_data$DATA, '%m')) < 6) | 
                (as.numeric(format(cat_data$DATA, '%m')) == 3 & as.numeric(format(cat_data$DATA, '%d')) > 14) |
                (as.numeric(format(cat_data$DATA, '%m')) == 6 & as.numeric(format(cat_data$DATA, '%d')) < 21),];
media = apply(cat_data[,13:36], 1, mean, na.rm = TRUE); # Media diaria de los datos. 
cat_data = cbind(cat_data, media);
remove(media)
cat_conf=cat_data[cat_data$DATA > "2020-03-14" & cat_data$DATA < "2020-06-21", ]; # Fecha del confinamiento
estaciones = levels(as.factor(cat_conf$NOM.ESTACIO)) # Estaciones con datos del confinamiento

# Contraste normal ------------------------------------------------------------

for (i in 1:length(estaciones))
{
cat("---------", estaciones[i], "---------------\n")
contraste_normal(na.exclude(cat_data$media[cat_data$NOM.ESTACIO == estaciones[i]]))
fcoef(na.exclude(cat_data$media[cat_data$NOM.ESTACIO == estaciones[i]]))
Curtosis(na.exclude(cat_data$media[cat_data$NOM.ESTACIO == estaciones[i]]))
contraste_normal(na.exclude(cat_conf$media[cat_conf$NOM.ESTACIO == estaciones[i]]))
fcoef(na.exclude(cat_conf$media[cat_conf$NOM.ESTACIO == estaciones[i]]))
Curtosis(na.exclude(cat_conf$media[cat_conf$NOM.ESTACIO == estaciones[i]]))
cat("Diferencia de medias:")
cat((mean(na.exclude(cat_data$media[cat_data$NOM.ESTACIO == estaciones[i]])) - mean(na.exclude(cat_conf$media[cat_conf$NOM.ESTACIO == estaciones[i]])))/mean(na.exclude(cat_data$media[cat_data$NOM.ESTACIO == estaciones[i]])))
cat("\n")
}

# Se eligen las estaciones de Gracia-Sant Gervasi y Sabadell.

dt_gracia = cat_data[cat_data$NOM.ESTACIO == estaciones[3],];
conf_gracia = cat_conf[cat_conf$NOM.ESTACIO == estaciones[3],];
remove(estaciones)
dt_sabadell = cat_data[cat_data$NOM.ESTACIO == "Sabadell", ]
conf_sabadell = cat_conf[cat_conf$NOM.ESTACIO == "Sabadell",]
remove(cat_data, cat_conf)

# Histogramas -----------------------------------------------------------------

# Barcelona
hist(dt_gracia$media, breaks = sqrt(length(na.exclude(dt_gracia$media))), main = "Barcelona: Gràcia-St. Gervasi", freq = FALSE, 
     xlim = c(0, 150), ylim = c(0,0.02), xlab = expression(NO[2] (ug/m^3)), ylab = "Densidad")
hist(conf_gracia$media, breaks = sqrt(length(na.exclude(conf_gracia$media))), main = "Barcelona: Gràcia-St. Gervasi", freq = FALSE, 
     xlim = c(0, 60), ylim = c(0,0.06), xlab = expression(NO[2] (ug/m^3)), ylab = "Densidad")
# Sabadell
hist(dt_sabadell$media, breaks = sqrt(length(na.exclude(dt_sabadell$media))), main = "Sabadell", freq = FALSE, 
     xlim = c(0,140), ylim = c(0,0.035), xlab = expression(NO[2] (ug/m^3)), ylab = "Densidad")
hist(conf_sabadell$media, breaks = sqrt(length(na.exclude(conf_sabadell$media))), main = "Sabadell", freq = FALSE, 
     xlim = c(0, 40), ylim = c(0,0.20), xlab = expression(NO[2] (ug/m^3)), ylab = "Densidad");

# Comparación no paramétrica ----------------------------------------------

wilcox.test(dt_gracia$media, conf_gracia$media, "greater")
delta = median(dt_gracia$media, na.rm = T)-median(conf_gracia$media, na.rm = T)
delta; delta/median(dt_gracia$media, na.rm = T)
wilcox.test(dt_sabadell$media, conf_sabadell$media, "greater")
delta = median(dt_sabadell$media, na.rm = TRUE) - median(conf_sabadell$media, na.rm = TRUE)
delta; delta/median(dt_sabadell$media, na.rm = T)
remove(delta)

# Climatología ------------------------------------------------------------

climcat <- data.frame()
k = 1;
for (i in 3:6)
{
  for (j in 1:31)
  {
    climcat[k, 1] = i;
    climcat[k, 2] = j;
    climcat[k, 3] = median(dt_gracia$media[ as.numeric(format(dt_gracia$DATA, '%d')) == j & as.numeric(format(dt_gracia$DATA, '%m')) == i & as.numeric(format(dt_gracia$DATA, '%Y')) < 2020], na.rm=TRUE)
    climcat[k, 4] = median(dt_sabadell$media[ as.numeric(format(dt_sabadell$DATA, '%d')) == j & as.numeric(format(dt_sabadell$DATA, '%m')) == i & as.numeric(format(dt_sabadell$DATA, '%Y')) < 2020], na.rm = TRUE)
    k = k+1
  }
}
remove(i,j,k)
names(climcat) =c("Mes","Dia","gracia", "sabadell");
climcat = climcat[!is.na(climcat$gracia) & !is.na(climcat$sabadell), ];
plot(as.Date(paste(as.character(climcat$Dia), as.character(climcat$Mes)), "%d %m"), climcat$gracia, 
     type = 'l', lty = 2, xlab = "Fecha", ylim = c(0,80), ylab = expression(NO[2]~(ug/m^3)), main = "Barcelona: Gràcia-Sant Gervasi", cex.lab=0.83)
lines(as.Date(paste(as.character(conf_gracia$DATA, '%d'), as.character(conf_gracia$DATA, '%m')), "%d %m"), conf_gracia$media, type='l')
legend(x = "topright",c("Mediana", "2020"), lty = c(2,1), horiz = T, cex = 0.8)
plot(as.Date(paste(as.character(climcat$Dia), as.character(climcat$Mes)), "%d %m"), climcat$sabadell, 
     type = 'l', lty = 2, xlab = "Fecha", ylim = c(0,80), ylab = expression(NO[2]~(ug/m^3)), main = "Sabadell", cex.lab=0.83)
lines(as.Date(paste(as.character(conf_sabadell$DATA, '%d'), as.character(conf_sabadell$DATA, '%m')), "%d %m"), conf_sabadell$media, type='l')
legend(x = "topright",c("Mediana", "2020"), lty = c(2,1), horiz = T, cex = 0.80)

anom_gra = conf_gracia$media - climcat$gracia;
anom_sab = conf_sabadell$media - climcat$sabadell;
remove(climcat, conf_gracia, conf_sabadell);
hist(anom_gra, breaks = sqrt(length(anom_gra)), freq =F, xlim = c(-70,-10), ylim = c(0,0.06), main = "Barcelona: Gràcia-St. Gervasi", xlab = expression(NO[2]~(ug/m^3)), ylab = "Densidad")
curve(dnorm(x, mean(anom_gra, na.rm = TRUE), sd(anom_gra, na.rm = TRUE)), add = TRUE, type = 'l');
hist(anom_sab, breaks = sqrt(length(anom_sab)), freq =F, xlim = c(-50, -10), ylim = c(0,0.06), main = "Sabadell", xlab = expression(NO[2]~(ug/m^3)), ylab = "Densidad")
curve(dnorm(x, mean(anom_sab, na.rm = TRUE), sd(anom_sab, na.rm = TRUE)), add = TRUE, type = 'l');

contraste_normal(anom_gra)
fcoef(anom_gra)
Curtosis(anom_gra) # Repetir esta parte, ambas son normales. 
contraste_normal(anom_sab)
fcoef(anom_sab)
Curtosis(anom_sab)

t.test(anom_gra)
t.test(anom_sab)
remove(anom_gra, anom_sab)
sab = cbind(as.character(dt_sabadell$DATA), dt_sabadell$media); colnames(sab) <- c("fecha", "NO2");
write.csv(sab, "./meteo/sab.csv")
bar = cbind(as.character(dt_gracia$DATA), dt_gracia$media); colnames(bar) <- c("fecha", "NO2");
write.csv(bar, "./meteo/bar.csv")
remove(bar, sab, dt_sabadell, dt_gracia)
