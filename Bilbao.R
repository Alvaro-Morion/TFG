  source("./normal.R")
  
  # Carga de datos y unificación --------------------------------------------
  archivos = dir("./Bilbao-Vitoria/")
  Arraiz= read.csv(file = paste("./Bilbao-Vitoria/", archivos[1], sep = ""), sep = ";", head = TRUE, col.names = c("Parámetro", "Fecha", "Arraiz"));
  Arraiz$Fecha = as.Date(Arraiz$Fecha, "%d/%m/%Y");
  Arraiz = Arraiz[!names(Arraiz)=="Parámetro"]; 
  Europa = read.csv(file = paste("./Bilbao-Vitoria/", archivos[2], sep = ""), sep = ";", head = TRUE, col.names = c("Parámetro", "Fecha", "Europa"));
  Europa$Fecha = as.Date(Europa$Fecha, "%d/%m/%Y");
  Europa = Europa[!names(Europa)=="Parámetro"]; 
  Mazarredo = read.csv(file = paste("./Bilbao-Vitoria/", archivos[3], sep = ""), sep = ";", head = TRUE, col.names = c("Parámetro", "Fecha", "Mazarredo"));
  Mazarredo$Fecha = as.Date(Mazarredo$Fecha, "%d/%m/%Y");
  Mazarredo = Mazarredo[!names(Mazarredo)=="Parámetro"];
  MdiazH = read.csv(file = paste("./Bilbao-Vitoria/", archivos[4], sep = ""), sep = ";", head = TRUE, col.names = c("Parámetro", "Fecha", "Maria Diaz Haro"));
  MdiazH$Fecha = as.Date(MdiazH$Fecha, "%d/%m/%Y");
  MdiazH = MdiazH[!names(MdiazH)=="Parámetro"];
  Mar = read.csv(file = paste("./Bilbao-Vitoria/", archivos[5], sep = ""), sep = ";", head = TRUE, col.names = c("Parámetro", "Fecha", "3 Marzo"));
  Mar$Fecha = as.Date(Mar$Fecha, "%d/%m/%Y");
  Mar = Mar[!names(Mar)=="Parámetro"];
  Gasteiz= read.csv(file = paste("./Bilbao-Vitoria/", archivos[6], sep = ""), sep = ";", head = TRUE, col.names = c("Parámetro", "Fecha", "Gasteiz"));
  Gasteiz$Fecha = as.Date(Gasteiz$Fecha, "%d/%m/%Y");
  Gasteiz = Gasteiz[!names(Gasteiz)=="Parámetro"];
  Herran = read.csv(file = paste("./Bilbao-Vitoria/", archivos[7], sep = ""), sep = ";", head = TRUE, col.names = c("Parámetro", "Fecha", "Herran"));
  Herran$Fecha = as.Date(Herran$Fecha, "%d/%m/%Y");
  Herran = Herran[!names(Herran)=="Parámetro"];
  rm(archivos);
  BV <-merge(Arraiz, Europa, by = "Fecha", all=TRUE);
  BV <-merge(BV, Mazarredo, by = "Fecha", all = TRUE);
  BV <-merge(BV, MdiazH, by = "Fecha", all = TRUE);
  BV <-merge(BV, Mar, by = "Fecha", all = TRUE);
  BV <-merge(BV, Gasteiz, by = "Fecha", all = TRUE);
  BV <-merge(BV, Herran, by = "Fecha", all = TRUE);
  rm(Arraiz, Europa, Mazarredo, MdiazH, Mar, Gasteiz, Herran);
  BV = cbind(as.numeric(format(BV$Fecha, '%d')), as.numeric(format(BV$Fecha, '%m')),as.numeric(format(BV$Fecha, '%Y')), BV)
  names(BV)[1] = "Dia";names(BV)[2] = "Mes"; names(BV)[3]="Anno";
  
  # Selección de datos y bondad del ajuste comprobación de asimetría y curtosis ----------------------------------
  
  data = BV[(BV$Mes>3 & BV$Mes<6) | (BV$Mes == 3  & BV$Dia > 14) | (BV$Mes == 6 & BV$Dia < 21),];
  rm(BV)
  confinamiento = data[data$Fecha >= "2020-03-15" & data$Fecha < "2020-06-21",]
  
  
  # Para ver el test de bondad de ajuste para las estaciones ejecutar este bucle. 
 
   for(i in 5:length(data)){
    cat("--------", names(data)[i],"---------\n")
    contraste_normal(na.exclude(data[,i]))
    fcoef(data[,i]);
    Curtosis(data[,i])
    contraste_normal(na.exclude(confinamiento[,i]))
    fcoef(confinamiento[,i]);
    Curtosis(confinamiento[,i])
  }

  # Histogramas concentraciones --------------------------------------------------------------------
  
  hist(data$Mazarredo, breaks = sqrt(length(data$Mazarredo)), freq =FALSE, main="Bilbao: Mazarredo", ylim = c(0,0.05), xlim = c(0, 80),
       xlab = expression(NO[2] (ug/m^3)), ylab ="Densidad");
  hist(confinamiento$Mazarredo, breaks = sqrt(length(confinamiento$Mazarredo)), freq =FALSE,
       main="Bilbao: Mazarredo", xlim = c(0,35), ylim = c(0, 0.08), xlab = expression(NO[2] (ug/m^3)), ylab = "Densidad");
  
  hist(data$Gasteiz, breaks = sqrt(length(data$Gasteiz)), freq =FALSE, main="Vitoria: Avda. Gasteiz", ylim = c(0, .05), xlim = c(0, 60),
       xlab = expression(NO[2] (ug/m^3)), ylab = "Densidad");
  hist(confinamiento$Gasteiz ,breaks = sqrt(length(confinamiento$Gasteiz)), ylim = c(0,0.1), freq =FALSE, xlim = c(0,30), 
       main="Vitoria: Avda. Gasteiz", xlab = expression(NO[2] (ug/m^3)), ylab = "Densidad");
  
  # Contrastes confinamiento periodo normal (no paramétricos)----------------------------------
  
  wilcox.test(data$Mazarredo, confinamiento$Mazarredo, "greater")
  delta = median(data$Mazarredo, na.rm = T)-median(confinamiento$Mazarredo, na.rm = T)
  delta; delta/median(data$Mazarredo, na.rm = T)
  wilcox.test(data$Gasteiz, confinamiento$Gasteiz, "greater")
  delta = median(data$Gasteiz, na.rm = TRUE) - median(confinamiento$Gasteiz, na.rm = TRUE)
  delta; delta/median(data$Gasteiz, na.rm = T)
  
  # estudio anomalía 2020 vs. medianas diarias (2012-2019) -----------------------------------------------------
  
  climat <- data.frame()
  k = 1;
  for (i in 3:6)
  {
    for (j in 1:31)
    {
      climat[k, 1] = i;
      climat[k, 2] = j;
      climat[k, 3] = median(data$Mazarredo[data$Dia == j & data$Mes == i & data$Anno < 2020], na.rm=TRUE)
      climat[k, 4] = median(data$Gasteiz[data$Dia == j & data$Mes == i & data$Anno < 2020], na.rm=TRUE)
      k = k+1
    }
  }
  remove(i,j,k)
  names(climat) =c("Mes","Dia","Mazarredo", "Gasteiz")
  climat = climat[!is.na(climat$Mazarredo) & !is.na(climat$Gasteiz), ]
  plot(as.Date(paste(as.character(climat$Dia), as.character(climat$Mes)), "%d %m"), climat$Mazarredo, 
       type = 'l', lty = 2, ylim = c(0,50), xlab = "Fecha", ylab = expression(NO[2]~(ug/m^3)), main = "Bilbao: Mazarredo", cex.lab=0.83)
  lines(as.Date(paste(as.character(confinamiento$Dia), as.character(confinamiento$Mes)), "%d %m"), confinamiento$Mazarredo, type = 'l')
  legend(x = "topright",c("Mediana", "2020"), lty = c(2,1))
  anom_Maz = confinamiento$Mazarredo - climat$Mazarredo
  anom_Gas = confinamiento$Gasteiz - climat$Gasteiz
  contraste_normal(anom_Maz);
  fcoef(anom_Maz);
  Curtosis(anom_Maz);
  hist(anom_Maz, breaks = sqrt(length(confinamiento$Mazarredo)), main = "Bilbao: Mazarredo", freq = FALSE, ylim = c(0,0.06),
       xlab = expression(NO[2](ug/m^3)), ylab = "Densidad")
  curve(dnorm(x, mean(anom_Maz, na.rm = TRUE), sd(anom_Maz, na.rm = TRUE)), add = TRUE, type = 'l');
  mean(anom_Maz);
  t.test(anom_Maz, conf.level=0.95)
  t.test(anom_Maz/median(climat$Mazarredo))
  plot(as.Date(paste(as.character(climat$Dia), as.character(climat$Mes)), "%d %m"), climat$Gasteiz, 
       type = 'l', lty = 2, ylim = c(0,50), xlab = "Fecha", ylab = expression(NO[2](ug/m^3)), main = "Vitoria: Avenida Gasteiz", cex.lab=0.83)
  lines(as.Date(paste(as.character(confinamiento$Dia), as.character(confinamiento$Mes)), "%d %m"), confinamiento$Gasteiz, type = 'l')
  legend(x = "topright",c("Mediana", "2020"), lty = c(2,1))
  contraste_normal(anom_Gas);
  fcoef(anom_Gas);
  Curtosis(anom_Gas);
  hist(anom_Gas, breaks = sqrt(length(confinamiento$Gas)), main = "Vitoria: Avda. Gasteiz", freq = FALSE, ylim = c(0,0.07),
       xlab = c(expression(NO[2]~(ug/m^3))), ylab = "Densidad")
  curve(dnorm(x, mean(anom_Gas, na.rm = TRUE), sd(anom_Gas, na.rm = TRUE)), add = TRUE, type = 'l');
  mean(anom_Gas)
  t.test(anom_Gas, conf.level=0.95)
  t.test(anom_Gas/median(climat$Gas));
  maz = cbind(as.character(data$Fecha), data$Mazarredo); colnames(maz) = c("fecha", "NO2");
  write.csv(maz, "./meteo/Maz.csv")
  Gas = cbind(as.character(data$Fecha), data$Gasteiz); colnames(Gas) = c("fecha", "NO2");
  write.csv(Gas, "./meteo/Gas.csv")

  
  