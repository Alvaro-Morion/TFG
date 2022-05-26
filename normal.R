contraste_normal <- function(x, alpha = 0.05){
  mu=mean(x)
  sigma=sd(x) # Cálculo de parámteros
  nbreak=sqrt(length(x))
  h = hist(x, breaks=nbreak, plot = FALSE);
  i = 1;
  while(i<=length(h$counts)) #Agrupamiento de intervalos >5 en cada uno
  {
    if (h$counts[i] < 5 & i < length(h$counts)){
      h$breaks = h$breaks[-(i+1)]
      h$counts[i] = h$counts[i]+h$counts[i+1]
      h$counts = h$counts[-(i+1)]
    }
    if(h$counts[i] < 5 & i == length(h$counts)){
      h$breaks = h$breaks[-i]
      h$counts[i-1] = h$counts[i-1] + h$counts[i]
      h$counts = h$counts[-i]
      i = i-1
    }
    else
      i=i+1;
  }
  h$breaks[length(h$counts)+1]= max(x)+0.5
  h = hist(x, h$breaks, plot=FALSE);
  obs = h$counts
  esp = vector("numeric", length(h$breaks)-1)
  for (i in 2:length(h$breaks)-1){
    esp[i-1] = pnorm(h$breaks[i], mu, sigma)-pnorm(h$breaks[i-1], mu, sigma);
  }
  esp[i] = 1-sum(esp);
  #hist(x, breaks=nbreak, freq =FALSE); para graficar
  #curve(dnorm(x, mu, sigma), add=TRUE, col='Red');
  E = esp*length(x);
  chisq = sum((obs-E)^2/E);
  crit = qchisq(alpha, df = length(x)-3, lower.tail = FALSE);
  cat("Contraste chi^2 de bondad de ajuste a una normal:\n", "Media", mu, "\t Desviación típica", sigma, "\n Estadístico", chisq, "Valor crítico para la significación dada", crit, "\n Se acepta la hipótesis nula de distribución normal", chisq<crit, "\n")
} # Contraste chi^2 de normalidad

fcoef=function(x) # Coeficiente de simetría
{
  x=x[!is.na(x)]
  g=sum((x-mean(x))^3)/(length(x)*sd(x)^3)
  err = qnorm(0.025, mean = 0, sd = sqrt(6/length(x)), lower.tail = FALSE);
  if (g <= err && g >= -err){
    cat("La distribución es simétrica: g=", g , "Intervalo de aceptación de simetría:",
        c(-err,+err), "\n");
  }
  else{
    if (g > err){
      cat("La distribución tiene una cola a la derecha. g=", g,
          "Intervalo de aceptación de simetría:", c(-err,+err), "\n");
    }
    else {
      cat("La distribución tiene una cola a la derecha. g=", g,
          "Intervalo de aceptación de simetrñia:", c(-err,+err), "\n");
    }
  }
}

Curtosis<-function(x) #Coeficiente de curtosis
{
  x=x[!is.na(x)]
  C=sum((x-mean(x))^4)/(length(x)*sd(x)^4)
  err1 = qnorm(0.025, mean = 3, sd = sqrt(24/length(x)), lower.tail = TRUE);
  err2 = qnorm(0.025, mean = 3, sd = sqrt(24/length(x)), lower.tail = FALSE);
  if (C <= err2 && C >= err1){
    cat("La distribución es mesocúrtica: C=", C , "Intervalo de aceptación de mesocúrtica:",
        c(err1,err2), "\n");
  }
  else{
    if (C > err2){
      cat("La distribución es leptocúrtica C=", C,
          "Intervalos de aceptación de mesocúrtica:", c(err1,err1), "\n");
    }
    else {
      cat("La distribución es platicúrtica. C=", C,
          "Intervalos de aceptación de mesocúrtica:", c(err1,err2), "\n");
    }
  }
}