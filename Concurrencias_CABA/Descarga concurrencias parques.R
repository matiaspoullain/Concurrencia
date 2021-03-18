#Descarga concurrencias parques CABA:
parques <- read.csv("Nombres parques CABA.csv", encoding = "UTF-8")

parques <- paste(parques$Nombre.de.la.plaza.o.plazoleta, ", Ciudad de Buenos Aires", sep = "")

library(sgat)
initialization_sgat()


va.por <- parques[1]
salieron.mal <- c()

for(resta in parques[which(parques == va.por):length(parques)]){
  print(paste("Va por ", which(parques == va.por), " de ", length(parques), ": ", which(parques == va.por)/length(parques), sep = "" ))
  concurrencia.parques.caba <- 1
  class(concurrencia.parques.caba) <- "try-error"
  intento <- 1
  while(class(concurrencia.parques.caba) == "try-error" & intento <= 5){
    concurrencia.parques.caba <- try(sgat(resta, carpeta.guardado = "CSVs Concurrencias/Parques CABA", tiempo.espera = 15), silent = TRUE)
    intento <- intento + 1
  }
  if(class(concurrencia.parques.caba) == "try-error"){
    salieron.mal <- c(salieron.mal, resta)
  }
  va.por <- resta
  
}
