#Descarga concurrencias parques CABA:
library(git2r)
library(drat)
parques <- read.csv("Nombres parques CABA.csv", encoding = "UTF-8")

parques <- paste(parques$Nombre.de.la.plaza.o.plazoleta, ", Ciudad de Buenos Aires", sep = "")

library(sgat)
initialization_sgat()


va.por <- parques[1]
salieron.mal <- c()
repeticiones <- 0

for(resta in parques[which(parques == va.por):length(parques)]){
  print(paste("Va por ", which(parques == va.por), " de ", length(parques), ": ", 100* which(parques == va.por)/length(parques), " %", sep = "" ))
  concurrencia.parques.caba <- 1
  class(concurrencia.parques.caba) <- "try-error"
  intento <- 1
  while(class(concurrencia.parques.caba) == "try-error" & intento <= 10){
    concurrencia.parques.caba <- try(sgat(resta, carpeta.guardado = "CSVs Concurrencias/Parques CABA", tiempo.espera = 15), silent = TRUE)
    intento <- intento + 1
    repeticiones <- repeticiones + 1
  }
  if(class(concurrencia.parques.caba) == "try-error"){
    salieron.mal <- c(salieron.mal, resta)
  }
  if(repeticiones >= 100){
    Sys.sleep(300)
    repeticiones <- 0
  }
  va.por <- resta

}
git2r::config(user.name = "matiaspoullain", user.email = "matias.poullain")
git2r::status()
git2r::add(repo = ".", path = NULL, force = FALSE)
git2r::commit(repo = ".", message = "commit message")
git2r::push(credentials = )

