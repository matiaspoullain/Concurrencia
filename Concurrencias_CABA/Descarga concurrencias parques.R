#Descarga concurrencias parques CABA:
library(git2r)
parques <- read.csv("Concurrencias_CABA/Nombres parques CABA.csv", encoding = "UTF-8")

parques <- paste(parques$Nombre.de.la.plaza.o.plazoleta, ", Ciudad de Buenos Aires", sep = "")

salieron.mal <- read.csv("Concurrencias_CABA/parques salieron mal.csv")
salieron.mal <- salieron.mal$salieron.mal
library(sgat)
initialization_sgat()

  parques <- salieron.mal
  va.por <- parques[1]
  salieron.mal <- c()
  repeticiones <- 0

  for(resta in parques[which(parques == va.por):length(parques)]){
    print(paste("Va por ", which(parques == va.por), " de ", length(parques), ": ", 100* which(parques == va.por)/length(parques), " %", sep = "" ))
    concurrencia.parques.caba <- 1
    class(concurrencia.parques.caba) <- "try-error"
    intento <- 1
    while(class(concurrencia.parques.caba) == "try-error" & intento <= 20){
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

  num <- 1:length(salieron.mal)
  df.salieron.mal <- data.frame(num, salieron.mal)
  write.csv(df.salieron.mal, "Concurrencias_CABA/parques salieron mal.csv")

  git2r::config(user.name = "matiaspoullain", user.email = "matias.poullain")
  git2r::status()
  git2r::add(repo = ".", path = getwd(), force = FALSE)
  git2r::commit(repo = ".", message = "datos parques", all = TRUE)
  shell("git push")

