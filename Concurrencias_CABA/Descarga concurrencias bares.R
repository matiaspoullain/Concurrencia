#Descarga concurrencias bares CABA:
library(git2r)
bares <- read.csv("Concurrencias_CABA/Nombres bares CABA.csv")

bares <- paste(bares$caba, ", Ciudad de Buenos Aires", sep = "")

salieron.mal <- read.csv("Concurrencias_CABA/bares salieron mal.csv")
salieron.mal <- salieron.mal$salieron.mal
ya.hizo <- read.csv("Concurrencias_CABA/Bares que ya inicio.csv")
ya.hizo <- ya.hizo$x
bares <- bares[!bares %in% ya.hizo]
library(sgat)
initialization_sgat()

#bares <- salieron.mal
ya.hizo <- c()
salieron.mal <- c()
repeticiones <- 0

for(resta in bares){
  print(paste("Va por ", which(bares == resta), " de ", length(bares), ": ", 100* which(bares == resta)/length(bares), " %", sep = "" ))
  ya.hizo <- c(ya.hizo, resta)
  write.csv(ya.hizo, "Concurrencias_CABA/Bares que ya inicio.csv")
  concurrencia.bares.caba <- 1
  class(concurrencia.bares.caba) <- "try-error"
  intento <- 1
  while(class(concurrencia.bares.caba) == "try-error" & intento <= 5){
    concurrencia.bares.caba <- try(sgat(resta, carpeta.guardado = "CSVs Concurrencias/bares CABA nuevo sgat", tiempo.espera = 15), silent = TRUE)
    intento <- intento + 1
    repeticiones <- repeticiones + 1
  }
  if(class(concurrencia.bares.caba) == "try-error"){
    salieron.mal <- c(salieron.mal, resta)
    write.csv(salieron.mal, "Concurrencias_CABA/bares salieron mal.csv")
  }
  if(repeticiones >= 100){
    Sys.sleep(300)
    repeticiones <- 0
  }
}

git2r::config(user.name = "matiaspoullain", user.email = "matias.poullain")
git2r::status()
git2r::add(repo = ".", path = getwd(), force = FALSE)
git2r::commit(repo = ".", message = "datos bares", all = TRUE)
shell("git push")



#Hay lugares que no descargaron bien las coordenadas
