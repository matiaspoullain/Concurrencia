#Descarga concurrencias parques CABA:
library(git2r)
bares <- read.csv("Concurrencias_CABA/Nombres bares CABA.csv")

bares <- paste(bares$caba, ", Ciudad de Buenos Aires", sep = "")

library(sgat)
initialization_sgat()


va.por <- bares[1]
salieron.mal <- c()
repeticiones <- 0

for(resta in bares[which(bares == va.por):length(bares)]){
  print(paste("Va por ", which(bares == va.por), " de ", length(bares), ": ", 100* which(bares == va.por)/length(bares), " %", sep = "" ))
  concurrencia.bares.caba <- 1
  class(concurrencia.bares.caba) <- "try-error"
  intento <- 1
  while(class(concurrencia.bares.caba) == "try-error" & intento <= 10){
    concurrencia.bares.caba <- try(sgat(resta, carpeta.guardado = "CSVs Concurrencias/bares CABA", tiempo.espera = 15), silent = TRUE)
    intento <- intento + 1
    repeticiones <- repeticiones + 1
  }
  if(class(concurrencia.bares.caba) == "try-error"){
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
write.csv(df.salieron.mal, "Concurrencias_CABA/bares salieron mal.csv")


git2r::config(user.name = "matiaspoullain", user.email = "matias.poullain")
git2r::status()
git2r::add(repo = ".", path = getwd(), force = FALSE)
git2r::commit(repo = ".", message = "subida datos bares", all = TRUE)
shell("git push")

