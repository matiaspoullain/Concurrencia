#Unificacion bases de parques:

archivos.parques <- list.files("CSVs Concurrencias/Parques CABA", full.names = TRUE)
archivos.parques.nuevo.sgat <- list.files("CSVs Concurrencias/Parques CABA nuevo sgat", full.names = TRUE)

concurrencias <- data.frame()

for(i in c(archivos.parques, archivos.parques.nuevo.sgat)){
  print(paste("va por", i))
  if(file.info(i)$size > 1000){
    concurrencia <- read.csv(i)
    concurrencias <- rbind(concurrencias, concurrencia)
  }
}


write.csv(concurrencias, "Concurrencias_CABA/Analisis/Concurrencias parques CABA.csv", row.names = FALSE, fileEncoding = "UTF-8")

#Para bares:
archivos.bares.nuevo.sgat <- list.files("CSVs Concurrencias/bares CABA nuevo sgat", full.names = TRUE)

concurrencias <- data.frame()


for(i in archivos.bares.nuevo.sgat){
  print(paste("va por", i))
  if(file.info(i)$size > 1000){
    concurrencia <- read.csv(i)
    concurrencias <- rbind(concurrencias, concurrencia)
  }
}

write.csv(concurrencias, "Concurrencias_CABA/Analisis/Concurrencias bares CABA.csv", row.names = FALSE, fileEncoding = "UTF-8")
