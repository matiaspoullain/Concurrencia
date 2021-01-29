source("Script scraping concurrencias.R")
source("Restaurantes Ushuaia.R")
source("Restaurantes tripadvisor.R")

#Ejemplos:

sarkis <- concurrencia.lugar("sarkis")

museo <- concurrencia.lugar("museo nacional de bellas artes")

cervelar <- concurrencia.lugar("cervelar belgrano")

#para hacerlo con varios lugares al mismo tiempo:
lugares <- c("anses villa urquiza", "sportclub cabildo", "parroquia san patricio", "freddo cabildo 1700")

varios <- lapply(lugares, concurrencia.lugar)



super1 <- concurrencia.lugar("Carrefour ushuaia")
super2 <- concurrencia.lugar("La anonima San martin ushuaia")
super3 <- concurrencia.lugar("La anonima perito moreno ushuaia")
super4 <- concurrencia.lugar("La anonima gdor paz ushuaia")
santos <- concurrencia.lugar("Santos cerveceria ushuaia")
kuar <- concurrencia.lugar("kuar 1900 ushuaia")
atlantico <- concurrencia.lugar("dutty free shop atlantico sur ushuaia")
shopping <- concurrencia.lugar("Paseo del fuego shopping ushuaia")
heladeria1 <- concurrencia.lugar("Gadget san martin ushuaia")
heladeria2 <- concurrencia.lugar("Freddo san martin ushuaia")
heladeria3 <- concurrencia.lugar("Grido helado magallanes ushuaia")
mercado <- concurrencia.lugar("El mercado ushuaia")
ramos <- concurrencia.lugar("Ramos generales ushuaia")
hardrock <- concurrencia.lugar("Hard rock ushuaia")
krund <- concurrencia.lugar("krund ushuaia")
grut <- concurrencia.lugar("cerveceria grut ushuaia")
playa <- concurrencia.lugar("playa larga reserva natural ushuaia")
monumento <- concurrencia.lugar("monumento a los heroes de malvinas ushuaia")
monumento2 <- concurrencia.lugar("monumento galicia en ushuaia")
mirador <- concurrencia.lugar("mirador Mingo moreno ushuaia")
cartel <- concurrencia.lugar("cartel ushuaia")
cartel2 <- concurrencia.lugar("cartel del fin del mundo, ushuaia")
marcopolo <- concurrencia.lugar("marcopolo freelife ushuaia")
andino <- concurrencia.lugar("andino gourmet ushuaia")
taberna <- concurrencia.lugar("taberna del viejo lobo ushuaia")
viagro <- concurrencia.lugar("viagro ushuaia")



#con ushuaia_restaurantes:
restaurantes <- ushuaia_restaurants()


info.restaurantes <- lapply(restaurantes, concurrencia.lugar, carpeta.guardado = "CSVs Concurrencias/Restaurantes")

va.por <- restaurantes[67]
for(resta in restaurantes[which(restaurantes == va.por):length(restaurantes)]){
  concurrencia.lugar(resta, carpeta.guardado = "CSVs Concurrencias/Restaurantes", tiempo.espera = 7)
  va.por <- resta
}

which(restaurantes == va.por)



#con restaurantes de tripadvisor:

rest.pinamar <- restaurantes_general("Pinamar, argentina")

restaurantes_general("san carlos de bariloche, Argentina", 65)

restaurantes_general("gualeguaychu, entre rios, argentina", 10)



rest.pinamar <- paste(rest.pinamar, ", Pinamar, Argentina", sep = "")

va.por <- rest.pinamar[5]
salieron.mal <- c()




for(resta in rest.pinamar[which(rest.pinamar == va.por):length(rest.pinamar)]){
  print(paste("Va por ", which(rest.pinamar == va.por), " de ", length(rest.pinamar), ": ", which(rest.pinamar == va.por)/length(rest.pinamar), sep = "" ))
  concurrencia.pinamar <- 1
  class(concurrencia.pinamar) <- "try-error"
  intento <- 1
  while(class(concurrencia.pinamar) == "try-error" & intento <= 5){
    concurrencia.pinamar <- try(sgat(resta, carpeta.guardado = "CSVs Concurrencias/restaurantes pinamar", tiempo.espera = 15), silent = TRUE)
    intento <- intento + 1
  }
  if(class(concurrencia.pinamar) == "try-error"){
    salieron.mal <- c(salieron.mal, resta)
  }
  va.por <- resta

}


for(resta in salieron.mal){
  print(paste("Va por ", which(salieron.mal == resta), " de ", length(salieron.mal), ": ", round(100*which(salieron.mal == resta)/length(salieron.mal),2), sep = "" ))
  concurrencia.pinamar <- 1
  class(concurrencia.pinamar) <- "try-error"
  intento <- 1
  while(class(concurrencia.pinamar) == "try-error" & intento <= 5){
    concurrencia.pinamar <- try(sgat(resta, carpeta.guardado = "CSVs Concurrencias/restaurantes pinamar", tiempo.espera = 15), silent = TRUE)
    intento <- intento + 1
  }
  if(class(concurrencia.pinamar) == "try-error"){
    salieron.mal <- c(salieron.mal, resta)
  }
  va.por <- resta
  
}
