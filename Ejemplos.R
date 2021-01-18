source("Script scraping concurrencias.R")

#Ejemplos:

sarkis <- concurrencia.lugar("sarkis")

museo <- concurrencia.lugar("museo nacional de bellas artes")


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

