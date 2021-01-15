source("Script scraping concurrencias.R")

#Ejemplos:

sarkis <- concurrencia.lugar("sarkis")

museo <- concurrencia.lugar("museo nacional de bellas artes")


#para hacerlo con varios lugares al mismo tiempo:
lugares <- c("anses villa urquiza", "sportclub cabildo", "parroquia san patricio", "freddo cabildo 1700")

varios <- lapply(lugares, concurrencia.lugar)
