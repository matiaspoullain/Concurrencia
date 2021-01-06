library(qdapRegex)
library(RSelenium)
library(dplyr)
library(stringr)
library(data.table)


##### LO QUE VIENE ACA SE CORRE UNA SOLA VEZ ANTES DE EMPEZAR EL SCRAPING####
# inicializa y connecta a un server

driver<- rsDriver()
remDr <- driver[["client"]]
remDr <- remoteDriver(remoteServerAddr = "localhost", 
                      port = 4567, 
                      browserName = "firefox")

#########################################################################


#Defino la funcion de scraping

concurrencia.lugar <- function(lugar.a.buscar){
  remDr$open() #abre firefox
  remDr$navigate("https://www.google.com.ar") #va a google.com.ar
  webElem <- remDr$findElement(using = "name", value = "q") #selecciona el recuadro de busqueda
  webElem$sendKeysToElement(list(lugar.a.buscar, "\uE007")) #escribe el lugar.a.buscar y hace la busqueda

  concurrencia <- NA_character_ #para empezar el loop que sigue
  
  while(is.na(concurrencia)){ 
    source <- remDr$getPageSource()[[1]] #codigo de fuente de la pagina de google
    concurrencia <- ex_between(source, 'class="cwiwob', 'px')[[1]] #extrae la cantidad de concurrencia en unidades de pixel que aparece en el grafico de concurrencia
  } #este loop se repite hasta que la pagina cargue y se pueda extraer informacion, supongo que la cantidad de veces que se repite depende de la velocidad de internet
  remDr$close() #cierra firefox, ya no se necesita
  
  concurrencia <- as.numeric(sub(".*:", "", concurrencia)) #me quedo solo con la parte interesante del string
  hora <- ex_between(source, 'data-hour=', ' jsaction')[[1]] #extrae la hora a la que corresponden las concurrencias
  hora <- as.numeric(gsub("[^0-9.-]", "", hora)) #me quedo solo con la parte interesante del string
  
  #En este punto se genera un problema: si la busqueda se realiza en una hora en la que el local esta abierto, google agrega además la concurrencia actual observada, por lo que hay mas numeros de concurrencia que de horarios, este if sirve para ignorar ese numero de mas
  
  if(length(concurrencia) != length(hora)){ 
    loc.concurrencia <- data.table(concurrencia, data.frame(str_locate_all(source, 'class="cwiwob'))) #ubicaciones de los characteres encontrados
    loc.hora <- data.table(hora, data.frame(str_locate_all(source, 'data-hour='))) 
    setkey(loc.concurrencia, start) #para hacer el join
    setkey(loc.hora, start)
    df <- as.data.frame(loc.concurrencia[loc.hora, roll = "nearest" ]) #join menos estricto, joinea segun cercania de caracteres
    df <- df[, c(4,1)]
  } else{
    df <- data.frame(hora, concurrencia) #si no hay esa concurrencia de mas, directamente junto la hora y la concurrencia
  }
  #aca hago mas linda la tabla
  df$lugar <- lugar.a.buscar 
  df$dia <- weekdays(Sys.Date()) #agrego el dia de la semana por que no pude hacer que me muestre lo de los otros dias, solo lo del dia de hoy
  df$fecha.de.busqueda <- Sys.Date() #este lo agrego por las dudas, no se cuanto cambia segun el dia que se busca
  df <- df[,c(3, 4, 1, 2, 5)]
  
  dir.create("Concurrencias", showWarnings = FALSE) #crea la carpeta concurrencia si no existe aun
  write.csv(df, file = paste("Concurrencias/Concurrencia ", lugar.a.buscar, " ", weekdays(Sys.Date()), " ", Sys.Date(),".csv", sep = "") )
  df
}
  
  




#ejemplo:
sarkis <- concurrencia.lugar("sarkis")

museo <- concurrencia.lugar("museo nacional de bellas artes")


#para hacerlo con varios lugares al mismo tiempo:
lugares <- c("anses villa urquiza", "sportclub cabildo", "freddo cabildo y jose hernandez")

varios <- lapply(lugares, concurrencia.lugar)
