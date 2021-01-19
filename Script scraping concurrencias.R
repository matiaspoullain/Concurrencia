library(qdapRegex)
library(RSelenium)
library(dplyr)
library(stringr)
library(data.table)
library(plyr)
library(R.utils)


##### LO QUE VIENE ACA SE CORRE UNA SOLA VEZ ANTES DE EMPEZAR EL SCRAPING####
# inicializa y conecta a un server

source("Inicializacion.R")

#########################################################################


#Defino la funcion de scraping

concurrencia.lugar.por.dia <- function(lugar.a.buscar, dia.semana, tiempo.espera = 5){
  remDr$open() #abre firefox
  remDr$navigate("https://www.google.com.ar") #va a google.com.ar
  webElem <- remDr$findElement(using = "name", value = "q") #selecciona el recuadro de busqueda
  webElem$sendKeysToElement(list(paste(lugar.a.buscar, "horarios", dia.semana), "\uE007")) #escribe el lugar.a.buscar y hace la busqueda
  
  apertura <- NA_character_ #para empezar el loop que sigue
  try(withTimeout(while(is.na(apertura)){ 
    source <- remDr$getPageSource()[[1]] #codigo de fuente de la pagina de google
    apertura <- ex_between(source, '"TLou0b JjSWRd">', '<')[[1]] #entre estos characteres, google dice si el lugar esta cerrado o abierto este dia, si esta cerrado se cierra firefox y se vuelve a empezar
  }, timeout = tiempo.espera, onTimeout = "silent"), silent = TRUE)
  if(is.na(apertura)){
    remDr$close()
    return("Sin datos de concurrencia")
  } else if(!("Cerrado" %in% apertura)){
    concurrencia <- NA_character_ #para empezar el loop que sigue
    try(withTimeout(while(is.na(concurrencia)){ 
      source <- remDr$getPageSource()[[1]] #codigo de fuente de la pagina de google
      concurrencia <- ex_between(source, 'class="cwiwob', 'px')[[1]] #extrae la cantidad de concurrencia en unidades de pixel que aparece en el grafico de concurrencia
    }, timeout = tiempo.espera, onTimeout = "silent"), silent = TRUE) #este loop se repite hasta que la pagina cargue y se pueda extraer informacion, supongo que la cantidad de veces que se repite depende de la velocidad de internet
    if(!is.na(concurrencia)){
      coordenadas <- ex_between(source, 'data-url="/maps/place/', ',15z')
      remDr$close() #cierra firefox, ya no se necesita
      
      coordenadas <- sub(".*@", "", coordenadas)
      latitud <- sub(",.*", "", coordenadas)
      longitud <- sub(".*,", "", coordenadas)
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
      df$dia <- case_when(hora > 23 & dia.semana == "lunes" ~ "martes",
                          hora > 23 & dia.semana == "martes" ~ "miercoles",
                          hora > 23 & dia.semana == "miercoles" ~ "jueves",
                          hora > 23 & dia.semana == "jueves" ~ "viernes",
                          hora > 23 & dia.semana == "viernes" ~ "sabado",
                          hora > 23 & dia.semana == "sabado" ~ "domingo",
                          hora > 23 & dia.semana == "domingo" ~ "lunes",
                          TRUE ~ dia.semana)
      df$hora <- if_else(df$hora > 23, df$hora - 24, df$hora)
      df$fecha.de.busqueda <- Sys.Date() #este lo agrego por las dudas, no se cuanto cambia segun el dia que se busca
      df$latitud <- as.numeric(latitud)
      df$longitud <- as.numeric(longitud)
      df <- df[,c(3, 4, 1, 2, 6, 7, 5)]
      df
    }else{
      remDr$close()
      return("Sin datos de concurrencia")
    }
    
  }else{
    remDr$close()
  }
}

concurrencia.lugar <- function(lugar.a.buscar, tiempo.espera = 5, carpeta.guardado = "CSVs Concurrencias"){
  dias.semana <- c("martes", "miercoles", "jueves", "viernes", "sabado", "domingo")
  df <- concurrencia.lugar.por.dia(lugar.a.buscar, "lunes", tiempo.espera)
  if(!is.data.frame(df)){
    if(length(df) == 0){
      i <- TRUE
    }else if(df == "Sin datos de concurrencia") {
      i <- FALSE
    }
  }else{
    i <- TRUE
  }
  if(i){
    for(dia.semana in dias.semana){
      datos <- concurrencia.lugar.por.dia(lugar.a.buscar, dia.semana, tiempo.espera)
      df <- rbind(df, datos)
      dir.create(carpeta.guardado, showWarnings = FALSE) #crea la carpeta concurrencia si no existe aun
      write.csv(df, file = paste(carpeta.guardado, "/Concurrencia ", lugar.a.buscar, " ", Sys.Date(),".csv", sep = ""))  
    }
  }
  df
}
 