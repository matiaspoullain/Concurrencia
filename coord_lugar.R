coord_lugar <- function(lugar.a.buscar, tiempo.espera = 10) {
  remDr$open(silent = TRUE) # abre firefox
  remDr$navigate("https://www.google.com.ar") # va a google.com.ar
  webElem <- remDr$findElement(using = "name", value = "q") # selecciona el recuadro de busqueda
  webElem$sendKeysToElement(list(paste(lugar.a.buscar, "horarios", "lunes"), "\uE007")) # escribe el lugar.a.buscar y hace la busqueda
  
  apertura <- NA_character_ # para empezar el loop que sigue
  coordenadas <- NA_character_
  x <- 0
  while (x <= tiempo.espera & replace(apertura[1], is.na(apertura[1]), "0") != "Cerrado" & is.na(coordenadas[1])) {
    source <- remDr$getPageSource()[[1]] # codigo de fuente de la pagina de google
    apertura <- qdapRegex::ex_between(source, '"TLou0b JjSWRd">', "<")[[1]] # entre estos characteres, google dice si el lugar esta cerrado o abierto este dia, si esta cerrado se cierra firefox y se vuelve a empezar
    coordenadas <- qdapRegex::ex_between(source, 'data-url="/maps/place/', ",15z")
    x <- x + 1
  }
  remDr$close()
  if (!is.na(coordenadas[1])) {

    coordenadas <- sub(".*@", "", coordenadas)
    latitud <- as.numeric(sub(",.*", "", coordenadas))
    longitud <- as.numeric(sub(".*,", "", coordenadas))
    df <- data.frame(lugar.a.buscar, latitud, longitud)
    
    df
    
  } else {
    return("Sin datos")
  }
}



#Ejemplo:
coord_lugar("arredondo")





