load_all()

sgat <- function(lugar.a.buscar, tiempo.espera = 5, carpeta.guardado = "CSVs Concurrencias"){
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
  for(dia.semana in dias.semana){
    if(i){
      datos <- concurrencia.lugar.por.dia(lugar.a.buscar, dia.semana, tiempo.espera)
      df <- rbind(df, datos)
    }
  }
  if(i){
    dir.create(carpeta.guardado, showWarnings = FALSE) #crea la carpeta concurrencia si no existe aun
    write.csv(df, file = paste(carpeta.guardado, "/Concurrencia ", lugar.a.buscar, " ", Sys.Date(),".csv", sep = ""))
  }
  df
}