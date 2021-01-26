# Descriptivo concurrencias Ushuaia:
library(dplyr)
library(ggplot2)

archivos <- list.files("CSVs Concurrencias/Restaurantes")

concurrencias <- data.frame()
for(i in archivos){
  concurrencia <- read.csv(paste("CSVs Concurrencias/Restaurantes/", i, sep = ""))
  concurrencias <- rbind(concurrencias, concurrencia)
}

concurrencias
dim(concurrencias)
length(archivos)
length(unique(interaction(concurrencias$latitud, concurrencias$longitud)))

str(concurrencias)

concurrencias$dia <- factor(concurrencias$dia, levels = unique(concurrencias$dia))


boxplt <- ggplot(concurrencias, aes(x = hora, y = concurrencia, group = hora)) +
  geom_boxplot() +
  facet_wrap(~dia)

ggsave("Graficos iniciales/Boxplot Uhsuaia.jpg", plot = boxplt, device="jpg",dpi=300 , width = 15, height = 7.5)

boxplt2 <- ggplot(concurrencias, aes(x = dia, y = concurrencia, group = dia)) +
  geom_boxplot() +
  facet_wrap(~hora)

ggsave("Graficos iniciales/Boxplot Uhsuaia 2.jpg", plot = boxplt2, device="jpg",dpi=300 , width = 20, height = 10)




#Mapa:
library(tmap)
library(sf)
library(tmaptools)
library(OpenStreetMap)

paleta <- c("#258039", "#CF3721", "#F5BE41", "#31A9B8")

library(RColorBrewer)
paleta6 <- colorRampPalette(paleta)(6)

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

df <- st_as_sf(x = concurrencias,                         
               coords = c("longitud", "latitud"),
               crs = projcrs)

box.ush <- st_bbox(df)

expansion <- 1/240

box.ush[[1]] <- box.ush[[1]] - expansion
box.ush[[2]] <- box.ush[[2]] - expansion
box.ush[[3]] <- box.ush[[3]] + expansion
box.ush[[4]] <- box.ush[[4]] + expansion


ushuaia <- read_osm(box.ush, ext = 1.1, raster = TRUE, type = "stamen-toner")


tmap_mode("plot") #"view" para que sea interactivo, "plot" para que sea estatico
mapa.ubicaciones <- tm_shape(ushuaia, bbox = box.ush) +
  tm_rgb(alpha = 0.5) +
  tm_shape(df) +
  tm_dots(shape = 21, col = paleta[3], size = 0.2) +
  tm_scale_bar(text.size = 0.3, color.dark = "grey", text.color = "grey", position = c("center", "top")) + #escala
  tm_compass(north = 180, text.color = "grey", color.dark = "grey", position = c( "left", "top"), text.size = 0.3, cardinal.directions = "S")+ #flecha de norte y su posicion osbre el mapa
  tm_grid(projection = projcrs, lines = FALSE, labels.inside.frame= TRUE, labels.size = 0.3)

mapa.ubicaciones

tmap_save(mapa.ubicaciones, filename="Graficos iniciales/Mapa.png", width = 750, height = 750)#https://stackoverflow.com/questions/42086603/producing-an-inset-map-with-the-tmap-package-in-r




#animacion mapa:
#primero creo los datos para cada horario de la semana y cada lugar:
exp.grd <- expand.grid(0:23, unique(concurrencias$dia), unique(concurrencias$lugar))

names(exp.grd) <- c("hora", "dia", "lugar")

df.anim <- merge(exp.grd, concurrencias[, c("hora", "dia", "lugar", "concurrencia")], by = c("hora", "dia", "lugar"), all.x = TRUE)

df.anim <- merge(df.anim, unique(concurrencias[, c("lugar", "longitud", "latitud")]), by = c("lugar"), all.x = TRUE)

df.anim[is.na(df.anim$concurrencia), "concurrencia"] <- 0

df.anim$concurrencia.esc <- df.anim$concurrencia / max(df.anim$concurrencia)


df.anim <- st_as_sf(x = df.anim,                         
               coords = c("longitud", "latitud"),
               crs = projcrs)


dias.semana <- unique(concurrencias$dia)
y <- 1
for(dia in dias.semana){
  df.loop1 <- df.anim[df.anim$dia == dia,]
  x <- 0
  while(x < 24){
    df.loop2 <- df.loop1[df.loop1$hora == x,]
    mapa.animacion <- tm_shape(ushuaia, bbox = box.ush) +
      tm_rgb(alpha = 0.5) +
      tm_shape(df.loop2) +
      tm_dots(shape = 21, col = paleta[3], size = "concurrencia.esc", legend.size.show = FALSE, alpha = 0.7) +
      tm_scale_bar(text.size = 0.3, color.dark = "grey", text.color = "grey", position = c("center", "top")) + #escala
      tm_compass(north = 180, text.color = "grey", color.dark = "grey", position = c( "left", "top"), text.size = 0.3, cardinal.directions = "S")+ #flecha de norte y su posicion osbre el mapa
      tm_grid(projection = projcrs, lines = FALSE, labels.inside.frame= TRUE, labels.size = 0.3) +
      tm_layout(main.title= paste(dia, x, "hs", sep = " "), 
                title.position = c('left', 'top'),
                main.title.size = 0.4)
    tmap_save(mapa.animacion, filename = paste("Graficos iniciales/Mapas animacion/Mapa_", formatC(y, width=3, flag="0"), ".png", sep = ""), width = 750, height = 750)#https://stackoverflow.com/questions/42086603/producing-an-inset-map-with-the-tmap-package-in-r
    x <- x + 1
    y <- y + 1
    print(paste("Va por ", dia, " ", x, "hs: ", round(100 * (y-1) / (7*24), 2), "%", sep = ""))
  }
}

lista.imagenes <- list.files("Graficos iniciales/Mapas animacion")
