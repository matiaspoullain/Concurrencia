#Descriptiva inicial:

bares <- read.csv("Concurrencias_CABA/Analisis/Concurrencias bares CABA.csv", encoding = "UTF-8")
parques <- read.csv("Concurrencias_CABA/Analisis/Concurrencias parques CABA.csv", encoding = "UTF-8")

bares$tipo <- "bar"
parques$tipo <- "parque"

bares$dia <- factor(bares$dia, levels = c("lunes", "martes", "miercoles", "jueves", "viernes", "sabado", "domingo"))
parques$dia <- factor(parques$dia, levels = c("lunes", "martes", "miercoles", "jueves", "viernes", "sabado", "domingo"))

length(unique(bares$lugar))
length(unique(parques$lugar))

juntos <- rbind(bares, parques)

library(tidyverse)
str(bares)

windows()
ggplot(juntos, aes(x = as.factor(hora), y = concurrencia, col = tipo), alpha = 0.7) +
  geom_boxplot() +
  facet_grid(dia~.)

windows()
ggplot(juntos, aes(x = hora, y = concurrencia, col = tipo, group = lugar)) +
  geom_line(alpha = 0.3)


sum(duplicated(juntos[,1:4]))
sum(duplicated(juntos[1:5]))
sum(duplicated(bares[,1:4]))
sum(duplicated(parques[,1:4]))
dim(parques)

#Hay algunos duplicados con distintas concurrenciass, los busco:
conteos.dup <- juntos %>%
  group_by(lugar, dia, hora) %>%
  summarise(n = n(),
            minimo = min(concurrencia),
            maximo = max(concurrencia),
            media = mean(concurrencia),
            rango = maximo - minimo) %>%
  filter(n > 1 & rango != 0)

max(conteos.dup$n)

windows()
ggplot(conteos.dup, aes(x = hora, col = lugar)) +
  geom_point(aes(y = minimo)) +
  geom_point(aes(y = maximo)) +
  facet_grid(dia~.)

plot(density(conteos.dup$rango))

hist(conteos.dup$rango, breaks = 20)

ggplot(conteos.dup, aes(x = dia, y = rango)) +
  geom_jitter()

ggplot(conteos.dup, aes(x = hora, y = rango)) +
  geom_point()

windows()
ggplot(conteos.dup, aes(x = hora, y = rango, col = lugar)) +
  geom_point() +
  facet_grid(dia~.)

#Los agrupo con una media:

juntos2 <- juntos %>%
  group_by(lugar, dia, hora, latitud, longitud, tipo) %>%
  summarise(concurrencia = mean(concurrencia))


windows()
ggplot(juntos2, aes(x = as.factor(hora), y = concurrencia, col = tipo), alpha = 0.7) +
  geom_boxplot() +
  facet_grid(dia~.)

windows()
ggplot(juntos2, aes(x = hora, y = concurrencia, col = tipo, group = lugar)) +
  geom_line(alpha = 0.3)


library(sf)
library(sp)
library(leaflet)

#bares que no tienen coordenadas:
sin.coord <- pull(unique(juntos2[is.na(juntos2$longitud) | is.na(juntos2$latitud), "lugar"]))
library(sgat)
initialization_sgat()
coordenadas.lugares <- get_coords(sin.coord)

juntos2 <- merge(juntos2, coordenadas.lugares, by = "lugar", all.x = TRUE)

juntos2 <- juntos2 %>%
  mutate(latitud.x = as.character(latitud.x),
         longitud.x = as.character(longitud.x),
         latitud = as.numeric(if_else(is.na(latitud.x), latitud.y, latitud.x)),
         longitud = as.numeric(if_else(is.na(longitud.x), longitud.y, longitud.x)),
         latitud.x = NULL,
         latitud.y = NULL,
         longitud.x = NULL,
         longitud.y = NULL,)

lugares.para.mapa <- unique(juntos2[, c("lugar", "tipo", "longitud", "latitud")])

datos.mapa <- SpatialPointsDataFrame(coords = lugares.para.mapa[,c("longitud", "latitud")], data = lugares.para.mapa[,c("lugar", "tipo")], proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

datos.mapa <- st_as_sf(datos.mapa)

#Mapa:
getColor <- function(quakes) {
  sapply(quakes$tipo, function(mag) {
    if(mag == "parque") {
      "green"
    } else {
      "blue"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(datos.mapa)
)


leaflet(data = datos.mapa) %>%
  addTiles() %>%
  addMarkers(popup = ~as.character(lugar), label = ~as.character(lugar), icon=icons)


datos.mapa.bares <- datos.mapa[datos.mapa$tipo == "bar",]
datos.mapa.parques <- datos.mapa[datos.mapa$tipo == "parque",]

trago <- makeAwesomeIcon(icon='beer', library='fa', markerColor = 'blue', iconColor = 'white')
arbol <- makeAwesomeIcon(icon='tree', library='fa', markerColor = 'green', iconColor = 'white')

leaflet() %>%
  addTiles() %>%
  addAwesomeMarkers(data = datos.mapa.bares, popup = ~as.character(lugar), label = ~as.character(lugar), group = "Bares", icon = trago) %>%
  addAwesomeMarkers(data = datos.mapa.parques, popup = ~as.character(lugar), label = ~as.character(lugar), group = "Parques", icon = arbol) %>%
  addLayersControl(
    overlayGroups = c("Bares", "Parques"),
    options = layersControlOptions(collapsed = FALSE)
  )
