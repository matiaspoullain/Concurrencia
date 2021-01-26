#Shiny EPH individual

library(eph)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(rgdal)


#shinyWidgetsGallery()

Sys.setenv(LANG = "en")
options(encoding = 'UTF-8')

concurrencias <- read.csv("Concurrencias.csv")
concurrencias$dia <- factor(concurrencias$dia, levels = unique(concurrencias$dia))
concurrencias$lugar.abr <- paste(substr(concurrencias$lugar, 1, 10), "...", sep = "")
concurrencias$lugar.abr <- factor(concurrencias$lugar.abr, levels = sort(unique(concurrencias$lugar.abr)))

#mapa
library(leaflet)
library(tmap)
library(sf)
library(tmaptools)
library(OpenStreetMap)

paleta <- c("#258039", "#CF3721", "#F5BE41", "#31A9B8")

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

tmap_mode("view") #"view" para que sea interactivo, "plot" para que sea estatico



#####################
#EMPIEZA EL SHINY
#####################





#Preset del ggplot:

tema_mati <- function(){
    theme(panel.border = element_blank(),  panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", colour = "darkGREY",
                                          size = 1, linetype = "solid"),
          panel.grid.major.y = element_line( size=.1, color="darkGREY", linetype = "dashed" ),
          strip.background =element_rect(fill="#c9acff"))
}



#UI
ui <- fluidPage(
    titlePanel("Concurrencias en locales de Ushuaia"),
    sidebarLayout(
        sidebarPanel(pickerInput("lugar", "Seleccionar local", choices = sort(unique(as.character(concurrencias$lugar))), options = list("live-search" = TRUE, `actions-box` = TRUE), multiple = TRUE),
                     awesomeCheckboxGroup("dia.semana", "Día de la semana", choices = unique(as.character(concurrencias$dia)), selected = unique(concurrencias$dia), inline = TRUE),
                     ),
        mainPanel(tabsetPanel(
            tabPanel('Observaciones iniciales',
                     plotOutput('plot.concurrencia'),
                     leafletOutput("mapa"))
            )
        )
    )
)



#Server
server <- function(input, output, session){
    output$plot.concurrencia <- renderPlot({
        concurrencias %>%
            filter(lugar %in% input$lugar,
                   dia %in% input$dia.semana) %>%
            ggplot(aes(x = hora, y = concurrencia, fill = lugar.abr)) +
            geom_col(position = "dodge") +
            xlab("Hora del día") +
            ylab("Concurrencia") +
            tema_mati() +
            facet_grid(dia~.)
    })
    
    output$mapa <- renderLeaflet({
        tmap_leaflet(tm_shape(df[df$lugar %in% input$lugar,], bbox = box.ush) +
                         tm_dots(shape = 21, col = paleta[3], size = 0.2) +
                         tm_text("lugar.abr", ymod = -0.6, size = 1, just = "center")
        )
    })
    
}


#App
shinyApp(ui = ui, server = server)
