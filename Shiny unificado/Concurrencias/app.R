
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(tmap)
library(sf)
library(tmaptools)
library(OpenStreetMap)
library(stringr)
library(ggplot2)
library(dplyr)
library(plotly)
library(lemon)


Sys.setenv(LANG = "en")
options(encoding = 'UTF-8')

tema_mati <- function(){
    theme(panel.border = element_blank(),  panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white", colour = "darkGREY",
                                          size = 1, linetype = "solid"),
          panel.grid.major.y = element_line(size = 0.1, color="darkGREY", linetype = "dashed" ),
          strip.background =element_rect(fill="#c9acff"))
}

paleta <- c("#258039", "#CF3721", "#F5BE41", "#31A9B8")

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

expansion <- 1/240





header <- dashboardHeader(
    dropdownMenu(
        type = "messages",
        messageItem(
            from = "Admin",
            message = "Proximamente se agregarán otros",
        )
    )
)


sidebar <- dashboardSidebar(
    sidebarMenu(
        pickerInput("ciudad", "Seleccionar ciudad", choices = c("Pinamar", "Ushuaia"), selected = "Pinamar", options = list("live-search" = TRUE, `actions-box` = TRUE), multiple = FALSE),
        uiOutput('pickerLugar'),
        uiOutput('checkDia'),
        sliderTextInput(inputId = "hora", label = "Seleccionar intervalo de horas del día", choices = 0:23, selected = c(0, 23))
        )
)



body <- dashboardBody(
    fluidRow(tabBox(width = 12,
        title = "Concurrencia",
        tabPanel("Observaciones iniciales", plotly::plotlyOutput('plot.concurrencia', height = "800px"),
                 leafletOutput("mapa")),
        tabPanel('Métricas resumen',
                 plotOutput('boxplot.dia'),
                 uiOutput('anim')),
        tabPanel('Más concurridos',
                 DT::DTOutput("mas.concurridos"),
                 leafletOutput("mapa.ranking"))
        )
    )
    )


ui <- dashboardPage(header, sidebar, body)


server <- function(input, output, session){
    
    concurrencias <- reactive({
        concurrencias <- read.csv(paste("Concurrencias ", input$ciudad, ".csv", sep = ""), fileEncoding = "UTF-8")
        concurrencias$dia <- factor(concurrencias$dia, levels = unique(concurrencias$dia))
        concurrencias$lugar.abr <- paste(substr(concurrencias$lugar, 1, 10), "...", sep = "")
        concurrencias$lugar.abr <- factor(concurrencias$lugar.abr, levels = sort(unique(concurrencias$lugar.abr)))
        nombre.direccion <- as.data.frame(str_split_fixed(concurrencias$lugar, ",", 3)[,1:2])
        names(nombre.direccion) <- c("nombre", "direccion")
        nombre.direccion$nombre <- trimws(nombre.direccion$nombre)
        nombre.direccion$direccion <- trimws(nombre.direccion$direccion)
        concurrencias <- cbind(concurrencias, nombre.direccion)
        concurrencias
    })
    
    output$pickerLugar = renderUI({
        pickerInput("lugar", "Seleccionar local", choices = sort(unique(as.character(concurrencias()$lugar))), selected = unique(as.character(concurrencias()$lugar))[1], options = list("live-search" = TRUE, `actions-box` = TRUE), multiple = TRUE)
        })
    
    
    output$checkDia = renderUI({
        awesomeCheckboxGroup("dia.semana", "Día de la semana", choices = unique(as.character(concurrencias()$dia)), selected = unique(concurrencias()$dia), inline = TRUE)
    })
    

    output$anim = renderUI({
        img(src=paste("Video completo ", input$ciudad, ".gif", sep = ""), align = "center", height='750px', width='750px')
    })
    
    
    df <- reactive({
        st_as_sf(x = concurrencias(),                         
                       coords = c("longitud", "latitud"),
                       crs = projcrs)
    })
    
    
    ranking <- reactive({
        concurrencias() %>%
            filter(dia %in% input$dia.semana,
                   between(hora, input$hora[1], input$hora[2])) %>%
            group_by(nombre, direccion, latitud, longitud) %>%
            summarise(Suma.concurrencias = sum(concurrencia, na.rm = TRUE)) %>%
            ungroup() %>%
            arrange(desc(Suma.concurrencias)) %>%
            mutate(Posición = row_number()) %>%
            relocate(Posición, nombre, direccion, Suma.concurrencias, latitud, longitud)
    })
    
    
    box.ush <- reactive({
        box.ush <- st_bbox(df())
        box.ush[[1]] <- box.ush[[1]] - expansion
        box.ush[[2]] <- box.ush[[2]] - expansion
        box.ush[[3]] <- box.ush[[3]] + expansion
        box.ush[[4]] <- box.ush[[4]] + expansion
        box.ush
        })
    
    
    ranking.mapa <- reactive({
        st_as_sf(x = as.data.frame(ranking()),                         
                 coords = c("longitud", "latitud"),
                 crs = projcrs)
    })
    
    output$plot.concurrencia <- plotly::renderPlotly({
        concurrencias() %>%
            filter(lugar %in% input$lugar,
                   dia %in% input$dia.semana) %>%
            ggplot(aes(x = hora, y = concurrencia, fill = lugar.abr)) +
            geom_col(position = "dodge") +
            xlab("Hora del día") +
            ylab("Concurrencia") +
            tema_mati() +
            facet_rep_grid(dia~., repeat.tick.labels = TRUE)
    })
    
    output$mapa <- renderLeaflet({
        tmap_leaflet(tm_shape(df()[df()$lugar %in% input$lugar,], bbox = box.ush()) +
                         tm_dots(shape = 21, col = paleta[3], size = 0.2) +
                         tm_text("lugar.abr", ymod = -0.6, size = 1, just = "center")
        )
    })
    
    output$boxplot.dia <- renderPlot({
        ggplot(concurrencias(), aes(x = hora, y = concurrencia, group = hora)) +
            geom_boxplot() +
            facet_grid(dia~.) +
            tema_mati()
    })
    
    
    
    output$mas.concurridos <- DT::renderDT({
        ranking()%>%
            select(Posición, nombre, direccion, Suma.concurrencias)})
    
    output$mapa.ranking <- renderLeaflet({
        ranking.mapa <- ranking.mapa()
        tmap_leaflet(tm_shape(ranking.mapa, bbox = box.ush()) +
                         tm_dots(shape = 21, size = 0.2, col = "Suma.concurrencias") +
                         tm_text("Posición", size = 1, just = "center")
        )
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
