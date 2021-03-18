
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
library(tidyr)



Sys.setenv(LANG = "en")
options(encoding = 'UTF-8')

source("mobility.R")

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

convertMenuItem <- function(mi,tabName) {
    mi$children[[1]]$attribs['data-toggle']="tab"
    mi$children[[1]]$attribs['data-value'] = tabName
    if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
        mi$attribs$class=NULL
    }
    mi
}







header <- dashboardHeader(
    dropdownMenu(
        type = "messages",
        messageItem(
            from = "Admin",
            message = "Proximamente se agregarán otros"
        )
    )
)


sidebar <- dashboardSidebar(
    sidebarMenu(
        convertMenuItem(tabName = "tab1", menuItem("Concurrencias", tabName = "concurrencias", icon = icon("glass-cheers"),
                 pickerInput("ciudad", "Seleccionar ciudad", choices = c("Pinamar", "Ushuaia"), selected = "Pinamar", options = list("live-search" = TRUE, `actions-box` = TRUE), multiple = FALSE),
                 uiOutput('pickerLugar'),
                 uiOutput('checkDia'),
                 sliderTextInput(inputId = "hora", label = "Seleccionar intervalo de horas del día", choices = 0:23, selected = c(0, 23)))
                 ),
        convertMenuItem(tabName = "tab2", menuItem("Movilidad", tabName = "movilidad", icon = icon("route"),
                 uiOutput("provincia"),
                 uiOutput("partido"))
        )
    )
)



body <- dashboardBody(
    tabItems(
        tabItem(tabName = "concurrencias",
                tabBox(width = 12,
                       title = "Concurrencia",
                       tabPanel("Observaciones iniciales", 
                                plotly::plotlyOutput('plot.concurrencia', height = "800px"),
                                leafletOutput("mapa")),
                       tabPanel('Métricas resumen',
                                plotOutput('boxplot.dia'),
                                uiOutput('anim')),
                       tabPanel('Más concurridos',
                                DT::DTOutput("mas.concurridos"),
                                leafletOutput("mapa.ranking")),
                       tabPanel('Variación en el tiempo',
                                plotly::plotlyOutput('plot.movilidad.actividades'),
                                awesomeRadio(
                                    inputId = "actividad",
                                    label = "Seleccione actividad",
                                    choices = c("Alimentos y farmacias", "Comercio minorista y entretenimiento",  "Parques", "Terminales de transporte público", "Zonas de trabajo", "Zonas residenciales"),
                                    selected = "Alimentos y farmacias",
                                    inline = TRUE,
                                    status = "success"),
                                plotly::plotlyOutput('plot.movilidad.arg'))
                )
        ),
        tabItem(tabName = "movilidad",
                actionBttn(
                    inputId = "boton",
                    label = "Refrescar datos",
                    style = "fill", 
                    color = "success"),
                plotly::plotlyOutput('plot.movilidad.actividades.partido'),
                awesomeRadio(
                    inputId = "actividad2",
                    label = "Seleccione actividad",
                    choices = c("Alimentos y farmacias", "Comercio minorista y entretenimiento",  "Parques", "Terminales de transporte público", "Zonas de trabajo", "Zonas residenciales"),
                    selected = "Alimentos y farmacias",
                    inline = TRUE,
                    status = "success"),
                plotly::plotlyOutput('plot.movilidad.arg.partido')
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
        awesomeCheckboxGroup("dia.semana", "Día de la semana", choices = unique(as.character(concurrencias()$dia)), selected = unique(concurrencias()$dia), inline = FALSE)
    })
    

    output$anim = renderUI({
        img(src=paste("Video completo ", input$ciudad, ".gif", sep = ""), align = "center", height='750px', width='750px')
    })
    
    argentina <- reactive({
        input$boton
        URL <- "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip"
        temp <- "descargados.zip"
        utils::download.file(URL,temp, method = "curl")
        mobility_var("AR") %>%
            mutate(sub_region_1 = case_when(sub_region_1 == "" ~ "Total país",
                                            TRUE ~ as.character(sub_region_1)),
                   sub_region_2 = case_when(sub_region_1 == "Total país" ~ "Total país",
                                            sub_region_2 == "" ~ "Total provincia",
                                            TRUE ~ as.character(sub_region_2)))
    })
    
    output$provincia <- renderUI({
        pickerInput(
            inputId = "provincia",
            label = "Seleccionar provincia", 
            selected = "Total país",
            choices = sort(unique(argentina()$sub_region_1)),
            options = list("live-search" = TRUE)
        )
    })
    
    output$partido <- renderUI({
        pickerInput(
            inputId = "partido",
            label = "Seleccionar partido", 
            selected = "Total provincia",
            choices = sort(unique(argentina()[argentina()$sub_region_1 == input$provincia, "sub_region_2"])),
            options = list("live-search" = TRUE)
        )
    })
    
    base.partido <- reactive({
        datos <- argentina()[argentina()$sub_region_1 == "Total país" | (argentina()$sub_region_1 == input$provincia & argentina()$sub_region_2 == input$partido),]

        datos$date <- as.Date(datos$date)
        
        names(datos)[3] <- "Provincia"
        names(datos)[4] <- "Partido"
        names(datos)[9] <- "Fecha"
        names(datos)[10] <- "Comercio minorista y entretenimiento"
        names(datos)[11] <- "Alimentos y farmacias"
        names(datos)[12] <- "Parques"
        names(datos)[13] <- "Terminales de transporte público"
        names(datos)[14] <- "Zonas de trabajo"
        names(datos)[15] <- "Zonas residenciales"
        
        datos <- datos[, c(3, 4, 9:15)] %>% gather(Actividad, Cambio_porcentual_a_referencia, -c(Provincia, Partido, Fecha))
        names(datos)[5] <- "Cambio porcentual a referencia"
        
        datos
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
            facet_grid(dia~.)
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
    
    
    
    region.arg <- reactive({
        datos <- argentina() %>% mutate(sub_region_2 = case_when(sub_region_2 == "Pinamar Partido" ~ "Pinamar",
                                                           sub_region_2 == "Ushuaia Department" ~ "Ushuaia",
                                                           TRUE ~ as.character(sub_region_2)))
        datos$date <- as.Date(datos$date)
        
        names(datos)[4] <- "Region"
        names(datos)[9] <- "Fecha"
        names(datos)[10] <- "Comercio minorista y entretenimiento"
        names(datos)[11] <- "Alimentos y farmacias"
        names(datos)[12] <- "Parques"
        names(datos)[13] <- "Terminales de transporte público"
        names(datos)[14] <- "Zonas de trabajo"
        names(datos)[15] <- "Zonas residenciales"
        
        names(datos[, c(4, 9:15)])
        
        datos <- datos[, c(4, 9:15)] %>% gather(Actividad, Cambio_porcentual_a_referencia, -c(Region, Fecha))
        names(datos)[4] <- "Cambio porcentual a referencia"
        
        datos
    })
    
    output$plot.movilidad.actividades <- renderPlotly({
        g <- region.arg() %>%
            filter(Region == input$ciudad) %>%
            ggplot(aes(x = Fecha, y = `Cambio porcentual a referencia`, col = Actividad)) +
            geom_line(alpha = 0.8) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            tema_mati()
        
        ggplotly(g, dynamicTicks = TRUE)
    })
    
    output$plot.movilidad.arg <- renderPlotly({
        g <- region.arg() %>%
            filter(Region %in% c(input$ciudad, "Total país") &
                Actividad == input$actividad) %>%
            mutate(transparencia = case_when(Region == input$ciudad ~ 0.9,
                                             Region == "Total país" ~ 0.5)) %>%
            ggplot(aes(x = Fecha, y = `Cambio porcentual a referencia`, color = Region, alpha = transparencia)) +
            geom_line()+
            scale_alpha_continuous(guide=FALSE, range = c(0.2, 1)) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            tema_mati()
        
        ggplotly(g, dynamicTicks = TRUE)
    })
    
    output$plot.movilidad.actividades.partido <- renderPlotly({
        g <- base.partido() %>%
            filter(Partido == input$partido) %>%
            ggplot(aes(x = Fecha, y = `Cambio porcentual a referencia`, col = Actividad)) +
            geom_line(alpha = 0.8) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            tema_mati()
        
        ggplotly(g, dynamicTicks = TRUE)
    })
    
    output$plot.movilidad.arg.partido <- renderPlotly({
        g <- base.partido() %>%
            filter(Actividad == input$actividad2) %>%
            mutate(transparencia = case_when(Partido == input$partido ~ 0.9,
                                             Partido == "Total país" ~ 0.5)) %>%
            ggplot(aes(x = Fecha, y = `Cambio porcentual a referencia`, color = Partido, alpha = transparencia)) +
            geom_line()+
            scale_alpha_continuous(guide=FALSE, range = c(0.2, 1)) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            tema_mati()
        
        ggplotly(g, dynamicTicks = TRUE)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
