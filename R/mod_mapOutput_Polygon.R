#' @title mod_mapOutput and mod_map
#'
#' @description Shiny module to generate the map
#'
#' @param id shiny id
#'
#' @export
#'
#'
# ....... FUNCION INICIALIZA UI mapa ..........
# .............................................

#       .) PRIMERO
#               .) INICIALIZAR Librarys
#               .) URL de polígonos
#       .) SEGUNDO
#               .) Activa el NS
#               .) Crea el LEAFLET MAPA => leafletOutput("map_daily_polygon"))
#               .) Crea Map Container Reactivo =>  UIOUTPUT

mod_mapOutput_polygon <- function(id) {

  # ns
  ns <- shiny::NS(id)
  shiny::tagList(
    leaflet::leafletOutput(ns("map_daily_polygon"), height = 600),
    shiny::uiOutput(ns('map_container_polygon'))
  )
}

#' mod_map server function
#'
#' @details mod_map is in charge of setting the points/polygons (sf) and rasters
#'   in the leaflet projection.
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives,main_data_reactives reactives
#' @param parent_session session object to change active tab
#' @param lang lang selected
#'
#' @export
#'
#' @rdname mod_mapOutput
#'

#' @export
#'
#'
# ..................... FUNCION PRINCIPAL ......................
# ..............................................................

#       .) Tiene 3 PARTES
#               .) VARIABLES (input, output,...)
#               .) OUTPUT MAP
#               .) OBSERVE


mod_map_polygon <- function(
  input, output, session,
  data_reactives, main_data_reactives, 
  parent_session, lang
) {
  library(sf)
  source('data-raw/polygon_objects_creation.R')
  
  # ................ MAPA INICIO ..................
  # ...............................................
  
  #   .) Es el MAPA de LEAFLET que sale por defecto al principio
  #   .) Una vez el usuario APRETA BOTON PROYECTA MAPA
  #   .) Se visualiza los PLOTS
  
  
  pantalla_inicio = function(poly,entorno){
    
    #   .) LABEL:
    #   .) Definimos el HTML que mostrarà en pasar el mouse
    #   .) Usamos del ATRIBUTO nom y area_km2 de los POLÍGONSO
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Àrea / km<sup>2</sup>",
      poly$nom, poly$area_km2
    ) %>% lapply(htmltools::HTML)
    
    #   .) LEAFLET 1ra Parte:
    #   .) Definimos ZOMM, ENCUADRE, TILES,...
    
    leaflet <- leaflet() %>%
      setView(1.8756609,41.9070227, zoom=8)  %>%
      addTiles(group = "OSM (default)")  %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")
    
    #   .) LEAFLET 2nda Parte:
    #   .) Usamos 2 variebles de la función:
    
    #               .) POLY     = SF (provincias o comarcas)
    #               .) ENTORNO  = String ("provincias","comarcas")
    
    leaflet <- leaflet %>%
      addPolygons(data = poly,
                  color = ~ ifelse(entorno == "provincia", "#520607", "#054a15"),
                  weight = 0.5, 
                  smoothFactor = 0.5,
                  opacity = 1.0, 
                  fillOpacity = 0.4,
                  fillColor = ~ ifelse(entorno == "provincia", "red", "green"),
                  highlightOptions = highlightOptions(
                    color = "white", 
                    weight = 2,
                    fillOpacity = 0.4,
                    fillColor = ~ ifelse(entorno == "provincia", "green", "red"),
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
        options = layersControlOptions(collapsed = FALSE))
    
  }
   

   
  # ....................... OUTPUT MAP ...........................
  # ..............................................................

  # ....... DATA INPUTS ..........
  # ..............................

  #       .) Indicamos DONDE se harà el OUTPUT
  #       .) Lo indicamos con      => output$map_daily_polygon
  #       .) Usamos la función     => renderLeaflet

  #       .) EL OUTPUT$map_daily_polygon:
  #               .) APUNTA ala función = MOD_MAPOUTPUT
  #               .) MOD_MAPOUTPUT le asigna = leafletOutput


  # ..................... OBSERVER MAP ...........................
  # ..............................................................

  #       .) OBSERVER
  #       .) PRIMERO => VALIDAMOS el ENTORNO REACTIVE
  #               .) Significa que SHINY antes de CONTINUAR espera a tener INFO de ENTORNO
  #               .) Osea que la variable sea (Pronvincia, Comarca o No Polígono)
  #               .) Sino validamos, SHYNI se bloquea ya que al inicio da NULL
  
  #       .) SEGUNDO =>
  #               .) Dependiendo del VALOR del ENTORNO
  #               .) Asignamos a f(x) PANTALLA INICIO las variables
  #                         .) SF       = Geometría a Proyectar
  #                         .) ENTORNO  = Definirà el COLOR y ESTILO dela geometría a proyectar
           

  shiny::observe({
     
    shiny::validate(shiny::need(data_reactives$entorno_reactive, 'no entorno selected'))
    
    entorno <- data_reactives$entorno_reactive
      
    if(entorno == "provincia"){
      output$map_daily_polygon <- leaflet::renderLeaflet({
        pantalla_inicio(provincias,entorno)
      })
      
    } else if (entorno == "comarca") {
      output$map_daily_polygon <- leaflet::renderLeaflet({
        pantalla_inicio(comarcas,entorno)
      })
    } else if (entorno == "no_polygon") {
      output$map_daily_polygon <- leaflet::renderLeaflet({
        leaflet() %>%
          setView(1.8756609,41.9070227, zoom=8)  %>%
          addTiles(group = "OSM (default)")  %>%
          addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
          addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
          addLayersControl(
            baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
            options = layersControlOptions(collapsed = FALSE))
      })
    }


  })




  ## reactives to return ####
  map_reactives <- shiny::reactiveValues()
  shiny::observe({
    # map_reactives$map_daily_polygon_shape_click <- input$map_daily_polygon_shape_click
    # map_reactives$map_daily_polygon_marker_click <- input$map_daily_polygon_marker_click

  })
  return(map_reactives)

}
