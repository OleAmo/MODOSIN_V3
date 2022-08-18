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
  # data_reactives, modosin_data_polygon, data_reactives_polygon,
  data_reactives_polygon,modosin_data_polygon, 
  parent_session, lang
) {
  library(sf)
  source('data-raw/polygon_objects_creation.R')
  
  # .............................. MAPA INICIO ................................
  # ...........................................................................
  
  #   .) Es el MAPA de LEAFLET que sale por defecto al principio
  #   .) Una vez el usuario APRETA BOTON PROYECTA MAPA
  #   .) Se visualiza los PLOTS
  
  
  pantalla_inicio = function(poly,layers){
    
    #   .) LABEL:
    #   .) Definimos el HTML que mostrarà en pasar el mouse
    #   .) Usamos del ATRIBUTO nom y area_km2 de los POLÍGONSO
    
    # .......... DIFERENTES LABELS ...........
    # ........................................
    
    #          .) Creo 2 tipos de LABELS
    #          .) Ya que 2 SF usan AREA Km2
    #          .) Y 2 otro SF usan AREA Ha
    
    a <- sprintf(
      "<strong>%s</strong><br/>%g Àrea / km<sup>2</sup>",
      poly$nom , poly$area_km2
    ) %>% lapply(htmltools::HTML)
    
    b <- sprintf(
      "<strong>%s</strong><br/>%g Àrea / Ha",
      poly$nom , poly$Area_Ha
    ) %>% lapply(htmltools::HTML)
    
    
    
    if(as.character(layers) == "provincia" | as.character(layers) == "comarca" ) {
        
      labels <- a
      
    } else if (as.character(layers) == "nucleos" | as.character(layers) =="embass") {
      labels <- b
    }
    
    # ......... DIFERENTES ESTILOS ...........
    # ........................................
    
    #          .) Tengo 4 SF diferentes
    #          .) Creo ESTILOS diferentes
    
    fill_poly <- function (a){
      if (a == "provincia") {
        return("red")
      } else if (a == "comarca") {
        return("green")
      } else if (a == "nucleos" ) {
        return("black")
      } else if (a == "embass"){
        return("#5cc8fa")
      }
    }
    
    color_poly <- function (a){
      if (a == "provincia") {
        return("#870522")
      } else if (a == "comarca") {
        return("#03400d")
      } else if (a == "nucleos" ) {
        return("black")
      } else if (a == "embass"){
        return("blue")
      }
    }
    
    weight_poly <- function (a){
      if (a == "nucleos") {
        return(1)
      } else {
        return(0.5)
      }  
    }
    

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
    #               .) layers  = String ("provincias","comarcas")
    
    leaflet <- leaflet %>%
      addPolygons(data = poly,
                  color = color_poly(layers),
                  weight = weight_poly(layers), 
                  smoothFactor = 0.5,
                  opacity = 1.0, 
                  fillOpacity = 0.4,
                  fillColor =   fill_poly(layers),
                  highlightOptions = highlightOptions(
                    color = "white", 
                    weight = 2,
                    fillOpacity = 0.4,
                    fillColor =  ~ ifelse(layers == "provincia", "green", "red"),
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
  #       .) PRIMERO => VALIDAMOS el layers REACTIVE
  #               .) Significa que SHINY antes de CONTINUAR espera a tener INFO de layers
  #               .) Osea que la variable sea (Pronvincia, Comarca o No Polígono)
  #               .) Sino validamos, SHYNI se bloquea ya que al inicio da NULL
  
  #       .) SEGUNDO =>
  #               .) Dependiendo del VALOR del layers
  #               .) Asignamos a f(x) PANTALLA INICIO las variables
  #                         .) SF       = Geometría a Proyectar
  #                         .) layers  = Definirà el COLOR y ESTILO dela geometría a proyectar
           

  shiny::observe({
     
    shiny::validate(shiny::need(data_reactives_polygon$layers_select, 'no layers selected'))
    
    layers <-data_reactives_polygon$layers_select
  
  
    if(layers == "provincia"){
      output$map_daily_polygon <- leaflet::renderLeaflet({
        pantalla_inicio(provincias,layers)
      })
      
    } else if (layers == "comarca") {
      output$map_daily_polygon <- leaflet::renderLeaflet({
        pantalla_inicio(comarcas,layers)
      })
    } else if (layers == "nucleos") {
      
          area <- data_reactives_polygon$nucleos_area
          
          nucleo_area <- function(nucleos,area) {
            a_min <- area %>% 
              strsplit(split = "-") %>%
              .[[1]] %>%
              .[1] %>%
              as.numeric() 
            a_max <- area %>% 
              strsplit(split = "-") %>%
              .[[1]] %>%
              .[2] %>%
              as.numeric() 
            
            res <- nucleos %>% dplyr::filter(., Area_Ha >= a_min & Area_Ha < a_max)
            return(res)
            
          }
          
          
          if (area  == "0-10") {
            output$map_daily_polygon <- leaflet::renderLeaflet({
              pantalla_inicio(nucleo_area(nucleos,area),layers)
            })

          } else if (area  == "10-25") {
            output$map_daily_polygon <- leaflet::renderLeaflet({
              pantalla_inicio(nucleo_area(nucleos,area),layers)
            })

          } else if (area  == "25-50") {
            output$map_daily_polygon <- leaflet::renderLeaflet({
              pantalla_inicio(nucleo_area(nucleos,area),layers)
            })

          } else if (area  == "50-100") {
            output$map_daily_polygon <- leaflet::renderLeaflet({
              pantalla_inicio(nucleo_area(nucleos,area),layers)
            })

          } else if (area  == "100-250") {
            output$map_daily_polygon <- leaflet::renderLeaflet({
              pantalla_inicio(nucleo_area(nucleos,area),layers)
            })

          } else if (area  == "1000-99999") {
            output$map_daily_polygon <- leaflet::renderLeaflet({
              pantalla_inicio(nucleo_area(nucleos,area),layers)
            })

          }
            
      
    } else if (layers == "embass") {
      output$map_daily_polygon <- leaflet::renderLeaflet({
        pantalla_inicio(embass,layers)
      })
  
    } else if (layers == "no_polygon") {
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
