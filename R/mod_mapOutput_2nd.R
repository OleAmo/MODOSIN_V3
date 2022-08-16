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

# ..................... FUNCION PRINCIPAL ......................
# ..............................................................

#       .) Tiene 3 PARTES
#               .) VARIABLES (input, output,...)
#               .) OUTPUT MAP
#               .) OBSERVE


# ........ VARIABLES GLOBALES ..........
# ......................................


# provincias  
# comarcas
# provincias_simplify
# comarcas_simplify


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
   
   # ............. CREATE DATA_DAY .................
   # ...............................................
   
   #   .) DATA DAY son los PLOTS de SOLO UNA FECHA
   #   .) ARGUMENTOS:
   #             .) FECHA
   #             .) SF = Creado por MODOSINDB
   
   
   table_create = function(fecha, sf){
     fecha_format <- as.Date(fecha)
     data_day <- sf %>%
       data.frame() %>%
       dplyr::filter(date == fecha_format)
     return(data_day)
   } 
   
   
   # ............ MAPA PLOTS DATADAY ...............
   # ...............................................
   
   
   #   .) Una vez el usuario APRETE BOTON PROYECTAR
   #   .) Se proyectaran los PLOTS en f(X) de:
   #   .) VARIABLE / SIZE / QUANTILES


   leaflet_create <- eventReactive(data_reactives$boto_reactive, {

     # ......... INICIALIZAR DATOS ............
     # ........................................

     #      .) VARIABLE:
     #            .) Es la variable a PROYECTAR
     #            .) La obtenemos del REACTIVE constante => MOD_DATAINPUT 
     #      .) DATA DAY:
     #            .) Uso la funcion TABLE_CREATE
     #            .) Necesito la FECHA = data_reactives$fecha_reactive
     #            .) Necesito el SF    = main_data_reactives$data_day
     
     variable <- data_reactives$variable_reactive
     
     fecha <- data_reactives$fecha_reactive
     sf <- main_data_reactives$data_day
     
     data_day <- table_create(fecha,sf)


     # ......... PROYECTAR TABLA ..............
     # ........................................

     #      .) Creo un DF Filtrado
     #      .) Para usar el LEAFLET necesitamos:
     #            .) Longitud
     #            .) Latitud

     #     .) Para tener Long/Lat necesitamos:
     #            .) geometría en formato WKB

     #      .) GEOM to WKB  =>  sf:::st_as_sfc.WKB
     #      .) WKB to LON   =>  sf::st_coordinates(geom_WKB)[,1]
     #      .) WKB to LAT   =>  sf::st_coordinates(geom_WKB)[,2]


     # ....... INDICE de la VARIABLE  .........
     # ........................................

     #      .) Quiero SABER el ÍNDICE de la VARIABLE
     #           .) Por ejemplo si quiero PRECIPITATION
     #           .) Tengo que saber que es el 2
     #      .) Ya que USARE par AUTOMATIZAR los POPUPS y el resot  => data_filter[[2]]


     num_i <- as.numeric(match(variable,names(data_day)))
     selected_var <- as.symbol(names(data_day)[num_i])

     data_filter <- data_day %>%
       dplyr::filter(!is.na(data_day[[num_i]])) %>%
       dplyr::select(plot_id, selected_var, date, plot_origin, geom) %>%
       dplyr::mutate(lon = sf::st_coordinates(.data$geom)[,1],
                     lat = sf::st_coordinates(.data$geom)[,2])%>%
       dplyr::filter(!is.na(lon) | !is.na(lat))

     variable_valores <- round(data_filter[[2]], digits=2)

     # ...... PALETA DE COLORES CONTINUO ......
     # ........................................

     pal <- colorNumeric(palette = "YlGnBu", domain = data_filter[[2]])

     # ...... PALETA DE COLORES QUANTIL .......
     # ........................................

     #       .) Para hacer los QUANTILES
     #       .) NECESSITAMOS: Valores Únicos

     #       .) Creamos Función COLORQUANTILE
     #       .) QPAL = Le indicamos:
     #                .) PALETA
     #                .) DATOS (únicos)
     #                .) N = numero de breaks
     #       .) QPAL_COLORS = Usando la función nos da COLORES para cada BREAK
     #       .) QPAL_LABS 1 = para cada break indicamos los valores
     #       .) QPAL_LABS 2 = creamos el RANGO escrito

     #       .) Aparte CREAMOS el Rango de Break (20%, 40%,...)
     #       .) Y lo unimos con PASTE al LABS_2



     df_unique <- data_filter[[2]] %>% unique()

     qpal <- colorQuantile("RdYlBu", df_unique, n = 5)    # Función COLORQUANTILE
     qpal_colors <- unique(qpal(sort(df_unique)))         # Colores para cada BRAKE
     qpal_labs <- quantile(round(df_unique, digits = 2), seq(0, 1, .2)) # Valores para cada Break

     vec <- c()
     long <- length(qpal_labs)

     for (i in 1:long) {          # Crear VECTOR con cada break (20% 40% ...)
       n <- names(qpal_labs[i])
       vec <- append(vec, n)
     }

     qpal_labs <- paste(lag(qpal_labs), round(qpal_labs,digits = 2), sep = " - ")[-1] # creamos el Rango Escrito

     long <- length(vec)         # Crear EMAPLME Break + Rango Break (20% 0.23 - 0.35 / 40%  0.35 - 0.51...)
     for (i in 2:long) {
       qpal_labs[[i-1]] <- paste('[',vec[i],'] ',qpal_labs[[i-1]])

     }

     # ............... POP UP  ................
     # ........................................

     popInfo<-paste(
       "<h4 style= 'border-bottom: thin dotted #43464C; padding-bottom:4px; margin-bottom:4px;
           font-family: Tahoma, Geneva, sans-serif; color:#43464C;'> Plot_id = ",data_filter$plot_id,"</h4>
        
       <span style='color:#9197A6;'>  
           ",variable," : ", variable_valores,"<br>",
            paste("Ubicacion: ",data_filter$plot_origin, sep=""),"<br>",
            paste("Fecha: ",data_filter$date, sep=""),"</span>"
     )

     # ........ FUNCION SIZE RADIO ............
     # ........................................

     #     .) Función que determina el tamaño del Radio/Parcela
     #     .) En f(x) del que hayamos seleccionado en RADIO BUTTON SIZE
     #     .) Hará que el radio se:
     #            .) Estandard = SIMPRE VALOR 6
     #            .) Variable = en f(x) del rango de la Variable seleccionada


     size_radi = function(a){

       # ..... FUNCION RANGO/PALETA COLOR  ......
       # ........................................

       #     .) Hay rango (min-max) de paletas que son demasiado grandes
       #     .) Ej. => LAI (0-34.000) y visualiza parcelas demasiado grandes
       #     .) Para evitar eso
       #            .) Calculo el Mínimo / Máximo
       #            .) Y si es > 20 le assigno un tope de 6 como radio
       #            .) Si es < 20 el radio serà en f(x) de la variable


       num <- as.numeric(match(variable,names(data_filter)))  # indice de la variable en DATA_FILTER

       summary <- summary(data_filter[num])                   # calculo SUMMARY en funcion del Índice de la variable

       min <- summary[1] %>%
         strsplit(split = ":") %>%
         .[[1]] %>%
         .[2] %>%
         as.numeric()

       max <- summary[6] %>%
         strsplit(split = ":") %>%
         .[[1]] %>%
         .[2] %>%
         as.numeric()

       if(a == "const"){              # si seleccionamos CONSTANTE en el combo => nos darà radio 6 siempre
         return(6)
       } else {
         if( abs(max-min) > 20 ){      # si seleccionamos VARIABLE en el combo
           return(8)                         # si (max-min) > 20 nos darà radio de 6 siempre
         } else {                            # si (max-min) < 20 nos darà radio en f(x) de la variable
           return(~ variable_valores)
         }
       }
     }


     # --------------------------------------------------------------------------
     # ---------------------------  Función LEAFLET  ----------------------------
     # --------------------------------------------------------------------------

     #     .) LEFLET la dividimos en 2 PARTES
     #     .) Lo hacemos ya que DEPENDIENDO del CHECK BOX Leyenda
     #     .) Activaremos con las funciones siguiente diferente
     #              .) addCircleMarkers
     #              .) addLegend


     leaflet_map <- leaflet(data=data_filter) %>%

       # ..... LOCALIZACIÓN / ZOMM  ......
       # .................................

       #      .) Localización + ZOOM

       setView(1.8756609,41.9070227, zoom=8)  %>%


       # ....... CAPES DE FONDO  .........
       # .................................

       #      .) Tipos de Capas de Fondo
       #      .) Se describen en ADDTILES / ADDPROVIDERTILES
       #      .) El MENÚ se crea con ADD_LAYERS_CONTROL

       # addTiles("http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%

     addTiles(group = "OSM (default)") %>%
       addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
       addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%

       addLayersControl(
         baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
         options = layersControlOptions(collapsed = FALSE)
       )


     # ....... AÑADIR PUNTOS ...........
     # .................................

     #      .) RADIUS = Aplicamos f(x) SIZE_RADI
     #      .) COLOR = Aplicamos IF
     #               .) Si el COMBO de LEGEND = "Conti"  => Aplicamos  ~ PAL
     #               .) Si el COMBO de LEGEND = "Quanti" => Aplicamos  ~ QPAL



     if(data_reactives$legend_reactive == "conti") {

       leaflet_map %>%
         addCircleMarkers(
           lat = ~ lat,
           lng = ~ lon,
           weight= 1,
           opacity= 0.8,
           fillOpacity= 0.6,
           radius= size_radi(data_reactives$size_reactive),
           color = ~ pal(data_filter[[2]]),
           popup = popInfo) %>%

         addLegend(
           position = "bottomright",
           title = paste(as.character(selected_var),' (Contínua) '),
           pal = pal,
           values = ~ data_filter[[2]],
           opacity = 1)
     } else {

       leaflet_map %>%
         addCircleMarkers(
           lat = ~ lat,
           lng = ~ lon,
           weight= 1,
           opacity= 0.8,
           fillOpacity= 0.6,
           radius= size_radi(data_reactives$size_reactive),
           color = ~ qpal(data_filter[[2]]),
           popup = popInfo) %>%

         addLegend(
           position = "bottomright",
           title = paste(as.character(selected_var),' (Quantiles) '),
           colors = qpal_colors,
           labels = qpal_labs,
           opacity = 1)
     }

   })

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
   #       .) Antes de APRETAR Botón 
   #               .) el valor de $BOTO_REACTIVE = NULL
   #               .) PROYECTAR mapa inicio
   #       .) Después de APRETAR Botón 
   #               .) el valor de $BOTO_REACTIVE = 1,2,3...
   #               .) PROYECTAR PLOTS de UNA FECHA


  shiny::observe({
     
    shiny::validate(
      shiny::need(data_reactives$entorno_reactive, 'no entorno selected')
    )

    boton_activated <- data_reactives$boto_reactive
    entorno <- data_reactives$entorno_reactive
     
 
    if(boton_activated == 0) {
      
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

          
          
      # 
      # output$map_daily_polygon <- leaflet::renderLeaflet({
      #   
      #   if(entorno == "provincia"){
      #     pantalla_inicio(provincias)
      #   } else {
      #     pantalla_inicio(comarcas)
      #   }
      #   
      #   
      # })
      
    } else if (boton_activated >= 1) {
      output$map_daily_polygon <- leaflet::renderLeaflet({
        leaflet_create()
      })
      
    }
    
    # if(is.null(boton_activated)) {
    #   print("waiting...")
    #   output$map_daily_polygon <- leaflet::renderLeaflet({
    #     pantalla_inicio()
    #   })
    #   
    # } else if (boton_activated >= 1) {
    #   print("ACTIVADO")
    #   output$map_daily_polygon <- leaflet::renderLeaflet({
    #     leaflet_create()
    #   })
    #   
    # }
    
    
    
    


    # shiny::validate(
    #   shiny::need(data_reactives$display_daily, 'no polygon/plots selected'),
    #   shiny::need(data_reactives$var_daily, 'no var selected')
    # )
    # 
    # display_daily <- data_reactives$display_daily
    # var_daily <- data_reactives$var_daily
    # 
    # if (display_daily == 'none') {
    #   leaflet::leafletProxy('map_daily_polygon') %>%
    #     leaflet::clearGroup('display_daily')
    #   return()
    # }
    # 
    # # if plots do markers, if polys do polygons
    # if (display_daily == 'IFN plots') {
    #   leaflet::leafletProxy('map_daily_polygon') %>%
    #     leaflet::clearGroup('display_daily') %>%
    #     leaflet::addCircleMarkers(
    #       data = nfi4_plots,
    #       group = 'display_daily',
    #       label = ~plot_id,
    #       layerId = ~plot_id,
    #       clusterOptions = leaflet::markerClusterOptions()
    #     )
    # } else {
    #   if (display_daily == 'file') {
    #     shiny::validate(
    #       shiny::need(data_reactives$user_file_sel, 'no file uploaded yet'),
    #       shiny::need(main_data_reactives$timeseries_data$sf, 'No sf yet')
    #     )
    # 
    #     file_data <- main_data_reactives$timeseries_data$sf
    # 
    #     # if file is polygons we need to draw polygons, if file are points
    #     # we need to draw markers
    #     if (all(sf::st_is(file_data, c('MULTIPOLYGON', 'POLYGON')))) {
    #       leaflet::leafletProxy('map_daily_polygon') %>%
    #         leaflet::clearGroup('display_daily') %>%
    #         leaflet::addPolygons(
    #           data = file_data,
    #           group = 'display_daily',
    #           label = ~file_data %>% dplyr::pull(1),
    #           layerId = ~file_data %>% dplyr::pull(1),
    #           weight = 2, smoothFactor = 1,
    #           opacity = 1.0, fill = TRUE, fillOpacity = 0,
    #           color = 'black',
    #           highlightOptions = leaflet::highlightOptions(
    #             color = "#CF000F", weight = 2,
    #             bringToFront = TRUE,
    #             fill = TRUE, fillOpacity = 0
    #           )
    #         )
    #     }
    # 
    #     if (all(sf::st_is(file_data, c('MULTIPOINT', 'POINT')))) {
    #       leaflet::leafletProxy('map_daily_polygon') %>%
    #         leaflet::clearGroup('display_daily') %>%
    #         leaflet::addCircleMarkers(
    #           data = file_data,
    #           group = 'display_daily',
    #           label = ~file_data %>% dplyr::pull(1),
    #           layerId = ~file_data %>% dplyr::pull(1)
    #         )
    #     }
    #   } else {
    #     polygon_object_name <- glue::glue("{tolower(display_daily)}_polygons")
    # 
    #     leaflet::leafletProxy('map_daily_polygon') %>%
    #       leaflet::clearGroup('display_daily') %>%
    #       leaflet::addPolygons(
    #         data = rlang::eval_tidy(rlang::sym(polygon_object_name)),
    #         group = 'display_daily',
    #         label = ~poly_id, layerId = ~poly_id,
    #         weight = 1, smoothFactor = 1,
    #         opacity = 1.0, fill = TRUE, fillOpacity = 0,
    #         color = '#6C7A89FF',
    #         highlightOptions = leaflet::highlightOptions(
    #           color = "#CF000F", weight = 2,
    #           bringToFront = TRUE,
    #           fill = TRUE, fillOpacity = 0
    #         )
    #       )
    #   }
    # }

  })

  ## observers to change the active tab ####
  # shiny::observeEvent(
  #   eventExpr = input$map_daily_polygon_shape_click,
  #   handlerExpr = {
  #     # go to series
  #     shiny::updateTabsetPanel(
  #       parent_session, 'main_panel_tabset',
  #       selected = 'series_panel'
  #     )
  #   },
  #   priority = 1000
  # )
  # shiny::observeEvent(
  #   eventExpr = input$map_daily_polygon_click,
  #   handlerExpr = {
  #     # go to series
  #     shiny::updateTabsetPanel(
  #       parent_session, 'main_panel_tabset',
  #       selected = 'series_panel'
  #     )
  #   },
  #   priority = 1000
  # )
  # shiny::observeEvent(
  #   eventExpr = input$map_daily_polygon_marker_click,
  #   handlerExpr = {
  #     # go to series
  #     shiny::updateTabsetPanel(
  #       parent_session, 'main_panel_tabset',
  #       selected = 'series_panel'
  #     )
  #   },
  #   priority = 1000
  # )

  ## reactives to return ####
  map_reactives <- shiny::reactiveValues()
  shiny::observe({
    # map_reactives$map_daily_polygon_shape_click <- input$map_daily_polygon_shape_click
    # map_reactives$map_daily_polygon_marker_click <- input$map_daily_polygon_marker_click
    # map_reactives$map_daily_polygon_click <- input$map_daily_polygon_click
    # map_reactives$map_daily_polygon_draw_all_features <-
    #   input$map_daily_polygon_draw_all_features
  })
  return(map_reactives)

}
