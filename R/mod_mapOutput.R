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
#               .) Crea el LEAFLET MAPA => leafletOutput("map_daily"))
#               .) Crea Map Container Reactivo =>  UIOUTPUT

mod_mapOutput <- function(id) {

  # ns
  ns <- shiny::NS(id)
  shiny::tagList(
    leaflet::leafletOutput(ns("map_daily"), height = 600),
    shiny::uiOutput(ns('map_container'))
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


mod_map <- function(
  input, output, session,
  data_reactives, main_data_reactives,
  parent_session, lang
) {

   pantalla_inicio <- eventReactive( data_reactives$fecha_reactive, {

    leaflet() %>%
      setView(1.8756609,41.9070227, zoom=8)  %>%
      addTiles(group = "OSM (default)")  %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      addPolygons(data = provincias,
                  fillColor = "#7bb8e0",
                  fillOpacity = 0.2,
                  weight = 1,
                  color = "#034773",
                  opacity = 0.8,
                  group = "provincias") %>%
      addPolygons(data = comarcas,
                  fillColor = "#72cc77",
                  fillOpacity = 0.2,
                  weight = 1,
                  color = "#025418",
                  opacity = 0.8,
                  group = "comarcas") %>%
      addPolygons(data = provincias_simplify,
                  fillColor = "#7bb8e0",
                  fillOpacity = 0.2,
                  weight = 1,
                  color = "#034773",
                  opacity = 0.8,
                  group = "provincias_simplify") %>%
      addPolygons(data = comarcas_simplify,
                  fillColor = "#72cc77",
                  fillOpacity = 0.2,
                  weight = 1,
                  color = "#025418",
                  opacity = 0.8,
                  group = "comarcas_simplify") %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
        overlayGroups = c("provincias","provincias_simplify", "comarcas","comarcas_simplify"),
        options = layersControlOptions(collapsed = FALSE)) %>%

      hideGroup(c("provincias","provincias_simplify","comarcas","comarcas_simplify"))


  })


   leaflet_create <- eventReactive(input$boto, {

     # ......... INICIALIZAR DATOS ............
     # ........................................

     #      .) Obtengo FECHA / VARIABLE / TABLE
     #      .) Son Valores REACTIVES
     #      .) Los obtengo de los COMBOS

     variable <- variable_reactive()

     # ......... CREAR TABLA  .................
     # ........................................

     #      .) Uso la funcions GET DATA de MODOSIN
     #      .) Uso la FECHA ( valor reactivo )

     data_day <- table_create()


     # ......... PROYECTAR TABLA ..............
     # ........................................

     #      .) Creo un DF Filtrado
     #      .) Para usar el LEAFLET necesitamos:
     #           .) Longitud
     #           .) Latitud

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
       filter(!is.na(data_day[[num_i]])) %>%
       select(plot_id, selected_var, date, plot_origin, geom) %>%
       dplyr::mutate(lon = sf::st_coordinates(.data$geom)[,1],
                     lat = sf::st_coordinates(.data$geom)[,2])%>%
       filter(!is.na(lon) | !is.na(lat))

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

     popInfo<-paste("<h4 style=  'border-bottom: thin dotted #43464C;
                                     padding-bottom:4px;
                                     margin-bottom:4px;
                                     font-family: Tahoma, Geneva, sans-serif;
                                     color:#43464C;'>

                      Plot_id = ",data_filter$plot_id,"</h4>
                     <span style='color:#9197A6;'>
                            ",variable," : ", variable_valores, "<br>",
                    paste("Ubicacion: ",data_filter$plot_origin, sep=""), "<br>",
                    paste("Fecha: ",data_filter$date, sep=""),

                    "</span>"
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



     if(legend_reactive() == "conti") {

       leaflet_map %>%
         addCircleMarkers(
           lat = ~ lat,
           lng = ~ lon,
           weight= 1,
           opacity= 0.8,
           fillOpacity= 0.6,
           radius= size_radi(size_reactive()),
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
           radius= size_radi(size_reactive()),
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
  #       .) Lo indicamos con      => output$map_daily
  #       .) Usamos la función     => renderLeaflet

  #       .) EL OUTPUT$map_daily:
  #               .) APUNTA ala función = MOD_MAPOUTPUT
  #               .) MOD_MAPOUTPUT le asigna = leafletOutput


  ## map output ####
  output$map_daily <- leaflet::renderLeaflet({

    pantalla_inicio()

    # shiny::req(
    #   main_data_reactives$raster_selected_daily,
    #   data_reactives$var_daily
    # )

    # shiny::validate(
    #   shiny::need(main_data_reactives$raster_selected_daily, 'no raster yet'),
    #   shiny::need(data_reactives$var_daily, 'no var yet')
    # )

    # raster_daily <- main_data_reactives$raster_selected_daily
    # var_daily <- data_reactives$var_daily
    #
    # leaflet_raster <- raster_daily[[var_daily]]
    #
    # palette <- leaflet::colorNumeric(
    #   palette = palettes_dictionary[[var_daily]][['pal']],
    #   # domain = c(
    #   #   palettes_dictionary[[var_daily]][['min']],
    #   #   palettes_dictionary[[var_daily]][['max']]
    #   # ),
    #   domain = raster::values(leaflet_raster),
    #   na.color = 'transparent',
    #   reverse = palettes_dictionary[[var_daily]][['rev']]
    # )
    #
    # # legend palette
    # legend_palette <- leaflet::colorNumeric(
    #   palette = palettes_dictionary[[var_daily]][['pal']],
    #   # domain = c(
    #   #   palettes_dictionary[[var_daily]][['min']],
    #   #   palettes_dictionary[[var_daily]][['max']]
    #   # ),
    #   domain = raster::values(leaflet_raster),
    #   na.color = 'transparent',
    #   reverse = !palettes_dictionary[[var_daily]][['rev']]
    # )
    #
    # leaflet::leaflet() %>%
    #   leaflet::setView(1.744, 41.726, zoom = 8) %>%
    #   leaflet::addProviderTiles(
    #     leaflet::providers$Esri.WorldShadedRelief,
    #     group = translate_app('Relief', lang())
    #   ) %>%
    #   leaflet::addProviderTiles(
    #     leaflet::providers$Esri.WorldImagery,
    #     group = translate_app('Imagery', lang())
    #   ) %>%
    #   leaflet::addLayersControl(
    #     baseGroups = c(translate_app('Relief', lang()), translate_app('Imagery', lang())),
    #     overlayGroups = c('raster'),
    #     options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
    #   ) %>%
    #   leaflet::addRasterImage(
    #     leaflet_raster, project = FALSE, group = 'raster',
    #     colors = palette, opacity = 1
    #   ) %>%
    #   leaflet::addLegend(
    #     pal = legend_palette, values = raster::values(leaflet_raster),
    #     title = translate_app(var_daily, lang()),
    #     position = 'bottomright', opacity = 1,
    #     labFormat = leaflet::labelFormat(
    #       transform = function(x) {sort(x, decreasing = TRUE)}
    #     )
    #   )
  })

  # ..................... OBSERVER MAP ...........................
  # ..............................................................

  # ....... DATA INPUTS ..........
  # ..............................

  #       .)

  ## observers to update the map ####
  # draw polygons observer
  shiny::observe({

    shiny::validate(
      shiny::need(data_reactives$display_daily, 'no polygon/plots selected'),
      shiny::need(data_reactives$var_daily, 'no var selected')
    )

    display_daily <- data_reactives$display_daily
    var_daily <- data_reactives$var_daily

    if (display_daily == 'none') {
      leaflet::leafletProxy('map_daily') %>%
        leaflet::clearGroup('display_daily')
      return()
    }

    # if plots do markers, if polys do polygons
    if (display_daily == 'IFN plots') {
      leaflet::leafletProxy('map_daily') %>%
        leaflet::clearGroup('display_daily') %>%
        leaflet::addCircleMarkers(
          data = nfi4_plots,
          group = 'display_daily',
          label = ~plot_id,
          layerId = ~plot_id,
          clusterOptions = leaflet::markerClusterOptions()
        )
    } else {
      if (display_daily == 'file') {
        shiny::validate(
          shiny::need(data_reactives$user_file_sel, 'no file uploaded yet'),
          shiny::need(main_data_reactives$timeseries_data$sf, 'No sf yet')
        )

        file_data <- main_data_reactives$timeseries_data$sf

        # if file is polygons we need to draw polygons, if file are points
        # we need to draw markers
        if (all(sf::st_is(file_data, c('MULTIPOLYGON', 'POLYGON')))) {
          leaflet::leafletProxy('map_daily') %>%
            leaflet::clearGroup('display_daily') %>%
            leaflet::addPolygons(
              data = file_data,
              group = 'display_daily',
              label = ~file_data %>% dplyr::pull(1),
              layerId = ~file_data %>% dplyr::pull(1),
              weight = 2, smoothFactor = 1,
              opacity = 1.0, fill = TRUE, fillOpacity = 0,
              color = 'black',
              highlightOptions = leaflet::highlightOptions(
                color = "#CF000F", weight = 2,
                bringToFront = TRUE,
                fill = TRUE, fillOpacity = 0
              )
            )
        }

        if (all(sf::st_is(file_data, c('MULTIPOINT', 'POINT')))) {
          leaflet::leafletProxy('map_daily') %>%
            leaflet::clearGroup('display_daily') %>%
            leaflet::addCircleMarkers(
              data = file_data,
              group = 'display_daily',
              label = ~file_data %>% dplyr::pull(1),
              layerId = ~file_data %>% dplyr::pull(1)
            )
        }
      } else {
        polygon_object_name <- glue::glue("{tolower(display_daily)}_polygons")

        leaflet::leafletProxy('map_daily') %>%
          leaflet::clearGroup('display_daily') %>%
          leaflet::addPolygons(
            data = rlang::eval_tidy(rlang::sym(polygon_object_name)),
            group = 'display_daily',
            label = ~poly_id, layerId = ~poly_id,
            weight = 1, smoothFactor = 1,
            opacity = 1.0, fill = TRUE, fillOpacity = 0,
            color = '#6C7A89FF',
            highlightOptions = leaflet::highlightOptions(
              color = "#CF000F", weight = 2,
              bringToFront = TRUE,
              fill = TRUE, fillOpacity = 0
            )
          )
      }
    }

  })

  ## observers to change the active tab ####
  shiny::observeEvent(
    eventExpr = input$map_daily_shape_click,
    handlerExpr = {
      # go to series
      shiny::updateTabsetPanel(
        parent_session, 'main_panel_tabset',
        selected = 'series_panel'
      )
    },
    priority = 1000
  )
  shiny::observeEvent(
    eventExpr = input$map_daily_click,
    handlerExpr = {
      # go to series
      shiny::updateTabsetPanel(
        parent_session, 'main_panel_tabset',
        selected = 'series_panel'
      )
    },
    priority = 1000
  )
  shiny::observeEvent(
    eventExpr = input$map_daily_marker_click,
    handlerExpr = {
      # go to series
      shiny::updateTabsetPanel(
        parent_session, 'main_panel_tabset',
        selected = 'series_panel'
      )
    },
    priority = 1000
  )

  ## reactives to return ####
  map_reactives <- shiny::reactiveValues()
  shiny::observe({
    map_reactives$map_daily_shape_click <- input$map_daily_shape_click
    map_reactives$map_daily_marker_click <- input$map_daily_marker_click
    map_reactives$map_daily_click <- input$map_daily_click
    # map_reactives$map_daily_draw_all_features <-
    #   input$map_daily_draw_all_features
  })
  return(map_reactives)

}
