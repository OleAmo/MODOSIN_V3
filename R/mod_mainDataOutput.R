#' @title mod_mainDataOutput and mod_mainData
#'
#' @description Shiny module to get the data
#'
#' @param id
#'
#' @export
mod_mainDataOutput <- function(id) {
  ns <- shiny::NS(id)
  return()
}

#' @title mod_mainData server function
#'
#' @details mod_mainData always return the data in the 3043 projection
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data_reactives,map_reactives reactives from modules
#' @param catdroughtdb object to access the meteoland db
#' @param lang lang selected
#'
#' @importFrom dplyr n
#'
#' @export
#'
#' @rdname mod_mainDataOuput
mod_mainData <- function(
  input, output, session,
  data_reactives, map_reactives,
  modosindb, lang
) {
  

  # ....... WAITER / HOSTESS ..........
  # ...................................
  
  #       .) https://shiny.john-coene.com/waiter/
  #       .) Paquete de R que permite crear LOADING SCREENS
  
  #       .) INICIALIZAMOS:
  #              .) Fuera de REACTIVE careamos OBJECTO con la classe:
  #              .) WAITER::HOSTESS$new
  #       .) SEGUNDO
  #              .) SET_LOADER = image SVG, tipo progress y fill direction
  
    
  

  hostess_plots <- waiter::Hostess$new(infinite = TRUE)
  hostess_plots$set_loader(waiter::hostess_loader(
    svg = 'images/hostess_image.svg',
    progress_type = 'fill',
    fill_direction = 'btt'
  ))

  
  # **************************************************************************************
  # ------------------------------     REACTIVE   ----------------------------------------
  # **************************************************************************************
  
 
  
  # ***************************************************
  # ------------------    DATA DAY   ------------------
  # ***************************************************
  
  library(data.table)
  
  data_day <- shiny::reactive({
    
    
    #       .) Devolvemos el SF de todos Plots / Todas fechas
    #       .) Creamos el LOADING SCREEN minentra cargamos PLOTS
    
    #       .) IMPORTANTE:
    #       .) Antes de todo VALIDAMOS FECHA
    #       .) Sin esto la app se bloquea
    
    
    shiny::validate(
      shiny::need(data_reactives$fecha_reactive, 'No date selected')
    )
    
    # ....... WAITER / HOSTESS ..........
    # ...................................
    
    #       .) TERCERO:
    #       .) Definir LUGAR de aparición = ID ( en APP.R / mainPanel)
    #       .) Definir => Get_Loader() definido anteriormente + HTML H3 + P
    
    
    waiter_map <- waiter::Waiter$new(
      id = 'overlay_div',
      html = shiny::tagList(
        hostess_plots$get_loader(),
        shiny::h3(translate_app("progress_plots", lang())),
        shiny::p(translate_app("progress_detail_plots", lang()))
    
      ),
      color = '#E8EAEB'
    )
    
    #       .) CUARTO: Show MAP + Star HOSTESS
    #       .) QUINTO: Definir EXIT Hostess / Map 
    
    waiter_map$show()
    hostess_plots$start()
    on.exit(hostess_plots$close(), add = TRUE)
    on.exit(waiter_map$hide(), add = TRUE)
    
     
    
    # ........... GET DATA ..............
    # ...................................
    
    #       .) Usamos MODOSIN DB (Definido en APP.R)
    #       .) Llamamos al Método GET DATA
    #       .) Creamos el SF data_day
    #       .) Es el SF de TODOS los PLOTS / TODAS las fechas
    #       .) Después ya lo filtraremos
    

    data_day <- modosindb$get_data()
    return(data_day)
  })

  
  
  
  
  # ***************************************************
  # -----------------    TIME SERIE   -----------------
  # ***************************************************
  
  
  
  time_serie <- shiny::reactive({
    
    
    shiny::validate(
      # shiny::need(data_reactives$variable_reactive, 'No variable selected')
      shiny::need(main_data_reactives$data_day, 'No data_day selected')
    )
    
    
    
    # ............. CLICK PLOT ..............
    # ........................................
    
    #      .) USAMOS = "map_reactives$map_daily_marker_click"
    #      .) DECLARADO en los EVENTOS en MOD_MAPOUTPUT.R
    
    #      .) obtengo LATITUD y LONGITUD
    #      .) Usando $lat / $long
    
    #      .) OBTENGO la Gemoetria CLICKADA
    #      .) Después la usaré per encontrar el ID clickado
                           
    print(map_reactives$map_daily_marker_click)
    
    clik_lat <- map_reactives$map_daily_marker_click$lat
    clik_long <- map_reactives$map_daily_marker_click$lng
    

    
    #      .) TRANSFORMAR Long + Lat => a POINT GEMETRY
    #      .) Una vez tenga la GEOMETRY POINT encontraé el PLOT_ID
    
    #      .) Con el PLOT ID de Plot que he CLICADO
    #      .) Podré FILTRAR todo el DATA DAY
    #      .) Y por lo tanto tener el TIME SERIE GRAFICO de SOLO el PLOT_ID Seleccionado
    
    
    if(!is.null(clik_lat) & !is.null(clik_long)) {
      DT <- data.table(
        place=c("click_pint"),
        longitude=c(clik_lat),
        latitude=c(clik_long))
      
      DT_sf = st_as_sf(DT, coords = c("longitude", "latitude"),
                       crs = 4326, agr = "constant")
      
      click_geom <- DT_sf$geometry
      
     }
    
    
    
    # ......... INICIALIZAR DATOS ............
    # ........................................
    
    #      .) FECHA / VARIABLE  / data_day_plot
    #      .) Obtenidos de los COMBOS 
    
    variable <- data_reactives$variable_reactive
    fecha <- data_reactives$fecha_reactive
    
    #      .) DATA_DAY
    #             .) Son los PLOTS de TODO el año
    #             .) ATENCIÓN:
    #                   .) Solo usamosm PLOT_ID = 171973
    #                   .) Después lo relacionaremos con el CLICK ala PARCELA 
    
    data_day_plot_inicial <- main_data_reactives$data_day
    # clicked_ID <- data_day_plot_inicial %>% 
    #                 dplyr::filter(geom == click_geom) %>%
    #                  dplyr::select(plot_id)
    # 
    # print(clicked_ID)
    
    
    
    data_day_plot <- data_day_plot_inicial  %>% dplyr::filter(plot_id == 171973)
    
    #      .) NUM_i
    #             .) Número de la columnas de la variable
    #             .) Lo usaremos par obtener TODAS los valores de UNA VARIABLE
    
    #      .) FECHA INICIAL = 1r dia de datos
    #             .) Lo usaremos para indicar el inicio de la fechas de gráficos

    #      .) VALUE DATE = valor de la variable en la fecha concreta
    #             .) Lo usaremos en LABEL EVENT
    
    num_i <- as.numeric(match(variable,names(data_day_plot)))
    fecha_inicial <- data_day_plot$date[1]
    value_date <- data_day_plot %>%
      dplyr::filter(date == fecha) %>%
      .[num_i] %>%
      .[[1]] %>%
      round(., digits = 4)

    #      .) LABEL EVENT
    #             .) Texto que saldrà en el grafico
    #             .) Indica fecha seleccionada + valor de variable escogido
    
    #      .) UNITS
    #             .) Son la DESCRIPCIÓN de la VARIABLE y las UNIDADES
    #             .) Lo usaremos al AXIS Y
    
    #      .) LABEL AXIS
    #             .) Texto que saldrà en la coordenada Y
    #             .) Indica fecha seleccionada + valor de variable escogido
    

    label_event <- paste(fecha," = ",variable," (",value_date,") ")
    units <- translate_app(variable, "eng")
    label_axis <- paste(toupper(variable)," [ ",units," ]")
    
    # ..................... GRAPHS ......................
    # ...................................................
    
    #       .) Creo FORMATO TS (Time Serie)
    #       .) Aplico EDICION con DYGRAPHS
    
    #             .) MAIN = Título
    #             .) AXIS = edito las Y
    #             .) OPTIONS = edito gráfico
    #             .) SERIE = texto del menú que sale en mover el mouse
    #             .) EVENT = en la fecha concreta escribir texto
    #             .) SHADING = Sombreado entre fecha y fecha
    #             .) RANGE SELECTOR = para hacer zoom al gráfico


    data_day_graph <- ts(data_day_plot[num_i][[1]], frequency = 1, start = as.Date(fecha_inicial))
    
    res <- data_day_graph %>%
              dygraphs::dygraph(. , main = paste(toupper(variable)," (Anual)")) %>%
              dygraphs::dyAxis("y", label = label_axis )%>%
              dygraphs::dyOptions(fillGraph = TRUE, fillAlpha = 0.4) %>%
              # dygraphs::dySeries(label = variable) %>%
              dygraphs::dyEvent(fecha, label_event, labelLoc = "bottom") %>%
              dygraphs::dyShading(from = as.Date(fecha)-7, to = as.Date(fecha)+7, color = "#f2a7a7") %>%
              dygraphs:: dyRangeSelector()

    return(res)
    
    

  })
  
  
  
  

  # map_shape_sf_builder <- shiny::reactive({
  #   shiny::validate(
  #     shiny::need(map_reactives$map_daily_shape_click, 'no map click')
  #   )
  # 
  #   clicked_poly <- map_reactives$map_daily_shape_click
  #   polygon_object <- rlang::eval_tidy(
  #     rlang::sym(glue::glue("{tolower(data_reactives$display_daily)}_polygons"))
  #   ) %>%
  #     dplyr::filter(
  #       poly_id == clicked_poly$id
  #     )
  # 
  #   return(polygon_object)
  # })
  # 
  # nfi_plots_sf_builder <- shiny::reactive({
  #   shiny::validate(
  #     shiny::need(map_reactives$map_daily_marker_click, 'no map click')
  #   )
  # 
  #   clicked_marker <- map_reactives$map_daily_marker_click
  # 
  #   point_sel <- tibble::tibble(
  #     point_id = clicked_marker$id,
  #     long = clicked_marker$lng,
  #     lat = clicked_marker$lat
  #   ) %>%
  #     sf::st_as_sf(
  #       coords = c('long', 'lat'),
  #       crs = 4326
  #     )
  # 
  #   return(point_sel)
  # }



  # ..................... DEVOLVER REACTIVOS  ....................
  # ..............................................................
  
  #      .) Creamos MAIN DATA REACTIVE
  #      .) ASSIGNAMOS los OBERSVERS
  
  # .... MAIN DATA REACTIVE ......
  # ..............................
  
  #      .) Es la variable que ALMACENA TODOS los REACTVES
  #      .) Cada reactive se ALMACENA con un $
  #      .) Devolvemos DATA_DAY
  
  main_data_reactives <- shiny::reactiveValues()
  
  shiny::observe({
   
     main_data_reactives$data_day <- data_day()
     main_data_reactives$timeserie <-  time_serie()
     
  })
  
  return(main_data_reactives)

}
