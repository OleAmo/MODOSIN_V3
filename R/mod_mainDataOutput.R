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
    # svg = 'images/hostess_image.svg',
    svg = 'images/forest.svg',
    progress_type = 'fill',
    fill_direction = 'btt'
  ))

  
  
  
  # ..........................  REACTIVE ............................
  # .................................................................
  
  
  
  data_day <- shiny::reactive({
    
    # ............ DATA_DAY .............
    # ...................................
    
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
    # main_data_reactives$timeseries_data <- timeseries_data()
     
  })
  
  return(main_data_reactives)

}
