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
  
  
  data_day <- shiny::reactive({
    
    # shiny::validate(
    #   shiny::need(data_reactives$date_daily, 'No date selected')
    # )
    
    # 2. waiter overlay related to map id
    # waiter_map <- waiter::Waiter$new(
    #   id = 'overlay_div',
    #   html = shiny::tagList(
    #     hostess_raster$get_loader(),
    #     shiny::h3(translate_app("progress_raster", lang())),
    #     shiny::p(translate_app("progress_detail_raster", lang()))
    #   ),
    #   color = '#E8EAEB'
    # )
    
    # waiter_map$show()
    # # waiter_map$update(
    # #   html = shiny::tagList(
    # #     hostess_raster$get_loader(
    # #       svg = 'images/hostess_image.svg',
    # #       progress_type = 'fill',
    # #       fill_direction = 'btt'
    # #     ),
    # #     shiny::h3(translate_app("progress_raster", lang())),
    # #     shiny::p(translate_app("progress_detail_raster", lang()))
    # #   )
    # # )
    # hostess_raster$start()
    # on.exit(hostess_raster$close(), add = TRUE)
    # on.exit(waiter_map$hide(), add = TRUE)
    
    # date
    # date_sel <- as.character(data_reactives$date_daily)
    
    # raster_res
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



  ## reactives to return ####
  main_data_reactives <- shiny::reactiveValues()
  shiny::observe({
    main_data_reactives$data_day <- data_day()
    # main_data_reactives$timeseries_data <- timeseries_data()
  })
  return(main_data_reactives)

}
