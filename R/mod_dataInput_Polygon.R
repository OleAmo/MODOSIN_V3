#' @title modosin_dataInput  and modosin_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
modosin_dataInput_polygon <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(
      ns('mod_data_container_polygon')
    )
  )
}

#' modosin_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param lang lang reactive
#'
#' @export
modosin_data_polygon <- function(
  input, output, session,
  modosindb, lang
) {

  # renderUI ####
  output$mod_data_container_polygon <- shiny::renderUI({
    
    # ......... INICIALIZAR .............
    # ...................................
    
    #       .) NS = IDs únicos
    #       .) LANG = F(x) definida en APP.R
    #       .) DATES_LANG = Cambio de nomenclatura de lengua
    
    ns <- session$ns
    lang_declared <- lang()
    dates_lang <- switch(
      lang_declared,
      'cat' = 'ca',
      'spa' = 'es',
      'eng' = 'en'
    )

    shiny::tagList(
      
      
      # ....... SELECCION ENTORNO .........
      # ...................................
      
      shiny::selectInput(
        ns('entorno_polygon'), translate_app('entorno_label_polygon', lang_declared),
        choices = shiny_set_names(list(
          'Provincia_select_entorno_polygon' = "provincia" ,
          'Comararca_select_entorno_polygon' = "comarca",
          'No_Polygon_label_polygon' = "no_polygon"), lang_declared)
            
      ),
      

      # shinyjs::hidden(
      #   shiny::div(
      #     id = ns('file_upload_panel'),
      #     shiny::fluidRow(
      #       shiny::column(
      #         6,
      #         shiny::fileInput(
      #           ns('user_file_sel'),
      #           translate_app('user_file_sel_label', lang()),
      #           accept = c('zip', 'gpkg'),
      #           buttonLabel = translate_app(
      #             'user_file_sel_buttonLabel', lang()
      #           ),
      #           placeholder = translate_app(
      #             'user_file_sel_placeholder', lang()
      #           )
      #         )
      #       ),
      #       shiny::column(
      #         6,
      #         shiny::p(translate_app('file_text', lang()))
      #       )
      #     )
      #   )
      # ) # end of hidden file selector



    ) # end of tagList

  })


  # ..................... DEVOLVER REACTIVOS  ....................
  # ..............................................................

  #      .) Creamos DATA REACTIVE
  #      .) ASSIGNAMOS los OBERSVERS

  # ...... DATA REACTIVE .........
  # ..............................

  #      .) Es la variable que ALMACENA TODOS los REACTVES
  #      .) Cada reactive se ALMACENA con un $


  data_reactives_polygon <- shiny::reactiveValues()

  # ...... DATA OBSERVE ..........
  # ..............................

  #      .) Creamos dentro de DATA_REACTIVE
  #      .) Todos los diferentes apartados con $

  shiny::observe({

    data_reactives_polygon$entorno_reactive <- input$entorno_polygon

  })

  # -------------------------- VALORES REACTIVOS ----------------------------
  # -------------------------------------------------------------------------

  #      .) Quiero tener constantemente los valores ACTIVOS
  #      .) FECHA / VARIABLE
  #      .) Son los que me darán la TABLA y la VARIABLE a VISUALIZAR



  return(data_reactives_polygon)
}
