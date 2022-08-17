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
        ns('entorno_polygon'), translate_app('entorno_label', lang_declared),
        choices = list(
          "----- AMBIENTAL -----" = shiny_set_names(list(
            'Provincia_select_entorno' = "provincia" ,
            'Comararca_select_entorno' = "comarca",
            'Embass_select_entorno' = "embass",
            'No_Polygon_label' = "no_polygon"), lang_declared),
          
          "----- TERRITORIO -----" = shiny_set_names(list(
            'Nucleos_select_entorno' = "nucleos"), lang_declared)
        )
   
      ),
      
      # shiny::selectInput(
      #   ns('areas'), translate_app('entorno_hidden', lang_declared),
      #   choices = list(
      #     "----- AREAS -----" = shiny_set_names(list(
      #       "< 25 Ha" = 25,
      #       "25 - 50 Ha" = 50,
      #       "50 - 100 Ha" = 100,
      #       "100 - 250 Ha" = 250,
      #       " > 1000 Ha" = 1000), lang_declared))
      #    
      #     
      #   )
      

      
      # ....... ENTORNO HIDDEN ...........
      # ..................................
      
      
      shinyjs::hidden(
        shiny::div(
          id = ns('file_upload_panel'),
          shiny::selectInput(
            ns('entorno_nucleos'), translate_app('entorno_hidden', lang_declared),
            choices = list(
              "< 10 Ha" = 10,
              "10 - 25 Ha" = 25,
              "25 - 50 Ha" = 50,
              "50 - 100 Ha" = 100,
              "100 - 250 Ha" = 250,
              " > 1000 Ha" = 1000
            )
          )
        )
      )

 

    ) # end of tagList

  })
  
  
  # ..................... OBSERVER HIDDENS  ......................
  # ..............................................................
  
  #      .) 
  
 
  shiny::observe({

    shiny::validate(
      shiny::need(input$entorno_polygon, 'no type')
    )
    display_entorno <- input$entorno_polygon

    if (display_entorno == 'nucleos') {
      shinyjs::show('file_upload_panel')
    } else {
      shinyjs::hide('file_upload_panel')
    }
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
    data_reactives_polygon$entorno_hidden <- input$entorno_nucleos
    # data_reactives_polygon$areas <- input$areas

  })

  # -------------------------- VALORES REACTIVOS ----------------------------
  # -------------------------------------------------------------------------

  #      .) Quiero tener constantemente los valores ACTIVOS
  #      .) FECHA / VARIABLE
  #      .) Son los que me darán la TABLA y la VARIABLE a VISUALIZAR



  return(data_reactives_polygon)
}
