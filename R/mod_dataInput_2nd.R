#' @title modosin_dataInput  and modosin_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#'
#' @export
modosin_dataInput_2nd <- function(id) {
  # ns
  ns <- shiny::NS(id)

  # UI ####
  shiny::tagList(
    shiny::br(),
    shiny::uiOutput(
      ns('mod_data_container_2nd')
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
modosin_data_2nd <- function(
  input, output, session,
  modosindb, lang
) {

  # renderUI ####
  output$mod_data_container_2nd <- shiny::renderUI({
    
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

    # ....... SELECCION VARIABLE ........
    # ...................................
    
    #       .) Variables según MIQUEL
    #               .) soil moisture: Theta, Psi, REW
    #               .) soil moisture: Theta, Psi, REW
    #               .) climate: PET
    #               .) evaporative surface: LAI
    #               .) water balance: Infiltration, RunOff, DeepDrainage, Esoil, Eplant
    #               .) drought stress: DDS
    


    soil_moisture_vars <- c("Theta", "Psi", "REW") %>%
      magrittr::set_names(translate_app(., lang_declared))
    climate_vars <- c("PET", "Precipitation") %>%
      magrittr::set_names(translate_app(., lang_declared))
    evap_surface_vars <- c('LAI') %>%
      magrittr::set_names(translate_app(., lang_declared))
    fwb_vars <- c("Interception", "Infiltration", 'Runoff', 'DeepDrainage', 'Esoil', 'Eplant') %>%
      magrittr::set_names(translate_app(., lang_declared))
    drought_stress_vars <- c("DDS", "LMFC") %>%
      magrittr::set_names(translate_app(., lang_declared))

    shiny::tagList(
      
      shiny::selectInput(
        ns('variable_2nd'), translate_app('var_daily_label_2nd', lang_declared),
        choices = shiny_set_names(list(
          'Soil moisture' = soil_moisture_vars,
          'Climate' = climate_vars,
          'Evaporative surface' = evap_surface_vars,
          'Water balance' = fwb_vars,
          'Drought stress' = drought_stress_vars), lang_declared)
      ),

      # ........ SELECCION FECHA ..........
      # ...................................

      shiny::dateInput(
        ns("fecha_2nd"), translate_app('date_daily_label_2nd', lang_declared),
        value = "2021-05-11",
        format = "yyyy/mm/dd",
        max = "2022-03-24",
        min =  "2021-03-25"
      ),
      
      # ....... SELECCION ENTORNO .........
      # ...................................
      
      shiny::selectInput(
        ns('entorno_2nd'), translate_app('entorno_label_2nd', lang_declared),
        choices = shiny_set_names(list(
          'Provincia_select_entorno_2nd' = "provincia" ,
          'Comararca_select_entorno_2nd' = "comarca",
          'No_Polygon_label_2nd' = "no_polygon"), lang_declared)
            
      ),
      
  
      # ......... RADIO BUTTONS ...........
      # ...................................
      
      
    
      shiny::radioButtons(
        ns("size_2nd"),translate_app("size_label_2nd", lang_declared),
        shiny_set_names(c("Constante_label" = "const", "funcio_label" = "var"),lang_declared)),
        

      shiny:: radioButtons(
        ns("legend_2nd"),translate_app("type_legend_label_2nd", lang_declared),
        shiny_set_names(c("Continua_label" = "conti", "Quantil_label" = "quant"),lang_declared)),
      
      # ......... ACCION BUTTON ...........
      # ...................................
      
      actionButton(ns("boto_2nd"), "Proyectar"),
      actionButton(ns("boto_save_2nd"), "Guardar"),

      # shiny::dateInput(
      #   ns('date_daily'), translate_app('date_daily_label', lang_declared),
      #   value = date_daily_choices[length(date_daily_choices)],
      #   min = date_daily_choices[1],
      #   max = date_daily_choices[length(date_daily_choices)],
      #   weekstart = 1, language = dates_lang
      # ),

      # # polygon sel
      # shiny::selectInput(
      #   ns('display_daily'), translate_app('display_daily_label', lang_declared),
      #   choices = c(
      #     'none', "Watersheds", "Counties", "Municipalities", "IFN plots", "file"
      #   ) %>%
      #     magrittr::set_names(translate_app(., lang_declared)),
      #   selected = 'none'
      # ),

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

  ## observers ####
  # observer to show the file upload panel if needed
  # shiny::observe({
  #
  #   shiny::validate(
  #     shiny::need(input$display_daily, 'no type')
  #   )
  #   display_daily <- input$display_daily
  #
  #   if (display_daily == 'file') {
  #     shinyjs::show('file_upload_panel')
  #   } else {
  #     shinyjs::hide('file_upload_panel')
  #   }
  # })
  ## returning inputs ####
  # reactive values to return and use in other modules







  # ..................... DEVOLVER REACTIVOS  ....................
  # ..............................................................

  #      .) Creamos DATA REACTIVE
  #      .) ASSIGNAMOS los OBERSVERS

  # ...... DATA REACTIVE .........
  # ..............................

  #      .) Es la variable que ALMACENA TODOS los REACTVES
  #      .) Cada reactive se ALMACENA con un $


  data_reactives_2nd <- shiny::reactiveValues()

  # ...... DATA OBSERVE ..........
  # ..............................

  #      .) Creamos dentro de DATA_REACTIVE
  #      .) Todos los diferentes apartados con $

  shiny::observe({

    data_reactives_2nd$fecha_reactive  <- input$fecha_2nd
    data_reactives_2nd$fecha_reactive_2 <- input$fecha_2_2nd
    data_reactives_2nd$variable_reactive<- input$variable_2nd
    data_reactives_2nd$size_reactive <- input$size_2nd
    data_reactives_2nd$legend_reactive <- input$legend_2nd
    data_reactives_2nd$boto_reactive <- input$boto_2nd
    data_reactives_2nd$boto_save_reactive <- input$boto_save_2nd
    data_reactives_2nd$entorno_reactive <- input$entorno_2nd


  })

  # -------------------------- VALORES REACTIVOS ----------------------------
  # -------------------------------------------------------------------------

  #      .) Quiero tener constantemente 2 valores ACTIVOS
  #      .) FECHA / VARIABLE
  #      .) Son los que me darán la TABLA y la VARIABLE a VISUALIZAR




  return(data_reactives_2nd)
}
