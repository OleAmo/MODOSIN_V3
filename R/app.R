#' function to launch the catdrought app
#'
#' @importFrom magrittr %>%
#'
#' @export
modosin_app <- function() {
  library(leaflet)
  source('data-raw/translations.R')
  
  ### DB access ################################################################
  modosindb <- lfcdata::modosin()

  ## JS code needed ############################################################
  keep_alive_script <- shiny::HTML(
    " var socket_timeout_interval;
      var n = 0;

      $(document).on('shiny:connected', function(event) {
        socket_timeout_interval = setInterval(function() {
          Shiny.onInputChange('alive_count', n++)
        }, 10000);
      });

      $(document).on('shiny:disconnected', function(event) {
        clearInterval(socket_timeout_interval)
      });"
        )



  ### Language input ###########################################################
  shiny::addResourcePath(
    'images', system.file('resources', 'images', package = 'modosinApp')
  )
  lang_choices <- c('cat', 'spa', 'eng')
  lang_flags <- c(
    glue::glue("<img class='flag-image' src='images/cat.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/spa.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/eng.png' width=20px><div class='flag-lang'>%s</div></img>")
  )

  # ++++++++++++++++++++++++++++++++ //  UI // ++++++++++++++++++++++++++++++++
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## UI ####
  ui <- shiny::tagList(

    # ............... INICIALIZAR ................
    # ............................................

    #       .) SHINYJS / Waiter / Hostess
    #       .) Head:
    #             .) JS files
    #             .) CSS files


    # shinyjs
    shinyjs::useShinyjs(),
    # waiter
    waiter::use_waiter(),
    waiter::use_hostess(),

    # css
    shiny::tags$head(
      # js script,
      shiny::tags$script(keep_alive_script),
      # custom css
      shiny::includeCSS(system.file('resources', 'modosin.css', package = 'modosinApp')),
      # corporative image css
      shiny::includeCSS(system.file('resources', 'corp_image.css', package = 'modosinApp'))
    ),


    # ................. NAVBAR ...................
    # ............................................

    #       .) Titulo
    #       .) 2 Pestañas   = tabPanel
    #                 .) Explora
    #                 .) Especificación Técnica
    #       .) Dropdown Lenguas  =pickerInput


    navbarPageWithInputs(
      # opts
      title = 'Modosin App',
      id = 'nav', collapsible = TRUE,

      # navbar with inputs (helpers.R) accepts an input argument, we use it for the lang
      # selector

      # ............ Dropdown Lenguas ..............
      # ............................................

      inputs = shinyWidgets::pickerInput(
        'lang', NULL,
        choices = lang_choices,
        selected = 'cat',
        width = '100px',
        choicesOpt = list(
          content = c(
            sprintf(lang_flags[1], lang_choices[1]),
            sprintf(lang_flags[2], lang_choices[2]),
            sprintf(lang_flags[3], lang_choices[3])
          )
        )
      ),

      # ............ Pestaña EXPLORA ...............
      # ............................................

      #       .) Tiene dos parte:
      #       .) MENÚ Izq = sidebarPanel
      #       .) MAPA     = mainPanel BLUE



      # navbarPage contents
      shiny::tabPanel(
        title = mod_tab_translateOutput('main_tab_translation'),
        shiny::sidebarLayout(
          ## options
          position = 'left', fluid = TRUE,

          # ............ MENÚ IZQUIERDA ................
          # ............................................

          #       .) 2 Pestañas
          #                 .) DATOS
          #                 .) GUARDAR

          sidebarPanel = shiny::sidebarPanel(
            width = 4,
            # this is gonna be a tabsetPanel, for data selection, save and help.
            # tabset panel
            shiny::tabsetPanel(
                  id = 'sidebar_tabset', type = 'pills',

                  # ......... Pestaña DATOS ........
                  # ................................
                  # data panel
                  shiny::tabPanel(
                    title = mod_tab_translateOutput('data_translation'),
                    value = 'data_inputs_panel',
                    modosin_dataInput('modosin_DATA')
                  ), # end of data panel

                  # ........ Pestaña GUARDAR .......
                  # ................................
                  # save panel
                  shiny::tabPanel(
                    title = mod_tab_translateOutput('save_translation'),
                    value = 'save_panel',
                    # mod_saveOutput('mod_saveOutput')
                  )
            )
          ), # end of sidebarPanel

          # ............. MAPA DERECHA .................
          # ............................................

          #       .) 2 Pestañas
          #                 .) MAPA
          #                 .) SERIES TEMPORALS

          mainPanel = shiny::mainPanel(
            width = 8,
            shiny::div(
              id = 'overlay_div',
              shiny::tabsetPanel(
                id = 'main_panel_tabset', type = 'pills',

                # ......... MAPA .........
                # ........................
                shiny::tabPanel(
                  # 'map',
                  title = mod_tab_translateOutput('map_translation'),
                  value = 'map_panel',
                  mod_mapOutput('mod_mapOutput')
                ),

                # .... SERIE TEMPORAL ....
                # ........................
                shiny::tabPanel(
                  title = mod_tab_translateOutput('series_tab_translation'),
                  value = 'series_panel',
                  # mod_tsOutput('mod_tsOutput')
                )
              )
            )
          )
        )
      ),

      # .... Pestaña ESPECIFICACIONES TECNICAS .....
      # ............................................

      #       .) Tiene UNA parte:
      #       .) Ecplicacion de la APP

      shiny::tabPanel(
        title = mod_tab_translateOutput('tech_specs_translation'),
        value = 'tech_spec_panel',
        # mod_techSpecsOutput('mod_techSpecsOutput')
      )


    ) # end of navbar
  ) # end of UI


  # ++++++++++++++++++++++++++++++ // SERVER // +++++++++++++++++++++++++++++++
  # +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## SERVER ####
  server <- function(input, output, session) {

    # .......... LANGUAGE REACTIVE ...............
    # ............................................

    #       .) Detecta el cambio de Lenguaje
    #       .) Lo Módulos depende este

    # lang reactive ####
    lang <- shiny::reactive({
      input$lang
    })

    # ............... ACTIVAR FUNCIONES DE MODULOS .................
    # ..............................................................

    # ....... DATA INPUTS ..........
    # ..............................

    #       .) modosin_data  = f(x) a LLAMAR ( principal del Módulo )
    #       .) modosin_DATA  = Es el ID usaremos a la UI cuando llamemos la f(x)
    #       .) modosindb     = DDBB del LfcData
    #       .) Lang          = lenguaje del REACTIVE

    data_reactives <- shiny::callModule(
      modosin_data ,'modosin_DATA', modosindb, lang
    )
    # map
    map_reactives <- shiny::callModule(
      mod_map, 'mod_mapOutput',
      data_reactives, main_data_reactives,
      session, lang
    )
    # # main data
    main_data_reactives <- shiny::callModule(
      mod_mainData, 'mod_mainDataOutput',
      data_reactives, map_reactives,
      modosindb, lang
    )
    # # ts
    # timseries_reactives <- shiny::callModule(
    #   mod_ts, 'mod_tsOutput',
    #   data_reactives, main_data_reactives,
    #   lang
    # )
    # # save
    # shiny::callModule(
    #   mod_save, 'mod_saveOutput',
    #   main_data_reactives, data_reactives,
    #   lang
    # )
    # # technical specifications module
    # shiny::callModule(
    #   mod_techSpecs, 'mod_techSpecsOutput',
    #   lang
    # )
    #
    # ## tab translations ####
    shiny::callModule(
      mod_tab_translate, 'main_tab_translation',
      'main_tab_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'data_translation',
      'data_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'map_translation',
      'map_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'series_tab_translation',
      'series_tab_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'save_translation',
      'save_translation', lang
    )
    shiny::callModule(
      mod_tab_translate, 'tech_specs_translation',
      'tech_specs_translation', lang
    )



  } # end of server function

  # Run the application
  modosinApp <- shiny::shinyApp(
    ui = ui, server = server
  )

  # shiny::runApp(nfi_app)
  return(modosinApp)

}
