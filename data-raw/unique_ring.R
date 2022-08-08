library(tidyverse)
source('data-raw/polygon_objects_creation.R')
source('data-raw/translations.R')
source('data-raw/palette_builder.R')

# use_data internal for all
usethis::use_data( 
  app_translations, 
  comarcas, provincias, 
  provincias_simplify, comarcas_simplify, 
  nternal = TRUE, overwrite = TRUE
)
