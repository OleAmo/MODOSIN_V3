# polygons and nfi plots objects


# .......... COMARCAS / PROVINCIAS ...........
# ............................................

#      .) Creamos POLIGONOS estandards
#      .) Creamos POLÍONGOS simplificados
#      .) Usamos el ST_READ para NO CONECTAR con la BBDD


comarcas <- st_read("SHAPES/COMARCAS/comarcas.shp")
provincias <- st_read("SHAPES/PROVINCIAS/provincias.shp")

provincias_simplify  <- st_read("SHAPES/PROVINCIAS/provincias.shp") %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform(4326) 
comarcas_simplify <- st_read("SHAPES/COMARCAS/comarcas.shp") %>%
  rmapshaper::ms_simplify(0.01) %>%
  sf::st_transform(4326)




# st_read("SHAPES/COMARCAS/comarcas.shp") %>%
#   {.} -> comarcas
# st_read("SHAPES/PROVINCIAS/provincias.shp")%>%
#   {.} -> provincias

# st_read("SHAPES/PROVINCIAS/provincias.shp") %>%
#   rmapshaper::ms_simplify(0.01) %>%
#   sf::st_transform(4326) %>%
#   {.} -> provincias_simplify 
# st_read("SHAPES/COMARCAS/comarcas.shp") %>%
#   rmapshaper::ms_simplify(0.01) %>%
#   sf::st_transform(4326) %>%
#   {.} -> comarcas_simplify


# usethis::use_data(comarcas,internal = TRUE, overwrite = TRUE)
# usethis::use_data(provincias,internal = TRUE, overwrite = TRUE)
# usethis::use_data(provincias_simplify,internal = TRUE, overwrite = TRUE)
# usethis::use_data(comarcas_simplify,internal = TRUE, overwrite = TRUE)

 
 
 
 
 