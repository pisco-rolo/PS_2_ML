# A| Definición de la función ---------------------------------------------
crear_variables_geograficas <- function(
      dataset., 
      dataset_bogota. = dataset_bogota,
      dataset_validacion. = dataset_validacion, 
      dataset_tm. = dataset_tm,
      dataset_ciclovia. = dataset_ciclovia, 
      radio. = radio, 
      nombreArchivo = 'datos_geograficos.rds',
      nombreGrafica = 'mapa_variables.png'
) {
  # 1| Extraer información --------------------------------------------------
  # Convertimos la base de datos original en un 'sf', de forma tal que sea más
  # sencillo manipular las variables de longitud y latitud.
  dataset. <- st_as_sf(x = dataset., coords = c('num_longitud', 'num_latitud'))
  st_crs(dataset.) <- 4326
  
  # 1.1| Universidades ------------------------------------------------------
  # Estimamos la distancia (en metros) de la Universidad más cercana.
  data_osm <- opq(bbox = getbb('Bogotá, Distrito Capital')) |> 
    add_osm_feature(key = 'amenity', value = 'university')
  
  sf_universidades <- osmdata_sf(data_osm)
  
  geom_universidades <- sf_universidades$osm_polygons |> 
    select(c('osm_id', 'name'))
  
  centroides <- gCentroid(as(geom_universidades$geometry, "Spatial"), byid = TRUE)
  centroides <- st_as_sf(centroides, coords = c('x', 'y'))
  
  matrix_distance <- st_distance(x = dataset., y = centroides)
  vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)
  
  # TODO. ¿En qué unidades está medida la distancia? Parece estar medido en 
  # metros.
  dataset. <- dataset. |> mutate(num_distancia_universidades = vector_distance)
  
  # 1.2| Zonas sociales -----------------------------------------------------
  # Conteo del número de restaurantes, bares, cafés, y pubs en un área.
  data_osm <- opq(bbox = getbb('Bogotá, Distrito Capital')) |> 
    add_osm_features(features = list('amenity' = 'bar',
                                     'amenity' = 'restaurant',
                                     'amenity' = 'cafe',
                                     'amenity' = 'pub'))
  
  sf_zonasSociales <- osmdata_sf(data_osm)
  
  # Eliminamos observaciones repetidas, por si un restaurante cuenta como bar o
  # viceversa, y así no contabilizamos dos veces.
  geom_zonasSociales <- sf_zonasSociales$osm_polygons |> 
    select(c('osm_id', 'name')) |> 
    drop_na() |> 
    distinct()
  
  centroides <- gCentroid(as(geom_zonasSociales$geometry, "Spatial"), byid = TRUE)
  centroides <- st_as_sf(centroides, coords = c('x', 'y'))
  
  matrix_distance <- st_distance(x = dataset., y = centroides)
  
  # Contamos los puntos de zonas sociales que se encuentran en un radio 
  # determinado alrededor de las casas.
  # TODO. Validar la predicción con diferentes rangos a la redonda.
  data_count <- rowSums(matrix_distance <= radio.)
  dataset. <- dataset. |> mutate(num_numero_ocio = data_count)
  
  # 1.3| Distancia de calles principales ------------------------------------
  # TODO. Incluir la forma cuadrática por el costo de vivir demasiado cerca de la 
  # principal.
  data_osm <- opq(bbox = getbb('Bogotá, Distrito Capital')) |> 
    add_osm_feature(key = 'highway', value = 'primary')
  
  # Desafortunadamente, imprime muchas calles. Sospechamos que no todas ellas
  # son principales, pero por ahora lo dejamos así.
  sf_calles <- osmdata_sf(data_osm)
  geom_calles <- sf_calles$osm_lines |>
    select(c('osm_id', 'name'))
  
  matrix_distance <- st_distance(x = dataset., y = geom_calles)
  vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)
  
  dataset. <- dataset. |> mutate(num_distancia_calles = vector_distance)
  
  # 1.4| Transmilenio -------------------------------------------------------
  # - Segmentar la ciudad en zonas residenciales y zonas empresariales.
  matrix_close <- st_nearest_feature(dataset., dataset_validacion.) 
  vector_close <- dataset_validacion.[matrix_close, ]
  vector_close <- st_drop_geometry(vector_close)
  
  dataset. <- dataset. |> bind_cols(vector_close)
  
  # - Distancia a la estación de TM más cercana.
  puntos_tm <- dataset_tm.$geometry # Para la gráfica final.
  puntos_tm <- st_as_sf(x = puntos_tm, crs = 4326)
  
  matrix_distance <- st_distance(x = dataset., y = dataset_tm.)
  vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)
  
  dataset. <- dataset. |> mutate(num_distancia_tm = vector_distance)
  
  # 1.5| Centros comerciales ------------------------------------------------
  # Distancia del centro comercial más cercano.
  # Nota. Se toma aquellos centros comerciales que tienen un polígono, pues
  # eso implica que son de un tamaño lo suficientemente grande como ser un centro
  # comercial. Al tomar los puntos que ubican los 'centros comerciales' aparecen
  # demasiados, y ello puede ser un error de clasificación.
  data_osm <- opq(bbox = getbb('Bogotá, Distrito Capital')) |> 
    add_osm_feature(key = 'shop', value = 'mall')
  
  sf_mall <- osmdata_sf(data_osm)
  geom_mall <- sf_mall$osm_polygons |> select(c('osm_id', 'name'))
  
  centroides <- gCentroid(as(geom_mall$geometry, "Spatial"), byid = TRUE)
  centroides <- st_as_sf(centroides, coords = c('x', 'y'))
  centroides_mall <- centroides # Para la gráfica final.
  
  matrix_distance <- st_distance(x = dataset., y = centroides)
  vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)
  
  dataset. <- dataset. |> mutate(num_distancia_mall = vector_distance)
  
  # 1.6| Parques ------------------------------------------------------------
  # Distancia del parque más cercano.
  data_osm <- opq(bbox = getbb('Bogotá, Distrito Capital')) |> 
    add_osm_feature(key = 'leisure', value = 'park')
  
  sf_parques <- osmdata_sf(data_osm)
  geom_parques <- sf_parques$osm_polygons |> select(c('osm_id', 'name'))
  centroides <- gCentroid(as(geom_parques$geometry, "Spatial"), byid = TRUE)
  centroides <- st_as_sf(centroides, coords = c('x', 'y'))
  
  matrix_distance <- st_distance(x = dataset., y = centroides)
  vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)
  
  dataset. <- dataset. |> mutate(num_distancia_parque = vector_distance)
  
  # Área del parque más cercano.
  posicion <- apply(X = matrix_distance, MARGIN = 1, FUN = function(x) {
    which(min(x) == x)
  })
  areas <- st_area(geom_parques)
  dataset. <- dataset. |> mutate(num_area_parque = as.numeric(areas[posicion]))
  
  # 1.7| CAI ----------------------------------------------------------------
  data_osm <-  opq(bbox = getbb('Bogotá, Distrito Capital')) |> 
    add_osm_feature(key = 'amenity', value = 'police')
  
  sf_cai <- osmdata_sf(data_osm)
  geom_cai <- sf_cai$osm_point
  
  matrix_distance <- st_distance(x = dataset., y = geom_cai)
  vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)
  
  dataset. <- dataset. |> mutate(num_distancia_cai = vector_distance)
  
  # 1.8| Ciclovía -----------------------------------------------------------
  # Distancia de la ciclovía más cercana.
  puntos_ciclovia <- dataset_ciclovia.$geometry # Para la gráfica final.
  puntos_ciclovia <- st_transform(puntos_ciclovia, 4326)
  
  matrix_distance <- st_distance(x = dataset., y = dataset_ciclovia.)
  vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)
  
  dataset. <- dataset. |> mutate(num_distancia_ciclovia = vector_distance)
  
  # 1.9| Parqueaderos -------------------------------------------------------
  # Conteo del número de parqueaderos en un área.
  data_osm <- opq(bbox = getbb('Bogotá, Distrito Capital')) |> 
    add_osm_feature(key = 'amenity', value = 'parking')
  
  sf_parking <- osmdata_sf(data_osm)
  
  # Eliminamos observaciones repetidas, por si un restaurante cuenta como bar o
  # viceversa, y así no contabilizamos dos veces.
  geom_parking <- sf_parking$osm_polygons |> 
    select(c('osm_id', 'name')) |> 
    drop_na() |> 
    distinct()
  
  centroides <- gCentroid(as(geom_zonasSociales$geometry, "Spatial"), byid = TRUE)
  centroides <- st_as_sf(centroides, coords = c('x', 'y'))
  
  matrix_distance <- st_distance(x = dataset., y = centroides)
  
  # Contamos los puntos de zonas sociales que se encuentran en un radio 
  # determinado alrededor de las casas.
  # TODO. Validar la predicción con diferentes rangos a la redonda.
  data_count <- rowSums(matrix_distance <= radio.)
  dataset. <- dataset. |> mutate(num_numero_parqueaderos = data_count)
  
  # 2| Visualización --------------------------------------------------------
  base_exp     = 1
  heightExp    = 1.2
  widthExp     = 1.2
  scale_factor = base_exp/widthExp
  
  nuevaGrafica <- ggplot() +
    geom_sf(data = dataset_bogota., size = .3, fill = NA) +
    geom_sf(data = centroides_mall, aes(shape = "Centros Comerciales")) + 
    geom_sf(data = puntos_ciclovia, aes(color = "Ciclovías")) +
    geom_sf(data = puntos_tm, aes(color = "TransMilenio")) +
    scale_color_manual(values = c("Ciclovías" = "green", "TransMilenio" = "red")) +
    scale_shape_manual(values = c("Centros Comerciales" = 17)) +
    theme_bw() +
    theme(axis.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 6),
          legend.position = "bottom") +
    labs(color = NULL, shape = NULL)
  
  nombreGrafica <- paste0(directorioResultados, nombreGrafica)
  ggsave(filename = nombreGrafica, plot = nuevaGrafica,
         width = 6 * widthExp, height = 4 * heightExp * widthExp , scale = scale_factor)
  
  # 3| Exportar -------------------------------------------------------------
  saveRDS(object = dataset., 
          file   = paste0(directorioDatos, nombreArchivo))
  
  return(dataset.)
}

# B| Generación de los datos ----------------------------------------------
if (primeraVez == TRUE) {
  dataset <- crear_variables_geograficas(
    dataset.      = dataset, 
    nombreArchivo = 'datos_geograficos.rds',
    nombreGrafica = 'mapa_variables.png'
  )
  
  dataset_kaggle <- crear_variables_geograficas(
    dataset.      = dataset_kaggle, 
    nombreArchivo = 'datos_geograficos_evaluacion.rds',
    nombreGrafica = 'mapa_variables_evaluacion.png'
  )
} else {
  dataset        <- readRDS(file = paste0(directorioDatos, 
                                          'datos_geograficos.rds'))
  dataset_kaggle <- readRDS(file = paste0(directorioDatos, 
                                          'datos_geograficos_evaluacion.rds'))
}
