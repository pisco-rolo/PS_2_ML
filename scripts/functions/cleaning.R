# 1| Expresiones regulares ------------------------------------------------
limpiar_casa <- function(.dataset) {
  sinonimos <- c("casa", "casas", "chalet", "campestre")
  .dataset <- .dataset |> 
    mutate(tex_cat_tipo_titulo = str_extract(tex_titulo,"([Aa]partamento(s)?|apto|ap|duplex|penthouse|apartaestudio|ap[a-z]estudio|[Cc]asas?|chalet|casa campestre|estudio|miniapartamento)")) |> 
    mutate(tex_cat_tipo_descripcion = str_extract(tex_descripcion,"([Aa]partamento(s)?|apto|ap|duplex|penthouse|apartaestudio|aparta estudio|ap[a-z+]estudio|[Cc]asas?|chalet|casa campestre|estudio|miniapartamento)" )) |> 
    mutate(tex_cat_tipo = ifelse(is.na(tex_cat_tipo_titulo), tex_cat_tipo_descripcion, tex_cat_tipo_titulo)) |> 
    mutate(bin_casa = case_when(tex_cat_tipo %in% sinonimos ~ 1,
                                TRUE ~ 0)) |> 
    # Implícitamente suponemos que, todo aquello que no sea una casa, es un
    # apartamento. Podríamos revisar más a fondo, dejando las variables
    # "tex_cat_tipo_titulo" y "tex_cat_tipo_descripcion", a ver cuáles
    # son NA y revisar si la descripción efectivamente no contiene la palabra.
    mutate(bin_casa = ifelse(is.na(bin_casa), 0, 1)) |> 
    select(-c("tex_cat_tipo_titulo", "tex_cat_tipo_descripcion", "tex_cat_tipo"))
  
  return(.dataset)
}

limpiar_metros <- function(.dataset) {
  .dataset <- .dataset |> 
    mutate(num_mt2 = str_extract(tex_descripcion, "\\b\\d+\\s*(?:m2|mts2|mts|m²|m\\^2|metros cuadrados|metros|mtrs|mtrs2|mts2|mt|mt2|mt23|m2?|metro cuadrados)\\b" )) |> 
    mutate(num_mt2 = as.integer(str_extract(num_mt2, "\\d+")))
  
  return(.dataset)
}

censura_metros <- function(.dataset) {
  # Y ponemos el límite de los apartamentos y las casas:
  .dataset <- .dataset |> 
    mutate(num_mt2 = ifelse(num_mt2 > 600 & cat_tipo=="apartamento", NA, num_mt2)) |>  
    # Reemplazamos NA's con la mediana.
    mutate(num_mt2 = ifelse(cat_tipo=="apartamento", replace_na(num_mt2, 104), num_mt2)) |> 
    mutate(num_mt2 = ifelse(num_mt2 > 1200 & cat_tipo=="casa", NA , num_mt2))
  
  return(.dataset)
}

limpiar_alcobas <- function(.dataset) {
  # Definimos las palabras clave y el patrón de extracción.
  sinonimos <- c("habitacion", "habitaciones", "alcoba", "alcobas", "cuarto", "cuartos")
  patron2 <- paste0("(\\w+|\\d+) (", paste(sinonimos, collapse = "|"), ")", "(\\w+|\\d+)")
  
  numeros_escritos <- c("uno|primero|primer", "dos|segundo|segund", 
                        "tres|tercero|tercer", "cuatro|cuarto", "cinco|quinto", 
                        "seis|sexto", "siete|septimo", "ocho|octavo", 
                        "nueve|noveno", "diez|decimo|dei")
  numeros_numericos <- as.character(1:10)
  
  # Extramos la información necesaria.
  .dataset <- .dataset |> 
    mutate(num_rooms = str_extract(tex_descripcion, "(\\w+|\\d+) (alcoba(s)?|habitacion(es)? |cuarto(s)?) (\\w+|\\d+)")) |> 
    mutate(num_rooms = str_extract(tex_descripcion, patron2)) |> 
    mutate(num_rooms = str_replace_all(num_rooms, setNames(numeros_numericos,numeros_escritos))) |> 
    mutate(num_rooms = as.integer(str_extract(num_rooms, "\\d+")))
    
  return(.dataset)
}

limpiar_piso <- function(.dataset) {
  # Se crea una nueva columna llamada "num_piso" que contiene el número de piso, 
  # y se descartan los valores que son mayores de 20 (puede ser cuestionable), 
  # posiblemente porque son considerados atípicos o incorrectos
  numeros_escritos <- c("uno|primero|primer", "dos|segundo|segund", 
                        "tres|tercero|tercer", "cuatro|cuarto", "cinco|quinto", 
                        "seis|sexto", "siete|septimo", "ocho|octavo", 
                        "nueve|noveno", "diez|decimo|dei")
  numeros_numericos <- as.character(1:10)
  
  .dataset <- .dataset |>
    mutate(tex_piso = str_extract(tex_descripcion, "(\\w+|\\d+) piso (\\w+|\\d+)")) |> 
    mutate(tex_piso = str_replace_all(tex_piso, setNames(numeros_numericos, numeros_escritos))) |> 
    mutate(num_piso = as.integer(str_extract(tex_piso, "\\d+"))) |> 
    mutate(num_piso = ifelse(num_piso > 20, NA, num_piso))
  
  # Imputación de valores faltantes en la variable 'num_piso'.
  # Este codigo calcula la de frecuencia de los valores en la variable 
  # 'num_piso' dentro de cada grupo de 'cat_tipo_2', luego ordena la tabla en 
  # orden descendente y selecciona el primer valor, que corresponde a la moda.
  .dataset <- .dataset |>  #  group_by(property_type_2) |>
    mutate(num_piso = ifelse(is.na(num_piso), as.integer(names(sort(table(num_piso), decreasing = TRUE)[1])), num_piso)) |>
    ungroup()
  
  return(.dataset)
}

limpiar_banos <- function(.dataset) {
  # Dado que la base de datos ya cuenta con una observación de baños, creamos
  # una temporal donde almacenar la información extraída mediante expresiones
  # regulares.
  .dataset <- .dataset |> 
    mutate(num_bano_2 = str_extract(string  = tex_descripcion,
                                    pattern = '(.*) (\\d{1,2}) (bano(s)?|baño(s)?|bao(s)?) (.*)', 
                                    group   = 2)) |> 
    mutate(num_bano = case_when(is.na(num_bano) ~ num_bano_2,
                                TRUE ~ num_bano)) |>
    select(-c('num_bano_2'))
  
  return(.dataset)
}

# 2| Datos geolocalizados -------------------------------------------------
crear_variables_geograficas <- function(
    .dataset, 
    .dataset_bogota = dataset_bogota,
    .dataset_validacion = dataset_validacion, 
    .dataset_tm = dataset_tm,
    .dataset_ciclovia = dataset_ciclovia, 
    .dataset_localidades = dataset_localidades,
    .dataset_censo = dataset_censo,
    .radio = radio, 
    nombreArchivo = 'datos_geograficos.rds',
    nombreGrafica = 'mapa_variables.png'
) {
  # 1| Extraer información --------------------------------------------------
  # Convertimos la base de datos original en un 'sf', de forma tal que sea más
  # sencillo manipular las variables de longitud y latitud.
  .dataset <- st_as_sf(x = .dataset, coords = c('num_longitud', 'num_latitud'))
  st_crs(.dataset) <- 4326
  
  # 1.1| Universidades ------------------------------------------------------
  # Estimamos la distancia (en metros) de la Universidad más cercana.
  data_osm <- opq(bbox = getbb('Bogotá, Distrito Capital')) |> 
    add_osm_feature(key = 'amenity', value = 'university')
  
  sf_universidades <- osmdata_sf(data_osm)
  
  geom_universidades <- sf_universidades$osm_polygons |> 
    select(c('osm_id', 'name'))
  
  centroides <- gCentroid(as(geom_universidades$geometry, "Spatial"), byid = TRUE)
  centroides <- st_as_sf(centroides, coords = c('x', 'y'))
  
  matrix_distance <- st_distance(x = .dataset, y = centroides)
  vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)
  
  # TODO. ¿En qué unidades está medida la distancia? Parece estar medido en 
  # metros.
  .dataset <- .dataset |> mutate(num_distancia_universidades = vector_distance)
  
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
  
  matrix_distance <- st_distance(x = .dataset, y = centroides)
  
  # Contamos los puntos de zonas sociales que se encuentran en un radio 
  # determinado alrededor de las casas.
  # TODO. Validar la predicción con diferentes rangos a la redonda.
  data_count <- rowSums(matrix_distance <= .radio)
  .dataset <- .dataset |> mutate(num_numero_ocio = data_count)
  
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
  
  matrix_distance <- st_distance(x = .dataset, y = geom_calles)
  vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)
  
  .dataset <- .dataset |> mutate(num_distancia_calles = vector_distance)
  
  # 1.4| Transmilenio -------------------------------------------------------
  # - Segmentar la ciudad en zonas residenciales y zonas empresariales.
  matrix_close <- st_nearest_feature(.dataset, .dataset_validacion) 
  vector_close <- .dataset_validacion[matrix_close, ]
  vector_close <- st_drop_geometry(vector_close)
  
  .dataset <- .dataset |> bind_cols(vector_close)
  
  # - Distancia a la estación de TM más cercana.
  puntos_tm <- .dataset_tm$geometry # Para la gráfica final.
  puntos_tm <- st_as_sf(x = puntos_tm, crs = 4326)
  
  matrix_distance <- st_distance(x = .dataset, y = .dataset_tm)
  vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)
  
  .dataset <- .dataset |> mutate(num_distancia_tm = vector_distance)
  
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
  
  matrix_distance <- st_distance(x = .dataset, y = centroides)
  vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)
  
  .dataset <- .dataset |> mutate(num_distancia_mall = vector_distance)
  
  # 1.6| Parques ------------------------------------------------------------
  # Distancia del parque más cercano.
  data_osm <- opq(bbox = getbb('Bogotá, Distrito Capital')) |> 
    add_osm_feature(key = 'leisure', value = 'park')
  
  sf_parques <- osmdata_sf(data_osm)
  geom_parques <- sf_parques$osm_polygons |> select(c('osm_id', 'name'))
  centroides <- gCentroid(as(geom_parques$geometry, "Spatial"), byid = TRUE)
  centroides <- st_as_sf(centroides, coords = c('x', 'y'))
  
  matrix_distance <- st_distance(x = .dataset, y = centroides)
  vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)
  
  .dataset <- .dataset |> mutate(num_distancia_parque = vector_distance)
  
  # Área del parque más cercano.
  posicion <- apply(X = matrix_distance, MARGIN = 1, FUN = function(x) {
    which(min(x) == x)
  })
  areas <- st_area(geom_parques)
  .dataset <- .dataset |> mutate(num_area_parque = as.numeric(areas[posicion]))
  
  # 1.7| CAI ----------------------------------------------------------------
  data_osm <-  opq(bbox = getbb('Bogotá, Distrito Capital')) |> 
    add_osm_feature(key = 'amenity', value = 'police')
  
  sf_cai <- osmdata_sf(data_osm)
  geom_cai <- sf_cai$osm_point
  
  matrix_distance <- st_distance(x = .dataset, y = geom_cai)
  vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)
  
  .dataset <- .dataset |> mutate(num_distancia_cai = vector_distance)
  
  # 1.8| Ciclovía -----------------------------------------------------------
  # Distancia de la ciclovía más cercana.
  puntos_ciclovia <- .dataset_ciclovia$geometry # Para la gráfica final.
  puntos_ciclovia <- st_transform(puntos_ciclovia, 4326)
  
  matrix_distance <- st_distance(x = .dataset, y = .dataset_ciclovia)
  vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)
  
  .dataset <- .dataset |> mutate(num_distancia_ciclovia = vector_distance)
  
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
  
  matrix_distance <- st_distance(x = .dataset, y = centroides)
  
  # Contamos los puntos de zonas sociales que se encuentran en un radio 
  # determinado alrededor de las casas.
  # TODO. Validar la predicción con diferentes rangos a la redonda.
  data_count <- rowSums(matrix_distance <= .radio)
  .dataset <- .dataset |> mutate(num_numero_parqueaderos = data_count)
  
  # 1.10| Localidad ---------------------------------------------------------
  pertenencia <- st_within(.dataset, .dataset_localidades)
  .dataset <- .dataset |> 
    mutate(nom_localidad = st_drop_geometry(.dataset_localidades[unlist(pertenencia), 'nom_localidad']) |> 
             _$nom_localidad)
  
  # 1.11| Estrato -----------------------------------------------------------
  pertenencia <- st_within(.dataset, .dataset_censo, prepared = FALSE)
  impute_na <- function(x) {
    ifelse(is.null(x), NA, x)
  }
  pertenencia <- lapply(pertenencia, impute_na)
  pertenencia <- unlist(as.list(pertenencia), use.names = TRUE)
  pertenencia <- st_drop_geometry(.dataset_censo[pertenencia, 'cat_estrato']) |> 
    _$cat_estrato
  
  # La posición geográfica no es perfecta. Algunas casas parecen estar ubicadas
  # en la calle -y ello puede ser un error de aproximación-. Luego, es necesario
  # señalar a qué polígono puede estar más cercana la observación, y ahí sí
  # imputar el valor del predominante.
  matrix_close <- st_nearest_feature(.dataset[is.na(pertenencia), ], .dataset_censo) 
  vector_close <- .dataset_censo[matrix_close, 'cat_estrato']
  vector_close <- st_drop_geometry(vector_close) |> _$cat_estrato
  pertenencia[is.na(pertenencia)] <- vector_close
  
  .dataset <- .dataset |> 
    mutate(cat_estrato = pertenencia)
  
  # No todas las ubicaciones tienen estrato o no siempre es adecuado imputar el
  # estrato predominante por manzana, por lo que mediante expresiones
  # regulares intentamos corregir la máxima información posible.
  .dataset <- .dataset |> 
    mutate(tex_estrato = str_extract(string  = tex_descripcion,
                                     pattern = '(.*) (estrato) (\\d{1}|uno|dos|tres|cuatro|cinco|seis) (.*)', 
                                     group   = 3)) |> 
    mutate(tex_estrato = str_to_upper(tex_estrato)) |> 
    mutate(tex_estrato = case_when(tex_estrato == '1' ~ 'UNO',
                                   tex_estrato == '2' ~ 'DOS',
                                   tex_estrato == '3' ~ 'TRES',
                                   tex_estrato == '4' ~ 'CUATRO',
                                   tex_estrato == '5' ~ 'CINCO',
                                   tex_estrato == '6' ~ 'SEIS',
                                   TRUE ~ tex_estrato)) |> 
    mutate(cat_estrato = case_when(is.na(cat_estrato) ~ tex_estrato,
                                   TRUE ~ cat_estrato))
  
  # 2| Visualización --------------------------------------------------------
  base_exp     = 1
  heightExp    = 1.2
  widthExp     = 1.2
  scale_factor = base_exp/widthExp
  
  nuevaGrafica <- ggplot() +
    geom_sf(data = .dataset_bogota, size = .3, fill = NA) +
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
  saveRDS(object = .dataset, 
          file   = paste0(directorioDatos, nombreArchivo))
  
  return(.dataset)
}
