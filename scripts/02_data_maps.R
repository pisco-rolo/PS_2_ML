# 1| Extraer información --------------------------------------------------
# Convertimos la base de datos original en un 'sf', de forma tal que sea más
# sencillo manipular las variables de longitud y latitud.
dataset <- st_as_sf(x = dataset, coords = c('num_longitud', 'num_latitud'))
st_crs(dataset) <- 4326

# 1.1| Universidades ------------------------------------------------------
data_osm <- opq(bbox = getbb('Bogotá, Distrito Capital')) |> 
  add_osm_feature(key = 'amenity', value = 'university')

sf_universidades <- osmdata_sf(data_osm)

geom_universidades <- sf_universidades$osm_polygons |> 
  select(c('osm_id', 'name'))

centroides <- gCentroid(as(geom_universidades$geometry, "Spatial"), byid = TRUE)
centroides <- st_as_sf(centroides, coords = c('x', 'y'))

matrix_distance <- st_distance(x = dataset, y = centroides)
vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)

# TODO. ¿En qué unidades está medida la distancia?
dataset <- dataset |> mutate(num_distancia_universidades = vector_distance)
