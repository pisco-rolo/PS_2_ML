# 1| Extraer información --------------------------------------------------
# Convertimos la base de datos original en un 'sf', de forma tal que sea más
# sencillo manipular las variables de longitud y latitud.
dataset <- st_as_sf(x = dataset, coords = c('num_longitud', 'num_latitud'), 
                    crs = 4326)
bogota <- opq(bbox = getbb("Bogotá Colombia"))

# 1.1| CAIs -------------------------------------------------------------------
cais <- bogota %>% 
  add_osm_feature(key="amenity",value="police") %>%
  osmdata_sf() #transformamos a un objeto sf

puntos_cais <- cais$osm_point #De la lista de cais, tomo los puntos.
st_crs(puntos_cais) #Ya usa el 4326

matrix_distance <- st_distance(x = dataset, y = puntos_cais)
vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)

# 1.2| Centros comerciales ----------------------------------------------------
mall <- bogota %>% 
  add_osm_feature(key="shop",value="mall") %>%
  osmdata_sf() #transformamos a un objeto sf

geom_mall <- mall$osm_polygons |> 
  select(c('osm_id', 'name'))

centroides <- gCentroid(as(geom_mall$geometry, "Spatial"), byid = TRUE)
centroides <- st_as_sf(centroides, coords = c('x', 'y'))

matrix_distance <- st_distance(x = dataset, y = centroides)
vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)

# 1.3| Estaciones transmilenio ------------------------------------------------
#Lo convertimos a 4326 ya que está en GWS84
puntos_tm <- dataset_tm$geometry
puntos_tm <- st_as_sf(x = puntos_tm, crs = 4326)
st_crs(puntos_tm)

matrix_distance <- st_distance(x = dataset, y = puntos_tm)
vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)

# 1.3| Ciclovías --------------------------------------------------------------
#Convertimos al sistema de referencia del dataset
puntos_ciclovia <- dataset_ciclovia$geometry
puntos_ciclovia <- st_as_sf(dataset)
st_crs(puntos_ciclovia)

matrix_distance <- st_distance(x = dataset, y = puntos_ciclovia)
vector_distance <- apply(X = matrix_distance, MARGIN = 1, FUN = min)


# 1.1| Mapa de nueva información ----------------------------------------------

ggplot()+
  geom_sf(data=puntos_tm) + theme_bw()





