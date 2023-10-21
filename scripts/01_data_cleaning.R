# 1| Importar -------------------------------------------------------------
# Importamos la base de datos de entrenamiento y de evaluación.
dataset <- read.csv(file = paste0(directorioDatos, 'train.csv'))
dataset_kaggle <- read.csv(file = paste0(directorioDatos,'test.csv'))

# Importamos la información de las estaciones de transmilenio en formato json.
# Nótese que las observaciones son puntos. 
dataset_tm <- st_read(paste0(directorioDatos,'estaciones_trocales_trm.geojson')) |>
  select(c('id_estacion' = 'numero_estacion', 
           'nom_estacion' = 'nombre_estacion',
           'lat_estacion' = 'latitud_estacion',
           'lon_estacion' = 'longitud_estacion',
           'geometry'))

# Así mismo, importamos el número de validaciones por estación y por hora,
# de forma tal que podamos determinar cuáles son las estaciones donde se 
# concentran las oportunidades laborales y en dónde se concentran las
# zonas residenciales.
# Nota. La base de datos es muy pesada, por lo que el análisis se hace una
# única vez y se exporta los resultados.

if (primeraVez == TRUE) {
  # Definimos las fechas a tomar. En particular, nos basamos en el 25 de
  # septiembre, sospechando que el lunes es el día con más movimiento en 
  # la ciudad. Así mismo, no realizamos el análisis el fin de semana dado que 
  # nos interesa las zonas con oportunidades laborales.
  dataset_validacion <- read.csv(file = paste0(
    directorioDatos, 'validaciones/validacionTroncal', '20230925', '.csv')
  )
  
  # Las variables de interés son la hora de validación y la estación donde
  # se registró el pago.
  pattern <- '^(\\d{4}-\\d{2}-\\d{2}) (\\d{2})(:\\d{2}:\\d{2})'
  dataset_validacion <- dataset_validacion |> 
    select(c(tex_estacion = 'Estacion_Parada', 
             tex_fecha = 'Fecha_Transaccion')) |> 
    mutate(id_estacion  = str_extract(string = tex_estacion, 
                                      pattern = '(\\(\\d+\\)) (.*)', group = 1),
           nom_estacion = str_extract(string = tex_estacion, 
                                      pattern = '(\\(\\d+\\)) (.*)', group = 2),
           date_fecha   = str_extract(string = tex_fecha, 
                                      pattern = pattern, group = 1),
           date_hora    = str_extract(string = tex_fecha, 
                                      pattern = pattern, group = 2)) |> 
    mutate(id_estacion = gsub(pattern = '[()]', replacement = '', x = id_estacion),
           date_fecha  = ymd(date_fecha),
           date_hora   = as.numeric(date_hora)) |> 
    select(c('id_estacion', 'nom_estacion', 'date_fecha', 'date_hora'))
  
  # Dado que lo que interesa es el número de validaciones por hora, vamos
  # a colapsar la base de datos a nivel de estación y hora de validación.
  # En particular, miramos qué proporción de las validaciones se realizó
  # en determinada hora del día.
  dataset_validacion <- dataset_validacion |> 
    filter(date_fecha %in% ymd('20230925')) |> 
    group_by(id_estacion, nom_estacion, date_hora) |> 
    summarise(num_validaciones = n()) |> 
    ungroup() |> 
    group_by(nom_estacion) |> 
    mutate(prop_validaciones = num_validaciones/sum(num_validaciones)) |> 
    ungroup()
  
  # La hora pico de la mañana comprende desde las 4a.m. hasta las 8:59a.m.
  # Por el contrario, la tarde comprende desde las 4p.m. hasta las 7:59a.m.
  # Así, contamos la proporción de validaciones en la mañana, en hora valle,
  # y en la tarde.
  dataset_validacion <- dataset_validacion |> 
    mutate(cat_hora = case_when(date_hora %in% 4:8 ~ 'Temprano',
                                date_hora %in% 16:19 ~ 'Tarde',
                                TRUE ~ 'Valle')) |> 
    group_by(id_estacion, nom_estacion, cat_hora) |> 
    summarise(prop_validaciones = sum(prop_validaciones)) |> 
    ungroup()
  
  # Finalmente, hacemos una comparación. Si una estación tiene la mayoría de
  # sus validaciones en hora valle, no la vamos a considerar ni residencial
  # ni laboral.
  dataset_validacion <- dataset_validacion |> 
    pivot_wider(names_from = 'cat_hora', values_from = 'prop_validaciones') |> 
    filter((Temprano > Valle) | (Tarde > Valle)) |> 
    mutate(bin_zonaResidencial = as.numeric(Tarde < Temprano),
           bin_zonaLaboral = as.numeric(Temprano < Tarde))
  
  # Adicionamos la posición espacial con base en las estaciones troncales
  # importadas previamente.
  dataset_validacion <- dataset_tm |> 
    left_join(y = dataset_validacion |> 
                select(-c('nom_estacion', 'Tarde', 'Temprano', 'Valle')), 
              by = 'id_estacion') |> 
    replace_na(list(bin_zonaResidencial = 0, bin_zonaLaboral = 0))
  
  saveRDS(object = dataset_validacion, 
          file = paste0(directorioDatos, 'tipo_de_barrio.rds'))
  
} else {
  dataset_validacion <- readRDS(file = paste0(directorioDatos, 'tipo_de_barrio.rds'))
}

# Importamos la información de ciclovías en formato shapefile. Nótese que
# las observaciones son líneas.
dataset_ciclovia <- st_read(paste0(directorioDatos,'ciclovia/Ciclovia.shp'))
dataset_ciclovia <- st_transform(dataset_ciclovia, crs = 4326)

# Importamos el mapa de Bogotá con la división de localidades.
dataset_bogota <- st_read(paste0(directorioDatos,'upla/UPla.shp')) |> 
  filter(grepl("RIO", UPlNombre) == FALSE)
dataset_bogota <- st_transform(dataset_bogota, crs = 4326)

dataset_localidades <- st_read(paste0(directorioDatos,'upla/Loca.shp'))
dataset_localidades <- st_transform(dataset_localidades, crs = 4326)
dataset_localidades <- dataset_localidades |> 
  select(c('id_localidad' = 'LocCodigo',
           'nom_localidad' = 'LocNombre',
           'geometry'))

# Importamos características del censo por manzanas de Bogotá.
dataset_censo <- st_read(paste0(directorioDatos, 'censo/censo_manzanas2018.shp'))
dataset_censo <- st_transform(dataset_censo, crs = 4326)

dataset_censo <- dataset_censo |> 
  filter((u_dpto == '11') & (u_mpio == '001')) |> 
  select(c('cat_estrato' = 'estrato'))

# Definimos los nombres de las columnas tal y como trabajaremos en el futuro.
nombres_variables   <- c('id_hogar', 'id_ciudad', 'num_precio', 'cat_mes', 
                         'cat_ano', 'num_area_total', 'num_area_cubierta', 
                         'num_habitacion', 'num_dormitorio', 'num_bano',
                         'cat_tipo', 'cat_venta', 'num_latitud', 
                         'num_longitud', 'tex_titulo', 'tex_descripcion')

colnames(dataset) <- nombres_variables
colnames(dataset_kaggle) <- nombres_variables

# 2| Limpieza -------------------------------------------------------------
# 2.1| Conteo de valores faltantes ----------------------------------------
# Determinamos el número de valores faltantes en cada una de las variables.
nombres_latex <- c('ID Hogar', 'Precio del inmueble', 'Mes', 'Año', 
                   'Área total', 'Área cubierta', 'Número de habitaciones', 
                   'Número de dormitorios', 'Número de baños', 'Tipo de vivienda',
                   'Latitud', 'Longitud', 'Título de la publicación',
                   'Descripción de la publicación')

num_datos_faltantes <- apply(X = dataset, MARGIN = 2,
                             FUN = function(x) {sum(is.na(x))})
tib_datos_faltantes <- tibble(Variable = nombres_latex,
                              Valores  = num_datos_faltantes)

# Exportamos los resultados a una tabla en LaTeX.
print(xtable(x = tib_datos_faltantes, type = "latex", 
             caption = "Valores faltantes para cada una de las variables originales.", 
             label = "Tab:valoresFaltantes", align = c('l', 'l', 'c'), 
             digits = c(0, 0, 0)), 
      file = "views/valoresFaltantes.tex",
      table.placement = 'H!',
      caption.placement = 'top',
      include.rownames = FALSE)

# 2.2| Limpieza con expresiones regulares ---------------------------------
# Nota. NO llenar los valores faltantes en 'surface_total' y 'surface_covered' con 0, pues
# no tiene sentido que apartamentos con cero metros cuadrados construidos tengan un precio positivo.
# Estas columnas pueden representar el área total de la propiedad y el área cubierta de la 
# propiedad, respectivamente.

# A| Validación de casa/apartamento ----------------------------------------
# Nota. Nos quedamos con cat_tipo dado que solo hay una diferencia de 200
# inmuebles.
dataset <- limpiar_casa(.dataset = dataset)
dataset_kaggle <- limpiar_casa(.dataset = dataset_kaggle)

# Comparaciones.
table(dataset$cat_tipo)
table(dataset$bin_casa)
table(dataset$cat_ano)

# B| Validación metros cuadrados ------------------------------------------
# TODO. Extraer los metros mediante expresiones regulares, y los datos faltantes 
# sí llenarlos con la media por localidad, estrato, y si es casa o apartamento
# -en lugar de cero, pues no tendría sentido pagar un precio positivo por un
# inmueble sin espacio-.
dataset <- limpiar_metros(.dataset = dataset)
dataset_kaggle <- limpiar_metros(.dataset = dataset_kaggle)

summary(dataset$num_mt2)

# Ponemos límites a los metros cuadrados según distribución por casa y 
# apartamento. Para ello, primero tomamos una estadística descriptiva:
tapply(dataset$num_mt2, dataset$cat_tipo, summary)
tapply(dataset$num_area_cubierta, dataset$cat_tipo, summary)

dataset <- censura_metros(.dataset = dataset)
dataset_kaggle <- censura_metros(.dataset = dataset_kaggle)

summary(dataset$num_mt2)
summary(dataset$num_area_total)

# TODO. Validar que el área total sea mayor o igual a la cubierta.
# dataset <- dataset |> 
#   mutate(d_area_true = ifelse(num_area_total > num_area_cubierta, 1, 0) )
# 
# table(combined_data$d_area_true)
# summary(combined_data$num_area_total)
# summary(combined_data$num_area_cubierta)
# Sospechamos que ambas variables con incoherentes. 

# TODO.Imputar medianas.

# C| Validación de alcobas ------------------------------------------------
# TODO. Censurar observaciones anómalas.
dataset <- limpiar_alcobas(.dataset = dataset)
dataset_kaggle <- limpiar_alcobas(.dataset = dataset_kaggle)

summary(dataset$num_rooms)

# D| Validación baños -----------------------------------------------------
dataset <- limpiar_banos(.dataset = dataset)
dataset_kaggle <- limpiar_banos(.dataset = dataset_kaggle)

# 3| Estadística descriptiva ----------------------------------------------
# Si bien los meses o años son variables numéricas, en realidad son categóricas
# para el análisis estadístico.
dataset <- dataset |> 
  mutate(cat_mes = as.character(cat_mes),
         cat_ano = as.character(cat_ano))

dataset_kaggle <- dataset_kaggle |> 
  mutate(cat_mes = as.character(cat_mes),
         cat_ano = as.character(cat_ano))

estadistica_descriptiva <- dataset |> 
  select(c(starts_with('num_'), starts_with('cat_'))) |>
  select(-c('cat_mes', 'num_latitud', 'num_longitud')) |> 
  tbl_summary(include = everything(),
              type = starts_with('num_') ~ 'continuous2',
              label = list(num_precio ~ 'Precio del inmueble',
                           cat_ano ~ 'Año', 
                           num_area_total ~ 'Área total',
                           num_area_cubierta ~ 'Área cubierta',
                           num_habitacion ~ 'Número de habitaciones',
                           num_dormitorio ~ 'Número de dormitorios',
                           num_bano ~ 'Número de baños',
                           cat_tipo ~ 'Tipo de vivienda'),
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "({min}, {max})"),
                               all_categorical() ~ "{n}  ({p}%)"),  #  / {N}
              missing_text = "(Valores faltantes)", 
              sort = all_categorical() ~ "alphanumeric") |> 
  add_stat_label(label = list(all_categorical() ~ "",
                              all_continuous() ~ c("Promedio (Desviación std)",
                                                   "Mínimo y máximo"))) |> 
  modify_header(label = "**Variable**") |> 
  bold_labels() 

gtsave(as_gt(estadistica_descriptiva), 
       filename="views/estadisticaDescriptiva.tex")

# 4| Gráficas -------------------------------------------------------------
# 4.1| Distribuciones de probabilidad -------------------------------------
# Importante ver no solo los precios de los inmuebles, sino el precio por 
# metro cuadrado.
base_exp     = 1
heightExp    = 1
widthExp     = 1.2
scale_factor = base_exp/widthExp

options(scipen=-1)
graficaExportar <- dataset |> 
  mutate(num_precio_m2 = round(x = num_precio/num_area_total, digits = 0)) |> 
  select(c('num_precio', 'num_precio_m2')) |>
  pivot_longer(cols = everything(), names_to = 'Variable', values_to = 'Valores') |> 
  mutate(Variable = case_when(Variable == 'num_precio' ~ 'Precio del inmueble',
                              Variable == 'num_precio_m2' ~ 'Precio por metro cuadrado')) |> 
  ggplot(mapping = aes(x = Valores, color = factor(1), fill = factor(1))) + 
  facet_wrap(facets = ~ Variable, nrow = 1, scales = 'free') +
  geom_histogram(aes(y=..density..), 
                 position = "identity", alpha = 0.3, show.legend = FALSE) +
  labs(title    = 'Distribución de probabilidad del precio de inmueble y por metro cuadrado.',
       subtitle = '',
       caption  = '',
       x        = 'Pesos colombianos\n(Medidos en millones)',
       y        = '') +
  geom_density(alpha = .2, show.legend = FALSE)  +
  scale_color_manual(values = c("#E69F00")) +
  scale_fill_manual(values = c("#E69F00")) +
  theme_classic() +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, .05))) +
  scale_x_continuous(expand = expansion(mult = c(0, .05)),
                     labels = scales::label_number(scale = 0.000001, 
                                                   accuracy = 1, 
                                                   big.mark = ',')) +
  theme(legend.position = 'bottom',
        text            = element_text(family = 'Georgia'),
        axis.text.x     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.text.y     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.title.x    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        axis.title.y    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        legend.text     = element_text(size = scale_factor * 10, family = 'Georgia'),
        strip.text      = element_text(size = scale_factor * 10, family = 'Georgia'),
        plot.title      = element_text(face = 'bold', size = 10, family = 'Georgia'),
        plot.subtitle   = element_text(size = 10, family = 'Georgia'),
        legend.key.size = unit(0.5 * scale_factor, 'cm')) 

nombreArchivo <- 'distribucion_precios.png'
ggsave(filename = paste0(directorioResultados, nombreArchivo), plot = graficaExportar,
       width = 6 * widthExp, height = 4 * heightExp * widthExp, scale = scale_factor)
options(scipen=999)
