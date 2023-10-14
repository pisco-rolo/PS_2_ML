# 1| Importar -------------------------------------------------------------
# Importamos la base de datos de entrenamiento y de evaluación.
dataset <- read.csv(file = paste0(directorioDatos, 'train.csv'))
dataset_kaggle <- read.csv(file = paste0(directorioDatos,'test.csv'))

#Importo la información de las estaciones de transmilenio en formato json.
dataset_tm <- st_read(paste0(directorioDatos,'estaciones_trocales_trm.geojson'))
#Importo la información de ciclovías en formato shapefile.
dataset_ciclovia <- st_read(paste0(directorioDatos,'ciclovia/Ciclovia.shp'))
#Importo mapa con localidades.
dataset_localidades <- st_read(paste0(directorioDatos,'upla/UPla.shp'))

# Definimos los nombres de las columnas tal y como trabajaremos en el futuro.
nombres_variables   <- c('id_hogar', 'id_ciudad', 'num_precio', 'cat_mes', 
                         'cat_ano', 'num_area_total', 'num_area_cubierta', 
                         'num_habitacion', 'num_dormitorio', 'num_bano',
                         'cat_tipo', 'cat_venta', 'num_latitud', 
                         'num_longitud', 'tex_titulo', 'tex_descripcion')
colnames(train) <- nombres_variables
colnames(test) <- nombres_variables

# Variables como la ciudad o si se vendió la propiedad no nos son de interés
# porque tienen un único valor y no aportan a la variabilidad de la predicción.
library(dplyr)
dataset <- train |> select(-c('id_ciudad', 'cat_venta'))
dataset_kaggle <- test |> select(-c('id_ciudad', 'cat_venta'))

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

#.  

# Verificar valores faltantes en dataset
missing_values_dataset <- sapply(dataset, function(x) sum(is.na(x)))

# Verificar valores faltantes en dataset_kaggle
missing_values_kaggle <- sapply(dataset_kaggle, function(x) sum(is.na(x)))

# Mostrar valores faltantes en dataset
print(missing_values_dataset)

# Mostrar valores faltantes en dataset_kaggle
print(missing_values_kaggle)


# TODO. Extraer los metros mediante expresiones regulares, y los datos faltantes sí llenarlos con
# la media en lugar de cero

# Extraer los metros de la columna num_area_total
dataset$num_area_total <- as.numeric(gsub(".*?(\\d+\\.?\\d*)\\s*m2.*", "\\1", dataset$num_area_total, ignore.case = TRUE))

# Extraer los metros de la columna num_area_cubierta
dataset$num_area_cubierta <- as.numeric(gsub(".*?(\\d+\\.?\\d*)\\s*m2.*", "\\1", dataset$num_area_cubierta, ignore.case = TRUE))


# Calculo de la media de num_area_total y num_area_cubierta excluyendo los valores nulos
mean_area_total <- mean(dataset$num_area_total, na.rm = TRUE)
mean_area_cubierta <- mean(dataset$num_area_cubierta, na.rm = TRUE)

# Llenar los valores nulos con la media en num_area_total
dataset$num_area_total[is.na(dataset$num_area_total)] <- mean_area_total

# Llenar los valores nulos con la media en num_area_cubierta
dataset$num_area_cubierta[is.na(dataset$num_area_cubierta)] <- mean_area_cubierta



# TODO. Rellenar número de habitaciones y número de baños.

# Baños e imputacion de valores faltantes con la media.
dataset$num_baño <- as.numeric(gsub(".*?(\\d+\\.?\\d*).*", "\\1", dataset$num_bano))
dataset_kaggle$num_baño <- as.numeric(gsub(".*?(\\d+\\.?\\d*).*", "\\1", dataset_kaggle$num_bano))

# Calculo la media de num_bano en dataset excluyendo los valores nulos
mean_num_bano <- mean(dataset$num_bano, na.rm = TRUE)
# Llenar valores nulos con la media en num_bano en dataset_kaggle
dataset$num_bano[is.na(dataset$num_bano)] <- mean_num_bano
  
# Calculo la media de num_bano en dataset_kaggle excluyendo los valores nulos
mean_num_bano_kaggle <- mean(dataset_kaggle$num_bano, na.rm = TRUE)
# llenar valores nulos con la media en num_bano en dataset_kaggle
dataset_kaggle$num_bano[is.na(dataset_kaggle$num_bano)] <- mean_num_bano_kaggle
  
# Verificar y corregir valores incorrectos en num_bano en dataset, no tiene sentido que el número de baños sea < 1.
dataset$num_bano[dataset$num_bano < 1] <- 1
dataset_kaggle$num_bano[dataset_kaggle$num_bano < 1] <- 1
hist(dataset$num_bano, main = "Distribución de Número de Baños", xlab = "Número de Baños")
#Cada intervalo muestra cuántas observaciones de la variable "Número de Baños" caen en ese rango específico.
# Las barras más altas indican que hay más propiedades con un número de baños en ese rango
# El histograma muestra que la mayoría de las propiedades en el conjunto de datos tienen un número de baños en el rango de 1 a 3 baños. Hay un número relativamente bajo de propiedades con más de 3 baños
# me parecio interesante mirar esta relacion entre "numero de baños y precio"
plot(dataset$num_bano, dataset$num_precio, xlab = "Número de Baños", ylab = "Precio de la Propiedad", main = "Relación entre Número de Baños y Precio")


# TODO. Validar que el área total sea mayor o igual a la cubierta.

# TODO. Verificar que el tipo de la propiedad sí sea casa o apartamento y sobreescribir la variable.
combined_data <- combined_data %>%
  mutate(property_type_2 = ifelse(grepl("casa", tex_descripcion), "casa", property_type))

combined_data <- combined_data %>%
  mutate(property_type_2 = ifelse(grepl("apto|apartamento", tex_descripcion), "apartamento", property_type_2)) %>%
  select(-property_type)

# TODO. Validar si una casa tiene garaje o parqueadero.
combined_data <- combined_data %>%
  mutate(cat_parqueadero = ifelse(grepl("garage|parqueadero", tex_descripcion, ignore.case = TRUE), 1, 0))

#con este codigo se intenta extraer información sobre el piso desde la descripción de la propiedad utilizando expresiones regulares. 
#Luego, se convierten los números escritos en números numéricos para estandarizarlos. 
#Finalmente, se crea una nueva columna llamada "piso_numerico" que contiene el número de piso, y se descartan los valores que son mayores de 20(puede ser cuestionable),posiblemente porque son considerados atípicos o incorrectos

combined_data <- combined_data %>%
  mutate(piso_info = str_extract(description, "(\\w+|\\d+) piso (\\w+|\\d+)"))

numeros_escritos <- c("uno|primero|primer", "dos|segundo|segund", "tres|tercero|tercer", "cuatro|cuarto", "cinco|quinto", "seis|sexto", "siete|septimo", "ocho|octavo", "nueve|noveno", "diez|decimo|dei")
numeros_numericos <- as.character(1:10)

combined_data <- combined_data %>%
  mutate(piso_info = str_replace_all(piso_info, setNames(numeros_numericos, numeros_escritos)))

combined_data <- combined_data %>%
  mutate(piso_numerico = as.integer(str_extract(piso_info, "\\d+")))

combined_data <- combined_data %>%
  mutate(piso_numerico = ifelse(piso_numerico > 20, NA, piso_numerico))

# Imputación de valores faltantes en la variable 'piso_numerico'
# Este codigo calcula la de frecuencia de los valores en la variable 'piso_numerico' dentro de cada grupo de 'property_type_2', luego ordena la tabla en orden descendente y selecciona el primer valor, que corresponde a la moda.
combined_data <- combined_data %>%
  group_by(property_type_2) %>%
  mutate(piso_numerico = ifelse(is.na(piso_numerico), as.integer(names(sort(table(piso_numerico), decreasing = TRUE)[1])), piso_numerico)) %>%
  ungroup()

# 2.3| Limpieza con imputación --------------------------------------------



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

# 4.2| Mapas --------------------------------------------------------------
leaflet() %>%
  addTiles() %>%
  addCircles(lng = dataset$num_longitud, 
             lat = dataset$num_latitud,
             col = '#F4A261')
             
