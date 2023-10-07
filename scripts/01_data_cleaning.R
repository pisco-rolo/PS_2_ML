# 1| Importar -------------------------------------------------------------
# Importamos la base de datos de entrenamiento y de evaluación.
dataset <- read.csv(file = paste0(directorioDatos, 'train.csv'))
dataset_kaggle <- read.csv(file = paste0(directorioDatos, 'test.csv'))

# Definimos los nombres de las columnas tal y como trabajaremos en el futuro.
nombres_variables   <- c('id_hogar', 'id_ciudad', 'num_precio', 'cat_mes', 
                         'cat_ano', 'num_area_total', 'num_area_cubierta', 
                         'num_habitacion', 'num_dormitorio', 'num_bano',
                         'cat_tipo', 'cat_venta', 'num_latitud', 
                         'num_longitud', 'tex_titulo', 'tex_descripcion')
colnames(dataset)        <- nombres_variables
colnames(dataset_kaggle) <- nombres_variables

# Variables como la ciudad o si se vendió la propiedad no nos son de interés
# porque tienen un único valor y no aportan a la variabilidad de la predicción.
dataset <- dataset |> select(-c('id_ciudad', 'cat_venta'))
dataset_kaggle <- dataset_kaggle |> select(-c('id_ciudad', 'cat_venta'))

# 2| Limpieza -------------------------------------------------------------
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
