# 1| Importar -------------------------------------------------------------
# Importamos la base de datos de entrenamiento y de evaluación.
dataset <- read.csv(file = paste0(Directoriodatos2, 'train.csv'))
dataset_kaggle <- read.csv(file = paste0(Directoriodatos2, 'test.csv'))


 
# Definimos los nombres de las columnas tal y como trabajaremos en el futuro.
nombres_variables   <- c('id_hogar', 'id_ciudad', 'num_precio', 'cat_mes', 
                         'cat_ano', 'num_area_total', 'num_area_cubierta', 
                         'num_habitacion', 'num_dormitorio', 'num_bano',
                         'cat_tipo', 'cat_venta', 'num_latitud', 
                         'num_longitud', 'tex_titulo', 'tex_descripcion')
colnames(dataset)        <- nombres_variables
colnames(data_kaggle) <- nombres_variables

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

# kev, import data
test <- read.csv("~/Desktop/Big Data and Maching learning /Repositorios /PS_2_ML/Directoriodatos2/test.csv")
View(test)

train <- read.csv("~/Desktop/Big Data and Maching learning /Repositorios /PS_2_ML/Directoriodatos2/train.csv")
View(train)

# 1| Importar -------------------------------------------------------------------------------------------
################################################################################
##################################  PS_2_ML  ###################################
############################ cleaning data -Kevin,revisar ###################################
# erase
rm(list = ls())

################## craga de datos desde mi compu ##########################
library(readr)
test <- read_csv("test.csv")
View(test)

library(readr)
train <- read_csv("train.csv")
View(train)

# 1. Exploración inicial de datos
head(test)
summary(test)

head(train)
summary(train)

#### como el objetivo es contrstuir un modelo predictivo  utilizando estos datos, 
### generalmente combinaría los conjuntos de datos de entrenamiento y prueba en uno solo
###  Esto te permitirá entrenar el modelo en el conjunto de entrenamiento y luego evaluarlo en el conjunto de prueba para medir su rendimiento.

library(dplyr)

# Combinar conjuntos de datos
combined_data <- bind_rows(train, test)

head(combined_data)
View(combined_data)


# Paso 2: Manejo de valores faltantes
# se podría llenar los valores faltantes en 'surface_total' y 'surface_covered' con 0
# Las columnas surface_total y  'surface_covered' se refieren a las características de una propiedad inmobiliaria, como un apartamento o una casa. 
# Estas columnas pueden representar el área total de la propiedad y el área cubierta de la propiedad, respectivamente.

combined_data <- combined_data %>%
  mutate(surface_total = ifelse(is.na(surface_total), 0, surface_total),
         surface_covered = ifelse(is.na(surface_covered), 0, surface_covered))



# Limpieza de la variable 'property_type' basada en la descripción
combined_data <- combined_data %>%
  mutate(property_type_2 = ifelse(grepl("casa", description), "casa", property_type))

combined_data <- combined_data %>%
  mutate(property_type_2 = ifelse(grepl("apto|apartamento", description), "apartamento", property_type_2)) %>%
  select(-property_type)

# Paso 3: Crear nuevas características o variables
# Por ejemplo, si queremos crear una variable que indique si la propiedad tiene garaje...
combined_data <- combined_data %>%
  mutate(has_garage = ifelse(grepl("garage", description, ignore.case = TRUE), 1, 0))


# se utiliza para extraer y convertir información sobre el número de piso desde la columna 'description' en 'combined_data' y manejar posibles errores o valores poco realistas en los datos
# Creación de la variable 'piso'

library(dplyr)
library(stringr)  # Agrega esta línea para cargar el paquete stringr

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

# Verificar el resumen de los datos combinados
summary(combined_data)









