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
                                    pattern = '(.*) (\d{1,2}) (bano(s)?|baño(s)?) (.*)', 
                                    group   = 2)) |> 
    mutate(num_bano = case_when(is.na(num_bano) ~ num_bano_2,
                                TRUE ~ num_bano)) |> 
    select(-c('num_bano_2'))
  
  return(.dataset)
}
