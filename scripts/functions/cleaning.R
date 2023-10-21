limpiar_casa <- function(.dataset) {
  .dataset <- .dataset |> 
    mutate(tipo_prop = str_extract(tex_titulo,"([Aa]partamentos?|apto|ap|duplex|penthouse|apartaestudio|ap[a-z]estudio|[Cc]asas?)")) |> 
    mutate(tipo_prop_desc = str_extract(tex_descripcion,"([Aa]partamentos?|apto|duplex|penthouse|apartaestudio|aparta estudio|ap[a-z+]estudio|[Cc]asas?)" )) |> 
    mutate(new_type = ifelse(is.na(tipo_prop),tipo_prop_desc, tipo_prop)) |> 
    mutate(d_type = case_when( new_type == "casa" ~ 1,
                               new_type == "Casa" ~ 1,
                               new_type == "casas" ~ 1)) |> 
    mutate(d_type = ifelse(is.na(d_type),0, 1))
  
  # TODO. Verificar que el tipo de la propiedad sí sea casa o apartamento y sobreescribir la variable.
  .dataset <- .dataset |>
    mutate(cat_tipo_2 = ifelse(grepl("casa", tex_descripcion), "casa", cat_tipo)) |> 
    mutate(cat_tipo_2 = ifelse(grepl("apto|apartamento", tex_descripcion), "apartamento", cat_tipo_2)) 
  
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
    mutate(num_mt2 = ifelse(num_mt2 > 600 & cat_tipo=="apartamento", NA , num_mt2)) |>  
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
