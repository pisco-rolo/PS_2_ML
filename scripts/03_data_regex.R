if (primeraVez == TRUE) {
  # A| Piso -----------------------------------------------------------------
  dataset <- limpiar_piso(.dataset = dataset)
  dataset_kaggle <- limpiar_piso(.dataset = dataset_kaggle) 
  
  summary(dataset$num_piso)
  
  # B| Antigüedad -----------------------------------------------------------
  # Nota. La antiguedad no la incluimos por la poca cantidad de observaciones.
  # En particular, pudimos obtener 274 datos de la antiguedad de los 
  # apartamentos.
  # dataset <- limpiar_antiguedad(.dataset = dataset)
  # dataset_kaggle <- limpiar_antiguedad(.dataset = dataset_kaggle)
  
  # C| Parqueadero ----------------------------------------------------------
  dataset <- dataset |>
    mutate(bin_parqueadero = as.numeric(str_detect(string = tex_descripcion, pattern = '[Gg]arajes?|[Pp]arqueaderos?')))
  
  dataset_kaggle <- dataset_kaggle |> 
    mutate(bin_parqueadero = as.numeric(str_detect(string = tex_descripcion, pattern = '[Gg]arajes?|[Pp]arqueaderos?')))
  
  # D| Elevador -------------------------------------------------------------
  dataset <- dataset |> 
    mutate(bin_elevador = as.numeric(str_detect(string = tex_descripcion, pattern = 'ascensor|elevador')))
  
  dataset_kaggle <- dataset_kaggle |> 
    mutate(bin_elevador = as.numeric(str_detect(string = tex_descripcion, pattern = 'ascensor|elevador')))

  # E| Remodelado -----------------------------------------------------------
  dataset <- dataset |> 
    mutate(bin_remodelada = as.numeric(str_detect(string = tex_descripcion, pattern = 'remodelad[ao]')))
  
  dataset_kaggle <- dataset_kaggle |> 
    mutate(bin_remodelada = as.numeric(str_detect(string = tex_descripcion, pattern = 'remodelad[ao]')))

  # F| Walking closet -------------------------------------------------------
  dataset <- dataset |> 
    mutate(bin_walking_closet = as.numeric(str_detect(string = tex_descripcion, pattern = 'walking?')))
  
  dataset_kaggle <- dataset_kaggle |> 
    mutate(bin_walking_closet = as.numeric(str_detect(string = tex_descripcion, pattern = 'walking?')))
  
  # Exportar ----------------------------------------------------------------
  saveRDS(object = dataset, file = paste0(directorioDatos, 'datos_regex.rds'))
  saveRDS(object = dataset_kaggle, 
          file = paste0(directorioDatos, 'datos_regex_evaluacion.rds'))
  
} else {
  dataset <- readRDS(file = paste0(directorioDatos, 'datos_regex.rds'))
  dataset_kaggle <- readRDS(file = paste0(directorioDatos, 'datos_regex_evaluacion.rds'))
}
