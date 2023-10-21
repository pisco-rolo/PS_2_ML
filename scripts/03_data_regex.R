if (primeraVez == TRUE) {
  # A| Piso -----------------------------------------------------------------
  dataset <- limpiar_piso(.dataset = dataset)
  dataset_kaggle <- limpiar_piso(.dataset = dataset_kaggle) 
  
  summary(dataset$num_piso)
  
  # B| Antigüedad -----------------------------------------------------------
  
  
  # C| Parqueadero ----------------------------------------------------------
  dataset <- dataset |>
    mutate(bin_parqueadero = ifelse(grepl("garage|parqueadero", tex_descripcion, ignore.case = TRUE), 1, 0))
  
  dataset_kaggle <- dataset_kaggle |> 
    mutate(bin_parqueadero = ifelse(grepl("garage|parqueadero", tex_descripcion, ignore.case = TRUE), 1, 0))
  
  # Exportar ----------------------------------------------------------------
  saveRDS(object = dataset, file = paste0(directorioDatos, 'datos_regex.rds'))
  saveRDS(object = dataset_kaggle, 
          file = paste0(directorioDatos, 'datos_regex_evaluacion.rds'))
  
} else {
  dataset <- readRDS(file = paste0(directorioDatos, 'datos_regex.rds'))
  dataset_kaggle <- readRDS(file = paste0(directorioDatos, 'datos_regex_evaluacion.rds'))
}
