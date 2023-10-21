if (primeraVez == TRUE) {
  dataset <- crear_variables_geograficas(
    .dataset      = dataset, 
    nombreArchivo = 'datos_geograficos.rds',
    nombreGrafica = 'mapa_variables.png'
  )
  
  dataset_kaggle <- crear_variables_geograficas(
    .dataset      = dataset_kaggle, 
    nombreArchivo = 'datos_geograficos_evaluacion.rds',
    nombreGrafica = 'mapa_variables_evaluacion.png'
  )
} else {
  dataset        <- readRDS(file = paste0(directorioDatos, 
                                          'datos_geograficos.rds'))
  dataset_kaggle <- readRDS(file = paste0(directorioDatos, 
                                          'datos_geograficos_evaluacion.rds'))
}
