# 1| Preparación de los datos ---------------------------------------------
# Solucionamos problemas de la base de datos. En particular, que los nombres
# sean consistentes. Si la variable inicia con 'cat_', indica que es 
# categórica y ello facilita la implementación del modelo.
dataset <- dataset |> 
  rename(cat_localidad = nom_localidad) |> 
  mutate(across(c(num_mt2, num_bano), ~replace_na(., mean(., na.rm=TRUE)))) 

dataset_kaggle <- dataset_kaggle |> 
  rename(cat_localidad = nom_localidad) |> 
  mutate(across(c(num_mt2, num_bano), ~replace_na(., mean(., na.rm=TRUE)))) 

# 2| Predicción -----------------------------------------------------------
# 2.1| xgboost ------------------------------------------------------------
# Definimos la grilla donde se buscarán los hiperparámetros que maximizan el
# pronóstico por fuera de muestra.
tune_grid_xgboost <- grid_regular(
  tree_depth(range = c(1L, 8L), trans = NULL),
  trees(range = c(100L, 2000L), trans = NULL),
  learn_rate(range = c(0.01, 0.3), trans = NULL),
  mtry(range = c(3L, 6L), trans = NULL),
  levels = c(tree_depth = 4, trees = 5, learn_rate = 4, mtry = 2)
)

# Definimos el motor del modelo. En particular, dejamos fijos algunos 
# hiperparámetros que no consideramos relevantes para la validación cruzada.
xgboost_model <- boost_tree(
  tree_depth = tune(), 
  trees = tune(),
  learn_rate = tune(),
  mtry = tune(), 
  min_n = 30,
  loss_reduction = 0,
  sample_size = .5
  ) |> 
  set_mode('regression') |> 
  set_engine('xgboost', objective = 'reg:squarederror')

# La base de datos que entra no puede tener un campo de geometría porque la
# implementación no lo permite. Por tanto, la eliminamos.
recipe_xgboost <- recipe(num_precio ~ ., 
                         data = dataset |> st_drop_geometry()) |> 
  update_role(id_hogar, new_role = 'ID') |> 
  step_dummy(all_nominal_predictors()) |> 
  step_poly(num_distancia_calles, degree = 2)

# Definimos el flujo de trabajo, el cual consta de aplicar un modelo a una
# receta (es decir, una selección dada de variables explicativas para una
# variable dependiente).
wf_xgboost <- workflow() |> 
  add_recipe(recipe_xgboost) |> 
  add_model(xgboost_model)

# Ponemos una semilla adicional, pues pasos anteriores pueden haber tenido un
# impacto en el generador de números pseudo-aleatorios. Así mismo, realizamos
# una validación espacial por bloques, incluyendo cinco folds.
set.seed(123)
block_folds <- spatial_block_cv(dataset, v = 5)

if (primeraVez == TRUE) {
  # Para cada combinación de parámetros, asignamos un valor de MAE en la
  # validación cruzada por bloques espaciales.
  tune_xgboost <- tune_grid(
    wf_xgboost,
    resamples = block_folds,
    grid = tune_grid_xgboost,
    metrics = metric_set(mae)
  )
  
  # El código tardó más de 3 horas en correr, por lo que es preferible no
  # ejecutarlo nuevamente. En su lugar, guardamos los resultados de los
  # hiperparámetros y, con ellos (señalados en el 'submit' de Kaggle), 
  # realizamos una estimación de la muestra de evaluación.
  saveRDS(object = tune_xgboost,
          file = paste0(directorioDatos, 'optim_parms_xgboost_2.rds'))
  
  best_parms_xgboost <- select_best(tune_xgboost, metric = 'mae')
  definitive_xgboost <- finalize_workflow(
    x = wf_xgboost, 
    parameters = best_parms_xgboost
  )
  
  definitive_xgboost_fit <- fit(object = definitive_xgboost, 
                                data   = dataset)
  
  # Mostramos las variables más importantes para el boosting.
  base_exp     = 1
  heightExp    = 1
  widthExp     = 1.2
  scale_factor = base_exp/widthExp
  
  graficaExportar <- definitive_xgboost_fit |> 
    extract_fit_parsnip() |> 
    vip(num_features = 10) +
    labs(title    = '',
         subtitle = '',
         caption  = '',
         x        = 'Porcentaje de importancia',
         y        = 'Variables más importantes') +
    scale_y_continuous(expand = expansion(mult = c(0, .05))) +
    theme_classic() +
    theme(legend.position = "bottom")
  
  nombreArchivo <- 'importancia_xgboost.png'
  ggsave(filename = paste0(directorioResultados, nombreArchivo), plot = graficaExportar,
         width = 6 * widthExp, height = 4 * heightExp * widthExp, scale = scale_factor)
  
  # Evaluamos el MAE de la validación cruzada para tener una noción del error
  # que podríamos encontrar. En particular, el error es cercano a los 156.7', 
  # calculado con el MAE, y tiene una desviación estándar de 5.4'. 
  prediccion <- tibble(
    id_hogar = dataset$id_hogar,
    precio_obs = dataset$num_precio,
    precio_est = predict(definitive_xgboost_fit, new_data = dataset) |> _$.pred
  )
  
  tune_xgboost |> show_best(metric = 'mae', n = 5) 
  
  # Finalmente, generamos la predicción de los datos por fuera de muestra y
  # guardamos los resultados para Kaggle. Con este modelo, el error es cercano
  # a los 207' por fuera de muestra, calculado con el MAE.
  prediccion <- tibble(
    property_id = dataset_kaggle$id_hogar,
    price = predict(definitive_xgboost_fit, new_data = dataset_kaggle) |> 
      _$.pred
  )
  
  # Nota. Dejamos comentada la exportación para no modificar el archivo que ya
  # publicamos en Kaggle.
  write.csv(x = prediccion,
            file = paste0(directorioResultados, 'xgboost_imp1_hip2.csv'),
            row.names = FALSE)
  
} else {
  tune_xgboost <- readRDS(file = paste0(directorioDatos, 
                                        'optim_parms_xgboost_2.rds'))
  prediccion_xgboost <- read.csv(file = paste0(directorioResultados, 
                                               'xgboost_imp1_hip2.csv'))
}

# 2.2| lightgbm -----------------------------------------------------------
# Los hiperparámetros del 'lightgbm' son idénticos a los del 'xgboost'. Luego,
# usaremos la misma grilla para recorrer los diferentes posibles valores.
lightgbm_model <- boost_tree(
  tree_depth = tune(), 
  trees = tune(),
  learn_rate = tune(),
  mtry = tune(), 
  min_n = 30,
  loss_reduction = 0,
  sample_size = .5
) |> 
  set_mode('regression') |> 
  set_engine('lightgbm', objective = "mae")

# Así mismo, el tratamiento de los datos también es el mismo. Por consiguiente,
# utilizaremos la misma receta que con 'xgboost'.
wf_lightgbm <- workflow() |> 
  add_recipe(recipe_xgboost) |> 
  add_model(lightgbm_model)

# Además, usaremos los mismos bloques espaciales para validar la capacidad
# predictiva de los distintos modelos, por lo que no volvemos a generar el
# objeto 'block_folds'.
if (primeraVez == TRUE) {
  # Para cada combinación de parámetros, asignamos un valor de MAE en la
  # validación cruzada por bloques espaciales.
  tune_lightgbm <- tune_grid(
    wf_lightgbm,
    resamples = block_folds,
    grid = tune_grid_xgboost,
    metrics = metric_set(mae)
  )
  
  # El código tardó más de 3 horas en correr, por lo que es preferible no
  # ejecutarlo nuevamente. En su lugar, guardamos los resultados de los
  # hiperparámetros y, con ellos (señalados en el 'submit' de Kaggle), 
  # realizamos una estimación de la muestra de evaluación.
  saveRDS(object = tune_lightgbm,
          file = paste0(directorioDatos, 'optim_parms_lightgbm_2.rds'))
  
  best_parms_lightgbm <- select_best(tune_lightgbm, metric = 'mae')
  definitive_lightgbm <- finalize_workflow(
    x = wf_lightgbm, 
    parameters = best_parms_lightgbm
  )
  
  definitive_lightgbm_fit <- fit(object = definitive_lightgbm,
                                 data   = dataset)
  
  # Evaluamos el MAE de la validación cruzada para tener una noción del error
  # que podríamos encontrar. En particular, el error es cercano a los 155.7', 
  # calculado con el MAE, y tiene una desviación estándar de 8.2'. 
  prediccion <- tibble(
    id_hogar = dataset$id_hogar,
    precio_obs = dataset$num_precio,
    precio_est = predict(definitive_lightgbm_fit, new_data = dataset) |> _$.pred
  )
  
  tune_lightgbm |> show_best(metric = 'mae', n = 5) 
  
  # Finalmente, generamos la predicción de los datos por fuera de muestra y
  # guardamos los resultados para Kaggle. Con este modelo, el error es cercano
  # a los 207' por fuera de muestra, calculado con el MAE.
  prediccion <- tibble(
    property_id = dataset_kaggle$id_hogar,
    price = predict(definitive_lightgbm_fit, new_data = dataset_kaggle) |> 
      _$.pred
  )
  
  # Nota. Dejamos comentada la exportación para no modificar el archivo que ya
  # publicamos en Kaggle.
  write.csv(x = prediccion,
            file = paste0(directorioResultados, 'lightgbm_imp1_hip2.csv'),
            row.names = FALSE)
  
} else {
  tune_xgboost <- readRDS(file = paste0(directorioDatos, 
                                        'optim_parms_lightgbm_2.rds'))
  prediccion_xgboost <- read.csv(file = paste0(directorioResultados, 
                                               'lightgbm_imp1_hip2.csv'))
}
# 2.3| Linear regression --------------------------------------------------------
# 2.4| Ridge --------------------------------------------------------
tune_grid_ridge <- grid_regular(
  # La penalización va desde 0.0001 hasta 1,000.
  penalty(range = c(0.001, 1000)), # Relacionado con la penalización a la función de pérdida.
  levels  = 100
)

ridge_model <- linear_reg(
  mixture = 0, 
  penalty = tune()
) %>%
  set_mode("regression") %>%
  set_engine("glmnet")

recipe_ridge <- recipe(num_precio ~ .,
                            data = dataset |> 
                              st_drop_geometry()) |> 
  update_role(id_hogar, new_role = 'ID') |> 
  # Una muestra de entrenamiento puede no tener todas las localidades, por lo 
  # que es necesario que se asigne categorías anteriormente no vistas a la
  # categoría 'new'.
  step_novel(all_nominal_predictors()) |> 
  step_poly(num_distancia_calles, num_distancia_mall, num_distancia_tm,
  degree = 2) |>
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors()) |> 
  # TODO. La interacción no la hemos podido hacer funcionar.
  #step_interact(terms = ~ bin_zonaResidencial:bin_parqueadero +
                  # starts_with("cat_estrato"):bin_zonaLaboral +
                  # starts_with("cat_estrato"):starts_with("cat_localidad") +
                  #bin_casa:bin_parqueadero
                # num_mt2:starts_with("cat_estrato") +
                # num_mt2:starts_with("cat_localidad") +
                # num_mt2:num_piso
  #) |>
  step_normalize(all_double_predictors())

wf_ridge <- workflow() |> 
  add_recipe(recipe_ridge) |> 
  add_model(ridge_model)

if (primeraVez == TRUE) {
  tune_ridge <- tune_grid(
    wf_ridge,
    resamples = block_folds,
    grid = tune_grid_ridge,
    metrics = metric_set(mae)
  )
}

saveRDS(object = tune_ridge,
        file = paste0(directorioDatos, 'optim_parms_ridge_1.rds'))

best_parms_ridge <- select_best(tune_ridge, metric = 'mae')
definitive_ridge <- finalize_workflow(
  x = wf_ridge,
  parameters = best_parms_ridge
)

definitive_ridge_fit <- fit(object = definitive_ridge,
                                 data   = dataset)

prediccion <- tibble(
  id_hogar = dataset$id_hogar,
  precio_obs = dataset$num_precio,
  precio_est = predict(definitive_ridge_fit, new_data = dataset) |> _$.pred
)

# Evaluamos el MAE de la validación cruzada para tener una noción del error
# que podríamos encontrar. En particular, el error es cercano a los 193.3', 
# calculado con el MAE, y tiene una desviación estándar de 5.2'. 
tune_grid_ridge |> show_best(metric = 'mae', n = 5) 

prediccion <- tibble(
  property_id = dataset_kaggle$id_hogar,
  price = predict(definitive_elasticNet_fit, new_data = dataset_kaggle) |> 
    _$.pred
)

# Nota. Dejamos comentada la exportación para no modificar el archivo que ya
# publicamos en Kaggle.
write.csv(x = prediccion,
          file = paste0(directorioResultados, 'elasticNet_imp1_hip2.csv'),
          row.names = FALSE)

} else {
  tune_elasticNet <- readRDS(file = paste0(directorioDatos,
                                           'optim_parms_elasticNet_2.rds'))
  prediccion_elasticNet <- read.csv(file = paste0(directorioResultados, 
                                                  'elasticNet_imp1_hip2.csv'))
}
  
# 2.5| Lasso --------------------------------------------------------
# 2.6| Elastic net --------------------------------------------------------
# Es importante que uno de los modelos genere predicciones suaves y no a partir
# de particiones de los datos, pues puede haber regularidades que se capturan
# con funciones lineales o cuadráticas típicas. Al estar Ridge y Lasso 
# incluidos en elastic net, preferimos usar elastic net.
tune_grid_elasticNet <- grid_regular(
  # La penalización va desde 0.0001 hasta 1,000.
  penalty(range = c(0.001, 1000), trans = NULL), # Relacionado con la penalización a la función de pérdida.
  mixture(range = c(0, 1), trans = NULL), # Relacionado con la ponderación a Lasso.
  levels = c(penalty = 100, mixture = 20)
)

elasticNet_model <- linear_reg(
  penalty = tune(),
  mixture = tune()
) |> 
  set_mode('regression') |> 
  set_engine('glmnet')

recipe_elasticNet <- recipe(num_precio ~ .,
                            data = dataset |> 
                              st_drop_geometry()) |> 
  update_role(id_hogar, new_role = 'ID') |> 
  # Una muestra de entrenamiento puede no tener todas las localidades, por lo 
  # que es necesario que se asigne categorías anteriormente no vistas a la
  # categoría 'new'.
  step_novel(all_nominal_predictors()) |> 
  step_poly(num_distancia_calles, num_distancia_mall, num_distancia_tm, degree = 2) |>
  step_dummy(all_nominal_predictors()) |> 
  step_zv(all_predictors()) |> 
  # TODO. La interacción no la hemos podido hacer funcionar.
  step_interact(terms = ~ bin_zonaResidencial:bin_parqueadero +
                  # starts_with("cat_estrato"):bin_zonaLaboral +
                  # starts_with("cat_estrato"):starts_with("cat_localidad") +
                  bin_casa:bin_parqueadero
                  # num_mt2:starts_with("cat_estrato") +
                  # num_mt2:starts_with("cat_localidad") +
                  # num_mt2:num_piso
  ) |>
  step_normalize(all_double_predictors())
  
wf_elasticNet <- workflow() |> 
  add_recipe(recipe_elasticNet) |> 
  add_model(elasticNet_model)

if (primeraVez == TRUE) {
  tune_elasticNet <- tune_grid(
    wf_elasticNet,
    resamples = block_folds,
    grid = tune_grid_elasticNet,
    metrics = metric_set(mae)
  )
  
  saveRDS(object = tune_elasticNet,
          file = paste0(directorioDatos, 'optim_parms_elasticNet_2.rds'))
  
  best_parms_elasticNet <- select_best(tune_elasticNet, metric = 'mae')
  definitive_elasticNet <- finalize_workflow(
    x = wf_elasticNet,
    parameters = best_parms_elasticNet
  )
  
  definitive_elasticNet_fit <- fit(object = definitive_elasticNet,
                                   data   = dataset)
  
  prediccion <- tibble(
    id_hogar = dataset$id_hogar,
    precio_obs = dataset$num_precio,
    precio_est = predict(definitive_elasticNet_fit, new_data = dataset) |> _$.pred
  )
  
  # Evaluamos el MAE de la validación cruzada para tener una noción del error
  # que podríamos encontrar. En particular, el error es cercano a los 193.3', 
  # calculado con el MAE, y tiene una desviación estándar de 5.2'. 
  tune_elasticNet |> show_best(metric = 'mae', n = 5) 
  
  prediccion <- tibble(
    property_id = dataset_kaggle$id_hogar,
    price = predict(definitive_elasticNet_fit, new_data = dataset_kaggle) |> 
      _$.pred
  )
  
  # Nota. Dejamos comentada la exportación para no modificar el archivo que ya
  # publicamos en Kaggle.
  write.csv(x = prediccion,
            file = paste0(directorioResultados, 'elasticNet_imp1_hip2.csv'),
            row.names = FALSE)
  
} else {
  tune_elasticNet <- readRDS(file = paste0(directorioDatos,
                                           'optim_parms_elasticNet_2.rds'))
  prediccion_elasticNet <- read.csv(file = paste0(directorioResultados, 
                                                  'elasticNet_imp1_hip2.csv'))
}
  
# 2.7| Ensemble -----------------------------------------------------------
# Los 3 modelos anteriores tienen ventajas y desventajas. Sin embargo, es 
# posible combinar las bondades de las diferentes predicciones mediante una
# combinación de pronósticos. Por ello, creamos una base de datos que
# contiene las predicciones de los tres modelos anteriores.
dataset_ensemble <- tibble(
  id_hogar = dataset$id_hogar,
  num_precio = dataset$num_precio,
  xgboost = predict(definitive_xgboost_fit, new_data = dataset) |> _$.pred,
  lightgbm = predict(definitive_lightgbm_fit, new_data = dataset) |> _$.pred,
  elasticNet = predict(definitive_elasticNet_fit, new_data = dataset) |> _$.pred
)

dataset_kaggle_ensemble <- tibble(
  id_hogar = dataset_kaggle$id_hogar,
  xgboost = predict(definitive_xgboost_fit, new_data = dataset_kaggle) |> _$.pred,
  lightgbm = predict(definitive_lightgbm_fit, new_data = dataset_kaggle) |> _$.pred,
  elasticNet = predict(definitive_elasticNet_fit, new_data = dataset_kaggle) |> _$.pred
)

# A las predicciones es importante agregarles su dimensión espacial, pues
# la validación por bloques necesita del sentido de contiguidad.
dataset_ensemble <- inner_join(x = dataset_ensemble, 
                               y = dataset |> select(c('id_hogar'))) |> 
  st_as_sf()

# Ponemos una semilla adicional, pues pasos anteriores pueden haber tenido un
# impacto en el generador de números pseudo-aleatorios. Así mismo, realizamos
# una validación espacial por bloques, incluyendo cinco folds.
set.seed(123)
block_folds <- spatial_block_cv(dataset_ensemble, v = 5)

# Nota. La forma automática toma una combinación lineal de todos los modelos
# posibles -es decir, incluyendo cualquier combinación entre los 
# hiperparámetros-. Sin embargo, ello no parece adecuado. Por consiguiente, a
# continuación creamos, manualmente, la combinación, tal que incluya únicamente
# la mejor combinación de hiperparámetros.
# Definimos la grilla donde se buscarán los hiperparámetros que maximizan el
# pronóstico por fuera de muestra.
tune_grid_emsemble <- grid_regular(
  # La penalización va desde 0.0001 hasta 1,000.
  penalty(range = c(-4, 3), trans = log10_trans()), # Relacionado con la penalización a la función de pérdida.
  mixture(range = c(0, 1), trans = NULL), # Relacionado con la ponderación a Lasso.
  levels = c(penalty = 20, mixture = 10)
)

ensemble_model <- linear_reg(
  penalty = tune(),
  mixture = tune()
) |> 
  set_mode('regression') |> 
  set_engine('glmnet')

recipe_ensemble <- recipe(num_precio ~ ., 
                          data = dataset_ensemble |> st_drop_geometry()) |> 
  update_role(id_hogar, new_role = 'ID')

wf_ensemble <- workflow() |> 
  add_recipe(recipe_ensemble) |> 
  add_model(ensemble_model)

if (primeraVez == TRUE) {
  tune_ensemble <- tune_grid(
    wf_ensemble,
    resamples = block_folds,
    grid = tune_grid_emsemble,
    metrics = metric_set(mae)
  )
  
  saveRDS(object = tune_ensemble,
          file = paste0(directorioDatos, 'optim_parms_ensemble_2.rds'))
  
  best_parms_ensemble <- select_best(tune_ensemble, metric = 'mae')
  definitive_ensemble <- finalize_workflow(
    x = wf_ensemble,
    parameters = best_parms_ensemble
  )
  
  definitive_ensemble_fit <- fit(object = definitive_ensemble,
                                 data   = dataset_ensemble)
  
  prediccion <- tibble(
    id_hogar = dataset_ensemble$id_hogar,
    precio_obs = dataset_ensemble$num_precio,
    precio_est = predict(definitive_ensemble_fit, new_data = dataset_ensemble) |> 
      _$.pred
  )
  
  # Evaluamos el MAE de la validación cruzada para tener una noción del error
  # que podríamos encontrar. En particular, el error es cercano a los 193.3', 
  # calculado con el MAE, y tiene una desviación estándar de 5.2'. 
  tune_elasticNet |> show_best(metric = 'mae', n = 5) 
  
  prediccion <- tibble(
    property_id = dataset_kaggle$id_hogar,
    price = predict(definitive_ensemble_fit, new_data = dataset_kaggle_ensemble) |> 
      _$.pred
  )
  
  # Nota. Dejamos comentada la exportación para no modificar el archivo que ya
  # publicamos en Kaggle.
  write.csv(x = prediccion,
            file = paste0(directorioResultados, 'ensemble_imp1_hip2.csv'),
            row.names = FALSE)
  
} else {
  tune_elasticNet <- readRDS(file = paste0(directorioDatos,
                                           'optim_parms_ensemble_2.rds'))
  prediccion_elasticNet <- read.csv(file = paste0(directorioResultados, 
                                                  'ensemble_imp1_hip2.csv'))
}
