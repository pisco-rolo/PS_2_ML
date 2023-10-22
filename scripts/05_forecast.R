# 1| Preparación de los datos ---------------------------------------------
dataset <- dataset |> 
  rename(cat_localidad = nom_localidad)

dataset_kaggle <- dataset_kaggle |> 
  rename(cat_localidad = nom_localidad)

# 2| Estimación del modelo ------------------------------------------------
tune_grid_xgboost <- grid_regular(
  tree_depth(range = c(1L, 8L), trans = NULL),
  trees(range = c(100L, 2000L), trans = NULL),
  learn_rate(range = c(0.01, 0.3), trans = NULL),
  mtry(range = c(3L, 6L), trans = NULL),
  levels = c(tree_depth = 4, trees = 5, learn_rate = 4, mtry = 2)
)

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

recipe_xgboost <- recipe(num_precio ~ ., data = dataset |> st_drop_geometry()) |> 
  update_role(id_hogar, new_role = 'ID') |> 
  # step_log(num_precio, base = exp(1)) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_poly(num_distancia_calles, degree = 2)

wf_xgboost <- workflow() |> 
  add_recipe(recipe_xgboost) |> 
  add_model(xgboost_model)

# Ponemos una semilla adicional.
set.seed(123)
block_folds <- spatial_block_cv(dataset, v = 5)

if (primeraVez == TRUE) {
  tune_xgboost <- tune_grid(
    wf_xgboost,
    resamples = block_folds,
    grid = tune_grid_xgboost,
    metrics = metric_set(mae)
  )
  
  saveRDS(object = tune_xgboost,
          file = paste0(directorioDatos, 'optim_parms_xgboost.rds'))
  
  best_parms_xgboost <- select_best(tune_xgboost, metric = 'mae')
  definitive_xgboost <- finalize_workflow(
    x = wf_xgboost, 
    parameters = best_parms_xgboost
  )
  
  definitive_xgboost_fit <- fit(object = definitive_xgboost, 
                                data   = dataset)
  prediccion <- tibble(
    property_id = dataset_kaggle$id_hogar,
    price = predict(definitive_xgboost_fit, new_data = dataset_kaggle) |> _$.pred
  )
  
  write.csv(x = prediccion, 
            file = paste0(directorioResultados, 'xgboost_imp1_hip1.csv'), 
            row.names = FALSE)
  
} else {
  tune_xgboost <- readRDS(file = paste0(directorioDatos, 'optim_parms_xgboost.rds'))
}


