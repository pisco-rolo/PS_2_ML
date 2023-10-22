# 1| Imputar --------------------------------------------------------------
dataset <- dataset |> 
  select(-c('num_area_total', 'num_area_cubierta', 'num_habitacion', 'cat_tipo',
            'tex_titulo', 'tex_descripcion', 'num_rooms', 'id_estacion',
            'nom_estacion', 'lat_estacion', 'lon_estacion', 'tex_estrato',
            'tex_piso')) |> 
  group_by(bin_casa, nom_localidad, cat_estrato) |> 
  mutate(num_mt2 = case_when(is.na(num_mt2) ~ median(num_mt2, na.rm = TRUE),
                             TRUE ~ num_mt2)) |> 
  mutate(num_bano = case_when(is.na(num_bano) ~ median(num_bano, na.rm = TRUE),
                              TRUE ~ num_bano)) |> 
  ungroup()

dataset_kaggle <- dataset_kaggle |> 
  select(-c('num_area_total', 'num_area_cubierta', 'num_habitacion', 'cat_tipo',
            'tex_titulo', 'tex_descripcion', 'num_rooms', 'id_estacion',
            'nom_estacion', 'lat_estacion', 'lon_estacion', 'tex_estrato',
            'tex_piso')) |> 
  group_by(bin_casa, nom_localidad, cat_estrato) |> 
  mutate(num_mt2 = case_when(is.na(num_mt2) ~ median(num_mt2, na.rm = TRUE),
                             TRUE ~ num_mt2)) |> 
  mutate(num_bano = case_when(is.na(num_bano) ~ median(num_bano, na.rm = TRUE),
                              TRUE ~ num_bano)) |> 
  ungroup()
