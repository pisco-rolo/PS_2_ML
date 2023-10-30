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

#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
# 3| Estadística descriptiva ----------------------------------------------
# Si bien los meses o años son variables numéricas, en realidad son categóricas
# para el análisis estadístico.
colnames(dataset)
class(dataset$num_precio)
summary(dataset$num_precio)

dataset <- dataset |> 
  mutate(cat_mes = as.character(cat_mes),
         cat_ano = as.character(cat_ano))

dataset_kaggle <- dataset_kaggle |> 
  mutate(cat_mes = as.character(cat_mes),
         cat_ano = as.character(cat_ano))
library(dplyr)
estadistica_descriptiva <- dataset |> st_drop_geometry() |>
  select(c(starts_with('num_'), starts_with('cat_'),starts_with('bin_'))) |>
  select(-c('cat_mes')) |> 
  tbl_summary(include = everything(),
              type = starts_with('num_') ~ 'continuous2', 
              label = list(num_precio ~ 'Precio del inmueble',
                           cat_ano ~ 'Año', 
                           num_dormitorio ~ 'Número de dormitorios',
                           num_bano ~ 'Número de baños',
                           bin_casa ~ 'Dummy de casa',
                           num_mt2 ~ 'Número de metros cuadrados',
                           num_distancia_universidades ~ 'Distancia a universidades',
                           num_numero_ocio ~ 'Número de lugares de ocio',
                           num_distancia_calles ~ 'Distancia a calles',
                           bin_zonaResidencial ~ 'Dummy de zona residencial',
                           bin_zonaLaboral ~ 'Dummy de zona laboral',
                           num_distancia_tm ~ 'Distancia a estación de Transmilenio',
                           num_distancia_mall ~ 'Distancia al mall',
                           num_distancia_parque ~ 'Distancia al parque',
                           num_area_parque ~ 'Área de parques',
                           num_distancia_cai ~ 'Distancia al CAI',
                           num_distancia_ciclovia ~ 'Distancia a ciclovía',
                           num_numero_parqueaderos ~ 'Número de parqueaderos',
                           cat_estrato ~ 'Estrato socioeconómico',
                           num_piso ~ 'Número de piso',
                           bin_parqueadero ~ 'Dummy de parqueadero',
                           bin_elevador ~ 'Dummy de elevador',
                           bin_remodelada ~ 'Dummy de remodelada',
                           bin_walking_closet ~ 'Dummy de walking closet' ),
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

