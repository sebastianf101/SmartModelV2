# Carga de Rangos
# Asume Parámetros bien cargados y 
# el dataframe df.scores con la muestra de Desarrollo!

# Tables ------------------------------------------------------------------

tryCatch({
  if (empty_param(par_rango_niveles)) 
    # No convierto stop() a error_custom pq aquí se usa para el caso alternativo    
    stop("Atención: Parámetro par_rango_niveles vacío!")
  # Notar que no se usa el param sheet si es json
  load_range(control_file, par_rango_niveles, 
             c("Nombre Nivel", "Regla", "Tasa de malos máxima"), 
             c("level_name", "rule", "mx_allwd_br"), sheet='Valid') |> 
    mutate(mx_allwd_br=as.numeric(mx_allwd_br)) -> tab_niv
  
  tab_niv |> 
    pull(mx_allwd_br) |> 
    check_sorted_score_levels() -> res
  
  tab_niv |> 
    arrange(level_order) |> 
    mutate(mn_allwd_br = lag(mx_allwd_br, default = 0)) |> 
    rename(TM_min = mn_allwd_br, TM_max = mx_allwd_br)
  
}, error = function(e) {
  message(e$message)
  message("Revisar definición de Niveles de Riesgo!")
  message("Usando Niveles de Riesgo basados en cuantiles alternativos!")
  df.scores |> 
    filter(.part == '1_Train') |> 
    mutate(Good=as.numeric(as.character(Good))) |> 
    select(score, Good, .weight) -> tab
  tab |> tab_niv_default_fct()
}
) -> tab_niv

tryCatch({
  if (empty_param(par_rango_segmentos)) 
    # No convierto stop() a error_custom pq aquí se usa para el caso alternativo        
    stop("Atención: Parámetro par_rango_segmentos vacío!")
  load_range(control_file, par_rango_segmentos, 
             c("Nombre Segmento", "Regla"), 
             c("level_name", "rule"), sheet = 'Valid')
}, error = function(e) {
  message(e$message)
  message("Revisar definición de Segmentos!")
  message("Usando Segmento único por default!")
  tibble(level_name='Unico', rule='TRUE', level_order=1)
}
) -> tab_seg

tryCatch({
  if (empty_param(par_rango_reportes)) 
    # No convierto stop() a error_custom pq aquí se usa para el caso alternativo        
    stop("Atención: Parámetro par_rango_reportes vacío!")
  load_range(control_file, par_rango_reportes, 
             c("Variables de corte"), c("report_name"), sheet='Valid') -> tab
  # Incorporo Segmento a la tabla de Reportes si fue declarado y no fue incluido manualmente. 
  tab |> 
    pull(report_name) |> 
    map_lgl(~ stringr::str_detect(.x, "\\bSegmento\\b")) |> 
    any() -> already_seg
  if (!empty_param(par_rango_segmentos) & !already_seg) {
    tibble(report_name='Segmento', level_order=1) |> 
      bind_rows(tab) |> 
      mutate(level_order = row_number()) -> tab
  }
  tab
}, error = function(e) {
  message(e$message)
  message("Revisar definición de Reportes!")
  message("Usando único Reporte por Segmento por default!")
  tibble(report_name='Segmento', level_order=1)
}
) -> tab_rep

#message("Carga exitosa de rangos")

