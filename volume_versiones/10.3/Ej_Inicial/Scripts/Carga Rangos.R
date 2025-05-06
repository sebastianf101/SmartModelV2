# Carga de Rangos
# Asume Parámetros bien cargados y 
# el dataframe df.scores con la muestra de Desarrollo!

# Por defecto crea par_tab_niv_nbins - 1 niveles! 
tab_niv_default <- function(par_df, 
                            par_tab_niv_nbins = 6, 
                            par_minpts = par_minpts2) {
  # Uso:
  # df.scores |> 
  #   filter(.part == '1_Train') |> 
  #   mutate(Good=as.numeric(as.character(Good))) |> 
  #   select(score, Good, .weight) -> tab
  # tab |> tab_niv_default()
  # # A tibble: 5 × 5
  # level_name rule         TM_max level_order TM_min
  # <chr>      <chr>         <dbl>       <int>  <dbl>
  #   1 A          score >= 974 0.0328           1 0     
  # 2 B          score >= 961 0.0521           2 0.0173
  # 3 C          score >= 936 0.0850           3 0.0328
  # 4 D          score >= 890 0.165            4 0.0521
  # 5 E          score < 890  1                5 0.0850
  #
  # Asume par_df con columnas score, Good yt .weight
  assertthat::assert_that(all(c("score", "Good", ".weight") %in% colnames(par_df)))
  
  par_df |> 
    rename(x=score, y=Good) |> 
    eqbin_Lin(nbins = par_tab_niv_nbins, 
              minpts = par_minpts, nvals.min = 2) -> tab
  
  tab$df |> 
    group_by(bin) |> 
    summarise(cut_lo=min(x), cut_median=median(x), cut_mean=mean(x), cut_hi=max(x), 
              CntRec=sum(.weight), CntGood=sum(y*.weight), CntBad=sum((1-y)*.weight)) |> 
    mutate(BadRate=CntBad/CntRec*100) |> 
    arrange(cut_lo) |> 
    mutate(orden = row_number()) -> tab_bins
  
  
  tab_bins |> 
    select(orden, cut_lo, BadRate) |> 
    arrange(desc(cut_lo)) |> 
    mutate(level_order = row_number(), 
           level_name = LETTERS[level_order], 
           TM = BadRate/100, 
           TM_min = lag(TM, default = 0), 
           TM_max = lead(TM, default = 1), 
           cut_last = lag(cut_lo, default = 1000), 
           rule = if_else(orden == 1, 
                          paste('score <', cut_last), 
                          paste('score >=', cut_lo))) -> tab_niv
  
  # Chequeo de tabla
  # La tasa debe ser creciente y 
  # el peor nivel debe ser orden 1
  # Así se verifica la equivalencia entre cut_lo y orden
  assertthat::assert_that(
    tab_niv |> pull(TM) |> check_sorted_score_levels(),
    tab_niv |>
      filter(level_order == max(level_order)) |> 
      mutate(check = (orden == 1)) |> 
      pull(check), 
    msg = "Fallo al crear niveles de riesgo por default! Crearlos manualmente!"
  )
  
  
  tab_niv |> 
    select(level_name, rule, TM_max, level_order, TM_min) -> tab_niv
  
  return(tab_niv)
  
}

# Tables ------------------------------------------------------------------

# Uso:
# > tab_niv_usuario(control_file, par_rango_niveles)
# # A tibble: 6 × 5                                                                                                       
# level_name rule         TM_max level_order TM_min
# <chr>      <chr>         <dbl>       <int>  <dbl>
#   1 BajoBajo   score > 955    0.03           1   0   
# 2 BajoMedio  score > 930    0.06           2   0.03
# 3 BajoAlto   score > 895    0.09           3   0.06
# 4 MedioBajo  score > 865    0.15           4   0.09
# 5 MedioMedio score > 750    0.18           5   0.15
# 6 Alto       score <= 750   1              6   0.18
#

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
  tab |> tab_niv_default()
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

