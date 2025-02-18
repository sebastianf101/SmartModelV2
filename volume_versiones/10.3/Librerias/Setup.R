
# Set control_file ------------------------------------------------------------

with_home_path("Params/Control de SmartModelStudio.json") -> param_json
with_home_path("Params/Control de SmartModelStudio.xlsx") -> param_xlsx
if (param_json |> fs::file_exists()) 
  param_json |> fs::path_real() -> control_file else 
    if (param_xlsx |> fs::file_exists()) 
      param_xlsx |> fs::path_real() -> control_file else 
        error_custom('No se encontró archivo de Control de Parámetros!', 
                     "i" = "No se encontraron ", param_json, " ni ", param_xlsx,
                     ">"=cli::col_red("Cod 309"))

# Load_dfParam ------------------------------------------------------------

if (control_file |> fs::path_ext() == 'xlsx') {
  dplyr::bind_rows(control_file |> readxl::read_excel(sheet = 'LTM', range = "A6:D1000"), 
                   control_file |> readxl::read_excel(sheet = 'Valid', range = "A6:D20"),
                   control_file |> readxl::read_excel(sheet = 'Proyecto', range = "A6:D1000")
  ) -> df_Param
  df_Param |> 
    rename(parameter=`Parámetro`, value=Valor, type=Tipo) -> df_Param
} else {
  control_file |> 
    jsonlite::fromJSON() |> 
    as_tibble() -> df_Param
}

# CheckParam --------------------------------------------------------------

df_Param |> 
  filter(!is.na(parameter) & !is.na(type)) -> df_Param

# Chequeos.  
df_Param |> 
  filter(! type %in% c('list', 'numeric', 'string', 'data.frame')) |> 
  plyr::empty() -> cnd 
if (!cnd)
  error_custom('Parámetros con tipos incorrectos!', 
               "i" = 'Los Tipos admitidos son list, numeric, string ó data.frame.',
               ">"=cli::col_red("Cod 308"))

# Ojo con que la lista de parámetros está hardcodeada. La dejo como condición necesaria
# Del json controlo todo excepto nombre_archivo
df_Param |> 
  filter(parameter %in% c('par_quick', 'cols_forzadas_a_cat', 'cols_forzadas_a_predictoras', 'cols_no_predictoras', 'cols_nulos_adic', 'data_source_delim', 'data_source_delim_path', 'data_source_odbc_dsn', 'data_source_query', 'data_source_scoring_delim_path', 'data_source_type', 'data_source_val_delim_path', 'keyring_svc_odbc', 'par_cant_reportes', 'par_cor', 'par_cor_show', 'par_discret', 'par_ids', 'par_iv', 'par_iv_cuantiles_gb_min', 'par_iv_tot_gb_min', 'par_iv_tot_min', 'par_maxlevels', 'par_minpts_cat', 'par_minpts1', 'par_minpts2', 'par_nbins1', 'par_nbins2', 'par_perf_bins', 'par_rango_niveles', 'par_rango_reportes', 'par_rango_segmentos', 'par_split', 'par_target', 'par_times', 'par_var_grupo', 'par_vars_segmento', 'par_weight', 'project_title', 'par_minpts_nulos', 'par_conf_level')) |> 
  summarise(q=n()) |> 
  mutate(check = (q >= 41)) |> 
  pull(check) -> cnd 
if (!cnd)
  error_custom('Parámetros faltantes!', 
               "i" = 'Falta alguno de los siguientes parámetros:',
               'par_quick', 'cols_forzadas_a_cat', 'cols_forzadas_a_predictoras', 'cols_no_predictoras', 'cols_nulos_adic', 'data_source_delim', 'data_source_delim_path', 'data_source_odbc_dsn', 'data_source_query', 'data_source_scoring_delim_path', 'data_source_type', 'data_source_val_delim_path', 'keyring_svc_odbc', 'par_cant_reportes', 'par_cor', 'par_cor_show', 'par_discret', 'par_ids', 'par_iv', 'par_iv_cuantiles_gb_min', 'par_iv_tot_gb_min', 'par_iv_tot_min', 'par_maxlevels', 'par_minpts_cat', 'par_minpts1', 'par_minpts2', 'par_nbins1', 'par_nbins2', 'par_perf_bins', 'par_rango_niveles', 'par_rango_reportes', 'par_rango_segmentos', 'par_split', 'par_target', 'par_times', 'par_var_grupo', 'par_vars_segmento', 'par_weight', 'project_title', 'par_minpts_nulos', 'par_conf_level', 
               ">"=cli::col_red("Cod 307"))

# LoadVarsParam -----------------------------------------------------------

df_Param |> 
  select(parameter, value, type) |> 
  pwalk(par_init, topenv()) -> res

# CheckNotebookParams -----------------------------------------------------

cols_forzadas_a_cat |> purrr::discard(string_has_no_data) -> cols_forzadas_a_cat
cols_forzadas_a_predictoras |> purrr::discard(string_has_no_data) -> cols_forzadas_a_predictoras
cols_no_predictoras |> purrr::discard(string_has_no_data) -> cols_no_predictoras 

cols_no_predictoras |> 
  intersect(cols_forzadas_a_predictoras) |>
  string_has_no_data() -> cnd 
if (!cnd)
  error_custom('Las listas cols_no_predictoras y cols_forzadas_a_predictoras deben disjuntas',
               "i" = 'cols_no_predictoras', cols_no_predictoras, 
               "i" = 'cols_forzadas_a_predictoras', cols_forzadas_a_predictoras, 
               ">"=cli::col_red("Cod 306"))

cols_forzadas_a_cat |> 
  intersect(cols_no_predictoras) |>
  string_has_no_data() -> cnd 
if (!cnd)
  error_custom('Las listas cols_no_predictoras y cols_forzadas_a_cat deben disjuntas',
               "i" = 'cols_no_predictoras', cols_no_predictoras, 
               "i" = 'cols_forzadas_a_cat', cols_forzadas_a_cat, 
               ">"=cli::col_red("Cod 305"))

# Post treatment ----------------------------------------------------------

data_source_delim <- data_source_delim  |> stringr::str_replace("\\\\t", "\t")
data_source_delim_path <- data_source_delim_path |> fs::path_abs(start = bsm_path)
data_source_val_delim_path <- data_source_val_delim_path |> fs::path_abs(start = bsm_path)
data_source_scoring_delim_path <- data_source_scoring_delim_path |> fs::path_abs(start = bsm_path)

if (string_has_no_data(par_ids)) {.ids <- '.id'}

#message("Carga exitosa de Parámetros.")

