#* @apiTitle Explorador de Variables API
#* @apiDescription Dados reporte, variables y grupo devuelve gráfico y tabla de IVs


# Setup -------------------------------------------------------------------

# Librerias
library(plotly)
library(listviewer)
library(gt)
library(plumber)

# Datos iniciales
fs::path(bsm_path, "Scripts/Carga Parametros.R") |> source()
"Modelo.zip" |> 
  with_working_path() |> 
  unzip(exdir = with_working_path("."))

# Por ahora no: "Vars Candidatas en Desarrollo por Grupos" = "Estab_ivs_cand_InS_x_grupo.Rdat", 
estab_ivs_files <- list("Vars Candidatas en Desarrollo" = "Estab_ivs_cand_InS.Rdat",
                        "Vars Modelo en Desarrollo" = "Estab_ivs_mod_InS.Rdat",
                        "Vars Modelo en Nueva Muestra" = "Estab_ivs_mod_OoS.Rdat")

estab_ivs_files |> 
  map(with_working_path) |> 
  keep(fs::file_exists) -> estab_ivs_available

estab_ivs_available |> map(read_rds) -> reports_list

#names(estab_ivs_available)[1] -> reporte

# Funciones API -----------------------------------------------------------

#* Retorna Reportes Disponibles
#* @get /reports
function() {
  list(reports = names(estab_ivs_available))
}

#* Retorna Variables Disponibles
#* @param report nombre del reporte
#* @get /variables
function(report) {
  reports_list[[report]] |> 
    pluck('xvars') |> pluck('variable') |> unique() -> variables
  list(variables = variables)
}

#* Retorna Grupos Disponibles
#* @param report nombre del reporte
#* @get /groups
function(report) {
  reports_list[[report]] |> 
    pluck('xvars') |> pluck('valor_grupo') |> unique() -> groups
  list(groups = groups)
}

#* Retorna plotly htmlwidget de variable elegida
#* @param report nombre del reporte
#* @param variable variable del reporte
#* @serializer htmlwidget
#* @get /plot
function(report, variable) {
  variable -> par_variable
  reports_list[[report]] |> 
    pluck('xvars') |>  
    filter(variable == par_variable) |> 
    mutate(var_name = paste(variable, 'en', valor_grupo)) |> 
    rename(direction = sentido) |> 
    select(tipo, variable, valor_grupo, var_name, direction, iv_tab) -> table
  table |> pluck('tipo', 1) -> var_type
  if (var_type=='Continua') {
    table |> iv_grouped_tab_cont_2_plotly() -> plot
  } else {
    table |> iv_grouped_tab_categ_2_plotly() -> plot
  } 
  plot |> plotly::as_widget()
}

#* @param report Nombre del reporte
#* @param variable Variable del reporte
#* @param width Desired width in pixels
#* @param height Desired height in pixels
#* @param resolution Resolution (ppi)
#* @serializer contentType list(type = "image/png")
#* @get /static_plot
function(report, variable, width = 2540, height = 1600, resolution = 300) {
  variable -> par_variable
  reports_list[[report]] |>
    pluck('xvars') |>
    filter(variable == par_variable) |>
    mutate(var_name = paste(variable, 'en', valor_grupo)) |>
    rename(direction = sentido) |>
    select(tipo, variable, valor_grupo, var_name, direction, iv_tab) -> table
  
  table |> pluck('tipo', 1) -> var_type
  
  if (var_type == 'Continua') {
    table |> iv_grouped_tab_cont_2_plot() -> p
  } else {
    table |> iv_grouped_tab_categ_2_plot() -> p
  }
  
  # Manually open PNG device with custom size
  tmp <- tempfile(fileext = ".png")
  png(tmp, width = as.numeric(width), 
      height = as.numeric(height), units = "px", 
      res = as.numeric(resolution))
  print(p)
  dev.off()
  
  # Return raw image bytes
  readBin(tmp, "raw", file.info(tmp)$size)
}

#* Retorna tabla html de variable y grupos elegida
#* @param report nombre del reporte
#* @param variable variable del reporte
#* @param group grupo de la variable
#* @get /table
function(report, variable, group) {
  variable -> par_variable  
  reports_list[[report]] |> 
    pluck('xvars') |>  
    filter(variable == par_variable) |> 
    mutate(var_name = paste(variable, 'en', valor_grupo)) |> 
    rename(direction = sentido) |> 
    select(tipo, variable, valor_grupo, var_name, direction, iv_tab) |> 
    filter(valor_grupo == group)-> table
  table |> pluck('tipo', 1) -> var_type
  table  |> pluck('iv_tab', 1) -> iv_table
  if (var_type =='Continua') 
    iv_table |> 
    det_iv_cont_gt(title = paste(variable, "en", group)) -> gt_table else 
      iv_table |> 
    det_iv_categ_gt(title = paste(variable, "en", group)) -> gt_table
  gt_table |> gt::as_raw_html() -> html_table
  list(html_table = html_table, var_type = var_type)
}
