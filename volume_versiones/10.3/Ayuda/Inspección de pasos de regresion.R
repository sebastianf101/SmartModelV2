#################################################################
# Script para inspeccionar el detalle de pasos de la regresión. #
#################################################################

fs::path(bsm_path, "Scripts/Carga Parametros.R") |> source()

# Inicio de script -------------------
readRDS("DF_completo.Rdat" |> 
          with_working_path()) -> df

"Res_Logit_Trad.Rdat" |> 
  with_working_path() |> readRDS() |> 
  pluck("det", "vars.fwd.res") |> 
  group_by(Paso) |> 
  mutate(Puesto = row_number(desc(LRT))) |> 
  relocate(Paso, Puesto, .before = Df) |> 
  readr::write_csv("./Trabajo/Detalle_Pasos_FwdK.csv")

"Detalle_Pasos_FwdK.csv" |> 
  with_working_path() |> 
  readr::read_csv() -> df_detalle_pasos

Paso <- 1

vars_paso <- function(detalle, paso) {
  detalle |> 
    filter(Paso==paso) |> 
    pull(Variable) -> vars_paso
  return(vars_paso)
}

logistica_simple <- function(df, variables) {
  
  df |> 
    select(Good, all_of(variables)) -> df_reg
  
  stats::glm(Good ~ ., family = binomial, data = df_reg) |> 
    broom::tidy() |> arrange(p.value) -> res_reg
  
  return(res_reg)
}

# Resultado paso 1
df |> logistica_simple(df_detalle_pasos |> vars_paso(paso=1))

# Resultado paso 2
df |> logistica_simple(df_detalle_pasos |> vars_paso(paso=2))

# Se pueden añadir otras variables para ver alternativas.
# Reemplazar Otra_variable_g por la variable a analizar.
# df |> logistica_simple(c("Otra_variable_g", df_detalle_pasos |> vars_paso(paso=2)))

# Fin de script -----------------

