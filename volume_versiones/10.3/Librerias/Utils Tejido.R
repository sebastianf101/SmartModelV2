# Startup Setup
options(encoding = "UTF-8")
# Es necesaria la barra al final! en knitr!

# Asume seteado antes bsm_path

absolute_paths <- function(path_str) 
  fs::path(bsm_path, path_str) |> fs::path_expand() |> paste0("/") 

"" |> absolute_paths() -> home_path
"Reportes" |> absolute_paths() -> report_path_output
"Auxil/Interm" |> absolute_paths() -> report_path_auxil
"Auxil/Cache" |> absolute_paths() -> report_path_cache
"Trabajo" |> absolute_paths() -> working_path

report_path_cache |> fs::dir_create()
report_path_auxil |> fs::dir_create()

with_working_path <- function(x) {fs::path(working_path,x) |> fs::path_abs()}
with_output_path <- function(x) {fs::path(report_path_output,x) |> fs::path_abs()}
with_home_path <- function(x) {fs::path(home_path,x) |> fs::path_abs()}

clean_knit_cache <- function() {
  if (report_path_cache |> fs::dir_exists()) report_path_cache |> fs::dir_ls(all = TRUE) |> fs::file_delete()
  if (report_path_auxil |> fs::dir_exists()) report_path_auxil |> fs::dir_ls(all = TRUE) |> fs::file_delete()
}

clean_loaded_packages <- function() {
  if (!is.null(sessionInfo()$otherPkgs)) {
    all_attached <- paste("package:", names(sessionInfo()$otherPkgs), sep = "")
    suppressWarnings(invisible(lapply(all_attached, detach, character.only = TRUE, unload = TRUE)))
  }
  return(invisible(TRUE))
}

tejer_cuaderno <- function(rmd_file, out_file=NULL, parms=NULL) { 
  rmarkdown::render(
    input = rmd_file, clean = TRUE, 
    output_dir = report_path_output, 
    intermediates_dir = report_path_auxil,
    params = parms   # Hay colisión de nombres si se usa params = params!
    # Se tuvo que mover este parámetro dentro de los cuadernos para que 
    # puedan ejecutarse de forma interactiva además.
    # Un bug en ese modo solo permite paths literales!
    #, knit_root_dir = fs::path_wd()
  )
}




