library(plumber)
'Tableros/Explorador_IVs_API_spec_integr.R' |> 
  with_home_path() |> 
  pr() |> 
  pr_run(host = "0.0.0.0", port = 3838)



