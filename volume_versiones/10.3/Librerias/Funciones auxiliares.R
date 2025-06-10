
# TODO: Incorporar versión de bin factor que imita a Modeler como opcional. Quedó comentada
# Al aplicar la discretización de un continua ver 

# Gral Utils --------------------------------------------------------------

msg_custom <- function(..., encl_left = '>>>>¡', encl_right = '!<<<<', 
                       call = rlang::caller_env()) {
  cli::format_message(message = c(encl_left, ..., encl_right), .envir = call) 
}

error_custom <- function(..., 
                         encl_left = '>>>>¡', encl_right = '!<<<<', 
                         err=NULL, call = rlang::caller_env()) {
  # NO uso msg_customo porque no separa por lineas!
  cli::cli_abort(message = c(encl_left, err$message, ..., encl_right), 
                 parent = err, .envir = call) 
}

# sec 19.5
commas <- function(...) stringr::str_c(..., collapse = ", ")
show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  invisible(df)
}

# Devuelve el tamaño del objeto en memoria RAM
cuanta_ram <- function(obj) {
  lobstr::obj_size(obj) |> 
    as.numeric() |> 
    scales::label_bytes(units = "auto_si", accuracy = 1)() 
}

# list(NULL, NA, character(0), character(1)) |> purrr::map_lgl(string_has_no_data)
# [1] TRUE TRUE TRUE TRUE
# Ojo que no anda con las versiones de base R
string_has_no_data <- function(c) {
  (rlang::is_missing(c) | rlang::is_null(c) | rlang::is_na(c) | (length(c)==0) | identical(c, character(1))) 
}

# Más general que string_has_no_data. 
# empty_param(par_weigth) == TRUE cuando se deja en blanco. 
empty_param <- function(param) {
  if (rlang::is_missing(param) || 
      rlang::is_null(param) || 
      (length(param)==0)) TRUE 
  else if (is.data.frame(param)) plyr::empty(param) 
  else if (is.character(param)) string_has_no_data(param) 
  else if (is.numeric(param) || is.list(param)) rlang::is_na(param) 
  else error_custom('Los tipos admitidos son list, numeric, string o data.frame.', 
                    "i" = "Se obtuvo {param}", 
                    ">"=cli::col_red("Cod 325")) 
}

# Ej:
# df_Param |> 
#   get_param_value('cols_no_predictoras') |> 
#   csv_string_2_vec -> cols_no_predictoras_new
csv_string_2_vec <- function(s) {
  s  |>  
    stringr::str_remove_all("\\'|") |> 
    stringr::str_remove_all('\\"') |> 
    stringr::str_split(",") |> 
    purrr::map(stringr::str_trim) |> 
    purrr::flatten_chr()
}

last2first <- function(v) {vctrs::vec_c(dplyr::last(v), head(v,-1))}
# 1:10 |> last2first
# [1] 10  1  2  3  4  5  6  7  8  9
first2last <- function(v) {vctrs::vec_c(dplyr::tail(v,-1), dplyr::first(v))}
# 1:10 |> first2last
#  [1]  2  3  4  5  6  7  8  9 10  1

max_na <- function(x, subs=NA) {
  x_sinna <- na.omit(x)
  ifelse(length(x_sinna)==0, subs, max(x_sinna)) 
}

min_na <- function(x, subs=NA) {
  x_sinna <- na.omit(x)
  ifelse(length(x_sinna)==0, subs, min(x_sinna)) 
}

# c(1,2,3) |> is.monotone()
# $res TRUE $incr TRUE
# 
# c(3,2,1) |> is.monotone()
# $res TRUE # $incr FALSE
# 
# c(1,2,1) |> is.monotone()
# $res FALSE $incr FALSE
# 
# To check wether vec is increasing:
# c(1,2,3) |> is.monotone() |> pluck("incr")
#
is.monotone <- function(vec) {
  if (!(vec |> is.unsorted())) 
    return(list(res=TRUE, incr=TRUE)) 
  else if (!(vec |> rev() |> is.unsorted())) 
    return(list(res=TRUE, incr=FALSE))
  else 
    return(list(res=FALSE, incr=FALSE))
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

check_names <- function(names_vector) {
  names_df <- tibble(name = names_vector) |> 
    mutate(
      valid_name = name == make.names(name),
      duplicated_name = duplicated(name) | duplicated(name, fromLast = TRUE)
    )
  
  all_valid <- all(names_df$valid_name)
  no_duplicates <- !any(names_df$duplicated_name)
  
  result <- list(
    all_valid = all_valid,
    no_duplicates = no_duplicates,
    invalid_names = names_df |> filter(!valid_name) |> pull(name),
    duplicated_names = names_df |> filter(duplicated_name) |> pull(name)
  )
  
  return(result)
}
# Parameter management ----------------------------------------------------

# df_Param |> 
#   get_param_value('project_path') 
get_param_value <- function(df, parameter_name) df |> 
  dplyr::filter(parameter == parameter_name) |> 
  dplyr::pull(value)

# df_Param |> 
#   select(parameter, value, type) |> 
#   pwalk(par_init)
par_init <- function(parameter, value, type, envir) {
  switch (type,
          'list' = value |> csv_string_2_vec() |> 
            (\(.) assign(x = parameter, value = ., envir = envir))(),
          'numeric' = value |> as.numeric() |> 
            (\(.) assign(x = parameter, value = ., envir = envir))(),
          'string' =  value |> 
            (\(.) assign(x = parameter, value = ., envir = envir))(),
          'data.frame' =  value |> as_tibble() |> 
            (\(.) assign(x = parameter, value = ., envir = envir))(),
          error_custom('Los tipos admitidos son list, numeric, string o data.frame!', 
                       "i" = "Se obtuvo", type,
                       "i" = "Revisar parámetro", parameter, 
                       ">"=cli::col_red("Cod 310"))
  )
}

# Establece la conexión usando el keyring opr defecto. 
# En Windows es la Credential Store
# Ejemplo de uso df_param |> odbc_connect_w_keyring() -> con
# query_res <- dbGetQuery(con, 'SELECT * FROM [BCRA].[dbo].[PERIODO_ACTUAL]')
# dbDisconnect(con)
odbc_connect_w_keyring <- function(df_param) {
  par_dsn <- df_param |> get_param_value('data_source_odbc_dsn')
  par_svc <- df_param |> get_param_value('keyring_svc_odbc')
  par_user <- keyring::key_list(par_svc)[1,2]
  par_pwd = keyring::key_get(par_svc, par_user)
  list(par_dsn, par_svc, par_user, par_pwd) |> purrr::map(~assert_that::assert_that(is.string(.), noNA(.)))
  assert_that::assert_that(odbcListDataSources() |> filter(name == par_dsn) |> not_empty())
  con <- odbc::dbConnect(odbc::odbc(), dsn = par_dsn, uid = par_user, pwd = par_pwd, timeout = 30, 
                         bigint = 'integer')
  assert_that::assert_that(odbc::dbIsValid(con))
  return(con)
}

# Las tres funciones siguientes se usan para convertir a nulos los valoes que aparecen en 
# Ej: cols_nulos_adic <- c("Contacto_Antiguedad = 0", "Contacto_Antiguedad = NA", "Ant_Laboral = 0")
# Datos_Modelo |> aplic_nulos_adic(cols_nulos_adic) -> Datos_Muestra
# 
anulador <- function(data, var, val) {
  if (class(data[[var]]) == 'numeric') {
    if (val == 'NA') NA_real_ -> new_val 
    else as.numeric(val) -> new_val
  } else if (class(data[[var]]) == 'character') {
    if (val == 'NA') NA_character_ -> new_val 
    else as.character(val) -> new_val
  } else cli::cli_abort(c("Error procesando conversión a nulos", 
                          "i" = "Se esperaba {var} numérica o string"))
  
  data |>  
    mutate("{ `var` }" := na_if(.data[[var]], new_val))
}

aplic_nulos_adic <- function(df, nulos_adic) {
  
  eq2list <- function(cad) {
    cad  |> stringr::str_split("=",simplify = T) |> stringr::str_trim() -> l
    tibble(var = l[1], val = l[2])
  }
  
  nulos_adic  |>  
    reduce(.init = tibble(var = character(), val = character()), 
           .f = ~ .x |> bind_rows(.y |> eq2list())) -> reglas
  
  (reglas$var |> setdiff(df |> colnames())) -> cnd_vec
  if (length(cnd_vec)>0)
    error_custom("Se encontraron reglas con variables inexistentes en los datos!", 
                 "x" = "Se encontrar{?ó/on} la{?s} variable{?s} {cnd_vec}",
                 ">"=cli::col_red("Cod 326")) else 
                   reduce2(.init = df, .x = reglas$var, .y = reglas$val, .f = anulador)
  
}

# bin_monot y auxiliares ------------------------------------------------

cut_v2 <- function(num_vec, breaks, labels = NULL,
                   include.lowest = FALSE, right = FALSE, dig.lab = 3,
                   ordered_result = TRUE, ...) {
  max_v <- max(num_vec, na.rm = T)
  v <- cut(x=num_vec, breaks=breaks, labels=labels, include.lowest=include.lowest, right=right, 
           dig.lab=dig.lab, ordered_result=ordered_result, ...)
  ext_sup <- paste0("[", max_v, "]")
  ve <- forcats::fct_expand(v, ext_sup)
  res <- if_else(num_vec==max_v & is.na(v), factor(ext_sup, levels = levels(ve), ordered = ordered_result), 
                 ve)
  return(res)
}

my_cut <- function(vec, x, y) {
  #' Transform vec by table
  #' (-inf, x[2]) -> y[1]
  #' (x[2], x[3]) -> y[2]
  #' [x(3), x[4]) -> y[3]
  #' ....
  #' [x(n-1), x[n]) -> y[n]
  #' [x(n), inf) -> y[n]
  #' 
  #' case when v <  x[2] then y[1]
  #'      when v <  x[3] then y[2]
  #'      ....
  #'      when v <  x[n] then y[n-1]
  #'      when v >= x[n] then y[n]  
  assertthat::assert_that(is.vector(vec, mode = "numeric"), length(vec) > 0,
                          is.vector(x, mode = "numeric"), length(x) > 1, 
                          is.vector(y, mode = "numeric"), 
                          length(x)==length(y), 
                          is.monotone(x)$incr) # check whether x is increasing.
  l <- length(x)
  vec_tr <- numeric(length = length(vec))
  for (i in 1:length(vec)) {
    v <- vec[i]
    j <- 2
    while (j <= l && v >= x[j]) j <- j + 1
    if (j > l) vec_tr[i] <- y[l] else vec_tr[i] <- y[j-1] 
  }
  return(vec_tr)
}

safe_cor <- purrr::quietly(~ wCorr::weightedCorr(x=..1, y=..2, weights=..3, method = "spearman"))

bin.sf <- function(df, nbins, minpts, nvals.min = 5,
                   verbose = F, tol = 10) {
  
  if (is.null(nbins) || floor(minpts)!=minpts) {
    msg <- paste("bins.sf: parametros inconsistentes", 
                 "\nNumero de bines nulo?", is.null(nbins), 
                 "\nCasos minimos x bin es entero?", minpts)
    return(list(error=T, msg=msg))
  }
  
  df |> 
    mutate(x=round(x,tol)) |> 
    group_by(x) |> 
    summarise(q=n(), w=sum(.weight)) -> df 
  
  df$x |> unique() -> vals 
  length(vals) -> nvals
  
  nvals <- nrow(df)
  if (nvals < nvals.min) {
    msg <- paste("Valores unicos insuficientes!", "Se esperaban", nvals.min, 
                 "valores únicos y se obtuvo", nvals, "Lista de valores: ", 
                 vals |> paste(collapse = ", "))
    return(list(error=T, msg=msg))
  }
  
  if (sum(df$w) < 1) {
    msg <- paste("El peso total no puede ser menor a 1!:",sum(df$w))
    return(list(error=T, msg=msg))
  }
  
  # 
  minbinw <- df |> summarise(w=sum(w)) |> pull(1)
  minbinw <- max(minpts, ceiling(minbinw/nbins))
  binlo <- vector(nbins, mode = df[["x"]] |> mode())
  binhi <- vector(nbins, mode = df[["x"]] |> mode())
  binq <- vector(nbins, mode = "integer")
  binw <- vector(nbins, mode = "double")
  binaw <- cumsum(rep(minbinw, nbins))
  startbin <- TRUE
  k <- 1
  aw <- w <- q <- 0
  for (i in 1:nvals) {
    q <- q + df[[i,"q"]]
    w <- w + df[[i,"w"]]
    aw <- aw + df[[i,"w"]]
    if (startbin) {
      binlo[k] <- df[[i,"x"]]
      startbin <- FALSE
    }
    if (aw >= binaw[k] || i >= nvals) {
      # Cierro el bin[k]
      binhi[k] <- df[[i,"x"]]
      binq[k] <- q
      binw[k] <- w
      k <- k + 1
      w <- q <- 0
      startbin <- TRUE
    }
  }
  k <- k - 1
  if (!startbin) {
    # El último bucket no llegó al mínimo
    # entonces reseteo el actual y lo sumo al anterior
    binhi[k] <- df[[nvals,"x"]]
    binq[k] <- binq[k] + q
    binw[k] <- binw[k] + w
  } # else el último bucket justo llegó al mínimo. En los dos casos k <- k - 1.
  binlo <- binlo[1:k]
  binhi <- binhi[1:k]
  binq <- binq[1:k]
  binw <- binw[1:k]
  
  # Al realizar un round(x, tol) al ppio puedo excluir casos. Corrijo los extremos para que no suceda.
  binlo[1] <- binlo[1] - 10^(-tol)
  binhi[k] <- binhi[k] + 10^(-tol)
  
  tibble(binlo = binlo, binhi = binhi, binq = binq, binw = binw) -> bins
  
  bins |> 
    mutate(ok=(!is.na(binlo) & !is.na(binhi) & binlo <= binhi & binq > 0 & binw > 0)) |> 
    summarise(all_ok=all(ok), cuales=list(which(!ok)), n=n()) -> bins_ok
  
  if (rlang::is_empty(bins_ok)) {
    msg <- paste("bines vacios!")
    return(list(error=T, msg=msg))
  }
  
  if (bins_ok$n == 0) {
    msg <- paste("bines vacios!")
    return(list(error=T, msg=msg))
  }
  
  if (bins_ok$n < 2) {
    msg <- paste("No se pudo partir en al menos dos bines con", 
                 round(100/nbins,1), "% de las observaciones y al menos", 
                 minpts, "observaciones (pesadas)")
    return(list(error=T, msg=msg))
  }
  
  if (!bins_ok$all_ok) {
    msg <- paste("Algunos bines inconsistentes!", bins_ok$cuales)
    return(list(error=T, msg=msg))
  }
  
  if (verbose) print(bin)
  return(list(bins=bins, error=F))
}

# asume que df tiene x y weight.  Le agrega bin. y además una tabla de lookup
eqbin_Lin <- function(df, nbins=50, minpts=ceiling(length(x)/nbins), nvals.min = 5) {
  # Para evitar vectores muy largos y debido a la falta de precision de table restringo la precision de x a 10 decimales
  b <- bin.sf(df, nbins = nbins, minpts = minpts, nvals.min=nvals.min)
  if (b$error) return(list(error=T, msg=b$msg))
  # Ojo que bin usa los cortes.lo! 
  df |> mutate(bin_idx = findInterval(x, b$bins$binlo, left.open = F),
               bin=b$bins$binlo[bin_idx]) |> 
    select(-bin_idx) -> df
  df |> group_by(bin) |> 
    summarise(cut_lo=min(x), cut_median=median(x), cut_mean=mean(x), cut_hi=max(x), 
              q=n(), w=sum(.weight)) |> 
    arrange(cut_lo) -> tab
  # Sanity check
  if (sum(tab$q) != sum(b$bins$binq)) {
    msg <- paste0("Error interno en binning.  Funcion: eqbin_Lin", 
                  "q=", sum(tab$q), "binq=", sum(b$bins$binq))
    return(list(error=T, msg=msg))
  }
  if (sum(tab$w) != sum(b$bins$binw)) {
    msg <- paste0("Error interno en binning.  Funcion: eqbin_Lin", 
                  "q=", sum(tab$w), "binq=", sum(b$bins$binw))
    return(list(error=T, msg=msg))
  }
  
  if (length(tab$cut_median) < nvals.min) {
    msg <- paste("La variable no alcanzó a partirse en dos grupos con valores no nulos con al menos",
                 round(100/nbins,1), "% de las observaciones y al menos", minpts, "observaciones")
    return(list(error=T, msg=msg))  
  } 
  
  return(list(df=df, tab=tab, error=F))
}

woe_nulos_df <- function(par_df, min_pts=100) {
  assertthat::assert_that(all(c("x", "y", ".weight") %in% colnames(par_df)))
  assertthat::assert_that(all(0 <= par_df$.weight))
  assertthat::assert_that(all(par_df$y %in% c(0,1)))
  par_df |> 
    mutate(Good=y*.weight, Bad=(1-y)*.weight) -> df
  df |> 
    summarise(TotGood=sum(Good), TotBad=sum(Bad)) |> 
    mutate(LnODDS=if_else(TotBad>0,log(TotGood/TotBad), NA_real_)) -> totals
  df |> filter(is.na(x)) -> df_nulos
  if (!plyr::empty(df_nulos)) {
    df_nulos |> 
      summarise(CntGood=sum(Good), CntBad=sum(Bad), CntRec=n()) |> 
      mutate(LnOdds=if_else(CntBad>0,log(CntGood/CntBad), NA_real_)) |> 
      tidyr::crossing(totals) |> 
      mutate(WoE_datos = LnOdds - LnODDS, 
             WoE_anulado = (CntRec < min_pts | CntBad < 1 | CntGood < 1),
             WoE=if_else(WoE_anulado, 0, WoE_datos)) |> 
      select(WoE_datos, WoE_anulado, WoE) -> res 
  } else {
    tibble(WoE_datos=0, WoE_anulado=FALSE, WoE=0) -> res
  }
  return(res)
}


# Retorna tabla de WoEs agrupando por bin.  
# Sin totales ni IV ni nulos. Auxiliar. 
# Asume que df tiene las variables x, bin, y y .weight.  
# También que x, y y .weight son numéricas y que existe mean(x)) 
woe_table <- function(df) {
  
  tab_iv_aux <- function(df) {
    df |> 
      group_by(bin) |>
      summarise(cut_lo=min(x), cut_median=median(x), cut_mean=mean(x), cut_hi=max(x), 
                CntRec=sum(.weight), CntGood=sum(y*.weight), 
                CntBad=sum((1-y)*.weight),
                GoodRate=CntGood/CntRec) |> 
      mutate(egr=(GoodRate != lag(GoodRate, n = 1, default = -Inf)), 
             rleid=cumsum(egr)) -> tab_rleids 
    
    tab_rleids |> select(bin, rleid) |> 
      inner_join(df, by = "bin") |> 
      group_by(rleid) |> 
      summarise(cut_lo=min(x), cut_median=median(x), cut_mean=mean(x), cut_hi=max(x), 
                CntRec=sum(.weight), CntGood=sum(y*.weight), 
                CntBad=sum((1-y)*.weight),
                GoodRate=CntGood/CntRec) |> 
      tidyr::crossing(totals) |> arrange(cut_lo) |> 
      mutate(Odds=if_else(CntBad>0,CntGood/CntBad,NA_real_), 
             LnOdds=log(Odds), 
             WoE=LnOdds - LnODDS) |> 
      mutate(WoE=coalesce(WoE,0), WoE=if_else(is.nan(WoE) | is.infinite(WoE),0,WoE)) |> 
      select(cut_lo, cut_median, cut_hi, CntRec, CntGood, CntBad, WoE) -> res
    return(res)
  }
  
  df |> 
    filter(y==0 | y==1) |> 
    select(x, bin, y, .weight) -> df
  
  totals <- df |> 
    summarise(TotRec=sum(.weight), TotGood=sum(y*.weight), 
              TotBad=sum((1-y)*.weight)) 
  
  assertthat::assert_that(totals$TotBad > 0, msg = "No se puede calcular tabla de IV sin malos!")
  assertthat::assert_that(totals$TotGood > 0, msg = "No se puede calcular tabla de IV sin buenos!")
  
  totals <- totals |> mutate(LnODDS=log(TotGood/TotBad))
  
  df |> filter(!is.na(x) & !is.na(bin)) |> 
    tab_iv_aux() |> arrange(coalesce(cut_median,cut_lo)) -> tab 
  
  return(tab)
}

group_2_ivtab_cont <- function(df, col, col_agrup, woes_breaks, 
                               woe_nulos, sentido, #sentido no lo uso por ahora
                               Good="Good", weight=".weight") {
  
  col_fmt <- function(col) { 
    fn <- scales::label_number(scale_cut = scales::cut_short_scale(), 
                               accuracy = 0.01, drop0trailing = TRUE)
    col |> purrr::map_chr(fn)
  }
  
  tab_iv_aux <- function(df, son_nulos) {
    if (son_nulos) {
      df |> 
        group_by(orden, bin_woe) |>
        summarise(cut_lo=NA_real_, cut_median=NA_real_, cut_mean=NA_real_,  cut_hi=NA_real_, # algunos grupos o incluso todo df puede tener cero casos.  
                  CntRec=sum(.weight), CntGood=sum(y*.weight), CntBad=sum((1-y)*.weight)) |> 
        ungroup() |> 
        tidyr::crossing(totals) |> 
        mutate(woe_est_lo = woe_nulos, woe_est_hi = woe_nulos) |> 
        mutate(cut_lo_prox = NA) |> 
        mutate(col_agrup="Nulos") -> df
    } else {
      df |> 
        group_by(orden, bin_woe) |>
        summarise(cut_lo=min_na(col), cut_median=median(col, na.rm = T), 
                  cut_mean=mean(col, na.rm = T), 
                  cut_hi=max_na(col), # algunos grupos o incluso todo df puede tener cero casos.  
                  CntRec=sum(.weight), CntGood=sum(y*.weight), CntBad=sum((1-y)*.weight)) |> 
        ungroup() |> 
        tidyr::crossing(totals) |> 
        arrange(cut_lo) |> 
        mutate(woe_est_lo = woes_breaks[orden], woe_est_hi = woes_breaks[orden+1]) |> 
        mutate(cut_lo_prox = lead(cut_lo, 1, default = NA)) |> 
        mutate(col_agrup=if_else(!is.na(cut_lo_prox), 
                                 paste0("[", col_fmt(cut_lo), ",", col_fmt(cut_lo_prox),")"), 
                                 paste0("[", col_fmt(cut_lo), ",", col_fmt(999),"]"))) -> df
    } # !son_nulos
    
    df |> 
      mutate(PctRec=CntRec/TotRec, PctGood=CntGood/TotGood, PctBad=CntBad/TotBad, 
             GoodRate=CntGood/CntRec, BadRate=CntBad/CntRec, 
             Odds=if_else(CntBad>0,CntGood/CntBad,NA_real_), LnOdds=log(Odds), 
             WoE=LnOdds - LnODDS, IV=WoE*(PctGood - PctBad),
             CntCumRec=cumsum(CntRec), CntCumGood=cumsum(CntGood), CntCumBad=cumsum(CntBad),
             Cutpoint=paste("<=",cut_hi)) |> 
      mutate(WoE=coalesce(WoE,0), WoE=if_else(is.nan(WoE) | is.infinite(WoE),0,WoE)) |> 
      mutate(IV=coalesce(IV,0), IV=if_else(is.nan(IV) | is.infinite(IV),0,IV)) |> # Trato casos bad cero.
      select(col_agrup, orden, bin_woe, woe_est_lo, woe_est_hi, cut_lo, cut_median, cut_hi, 
             CntRec, CntGood, CntBad, PctRec, PctGood, PctBad, BadRate, WoE, IV)
  }
  
  if (length(woes_breaks)<2) {
    msg <- paste("La variable", col, "debe tener 2 o más cortes para los valores no nulos!")
    return(list(x = col, y = Good, col_agrup = col_agrup, type = "Continua", 
                error = T, error_detalle = msg))
  }
  
  df |> select(all_of(c(col, col_agrup, Good, weight))) |> 
    mutate(col=.data[[col]], col_agrup=.data[[col_agrup]], 
           y=.data[[Good]], .weight=.data[[weight]]) |> 
    filter(y==0 | y==1) -> df
  
  if (df |> filter(!is.na(col)) |> plyr::empty()) {
    msg <- paste("No se puede calcular tabla de IV sin valores no nulos!")
    return(list(x = col, y = Good, col_agrup = col_agrup, type = "Continua", 
                error = T, error_detalle = msg))
  }
  
  totals <- df |> 
    summarise(TotRec=sum(.weight), TotGood=sum(y*.weight), TotBad=sum((1-y)*.weight)) 
  
  if (totals$TotBad <= 0) {
    msg <- paste("No se puede calcular tabla de IV sin malos!")
    return(list(x = col, y = Good, col_agrup = col_agrup, type = "Continua", 
                error = T, error_detalle = msg))
  }
  
  if (totals$TotGood <= 0) {
    msg <- paste("No se puede calcular tabla de IV sin buenos!")
    return(list(x = col, y = Good, col_agrup = col_agrup, type = "Continua", 
                error = T, error_detalle = msg))
  }
  
  totals <- totals |> mutate(LnODDS=log(TotGood/TotBad))
  
  # SF, 10.2: Corrección para incluir siempre el menor.  include.lowest incluye el mayor por defecto 
  # en este caso de right=F
  # Ha ocurrido que en por un tema de precision el menor de col_agrup sea menor que el establecido
  df |> 
    filter(!is.na(col)) |> 
    summarise(mn=min(col_agrup)) |> pull(mn) -> mn 
  
  sort(woes_breaks) -> woes_breaks  # redundante pero defensivo
  
  if (woes_breaks[1] - 1E-7 >= mn) {
    msg <- paste("Valores de ", col_agrup, "fuera del rango esperado!", 
                 "El mínimo fue:", mn, "y se esperaba", woes_breaks[1])
    return(list(x = col, y = Good, col_agrup = col_agrup, type = "Continua", 
                error = T, error_detalle = msg))
  }
  
  min(woes_breaks[1], mn) ->  woes_breaks[1]
  
  df |> 
    filter(!is.na(col)) |> 
    mutate(orden=cut(col_agrup, woes_breaks, include.lowest = T, ordered_result = T,
                     right=F, labels = FALSE)) |> 
    mutate(bin_woe=cut(col_agrup, woes_breaks, include.lowest = T, ordered_result = T,
                       right=F, labels = NULL)) -> df_nonulos 
  
  tab_iv_aux(df_nonulos, son_nulos = FALSE) -> tab
  
  if (nrow(df |> filter(is.na(col)) ) > 0) {
    
    which(order(c(woe_nulos, tab$WoE))==1)-0.5 -> orden_nulos
    
    df |> 
      filter(is.na(col)) |> 
      mutate(orden = orden_nulos, bin_woe = paste('[', round(woe_nulos,3), ']')) -> df_nulos
    
    tab_iv_aux(df_nonulos, son_nulos = FALSE) |> 
      bind_rows(tab_iv_aux(df_nulos, son_nulos = TRUE)) -> tab
  }
  
  row_totals <- tab |> 
    summarise(col_agrup='Total', orden=NA_integer_, bin_woe='Total', 
              woe_est_lo = 0, woe_est_hi = 0, 
              cut_lo=ifelse(sum(!is.na(cut_lo))==0,NA_real_, min(cut_lo, na.rm = T)), #if_else arruina la prevención del warning!
              cut_median=NA_integer_, 
              cut_hi=ifelse(sum(!is.na(cut_hi))==0,NA_real_, max(cut_hi, na.rm = T)),
              CntRec=sum(CntRec), CntGood=sum(CntGood), CntBad=CntRec-CntGood, 
              PctRec=sum(PctRec), PctGood=sum(PctGood), PctBad=sum(PctBad), 
              BadRate=CntBad/CntRec, WoE=0, IV=sum(IV))  
  
  tab <- tab |> arrange(orden) |> # Así la salida está ordenada por los WoE de Train. 
    bind_rows(row_totals) |> 
    mutate_at(c("PctRec", "PctGood", "PctBad", "BadRate", "IV"), ~.*100)
  
  return(list(x = col, y = Good, col_agrup = col_agrup, type = "Continua", 
              error = F, error_detalle = "", tab = tab))
}

# Cortes de WoEs para replicar los de ivtable con la variable ya transformada por stp o pwl.

woes_cuts <- function(tab_iv, type="pwl", sentido = 1) {
  if (!(type %in% c("stp", "pwl"))) 
    error_custom('Los tipos implementados son stp o pwl', 
                 "i" = "Se obtuvo", type,
                 "i" = "Revisar parámetro par_discret", 
                 ">"=cli::col_red("Cod 311"))
  else {
    tab_iv |> 
      filter(!is.na(cut_lo) & !is.na(cut_median) & !is.na(cut_hi) &!is.na(WoE)) |> 
      select(cut_lo, cut_median, cut_hi, WoE) |> 
      arrange(WoE) -> tab #Fundamental el orden! 
    if (type=="pwl") {
      if (sentido == 1) {
        tab |> 
          mutate(x1=cut_median, x2=coalesce(lead(x1), cut_hi), 
                 y1=WoE, y2=coalesce(lead(WoE),WoE+0.01), 
                 z=coalesce(0.5*(lead(cut_lo)+cut_hi), cut_hi), # SF, 202411 para asegurar vuelta a corte original
                 woes_cut=if_else(x1==x2, y2, (y2-y1)*(z-x1)/(x2-x1)+y1)) -> tab
      } else { # sentido == -1
        tab |> 
          mutate(x1=cut_median, x2=coalesce(lead(x1), cut_lo), 
                 y1=WoE, y2=coalesce(lead(WoE),WoE+0.01), 
                 z=coalesce(0.5*(lead(cut_hi) + cut_lo), cut_lo), # SF, 202411 para asegurar vuelta a corte original
                 woes_cut=if_else(x1==x2, y2, (y2-y1)*(z-x1)/(x2-x1)+y1))  -> tab
      } # sentido == -1
      tab |> summarise(w=first(WoE)) |> pull(w) -> w
      tab |> pull(woes_cut) -> wc
      c(w, wc) -> w
      unique(w) |> sort() -> w
    } else { # type == "stp"
      tab |> pluck("WoE") -> w 
      unique(w) |> sort() -> w
      max(w) + 0.01 -> w[length(w)+1] # El tratamiento del último intervalo es un poco diferente
    }
    return(w)
  }
}

tab_iv <- function(df, par_col=col, par_col_agrup=col_agrup) {
  df |> rename(x=all_of(par_col), x_transf=all_of(par_col_agrup), y=all_of('Good')) |> select(x,y,x_transf) -> df
  df |> filter(!is.na(x)) |> 
    mutate(x_agrup = x_transf |> 
             eqbin_Lin(par_nbins2, par_minpts2, nvals.min = 2) |> purrr::pluck('x_eq')) |>   
    bind_rows(df |> filter(is.na(x)) |> mutate(x_agrup = x_transf)) |> 
    agrup_2_ivtab_cont() -> tab
  return(tab)
}

# Si se quiere sólo una pasada igualar los segundos parámetros con los primeros
# Ej.: monotbin <- function(df, y, x, nbins1=20, minpts1=100, nbins2=nbins1, minpts2=minpts1)
# No se admiten NA, missings o similares. Isobin no los admite. Realizar imputación de missings antes. 
# nvals.min es un parámetro implícito que se bajó a 2 para tratar los flags pero hay que revisar esto. 
# Notar que si hay menos de par_iv_cuantiles_gb_min buenos o malos nulos se asigna cero. 
# No sé si tiene sentido tratar los flags como continuas. 
# Discretizo por rampas por defecto, usando 0 se discretiza por escalera

bin_monot <- function(df, y, x, nbins1=par_nbins1, minpts1=par_minpts1, 
                      nbins2=par_nbins2, minpts2=par_minpts2, 
                      minptsnulos=par_minpts_nulos, rampa=par_discret, 
                      verbose = FALSE) {
  # Ojo que eqbin corta la precision del argumento a 10 decimales
  # y smbinning.custom la corta a 4 decimales!
  # y, x son nombres de variables, no vectores!
  j=which(names(df) == x)
  col_agrup <- paste0(x,"_g")
  df_orig <- df |> select(all_of(c(x,y,".weight")))
  df <- df_orig |> mutate(x=.data[[x]], y=.data[[y]]) |> select(x, y, .weight)
  assertthat::assert_that(all(0 <= df$.weight))
  assertthat::assert_that(all(df$y %in% c(0,1)))
  
  woe_nulos_df(df, min_pts = minptsnulos) -> res_nulos
  res_nulos$WoE -> woe_nulos 
  df_nonulos <- df |> filter(!is.na(x))
  # La primera vez controlé que sean 5 valores únicos por lo menos
  # Lo bajé a 2 para que trate los flags pero no me convence mucho. 
  res <- df_nonulos |> eqbin_Lin(nbins1, minpts1, nvals.min = 2) 
  if (res$error) return(list(x = x, y = y, col_id = j, error=T, error_detalle=res$msg))
  x_eq <- res |> purrr::pluck("df") |> pull(bin)
  c <- x_eq |> safe_cor(df_nonulos |> pull(y), df_nonulos |> pull(.weight))
  if (!is.null(c$error) | is.na(c$result)) 
    return(list(x = x, y = y, col_id = j, error=T, 
                error_detalle=paste("Correlacion de Spearman entre variable agrupada y objetivo nula o cero. ",
                                    c$error, c$warnings)))
  if (abs(c$result)>=0.95) 
    return(list(x = x, y = y, col_id = j, error=T, 
                error_detalle=paste("Correlacion de Spearman entre variable agrupada y objetivo mayor a 0.95! ",
                                    c$result)))  
  c$result/abs(c$result) -> sentido # 1 si WoE crece con la variable, -1 si decrece. 
  
  res$df -> df_nonulos
  
  df_nonulos |> 
    group_by(bin) |> 
    summarise(w=sum(.weight),
              yw=sum(y*.weight)) |> 
    mutate(GR=yw/w, 
           SGR=GR*sentido) |> 
    arrange(bin) |> 
    mutate(y_mono=monotone::monotone(x=SGR, w=w)) -> tab_mono
  
  df_nonulos |> 
    inner_join(tab_mono |> select(bin, y_mono), by="bin") -> df_nonulos
  
  df_nonulos |> rename(x_orig=x, x=y_mono) |> 
    eqbin_Lin(nbins2, minpts2, nvals.min = 2) -> res
  if (res$error) return(list(x = x, y = y, col_id = j, error=T, error_detalle=res$msg))
  
  res$df |> rename(y_mono=x, x=x_orig) -> df_nonulos
  
  df |> filter(is.na(x)) |> bind_rows(df_nonulos) |> woe_table() -> woes_tab
  woes_tab |> woes_cuts(type = "stp", sentido) -> woes_stp_cuts
  woes_tab |> woes_cuts(type = "pwl", sentido) -> woes_pwl_cuts
  # SF: Cómo luego la traducción a sql usa as.character, para evitar errores de redondeo
  # redondeamos las tablas a 15 dígitos. 
  woes_tab |> round(15) -> woes_tab
  woes_stp_cuts |> round(15) -> woes_stp_cuts
  woes_pwl_cuts |> round(15) -> woes_pwl_cuts
  
  res <- list(x = x, y = y, col_id = j, type = "Continua", 
              error = F, error_detalle = "", 
              nbins1 = nbins1, minpts1 = minpts1, nbins2 = nbins2, 
              minpts2 = minpts2, minptsnulos = minptsnulos, 
              groups = NULL, sentido= sentido, spear_corr=c$result, 
              woe_nulos = woe_nulos, 
              WoE_anulado = res_nulos$WoE_anulado, 
              # Es el WoE que fue anulado si WoE_anulado. Igual a woe_nulos sino.
              WoE_datos = res_nulos$WoE_datos, 
              woes_tab = woes_tab, 
              woes_stp_cuts = woes_stp_cuts, 
              woes_pwl_cuts = woes_pwl_cuts)
  
  # Para lograr una comparación justa IV antes lo calculo con nbins2 y minpts2
  res_antes <- df_nonulos |> eqbin_Lin(nbins2, minpts2, nvals.min = 2) 
  if (res_antes$error) {
    res$error <- TRUE
    res$error_detalle <- res_antes$msg
    return(res)
  }
  
  df |> filter(is.na(x)) |> bind_rows(res_antes$df) |> woe_table() -> woes_tab_nomonot 
  
  df_orig |> 
    apply_grouping_monot(woes_tab_nomonot, x, col_agrup, rampa=0, woe_nulos = res$woe_nulos) |> 
    purrr::pluck("df") -> df
  
  df |> 
    group_2_ivtab_cont(x, col_agrup, 
                       woes_breaks = woes_tab_nomonot |> woes_cuts(type="stp", sentido), 
                       woe_nulos = woe_nulos, sentido = sentido) -> res_agrup
  
  if (res_agrup$error) {
    res$error <- TRUE
    res$error_detalle <- paste("Hubo un problema en el cálculo de tab_iv_antes", res_agrup$msg)
    return(res)
  } else tab_iv_antes <- res_agrup$tab
  
  tab_iv_antes |> 
    filter(col_agrup == 'Total') |> pull("IV") |> sum() -> iv_antes
  
  # Agregado para calcular diferentes versiones de IVs posteriores.
  df_orig |> 
    apply_grouping_monot(woes_tab, x, col_agrup, rampa=0, woe_nulos = res$woe_nulos) |> 
    purrr::pluck("df") -> df
  
  df |> 
    group_2_ivtab_cont(x, col_agrup, woes_breaks = res$woes_stp_cuts, 
                       woe_nulos = woe_nulos, sentido = sentido) -> res_agrup
  
  if (res_agrup$error) {
    res$error <- TRUE
    res$error_detalle <- paste("Hubo un problema en el cálculo de tab_iv_stp", res_agrup$msg)
    return(res)
  } else tab_iv_stp <- res_agrup$tab
  
  df_orig |> 
    apply_grouping_monot(woes_tab, x, col_agrup, rampa=1, woe_nulos = res$woe_nulos) |> 
    purrr::pluck("df") -> df
  
  df |> 
    group_2_ivtab_cont(x, col_agrup, woes_breaks = res$woes_pwl_cuts,
                       woe_nulos = woe_nulos, sentido = sentido) -> res_agrup
  
  if (res_agrup$error) {
    res$error <- TRUE
    res$error_detalle <- paste("Hubo un problema en el cálculo de tab_iv_pwl", res_agrup$msg)
    return(res)
  } else tab_iv_pwl <- res_agrup$tab
  
  # Verificación de seguridad. Los cortes se deben mantener. No incluyo col_agrup pq difiere el nombre para los nulos
  assertthat::are_equal(
    res$woes_tab |> arrange(cut_median) |> 
      select(cut_lo, cut_median, cut_hi, CntRec, CntBad, CntGood, WoE),
    tab_iv_stp |> arrange(cut_median) |> 
      filter(col_agrup != "Total" & col_agrup != 'Nulos') |> 
      select(cut_lo, cut_median, cut_hi, CntRec, CntBad, CntGood, WoE)) -> res_check
  
  if (!res_check) {
    res$error <- TRUE
    res$error_detalle <- paste("Fallo interno! woes_tab !=  tab_iv_stp", 
                               "en binning de variable", x)
    return(res)
  } 
  
  # assertthat::are_equal(
  #   tab_iv_stp |> arrange(col_agrup) |> select(CntRec, CntBad, BadRate, WoE),
  #   tab_iv_pwl |> arrange(col_agrup) |> select(CntRec, CntBad, BadRate, WoE), tol=0.01) -> cond
  # SF, 20240826
  # Relajo la tolerancia para que Contacto_Antiguedad en el dataset inicial pase.
  assertthat::are_equal(
    tab_iv_stp |> filter(col_agrup == 'Total') |> select(CntRec, CntBad, BadRate, WoE),
    tab_iv_pwl |> filter(col_agrup == 'Total') |> select(CntRec, CntBad, BadRate, WoE), 
    tol=0.05) -> cond
  if (!cond) warning(paste("Atención! Revisar discrepancias en entre tablas tab_iv_stp y tab_iv_pwl",
                           "en binning de variable", x))
  
  iv_monot_discret <- tab_iv_stp |> 
    filter(col_agrup == 'Total') |> summarise(IV=sum(IV)) |> pull()
  iv_monot_cont <- tab_iv_pwl |> 
    filter(col_agrup == 'Total') |> summarise(IV=sum(IV)) |> pull()
  # Dejo iv_despues por compatibilidad.  Ojo que rampas es un parámetro global!
  if (rampa==1) iv_despues <- iv_monot_cont else iv_despues <- iv_monot_discret
  res <- append(res, 
                list(tab_iv_stp=tab_iv_stp, tab_iv_pwl=tab_iv_pwl, 
                     iv_antes_monot=iv_antes, 
                     iv_monot_discret=iv_monot_discret, 
                     iv_monot_cont=iv_monot_cont, 
                     iv = iv_despues,  # La dejo por compatibilidad y agrego las otras pérd.
                     iv_perd = if_else(iv_antes==0,0,100*(iv_antes-iv_despues)/iv_antes),
                     iv_perd_discret = if_else(iv_antes==0,0,100*(iv_antes-iv_monot_discret)/iv_antes),
                     iv_perd_cont = if_else(iv_antes==0,0,100*(iv_antes-iv_monot_cont)/iv_antes)
                ))
  if (verbose) {
    res <- append(res, list(tab_iv_antes=tab_iv_antes, tab_mono=tab_mono))
  }
  return(res)
}

# TODO: Informar la distribución de col_agrup en df para comparar con la de res?
# Por defecto signa WoE cero a los nulos, lo que equivale a imputarles la media 
# de la población pero esto se cambia con el parámetro woe_nulos.
apply_grouping_monot <- function (df, woes_tab, x, col_agrup, rampa=rampa, woe_nulos=0) {
  df |> as_tibble() |> mutate(tmpname = NA) -> df
  ncol = ncol(df)
  col_id <- which(colnames(df)==x) # Es más robusto que el original ivout$col_id
  if (length(col_id)==1) {
    df[which(is.na(df[, col_id])), ncol] = woe_nulos
    b <- woes_tab$cut_median
    if (length(b)<2) {
      alerta=T   
      msg <- paste("La variable continua  ", x, " no tiene cortes definidos!")
    }
    else {
      if (rampa) { # Discretiza usando lineal a trozos
        df[which(df[, col_id] <= b[1]), ncol] <- woes_tab$WoE[1]
        for (i in 2:length(b)) {
          x1 <- b[i-1]
          x2 <- b[i]
          y1 <- woes_tab$WoE[i-1]
          y2 <- woes_tab$WoE[i]
          x <- df |> pluck(col_id)
          idx <- which(x > x1 & x <= x2)
          df[idx, ncol] <- (y2-y1)*(x[idx]-x1)/(x2-x1)+y1 
        }
        idx <- which(x >= b[length(b)])
        df[idx, ncol] <- woes_tab$WoE[length(b)]
      }
      else { # Discretiza usando escalera
        df[which(!is.na(df[, col_id])), ] |> 
          mutate(tmpname = 
                   my_cut(.data[[x]], 
                          x=woes_tab |> pull(cut_lo), 
                          y=woes_tab |> pull(WoE))) -> df[!is.na(df[, col_id]), ]
      }
      names(df)[names(df) == "tmpname"] = col_agrup
      alerta = F
      msg <- NA
    }
  }
  else {
    alerta=T   
    msg <- paste("Variable ",x ," en discretizacion no hallada en df!")
  }
  return(list(df=df, alerta=alerta, msg=msg))
}

# Sucessive Medians Binning -----------------------------------------------
# Binning por medianas sucesivas 

partir_x_med <- function(df, col, lim_izq, lim_der, woe_izq, woe_der, ext_izq=FALSE, 
                         nbins2=par_nbins2, minpts2=par_minpts2, Good='Good') {
  list(lim_izq, lim_der, woe_izq, woe_der) |> purrr::map(~assert_that::assert_that(is.number(.)))
  list(nbins2, minpts2) |> purrr::map(~assert_that::assert_that(is.count(.)))
  assert_that::assert_that(lim_izq < lim_der, woe_izq != woe_der)
  df |> ungroup() |> select(all_of(col), all_of(Good)) |> 
    rename(x=all_of(col), good=all_of(Good)) |> mutate(bad=1-good) |> 
    select(x,bad) -> tab 
  assert_that::assert_that(all(tab$bad %in% c(0,1)), is.numeric(tab$x))
  minpts <- min(minpts2, floor(length(tab$x)/nbins2))
  assert_that::assert_that(is.count(minpts))
  
  tab |> summarise(n=n(), goods=sum(1-bad), bads=sum(bad), bad_rate=bads/n, 
                   odds=if_else(bads>0, goods/bads, NA_real_)) -> tot 
  if (ext_izq) tab |> filter(lim_izq <= x & x <= lim_der) -> tab
  else tab |> filter(lim_izq < x & x <= lim_der) -> tab
  tab |>
    summarise(min=min(x), max=max(x), 
              n=n(), bads=sum(bad), goods=sum(1-bad)) |> 
    mutate(PctGood=goods/tot$goods, PctBad=bads/tot$bads, 
           bad_rate=bads/n, good_rate=goods/n, 
           odds=if_else(bads>0, goods/bads, NA_real_), 
           WoE=log(odds)-log(tot$odds), IV=WoE*(PctGood - PctBad)) -> tab_nopart
  list(raiz=tibble(lim_izq=lim_izq, lim_der=lim_der, woe_izq=woe_izq, woe_der=woe_der, 
                   n=tab_nopart$n, min=tab_nopart$min, max=tab_nopart$max, 
                   WoE=tab_nopart$WoE, bad_rate=tab_nopart$bad_rate)) -> res_no_part
  tab |>
    mutate(lte_med=(x <= median(x))) |> 
    group_by(lte_med) |> 
    summarise(min=min(x), max=max(x), 
              n=n(), bads=sum(bad), goods=sum(1-bad)) |> 
    mutate(PctGood=goods/tot$goods, PctBad=bads/tot$bads, 
           bad_rate=bads/n, good_rate=goods/n, 
           odds=if_else(bads>0, goods/bads, NA_real_), 
           WoE=log(odds)-log(tot$odds), IV=WoE*(PctGood - PctBad)) -> tab_part
  tab_part |> filter(lte_med) -> fila_izq 
  tab_part |> filter(!lte_med) -> fila_der  
  if (not_empty(fila_izq) & not_empty(fila_der)) {
    if (fila_izq$n >= minpts & fila_der$n >= minpts) {
      if (woe_izq <= fila_izq$WoE & fila_izq$WoE < fila_der$WoE & fila_der$WoE <= woe_der) 
        return(list(izq=partir_x_med(df, col, lim_izq, fila_izq$max, woe_izq, fila_der$WoE, 
                                     ext_izq, nbins2, minpts2), 
                    der=partir_x_med(df, col, fila_izq$max, lim_der, fila_izq$WoE, woe_der, 
                                     ext_izq=F, nbins2, minpts2)))
      else if (woe_izq >= fila_izq$WoE & fila_izq$WoE > fila_der$WoE & fila_der$WoE >= woe_der)  
        return(list(izq=partir_x_med(df, col, lim_izq, fila_izq$max, woe_izq, fila_der$WoE, 
                                     ext_izq, nbins2, minpts2), 
                    der=partir_x_med(df, col, fila_izq$max, lim_der, fila_izq$WoE, woe_der, 
                                     ext_izq=F, nbins2, minpts2))) 
      else return(res_no_part) 
    } else return(res_no_part) 
  } else return(res_no_part) 
}

red_tree_2_cuts <- function(node) {
  if (names(node)[[1]]=="raiz") 
    if (node$raiz$lim_der == Inf) return(c(node$raiz$lim_izq, Inf)) 
  else return(node$raiz$lim_izq)
  else return(c(red_tree_2_cuts(node$izq), red_tree_2_cuts(node$der))) 
}

bin_x_med <- function(df, col, col_agrup,  
                      lim_izq=-Inf, lim_der=Inf, 
                      woe_izq=-Inf, woe_der=Inf, ext_izq=T) {
  df |> filter(is.na(.data[[col]])) -> df_nulos
  df_nulos |> rename(x_v=all_of(col), y_v=Good) |> 
    woe_nulos(min_pts = par_iv_cuantiles_gb_min) -> woe_nulos 
  # Por ahora sólo interesa las tablas de IV no el modelado
  # df_nulos |> mutate("{col_agrup}" := woe_nulos) -> df_nulos
  df_nulos |> mutate("{col_agrup}" := "Missing") -> df_nulos
  df |> filter(!is.na(.data[[col]])) -> df_nonulos
  df_nonulos[[col]] -> x
  df_nonulos |> partir_x_med(col, lim_izq, lim_der, woe_izq, woe_der, ext_izq) -> res_part
  res_part |> red_tree_2_cuts() -> cortes
  df_nonulos |> mutate("{col_agrup}" := cut(.data[[col]], cortes)) -> df_nonulos
  df_nonulos |> bind_rows(df_nulos) -> df
  return(list(df=df, cortes=cortes))
}

# bin_factor() y auxiliares -----------------------------------------------

group_2_ivtab_factor <- function(df, col, col_agrup, Good="Good", 
                                 minpts=par_minpts_cat, minpts_nulos=par_minpts_nulos) {
  
  j=which(names(df) == col)
  
  df <- df |> select(all_of(c(col, col_agrup, Good, ".weight"))) |> 
    mutate(col=.data[[col]], col_agrup=.data[[col_agrup]], Good=.data[[Good]]) 
  
  totals <- df |> 
    summarise(TotRec=sum(.weight), TotGood=sum(Good*.weight), 
              TotBad=sum((1-Good)*.weight)) 
  
  if (totals$TotBad < minpts) {
    msg <- paste("No se puede calcular tabla de IV sin suficientes malos!", totals$TotBad, "<", minpts)
    return(list(x = col, y = Good, col_id = j, minpts=minpts, type = "Factor", 
                error = T, error_detalle = msg))
  }
  
  if (totals$TotGood < minpts) {
    msg <- paste("No se puede calcular tabla de IV sin suficientes buenos!", totals$TotGood, "<", minpts)
    return(list(x = col, y = Good, col_id = j, minpts=minpts, type = "Factor", 
                error = T, error_detalle = msg))
  }
  
  totals <- totals |> mutate(LnODDS=log(TotGood/TotBad))
  
  tab <- df |> group_by(col_agrup) |>
    summarise(Cutpoint=toString(unique(col_agrup)), 
              groups=paste("'", paste(sort(unique(col), na.last = T), collapse="','"), "'", sep=""), 
              CntRec=sum(.weight), CntGood=sum(Good*.weight), CntBad=sum((1-Good)*.weight), 
              cut_lo=min(col), cut_hi=max(col)) |> 
    tidyr::crossing(totals) |> 
    mutate(PctRec=CntRec/TotRec, PctGood=CntGood/TotGood, PctBad=CntBad/TotBad, 
           GoodRate=CntGood/CntRec, BadRate=CntBad/CntRec, 
           Odds=if_else(CntBad>0,CntGood/CntBad,NA_real_), LnOdds=log(Odds), 
           WoE=LnOdds - LnODDS, IV=WoE*(PctGood - PctBad)) |> 
    mutate(WoE_datos = WoE) |> 
    mutate(WoE=coalesce(WoE,0), WoE=if_else(is.nan(WoE) | is.infinite(WoE),0,WoE)) |> 
    mutate(IV=coalesce(IV,0), IV=if_else(is.nan(IV) | is.infinite(IV),0,IV)) 
  
  tab <- tab |> 
    mutate(WoE_anulado = case_when(
      col_agrup == '_Missings' & CntRec < minpts_nulos ~ TRUE, 
      col_agrup != '_Otros' & CntRec < minpts ~ TRUE, 
      CntBad < 1 | CntGood < 1 ~ TRUE, 
      .default = FALSE
    )) |> 
    mutate(WoE = if_else(WoE_anulado,0,WoE), IV = if_else(WoE_anulado,0,IV)) |> 
    arrange(BadRate) |> 
    mutate(CntCumRec=cumsum(CntRec), CntCumGood=cumsum(CntGood), 
           CntCumBad=cumsum(CntBad)) |> 
    arrange(desc(col_agrup)) |> select(-col_agrup) # col agrup es el orden de train
  totals <- tab |> 
    summarise(Cutpoint='Total', groups=NA_character_, CntRec=sum(CntRec), 
              CntGood=sum(CntGood), CntBad=CntRec-CntGood, 
              PctRec=sum(PctRec), PctGood=sum(PctGood), PctBad=sum(PctBad), 
              BadRate=CntBad/CntRec, IV=sum(IV), 
              # Ojo que convierte logical a int. Vale 0, 1 ó 2 cuando _Otros y _Missings se anulan. 
              WoE=0, WoE_datos = 0, WoE_anulado = sum(WoE_anulado))  
  tab <- tab |> bind_rows(totals) |> 
    mutate_at(c("PctRec", "PctGood", "PctBad", "BadRate", "IV"), ~.*100)
  return(list(x = col, y = Good, col_id = j, minpts=minpts, 
              minpts_nulos = minpts_nulos, 
              type = "Factor", 
              error = F, tab=tab, 
              WoE_anulado = totals |> pull(WoE_anulado)))
}

# Ej de uso: bins <- bin_factor(train, col, minpts = 30)
bin_factor <- function(df, col, minpts=par_minpts_cat, Good="Good", maxlevels=par_maxlevels) {
  
  j=which(names(df) == col)
  
  # Notar que los Missing se agrupan aparte 
  # pero si no reciben par_minpts_nulos terminan con un WoE cero en group_2_ivtab_factor. 
  frec_tab <- df |> 
    mutate(col=.data[[col]]) |> 
    group_by(col) |> 
    summarise(w=sum(.weight)) |> 
    mutate(col_agrup=if_else(w >= minpts, col, '_Otros')) |> 
    mutate(col_agrup=if_else(is.na(col), '_Missings', col_agrup))
  
  if (frec_tab |> nrow() > maxlevels) {
    msg <- paste("La cantidad de grupos", frec_tab |> nrow(), "supera maxlevels", maxlevels)
    return(list(x = col, y = Good, col_id = j, minpts=minpts, type = "Factor", 
                error = T, error_detalle = msg))
  }
  
  if (frec_tab |> count(col_agrup) |> nrow() < 2) {
    msg <- paste("No se pudo partir en grupos!")
    return(list(x = col, y = Good, col_id = j, minpts=minpts, type = "Factor", 
                error = T, error_detalle = msg))
  }
  
  # IV_antes
  df |> group_2_ivtab_factor(col = col, col_agrup = col, Good=Good) -> res
  if (res$error) return(res)
  iv_antes <- res$tab |> 
    filter(is.finite(IV) & Cutpoint!="Total") |> 
    summarise(IV=sum(IV)) |> pull()
  
  df |> 
    mutate(col=.data[[col]]) |> 
    select(col, Good, .weight) |> 
    inner_join(frec_tab, by="col") |> select(-w) -> df
  
  df |> 
    group_2_ivtab_factor(col="col", col_agrup="col_agrup", 
                         Good=Good, minpts = minpts) -> res
  if (res$error) return(res) 
  tab <- res$tab 
  
  woes_tab <- frec_tab |> inner_join(tab |> filter(is.finite(IV) & Cutpoint!="Total"), 
                                     by=c("col_agrup" = "Cutpoint")) |> 
    select(col, WoE) |> rename(!!sym(col) := col, !!sym(paste0(col,"_g")) := WoE)
  
  iv_despues = tab |> filter(is.finite(IV) & Cutpoint!="Total") |> summarise(IV=sum(IV)) |> pull()
  
  # bands y cuts sólo quedan por compatibilidad 
  res <- list(x = col, y = Good, col_id = j, minpts=minpts, type = "Factor", 
              error = F, error_detalle = NULL, 
              ivtable = tab, iv = iv_despues, iv_antes_discret=iv_antes, 
              iv_perd = if_else(iv_antes==0,0,(iv_antes-iv_despues)/iv_antes),
              iv_despues = iv_despues,
              groups = tab |> select(Cutpoint, groups) |> tibble::deframe(), 
              bands = NULL, cuts = NULL, woes_tab=woes_tab, 
              WoE_anulado = res$WoE_anulado)
  
}

apply_grouping_factor <- function(df, res_bf, col_agrup) {
  col <- res_bf$x
  wt <- res_bf |> purrr::pluck("woes_tab")
  WoE_Otros <- res_bf |> purrr::pluck("ivtable") |> filter(Cutpoint=="_Otros") |> pull(WoE)
  WoE_Missing <- res_bf |> purrr::pluck("ivtable") |> filter(Cutpoint=="_Missings") |> pull(WoE)
  df <- df |> left_join(wt, by=col) 
  no_categ <- df |> select(x=all_of(col), x_g=all_of(col_agrup)) |> 
    filter(is.na(x_g)) |> group_by(x) |> summarise(n=n())
  if (no_categ |> nrow()>0) {
    val_no_categ <- no_categ |> pull(x) |> paste(collapse = ', ')
    if (is_empty(WoE_Otros) || is.na(WoE_Otros)) {
      if (is_empty(WoE_Missing) || is.na(WoE_Missing)) {
        cat_no_categ <- '0'
        woe_no_categ <- 0
      } else {
        cat_no_categ <- '_Missings'
        woe_no_categ <- coalesce(WoE_Missing, 0)
      } 
    } else {
      cat_no_categ <- '_Otros'
      woe_no_categ <- coalesce(WoE_Otros, 0)
    }
    df[, col_agrup] <- woe_no_categ
    alerta=T   
    msg <- paste("En el dataset aparecieron valores de",col, "que no tenian WoEs asociados:'",
                 val_no_categ,"'. Se transformaron con valores de WoE de", cat_no_categ)} 
  else {
    alerta=F   
    msg <- NULL
  }
  return(list(df=df, no_categ=no_categ, alerta=alerta, msg=msg))
}

wilson_width <- function(bad_rate, n) {
  p_hat <- bad_rate
  z <- 2
  2*z/(1+z^2/n)*sqrt(z^2/(4*n^2)+p_hat*(1-p_hat)/n)
}

wilson_n <- function(bad_rate, w) {
  p_hat <- bad_rate
  z <- 2
  z^2/w^2*(2*p_hat*(1-p_hat)-w^2+sqrt((w^2-2*p_hat*(1-p_hat))^2+w^2*(1-w^2)))
}

graph_min_sizes <- function(bin_factor_res) {
  ivtable <- bin_factor_res |> purrr::pluck("ivtable")
  minpts <- bin_factor_res |> purrr::pluck("minpts")
  tab_prec_now <- ivtable |> select(Grupo=Cutpoint, CntRec, BadRate) |> 
    mutate(width=wilson_width(BadRate, CntRec), 
           prec_Real=width/(2*BadRate), Prec_Real=100*prec_Real)
  prec_range <- tab_prec_now |> pull(Prec_Real) 
  Prec <- seq(from= min(prec_range) * 0.9, to= max(prec_range) * 1.1, 
              by=1/10) 
  tab_prec_range <- ivtable |> select(Grupo=Cutpoint, CntRec, BadRate) |> 
    tidyr::crossing(Prec) |> mutate(prec=Prec/100, width=2*BadRate*prec,
                                    Casos_req=ceiling(wilson_n(BadRate, width))) 
  eje_y <- seq(from=0, to=tab_prec_range |> pull(Casos_req) |> max(), length.out=100)
  pretty_mod <- pretty(eje_y)[abs(pretty(eje_y) - minpts) > 0.25]
  g1 <- tab_prec_now |> 
    ggplot2::ggplot(ggplot2::aes(x=Prec_Real, y=CntRec, color=Grupo)) +
    ggplot2::geom_point() + 
    ggplot2::scale_y_continuous(breaks = c(pretty_mod, minpts), labels = c(pretty_mod, 'minpts')) + 
    ggplot2::labs(x='Imprecisión x (Tasa de Mora real +/- x% con 95% de confianza)', y='Mínimo de casos requeridos') +
    ggplot2::geom_text(ggplot2::aes(label=Grupo), size=3, nudge_y = 50) + 
    ggplot2::geom_label(ggplot2::aes(label=round(100*BadRate,1), fill=Grupo), color="white", size=3, nudge_y = 15) +     
    ggplot2::theme(legend.position = "bottom", axis.title=ggplot2::element_text(size=8)) + 
    ggplot2::geom_hline(yintercept = minpts, linetype=3)
  g2 <- g1 + ggplot2::geom_line(data=tab_prec_range, ggplot2::aes(x=Prec, y=Casos_req, color=Grupo)) 
  return(g2)
}

# Logistic Forward AK -----------------------------------------------------
# Ojo que se asume Good y .weight en train!
# usa parámetro par_conf_level
logit.step <- function(train, target, verbose = F, 
                       fmla = as.formula(paste(target, " ~ NULL", sep = ""))) { 
  max.vars <- length(colnames(train)) - 2 # - Good - .weight 
  if (max.vars>0) {
    train2 <- train
    fmla.all <- as.formula(paste(target, " ~ ", 
                                 paste(paste(colnames(train), collapse = "+")), " - ",target, 
                                 " - .weight", sep = ""))
    mod.step <- stats::glm(fmla, train2, family = "binomial", 
                           weights = train2$.weight) 
    idx <- 0    
    coef.step <- mod.step |> broom::tidy() |> 
      select(term, estimate) |>
      rename(Variable=term, Beta=estimate) |> 
      mutate(Paso=idx)
    coef.steps <- coef.step
    all.coef.positives <- 1
    vars.fwd.res <- c()
    var.sel <- "pp"
    vars.step <- c()
    while (idx <= max.vars && all.coef.positives > 0 && 
           length(var.sel)>0 && !is.na(var.sel)) {
      mod.curr <- mod.step
      if (max.vars > mod.curr$df.null - mod.curr$df.residual) {
        vars.fwd <- mod.curr |> add1(scope = fmla.all, test = "Chisq") |> 
          as_tibble(rownames="Variable") |> 
          slice_max(order_by = LRT, n = 10) # Grabo las 10 mejores variables por si hay demasiadas
        var.sel <- vars.fwd  |> 
          filter(`Pr(>Chi)` < par_conf_level & Variable != "<none>") |> 
          arrange(desc(LRT)) |> 
          top_n(1,LRT) |> select(Variable) |> pull()
        if (length(var.sel)>0 && !is.na(var.sel)) {
          mod.step <- update(mod.curr, as.formula(paste(".~.+", var.sel)))
          idx <- idx + 1
          # A futuro no renombrar y dejar la salida del tidy que tiene  term estimate std.error statistic  p.value
          mod.step |> broom::tidy() |> select(term, estimate) |>
            rename(Variable=term, Beta=estimate) |> 
            mutate(Paso=idx) -> coef.step
          coef.step |> 
            filter(Variable!="(Intercept)") |>   
            mutate(sign = sign(Beta), 
                   order=row_number()) -> tab_coef
          if (all(tab_coef$sign == 1)) { 
            all.coef.positives <- 1
            coef.steps <- coef.steps |> bind_rows(coef.step)  
            vars.fwd.res <- bind_rows(vars.fwd.res, 
                                      vars.fwd |> mutate(Paso=idx))
            vars.step <- c(vars.step, var.sel)
            fmla <- mod.curr$formula
          } else {
            # Apareció un coef negativo!
            # Salvo sólo las positivas anteriores si hay. 
            tab_coef |> 
              filter(sign == -1) |> 
              summarise(first_negative = min(order), 
                        last_negative = max(order)) -> tab_coef_neg
            tab_coef_neg |> 
              pull(first_negative) -> first_negative_coef
            tab_coef_neg |> 
              pull(last_negative) -> last_negative_coef            
            tab_coef |> 
              filter(order < first_negative_coef) |> 
              pull(Variable) -> positive_coef_vars
            tab_coef |> 
              filter(order >= first_negative_coef) |> 
              pull(Variable) -> neg_or_after_neg_coef_vars
            # FwdK elimina la ultima variable con coef negativo de las candidatas 
            # en el siguiente paso retoma el ajuste quedandose las variables antes 
            # de la primera negativa
            tab_coef |> 
              filter(order == last_negative_coef) |> 
              pull(Variable) -> var.sel
            vars.step <- neg_or_after_neg_coef_vars
            if (verbose) cli::cli_alert_info("Variable {var.sel} eliminada de las candidatas por coeficiente negativo")
            if (verbose && 
                (length(neg_or_after_neg_coef_vars) > 1)) cli::cli_alert_danger(
                  "Se retrocede más de un lugar en el FwdK! Se eliminan {neg_or_after_neg_coef_vars} de la fórmula actual")                     
            if (length(positive_coef_vars) == 0) {
              fmla <- as.formula(paste(target, " ~ NULL", sep = ""))
            } else {
              fmla <- as.formula(
                paste(target, "~", 
                      paste(positive_coef_vars, collapse = " + "), 
                      sep = " ")
              ) 
            }
            all.coef.positives <- -1            
          }
        }
        else  {
          var.sel <- NA
          all.coef.positives <- 0
        }
      }
      else {
        var.sel <- NA
        all.coef.positives <- 0
      }
    }
    return(list(all.coef.positives=all.coef.positives, idx=idx, 
                var.sel=var.sel, vars.step=vars.step, 
                coef.steps=coef.steps, 
                mod.curr=mod.curr, 
                fmla=fmla,
                ult.list.vars=vars.fwd |>  
                  filter(Variable != "<none>"), 
                vars.fwd.res=vars.fwd.res))
  }
}

logit.fwd <- function(train, target, verbose=F) { 
  train2 <- train
  idx <- 1
  vars.excl <- c()
  res.step <- logit.step(train2, target, verbose=verbose)
  vars.ajustes.fwd <- res.step$vars.fwd.res |> mutate(Ajuste = idx)
  if (is.null(res.step$vars.fwd.res) || nrow(res.step$vars.fwd.res) == 0)
    error_custom('Parámetros faltantes!', 
                 "i" = 'FwdK no pudo seleccionar variables!',
                 ">"=cli::col_red("204"))
  coef.steps <- res.step$coef.steps |> mutate(Ajuste=idx)
  while (res.step$all.coef.positives < 0 && 
         length(res.step$var.sel) > 0 && 
         !is.na(res.step$var.sel)) {
    if (!is.null(res.step$vars.fwd.res) && 
        nrow(res.step$vars.fwd.res) > 0) {
      vars.ajustes.fwd <- bind_rows(vars.ajustes.fwd, 
                                    res.step$vars.fwd.res |> mutate(Ajuste = idx))
    }
    vars.excl <- c(vars.excl, res.step$var.sel)
    train2 <- train2 |> select(-one_of(res.step$var.sel))
    res.step <- logit.step(train2, target, 
                           verbose=verbose, 
                           fmla=res.step$fmla) 
    idx <- idx + 1
    if (verbose) cli::cli_alert_info("Secuencia Fwd {idx}. Fórmula actual {res.step$fmla}")
    coef.steps <- bind_rows(coef.steps, 
                            res.step$coef.steps |> mutate(Ajuste=idx))
  }
  
  return(list(vars.excl=vars.excl, 
              vars.ajustes.fwd=vars.ajustes.fwd,  
              coef.steps=coef.steps, det=res.step))
}

# Generación de SQL ----------------------------------------------------------

lin_interp_sql <- function(x_var,x1,x2,y1,y2) {
  if (x1==x2) return(NULL)
  else return(paste("when",x_var, "<",x2,"then",
                    "(",y2,"-",y1,")*(",x_var,"-",x1,")/(",x2,"-",x1,")+",y1))
}

step_2_pwl <- function(xs, ys) {
  # le quito los valores para missing y total
  if(is.unsorted(xs) && is.unsorted(rev(xs)))
    error_custom('Se esperaba una serie ordenada', 
                 "i" = "Se obtuvo", xs,
                 "i" = "Revisar ", 
                 ">"=cli::col_red("Cod 318"))
  ys.n <- length(ys)
  # Si hay menos de 4 valores únicos entonces no hay dos puntos interiores. 
  # En este caso la interpolación es la simple
  if (n_distinct(xs) < 4) {
    # Asumo que el último elemento de $bands es el máximo sino hay un problema.
    if(!(length(xs)>=3 || nth(xs,-2) < nth(xs,-1))) 
      error_custom('Se esperaba una serie ordenada', 
                   "i" = "Se obtuvo", xs,
                   "i" = "Revisar ", 
                   ">"=cli::col_red("Cod 319"))      
    if (length(xs)>=3 && nth(xs,-3) < nth(xs,-2)) 
      return(tibble(x=c(nth(xs,-3),nth(xs,-2),nth(xs,-1)), 
                    y=c(nth(ys,-3),nth(ys,-2),nth(ys,-1))))
    else 
      return(tibble(x=c(nth(xs,-2),nth(xs,-1)), y=c(nth(ys,-2),nth(ys,-1))))
  } 
  else {
    # Si 2 o más puntos interiores se usan para construir la interpolación media.
    ys.m <- (ys[c(1,1:ys.n)]+ys[c(1:ys.n, ys.n)])/2
    if(!(length(ys.m)==length(xs))) 
      error_custom('Falló en interpolación', 
                   "i" = "Se esperaba", xs,
                   "i" = "de igual tamaño que", ys.m,  
                   ">"=cli::col_red("Cod 320"))
    
    # Verificar que sea TRUE!
    # rbind(xs, ys.m)
    # Agrego el punto intermedio entre xs[l-1] y xs[l] que continua la recta determinada 
    # por xs[l-2] y xs[l-1] y que tiene ordenada ys.m[l]
    ys.l <- length(ys.m)
    x <- lin_interp(ys.m[ys.l],ys.m[ys.l-2],ys.m[ys.l-1],xs[ys.l-2],xs[ys.l-1])
    # Me aseguro que es un punto intermedio entre xs[l-1] y xs[l] 
    x <- min(x,xs[ys.l])
    # xs <- append(xs, x, after = ys.l-1)
    # ys.m <- append(ys.m, ys.m[ys.l] , after = ys.l - 1)
    # Reemplazo directamente la última posición
    xs <- replace(xs,ys.l,x)
    ys.m <- replace(ys.m, ys.l, ys.m[ys.l])
    # rbind(xs, ys.m)
    # Ahora agrego el punto intermedio entre xs[1] y xs[2] que tiene ordenada ys.m[1]
    # notar que no importa que haya cambiado la longitud de xs y ys si se hace en este orden. 
    x <- lin_interp(ys.m[1],ys.m[2],ys.m[3],xs[2],xs[3])
    # de la misma manera me aseguro que el punto intermedio sea mayor que xs[1]
    x <- max(x, xs[1])
    # xs <- append(xs, x, after = 1)
    xs <- replace(xs,1,x)
    # ys.m <- append(ys.m, ys.m[1] , after = 1)
    ys.m <- replace(ys.m, ys.l, ys.m[ys.l])
    return(tibble(x=xs, y=ys.m))
  }
}

step_2_sql <- function(x_var, x_var_gen, step, woe_nulos) {
  res <- "case"
  if (step |> filter(is.na(x)) |> nrow() == 1) {
    res <- paste(res, "when",x_var, "is null then", woe_nulos)
    step <- step |> filter(!is.na(x)) 
  } 
  l <- nrow(step)
  if (2 <= l) 
    for (i in 2:l) 
      res <- paste(res, "when", x_var, "<", step$x[i], "then", step$y[i-1])
  res <- paste(res,"when",x_var, ">=",step$x[l],"then",step$y[l]) 
  # No debería ocurrir casos nulos o no contemplados.  Cero WoE es el WoE de toda la poblacion y de la mediana.
  res <- paste(res,"when",x_var, "is null then", woe_nulos)
  res <- paste(res, "else 0 end as", x_var_gen)
}

pwl_2_sql <- function(x_var, x_var_gen, pwl, woe_nulos) {
  if (pwl |> filter(is.na(x)) |> nrow() == 1) {
    res <- paste("case when",x_var, "is null then", woe_nulos)
    pwl <- pwl |> filter(!is.na(x)) 
    res <- paste(res, "when",x_var, "<=",pwl$x[1],"then",pwl$y[1])
  } else res <- paste("case when",x_var, "<=",pwl$x[1],"then",pwl$y[1])
  l <- nrow(pwl)
  for (i in 1:(l-1))
    res <- paste(res, lin_interp_sql(x_var,pwl$x[i],pwl$x[i+1],pwl$y[i],pwl$y[i+1]))
  res <- paste(res,"when",x_var, ">=",pwl$x[l],"then",pwl$y[l]) 
  # No debería ocurrir casos nulos o no contemplados.  Cero WoE es el WoE de toda la poblacion y de la mediana.
  res <- paste(res,"when",x_var, "is null then", woe_nulos)
  res <- paste(res, "else 0 end as", x_var_gen)
  res
}

ivout_2_sql <- function (ivout) {
  col <- ivout$x
  col_agrup <- ivout$var_gen
  if (ivout$type == "Continua" && ivout$rampa == 1) {
    return(pwl_2_sql(ivout$x, ivout$var_gen, 
                     ivout$woes_tab |> select(x=cut_median, y=WoE) |> arrange(x), 
                     ivout$woe_nulos))
  }
  else {
    if (ivout$type == "Continua" && ivout$rampa == 0) {
      return(step_2_sql(ivout$x, ivout$var_gen, 
                        ivout$woes_tab |> select(x=cut_lo, y=WoE) |> arrange(x), 
                        ivout$woe_nulos))
    } else { 
      if (ivout$type == "Factor") {
        when_clause <- ivout |> purrr::pluck('ivtable') |> 
          filter(!Cutpoint == "Total") |> select(Cutpoint, groups, WoE) |> 
          mutate(when_clause=if_else(Cutpoint=='_Missings', 
                                     paste("when", col, "is null then", WoE),
                                     paste("when", col, "in (", groups, ") then", WoE))) |> 
          summarise(when_clause=paste(when_clause, collapse = " ")) |> pull(when_clause)
        when_clause <- paste("case", when_clause, "else 0 end as", col_agrup, "", sep = " ")
        return(when_clause)
      } else {
        error_custom("Variable {ivout$x} tiene un problema en la discretización. ", 
                     "x" = "Tipo o Param rampas incorrectos. ", 
                     "i" = "Tipo {ivout$type}", 
                     "i" = "Rampas {ivout$rampa}", 
                     ">"=cli::col_red("Cod 321"))
      }
    }
  }
}


### Función para construir sentencia que traduce un modelo a sql
# Necesita transformaciones de variables especiales, discretizaciones, 
# el beta de calibración y coeficientes
# además de la tabla de sql con las variables primitivas y 
# Tiene efectos porque abre la conexión con
# Ejemplo de invocación
# mod_2_sql_source(tab.co, tab.bins) -> sent_sql
# 
# data_source_delim_path |> 
#   csv_2_score(con_target = TRUE,
#               spec_adic=datos_adic, 
#               mod_sql=sent_sql) -> scores_sql
# tabOrig_sql <- scores_sql |> select(Target, score) |> genTab_ntil(20)
# tab_all_sql <- scores_sql |> select(Target, score) |> genTab_f(genAsocCuantil(tabOrig_sql))
# ```
mod_2_sql_source <- function(par_coef=tab.coef, par_bins=tab.bins, 
                             con_target = TRUE) {
  
  # Antes tenía incorporada más funcionalidad para transformar variables. 
  transf.cad <- function(cad, par_vars) {
    for (i in 1:nrow(par_vars)) 
      cad <- cad |> stringr::str_replace(par_vars[[i,"var_orig"]], par_vars[[i,"var_tran"]])
    return(cad) 
  }
  
  val_2_chr <- function(val) {
    (suppressWarnings(val |> as.numeric() |> is.na())) -> p 
    if_else(p, paste0('"',val,'"'), as.character(val))
  }
  
  eq2list <- function(cad) {
    cad |> stringr::str_split("=",simplify = T) |> stringr::str_trim() -> l
    tibble(var = l[1], val = l[2])
  }
  
  tab <- par_coef |> select(Variable, Coef=Beta) |> filter(!is.na(Coef)) |> arrange(Variable)
  var.list <- tab$Variable[-1]
  vars2cases <- c()
  var.list.orig <- c()
  for (v in var.list) {
    idx <- which(sapply(par_bins, function(l) (l$var_gen==v)))[1]
    var.list.orig <- c(var.list.orig, par_bins[[idx]]$var)    
    vars2cases <- paste(vars2cases, ivout_2_sql(par_bins[[idx]]), sep=",\r\n\t")
  }
  vars2cases <- paste(vars2cases, " 1 as Intercept", sep = ", \r\n")
  
  # SF, 202409 simplifiqué la lógica se puede simplificar
  # Antes asumía una tabla real
  
  # Contruyo desde el query más interno al más externo. 
  'SELECT * FROM DATA_SOURCE' -> sql_source 
  
  var.list.orig -> initial_columns
  
  # Columnas controladas por parámetros
  if (!empty_param(par_ids)) 
    initial_columns |> c(par_ids) -> initial_columns
  if (!string_has_no_data(par_var_grupo)) 
    initial_columns |> c(par_var_grupo) -> initial_columns
  if (!string_has_no_data(par_vars_segmento)) 
    initial_columns |> c(par_vars_segmento) -> initial_columns
  if (con_target) initial_columns |> c(par_target) -> initial_columns
  if (!string_has_no_data(par_weight)) {
    initial_columns |> c(par_weight) -> initial_columns
  }
  if (!empty_param(par_rango_reportes)) {
    par_rango_reportes |> 
      pull(`Variables de corte`) |> 
      stringr::str_split(",\\s*") |> 
      unlist() |> 
      unique() |> 
      c(initial_columns) -> initial_columns
  }  
  initial_columns |> unique() -> initial_columns
  if (!string_has_no_data(cols_nulos_adic)) {
    cols_nulos_adic |> 
      reduce(.init = tibble(var = character(), val = character()), 
             .f = ~ .x |> bind_rows(.y |> eq2list() )) |> 
      group_by(var) |> 
      summarise(nulos = val |> val_2_chr() |> paste(collapse = ",")) |> 
      mutate(sent=paste('case when ',var,'in (', nulos, ') then null else ', var, 'end as', var)) -> tab_nulos_adic 
    
    initial_columns |> setdiff(tab_nulos_adic |> pull(var)) |> 
      paste(collapse = ", ") |> 
      paste(tab_nulos_adic |> pull(sent) |> paste(collapse = ", "), sep = ", ") -> cols_clause 
    
  } else {
    initial_columns |> paste(collapse = ", ") -> cols_clause 
  }
  
  # subquery más interno
  with_clause <- paste("with U as (", sql_source, ") ") 
  with_clause <- paste(with_clause, ",\r\n T as ( SELECT", cols_clause, "FROM U)")
  
  # Transformaciones 
  with_clause <- paste(with_clause, ",\r\n R as (select *",vars2cases, " from T) ", sep = "")
  # Coeficientes
  tab$Variable[[1]] <- "Intercept"
  with_clause <- paste(with_clause, ",\r\n S as (select *, ",
                       paste(tab$Variable, tab$Coef, sep = " * ", collapse = " + "),
                       " as eta from R) ")
  # Trans Final
  select_clause <- paste(with_clause, "\r\n select *, cast(1000/(1+exp(-eta)) as int) as score from S ", sep = "")
  scores_sql_source <- paste(with_clause, ", \r\n Q as (select *, cast(1000/(1+exp(-eta)) as int) as score_nue from S)", sep = "")
  
  return(scores_sql_source)
}

# Lectura y Scoreo --------------------------------------------------------

csv_2_score <- function(ds_path = data_source_delim_path, 
                        ds_delim = data_source_delim, ds_type = data_source_type, 
                        con_target = FALSE, spec_adic = datos_adic, mod_sql = sent_sql, 
                        cols_adic = NULL) 
{
  # Asume par_target en el Entorno!
  if (!string_has_no_data(par_vars_segmento)) c(par_vars_segmento, cols_adic) -> cols_adic  
  if (!string_has_no_data(par_var_grupo)) c(par_var_grupo, cols_adic) -> cols_adic  
  if (!string_has_no_data(par_weight)) c(par_weight, cols_adic) -> cols_adic
  if (!empty_param(par_rango_reportes)) {
    par_rango_reportes |> 
      pull(`Variables de corte`) |> 
      stringr::str_split(",\\s*") |> 
      unlist() |> 
      unique() |> 
      c(cols_adic) -> cols_adic
  }
  cols_adic |> unique() -> cols_adic
  
  cols_spec <- pluck(spec_adic, "spec_source")
  types_spec <- pluck(spec_adic, "types_source")
  if (!con_target) {
    cols_spec |> pluck('cols') <- cols_spec |> pluck('cols') |> discard_at(par_target)
    types_spec <- types_spec |> discard_at(par_target)
  }
  if (ds_type == "ODBC") {
    warning("Funcionalidad no testeada!")
    con <- odbc_connect_w_keyring(df_Param, verbose = FALSE)
  }
  else if (ds_type == "DELIM") {
    con <- odbc::dbConnect(RSQLite::SQLite(), ":memory:", verbose = FALSE)
    ds_delim <- stringr::str_replace(ds_delim, "\\\\t", "\t")
    
    ids <- pluck(spec_adic, ".ids")
    
    rlang::try_fetch(
      error = function(err) {
        error_custom(err=err, 
                     "x"="La lectura de {ds_path} tuvo un error!", 
                     "i"="Revisar muestra {.code ds_path} {ds_path}",
                     "i"="y {.code ds_delim} {ds_delim}",
                     ">"=cli::col_red("Cod 338"))
        
      }, 
      df <- vroom::vroom(fs::path(ds_path), delim = ds_delim, 
                         escape_double = FALSE, trim_ws = TRUE, show_col_types = F, 
                         col_types = cols_spec)
    )
    
    if (nrow(df) == 0) 
      error_custom("La lectura de {ds_path} no retornó ningún registro!", 
                   "i"="Revisar muestra {.code ds_path} {ds_path}",
                   "i"="y {.code ds_delim} {ds_delim}",
                   ">"=cli::col_red("Cod 312"))
    res <- df |> check_columns_and_types(types_spec)
    if (res$error > 0) 
      error_custom("Las columnas o tipos de {ds_path} tuvieron problemas!",
                   res$msg, 
                   ">"=cli::col_red("Cod 313"))
    copy_to(dest = con, df = df, name = "DATA_SOURCE")
  }
  else 
    error_custom("El tipo de la fuente de datos debe ser DELIM O ODBC. Fue: {ds_type}", 
                 "i"="Revisar parámetro {.code data_source_type}", 
                 ">"=cli::col_red("Cod 314"))    
  
  # Scoring
  if (con_target) {
    scores_sent_sql <- paste(mod_sql, "\r\n select *, score_nue as score,", 
                             par_target, "as target from Q")
  }
  else {
    scores_sent_sql <- paste(stringr::str_remove(mod_sql, 
                                                 paste0(",* *", par_target)), "\r\n select *, score_nue as score from Q")
  }
  
  rlang::try_fetch(
    error = function(err) {
      error_custom(err=err, 
                   "x"="El query de scoring tuvo un error!", 
                   "i"="Revisar muestra {.code ds_path} y {.code ds_delim}. ",
                   "i"="Revisar query de scoring: {scores_sent_sql |> stringr::str_sub(end = 1000)} ..." ,
                   ">"=cli::col_red("Cod 337"))
      
    }, 
    df <- odbc::dbGetQuery(con, scores_sent_sql)
  )
  odbc::dbDisconnect(con)
  
  if (nrow(df) == 0) 
    error_custom("El query de scoring no retornó ningún registro!", 
                 "i"="Revisar muestra {.code ds_path} y {.code ds_delim}", 
                 "i"="Revisar query de scoring: {scores_sent_sql |> stringr::str_sub(end = 1000)} ..." ,                 
                 ">"=cli::col_red("Cod 315"))    
  
  df |> colnames() -> columnas_muestra
  # Id check
  if (missing(ids) || all(ids == ".id")) {
    ids = ".id" 
    df <- mutate(df, .id = row_number(), .before = 1)
  } else {
    (ids |> setdiff(columnas_muestra)) -> cnd_vec
    if (length(cnd_vec)>0) 
      error_custom("La{?s} columna{?s} identificadora{?s} {cnd_vec} no existe{?n} en la muestra!", 
                   "i" = "Revisar parámetro {.code par_ids}",
                   "i" = "Revisar muestra {.code ds_path} y {.code ds_delim}",
                   ">"=cli::col_red("Cod 327")
      )
    df |> 
      summarise(unicos=n_distinct(across(all_of(ids))), n=n()) |> 
      mutate(ids_son_clave=unicos==n) |> 
      pull(ids_son_clave) -> cnd
    if (!cnd) 
      error_custom(
        "Las columna{?s} indentificadora{?s} - {(ids)} - no {?es/son} clave única!", 
        ">"=cli::col_red("Cod 328"))
  }
  
  # Weight check
  if (empty_param(par_weight)) {
    df |> mutate(.weight=1, .after = 1) -> df
  } else if (!all(par_weight %in% columnas_muestra)) {
    error_custom("La{?s} columna{?s} de pesos o frecuencia{?s} {par_weight} no existe{?n} en la muestra!", 
                 "i" = "Revisar parámetro {.code par_weight}",
                 ">"=cli::col_red("Cod 329"))
  } else {
    df |> mutate(.weight=.data[[par_weight]], .after = 1) -> df
    
    df |> 
      summarise(wt=sum(.weight), 
                wm=min(.weight), 
                wn=all(is.wholenumber(.weight))) -> res 
    if (!res$wm >= 0) 
      error_custom("Los pesos deben ser no negativos!", 
                   "i"="El mínimo fue {res$wm}", 
                   "i"="Los pesos se tomaron de la variable {par_weight}", 
                   ">"=cli::col_red("Cod 330"))
    if (!res$wt > 0) 
      error_custom("Los pesos no pueden ser todos ceros!", 
                   "i"="Los pesos se tomaron de la variable {par_weight}", 
                   ">"=cli::col_red("Cod 331"))
    if (!res$wn) 
      cli::cli_warn(c(
        "No todos los pesos son enteros!", 
        "x" = "Se esperan frecuencias en la variable {.var par_weight}.", 
        "i" = "La convergencia de la regresión logística {.emph NO} está garantizada!"), 
        ">"=cli::col_red("Cod 332")) 
  }
  
  # Cols_adic check
  (cols_adic |> setdiff(df |> colnames())) -> cnd_vec
  if (length(cnd_vec)>0) 
    error_custom("La{?s} columna{?s} {cnd_vec} no existe{?n} en la muestra con scores!", 
                 "i" = "Revisar muestra {.code ds_path}, {.code ds_delim} y {.code sent_sql}",
                 ">"=cli::col_red("Cod 336")
    )
  
  # Add .grupo?
  if (empty_param(par_var_grupo)) { 
    final_cols <- c(ids, ".weight", "score", cols_adic)
  } else if (!all(par_var_grupo %in% columnas_muestra)) {
    error_custom("La{?s} columna{?s} de grupo{?s} {par_var_grupo} no existe{?n} en la muestra!", 
                 "i" = "Revisar parámetro {.code par_var_grupo}", 
                 ">"=cli::col_red("Cod 335"))
  } else {
    df <- df |> 
      mutate(.grupo = .data[[par_var_grupo]]) 
    final_cols <- c(ids, ".weight", ".grupo", "score", cols_adic)
  }
  
  # Target check
  if (con_target) {
    if (!all(par_target %in% columnas_muestra)) 
      error_custom("La columna objetivo o target {par_target} no existe en la muestra!", 
                   "i" = "Revisar parámetro {.code par_target}",
                   ">"=cli::col_red("Cod 333"))
    df |> 
      mutate(Bad=.data[[par_target]]) |> 
      pull(Bad) -> bads 
    (bads %in% c(0,1)) -> cnd_vec
    which.min(c(cnd_vec, FALSE)) -> pos_break
    if (pos_break <= length(cnd_vec))
      error_custom("Se encontraron valores de la columna objetivo o target {par_target} no permitidos!", 
                   "i" = "Sólo se permiten valores 0 ó 1", 
                   ">"=cli::col_red("Cod 334"),
                   "x" = "Se encontró el valor ",
                   df |> slice(pos_break) |> 
                     select(all_of(par_target)) |> format_delim(delim = ','),
                   "x" = "En ", 
                   df |> slice(pos_break) |> 
                     select(all_of(.ids)) |> format_delim(delim = ','),
                   "i" = "Revisar parámetro {.code par_target}")
  }
  
  df |> summarise(scr_avg = mean(score), scr_avgw = mean(score*.weight)) -> tab
  if (!(tab$scr_avg |> dplyr::between(1, 999))) 
    error_custom("El query de scoring retornó un score no pesado medio de", 
                 tab$scr_avg, "!", 
                 ">"=cli::col_red("Cod 316"))    
  if (!(tab$scr_avgw |> dplyr::between(1, 999))) 
    error_custom("El query de scoring retornó un score pesado medio de", 
                 tab$scr_avgw, "!", 
                 ">"=cli::col_red("Cod 317"))     
  
  if (con_target) 
    df <- df |> 
    select(target, all_of(final_cols)) |> 
    relocate(target, .before = score) else 
      df <- df |> select(all_of(final_cols))
  
  return(df)
}

check_columns_and_types <- function(new, base) {
  if (is.data.frame(base)) {
    base |> map_chr(~ class(.x)) -> base_types
  } else {
    base -> base_types
  }
  # else asume chr vec de tipos
  setdiff(names(base_types), colnames(new)) -> missing_columns
  
  # Check for type mismatches in common columns
  intersect(names(base_types), colnames(new)) -> common_columns
  common_columns |> 
    keep(~ base_types[[.x]] != class(new[[.x]])) -> type_mismatches
  error <- 0
  msg <- ""
  if (length(missing_columns) > 0) {
    error <- 1
    msg <- paste(msg, "Error! Columnas faltantes:", missing_columns, "\n")
  } 
  if (length(type_mismatches) > 0) {
    error <- 1
    msg <- paste(msg, "Error! Tipos diferentes de los originales en:", type_mismatches, "\n")
  } 
  
  return(list(error=error, msg=msg, 
              missing_columns = missing_columns, 
              type_mismatches = type_mismatches))
}

# Unit Test
# Create test data frames
# train <- data.frame(
#   numeric_col = c(1.1, 2.2, 3.3),
#   integer_col = c(1L, 2L, 3L),
#   character_col = c("a", "b", "c"),
#   factor_col = factor(c("low", "medium", "high")),
#   logical_col = c(TRUE, FALSE, TRUE),
#   date_col = as.Date(c("2021-01-01", "2021-01-02", "2021-01-03"))
# )
# 
# test <- data.frame(
#   numeric_col = c(4.4, 5.5, 6.6),
#   integer_col = c(4L, 5L, 6L),
#   character_col = c("d", "e", "f"),
#   factor_col = factor(c("low", "medium", "high")),
#   logical_col = c(FALSE, TRUE, FALSE),
#   date_col = as.Date(c("2021-01-04", "2021-01-05", "2021-01-06"))
# )
# Introduce a type mismatch in the test data frame
# test$integer_col <- as.numeric(test$integer_col)
# # Drop a column in test df
# test <- test |> select(-logical_col)
# 
# # Run the function
# result <- check_columns_and_types(test, train)
# result
# 
# cat(result$msg)


# Range to new vars -------------------------------------------------------

# Ej Uso
# load_range(control_file, 'Valid', par_rango_niveles, c("Nombre Nivel", "Regla", "Tasa de malos máxima"), 
#            c("level_name", "rule", "max_allow_bad_rate")) -> tab_niv
load_range <- function(file, range, col_names, new_col_names, sheet='Valid') {
  file |> fs::path_ext() -> ext
  if (ext == 'xlsx') 
    file |> 
    readxl::read_excel(sheet = sheet, range = range, col_names = T) -> df 
  else if (ext == 'json') 
    if (is.data.frame(range)) 
      range -> df else 
        error_custom("Falló la carga de rango en dataframe!", 
                     "i" = "Se obtuvo {class(range)}", 
                     ">"=cli::col_red("Cod 322"))
  else error_custom("Se esperaba archivo de parámetros xlsx o json!", 
                    "i" = "Se obtuvo {ext}", 
                    ">"=cli::col_red("Cod 323"))
  df |> colnames() |> assertthat::are_equal(col_names) -> cnd
  if (!cnd) error_custom("Se esperaban los nombres de columnas ", 
                         col_names, " pero se obtuvieron", df |> colnames(), 
                         ">"=cli::col_red("Cod 324"))
  if (!missing(new_col_names)) colnames(df) <- new_col_names
  df |> mutate(level_order = row_number())
}

# Ej Uso
# tab_niv |> pull(max_allow_bad_rate) |> check_sorted_score_levels()
check_sorted_score_levels <- function(br) {
  assertthat::assert_that(!anyNA(br), msg = "Hay tasas de malo vacías!")
  assertthat::assert_that(!is.unsorted(br, strictly = TRUE), msg = "Las tasas de malos deben ser crecientes!")
  assertthat::assert_that(last(br) <= 1, msg = "Las peor tasa debe ser <= 1. En formato % es 100%.")
  assertthat::assert_that(first(br) > 0, msg = "Las mejor tasa debe ser > 0.")
}

# Ej Uso
# c(1:10, 10:1) |> vars_rep() 
# [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10"
vars_rep <- function(vec_char) {stringr::str_split(vec_char, pattern = " *, *") |> purrr::flatten_chr() |> unique()}

# Ej Uso
# df_work |> check_all_reps_vars_in_df(tab_rep)
check_all_reps_vars_in_df <- function(df, tab_rep) {
  assertthat::assert_that("report_name" %in% colnames(tab_rep), msg = "la tabla tab_rep debe tener la columna report_name")
  tab_rep |> pull(report_name) |> vars_rep() -> vars_rep
  all((vars_rep |> setdiff(c("Segmento", "score_niv"))) %in% colnames(df)) |> assertthat::assert_that(
    msg = "No se encuentran todas la variables de reporte listadas en los datos!")
  vars_rep
}


# Ej Uso
# df.new |> rename(score = score.new) |> 
#   range_2_newvar(tab_niv, "score_niv") |> 
#   range_2_newvar(tab_seg, "Segmento") |> 
#   select(1:5,ClienteNuevo, ClienteNuevoViejo, Segmento, score, score_niv) -> df_desa
# 
# df_desa |> group_by(score_niv) |> 
#   summarise(score_mn = min(score), score_mx = max(score), br = mean(bad)) |> 
#   arrange(br)
# 
# df_desa |> count(ClienteNuevo, ClienteNuevoViejo, Segmento) 
range_2_newvar <- function(df, tab_range, newvar) {
  all(c("level_name", "rule") %in% colnames(tab_range)) |> 
    assertthat::assert_that(msg = "To build {newvar} with case_when() the tab_range table need to have level_name and rule columns" |> 
                              stringr::str_glue())
  tab_range |> bind_rows(tibble(level_name = "Error", rule ="TRUE")) |> # For defense coding I add this last condition to the case_when() statement.
    purrr::pmap(function(rule, level_name, ...) expr(!!rlang::parse_expr(rule) ~ !!level_name)) -> conds
  df |> mutate({{newvar}} := case_when(!!!conds))
}

tab_niv_default_fct <- function(par_df, 
                                par_tab_niv_nbins = 5, 
                                par_minpts = par_minpts2) {
  # Uso:
  # df.scores |> 
  #   filter(.part == '1_Train') |> 
  #   mutate(Good=as.numeric(as.character(Good))) |> 
  #   select(score, Good, .weight) -> tab
  # tab |> tab_niv_default_fct()
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
  
  check_sorted_score_levels(tab_niv |> pull(TM)) -> chk_br
  
  tab_niv |>
    filter(level_order == max(level_order)) |> 
    mutate(check = (orden == 1)) |> 
    pull(check) -> chk_orden1
  
  # Chequeo de tabla
  # La tasa debe ser creciente y 
  # el peor nivel debe ser orden 1
  # Así se verifica la equivalencia entre cut_lo y orden
  if (!chk_br || !chk_orden1)
    cli::cli_warn(c(
      "x" = "Los niveles de riesgo por default no están ordenados!", 
      "i" = "Crear los niveles manualmente!",
      ">"=cli::col_red("Cod 210")))
  
  tab_niv |> 
    select(level_name, rule, TM_max, level_order, TM_min) -> tab_niv
  
  return(tab_niv)
  
}

corte_2_exprs <- function(corte) {
  corte |> stringr::str_split(pattern = " *, *") |> purrr::pluck(1) |> rlang::parse_exprs()
}

tab_rep_count_row <- function(l, df, var) {
  l |> corte_2_exprs() -> l_exprs
  df |> summarise({{var}} := l, q_reps = n_distinct(!!!l_exprs))
}

# Ej Uso
# df_work |> tab_rep_count(tab_rep, report_name)
tab_rep_count <- function(df, tab_rep, var) {
  tab_rep |> pull({{var}}) |> purrr::map_dfr(tab_rep_count_row, df, var)  
}

# Shiny App ---------------------------------------------------------------

binning_pack <- function(df) {
  tab = list()
  generada <- 1
  idx <- 1
  for (col in pred.cont) {
    res <- df |> bin_monot(y="Good",x=col, nbins1=par_nbins1, minpts1=par_minpts1, nbins2=par_nbins2, minpts2=par_minpts2, 
                           rampa=par_discret) 
    res_g <- df |> apply_grouping_monot(res, paste0(col,"_g"), rampa=par_discret, woe_nulos = res$woe_nulos)
    df <- res_g$df
    tab[[idx]] <- c(var=col, var_gen=paste0(col,"_g"), generada=generada, rampa=par_discret, res)
    idx <- idx + 1
  }
  
  for (col in (pred.fac)) {
    res <- df |> bin_factor(col, minpts=par_minpts_cat, Good="Good", maxlevels=100)
    res_g <- df |> apply_grouping_factor(res, paste0(col,"_g"))
    df <- res_g$df
    tab[[idx]] <- c(var=col, var_gen=paste0(col,"_g"), generada=generada, res)
    idx <- idx + 1
  }
  return(list(df=df, tab.bins=tab))
}

bin_tab_gt <- function(df, nomvar) {
  df |> 
    mutate("{paste0(nomvar, '_g')}":=round(df[[paste0(nomvar, "_g")]], 3)) |> 
    group_2_ivtab_factor(col = nomvar, col_agrup = paste0(nomvar, "_g")) |> 
    arrange(cut_lo) |>
    tab_fmt_fct(cut_lo, cut_hi) |> 
    select(-groups) |> 
    relocate(cut_lo, cut_hi, .after=Cutpoint) |> 
    gt::gt() |> 
    gt::tab_header(title = paste('Binning de ', nomvar)) |> 
    gt::fmt_number(c('WoE'), n_sigfig = 3, suffixing = T, locale = 'es_AR', drop_trailing_zeros = T) |> 
    gt::fmt_number(c('IV'), n_sigfig = 2, suffixing = T, locale = 'es_AR', drop_trailing_zeros = T) |> 
    gt::fmt_percent(c('PctRec', 'PctGood', 'PctBad', 'BadRate'), decimals = 1, scale = F, locale = 'es_AR') |> 
    gt::cols_label(Cutpoint = "Bin", CntRec = '# Obs', CntBad = '# Malos', 
                   PctRec = '% Obs', PctGood = '% Buenos', PctBad = '% Malos') |> 
    gt::sub_missing(columns = gt::everything())  
}

pesos_nue <- function(df, var = Tipo_Cliente, nivel = 'NUEVO', p_nue = prop_nue) {
  df |> summarise(n=n()) |> pull(n) -> tot
  df |> filter({{var}} == nivel) |> summarise(n=n()) |> pull(n) -> tot_nue
  w_vie <- 1
  if (p_nue==0 | tot_nue==0) w_nue <- 0 
  else if (p_nue==1) {w_nue <- 1; w_vie <- 0} 
  else w_nue <- (tot - tot_nue) / tot_nue * p_nue/(1-p_nue)
  df |> mutate(w = if_else({{var}} == nivel, w_nue, w_vie)) -> df
  return(df)
}

df_2_gt <- function(df) {
  df |> 
    group_by(Tipo_Cliente) |> 
    summarise(n=n()) |> 
    mutate(n_p=n/sum(n)) |> 
    rename(n_q=n) |> 
    select(Tipo_Cliente, n_q, n_p) |> 
    gt::gt(rowname_col = "Tipo_Cliente") |> 
    gt::tab_stubhead("Tipo_Cliente") |> 
    gt::cols_label(n_q="#", n_p="%") |> 
    gt::fmt_number(columns = c(n_q), decimals = 0) |> 
    gt::fmt_percent(columns = c(n_p), decimals = 0) |> 
    gt::grand_summary_rows(columns = c(n_q), fns = list(Total = ~sum(.)), 
                           formatter = gt::fmt_number, decimals = 0) |> 
    gt::grand_summary_rows(columns = c(n_p), fns = list(Total = ~sum(.)), 
                           formatter = gt::fmt_percent, decimals = 0) -> res
  return(res)
}

# Otras -------------------------------------------------------------------

Binning.plot <- function(result) {
  n <- nrow(result$ivtable)-2
  g <- tibble(x=result$bands[-1], y=result$ivtable[1:n, "BadRate"])
  g <- g |> ggplot2::ggplot(ggplot2::aes(x=x, y=y)) + 
    ggplot2::labs(x=result$x, y="Bad Rate")
  g <- g + ggplot2::geom_line(ggplot2::aes(colour="Binning")) + 
    ggplot2::geom_smooth(method = "loess", ggplot2::aes(colour="Suav"), se=F) + 
    ggplot2::geom_smooth(method = "lm", ggplot2::aes(colour="Lineal"), se=F) + 
    ggplot2::coord_cartesian(ylim = c(0, 1))
  return(g)  
}

na.sf.replace <- function (frame) {
  vars <- names(frame)
  for (j in vars) {
    x <- frame[[j]]
    pos <- is.na(x)
    if (any(pos)) {
      if (length(levels(x))) {
        xx <- as.character(x)
        xx[pos] <- "NA"
        x <- factor(xx, exclude = NULL)
      }
      frame[[j]] <- x
    }
  }
  frame    
}

vif_func <- function(in_frame,thresh=10,trace=T,...){
  # SF: Originalmente de https://www.kaggle.com/robertoruiz/dealing-with-multicollinearity
  # pero uso el vif del paquete car xq es innecesario sumar otro paquete como el fmsb 
  # Además cambié el método de impresión por kable.
  # de todas formas es una porquería como fabrica vif_vals y el control de flujo con break...  
  
  # require(car) #  library(fmsb)
  
  if(any(!'data.frame' %in% class(in_frame))) in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, vif(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      # prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      # Si hay mas de 4 variables es buena la impresión. Tendría que graficar.
      if (ncol(vif_init)<=4) print(knitr::kable(cbind(vif_init[,1], 
                                                      apply(vif_init[,-1], 2, function(x) round(as.numeric(x),2)))))
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      # Si llegamos a dos variables paramos
      if(length(var_names)<=2) break
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-vif(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        # prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        if (ncol(vif_vals)<=4) print(knitr::kable(cbind(vif_vals[,1], 
                                                        apply(vif_vals[,-1], 2, function(x) round(as.numeric(x),2)))))
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}

bin.table <- function(tab.bins, nomvar) {
  res <- sapply(tab.bins, function(l) (l$var==nomvar))
  tab <- tab.bins[[which(res)[1]]]
  return(tab)
}

woe.table <- function(tab.bins, nomvar) {
  bin.table(tab.bins, nomvar)$ivtable |> 
    knitr::kable(caption = paste("Binning de ", nomvar, "con binning monótono"), digits = 2)
}

# Deprecated
char_estab <- function(var.sel="edad", 
                       var.sel.agrup="edad_g", source.dev, source.sample) {
  select_clause <- paste("select", var.sel.agrup, ",min(", var.sel, ") as var_mn, 
                         max(", var.sel, ") as var_mx, count(1) as q  from ",
                         source.dev, "group by", var.sel.agrup)
  sampleDataQuery <- select_clause
  #inDataSource <- RxSqlServerData(sqlQuery = sampleDataQuery, connectionString = connStr)
  inDataSource <- RxSqlServerData(sqlQuery = sampleDataQuery, connectionString = connStr)
  tab.iv.1 <- rxImport(inDataSource) |> rename(q.dev=q)
  select_clause <- paste("select", var.sel.agrup, ",count(1) as q  from ",
                         source.sample, "group by", var.sel.agrup)
  sampleDataQuery <- select_clause
  #inDataSource <- RxSqlServerData(sqlQuery = sampleDataQuery, connectionString = connStr)
  inDataSource <- RxSqlServerData(sqlQuery = sampleDataQuery, connectionString = connStr)
  tab.iv.2 <- rxImport(inDataSource) |> rename(q.sam=q)
  tab.char.an <- tab.iv.1 |> full_join(tab.iv.2) |> arrange(var_mx) |> 
    replace_na(list(q.dev=0.1, q.sam=0.1))
  tab.char.an <- tab.char.an |> 
    mutate(t.dev=sum(q.dev), t.sam=sum(q.sam), f.dev=q.dev/t.dev, f.sam=q.sam/t.sam,
           dif.p=f.sam-f.dev, woe=log(f.sam/f.dev), iv=dif.p*woe) 
  tab <- tab.char.an |> select(var_mn, var_mx, f.dev, f.sam, dif.p, woe, iv)
  return(tab)
}

### Función para que devuelve un reporte de IV con totales.  
### Es bastante general, solo exije una variable de agrupamiento y Good.
### Está basada en agrup_factor.  Se aplica a continuas y factores porque sólo evalúa el IV
### luego de agrupar.  
### Tampoco calcula los integrantes o rangos de la variable sin agrupar de cada grupo.
repIV <- function(df, col, Good) {
  iv <- df |> group_2_ivtab_factor(col=col, 
                                   col_agrup=col, Good=Good) 
  iv <- iv |> select(-Cutpoint, -TotRec, -TotGood, -TotBad, -LnOdds, -LnODDS, -CntCumRec, 
                     -CntCumGood, -CntCumBad)
  return(iv)
}

# Auxiliares para la generación de Excels. 
tab_fmt_cont <- function(tab) {
  tab <- tab |> 
    select(cut_lo, cut_median, cut_mean, cut_hi, CntRec, CntGood, CntBad, PctRec, PctGood, PctBad, BadRate, WoE, IV)
  totals <- tab |> 
    summarise(cut_lo=min(cut_lo), cut_median=NA_integer_, cut_mean=NA_integer_, 
              cut_hi=max(cut_hi), CntRec=sum(CntRec), CntGood=sum(CntGood), CntBad=CntRec-CntGood, 
              PctRec=sum(PctRec), PctGood=sum(PctGood), PctBad=sum(PctBad), 
              BadRate=CntBad/CntRec, WoE=0, IV=sum(IV))  
  tab <- tab |> bind_rows(totals) |> 
    mutate_at(c("PctRec", "PctGood", "PctBad", "BadRate", "IV"), ~.*100)
  return(tab)
}

tab_fmt_fct <- function(tab, ...) {
  select_vars <- enquos(...)
  tab <- tab |> 
    select(Cutpoint, CntRec, CntGood, CntBad, 
           PctRec, PctGood, PctBad, BadRate, WoE, IV, WoE_datos, WoE_anulado, 
           !!!select_vars, groups)
  return(tab)
}

# list24lists <- function(long_list, name='Variable') {
#   l <- length(long_list) %/% 4 + 1
#   res <- list(tibble(!!name:=long_list) |> slice(1:l), 
#               matrix(numeric(), nrow=0, ncol=1), 
#               tibble(!!name:=long_list) |> slice((l+1):(2*l)), 
#               matrix(numeric(), nrow=0, ncol=1),                    
#               tibble(!!name:=long_list) |> slice((2*l+1):(3*l)), 
#               matrix(numeric(), nrow=0, ncol=1),
#               tibble(!!name:=long_list) |> slice((3*l+1):length(long_list)))
#   return(res)
# }

list24lists <- function(long_list, name='Var', long_min=12) {
  if (length(long_list) < long_min) return(tibble(!!name:=long_list))
  if (length(long_list) %% 4 > 0) # Relleno con NA_char
    long_list <- c(long_list, rep(NA_character_,4 - (length(long_list) %% 4)))
  l <- length(long_list) %/% 4
  res <- tibble(tibble(!!paste(name,1,"a",l):=long_list) |> slice(1:l), 
                tibble(!!paste(name,l+1,"a",2*l):=long_list) |> slice((l+1):(2*l)), 
                tibble(!!paste(name,2*l+1,"a",3*l):=long_list) |> slice((2*l+1):(3*l)), 
                tibble(!!paste(name,3*l+1,"a",4*l):=long_list) |> slice((3*l+1):length(long_list)),
                .name_repair="minimal")
  return(res)
}

df24df <- function(long_df, long_min=12) {
  if (nrow(long_df) < long_min) return(long_df)
  if (nrow(long_df) %% 4 > 0) # Relleno con NA
    long_df <- bind_rows(long_df, long_df |> slice(1:(4 - (nrow(long_df) %% 4))) |> mutate_all(~NA))
  l <- nrow(long_df) %/% 4
  res <- tibble(long_df |> slice(1:l), 
                long_df |> slice((l+1):(2*l)), 
                long_df |> slice((2*l+1):(3*l)), 
                long_df |> slice((3*l+1):nrow(long_df)), .name_repair="universal") |> 
    suppressMessages()
  return(res)
}

df23df <- function(long_df, long_min=12) {
  if (nrow(long_df) < long_min) return(long_df)
  if (nrow(long_df) %% 3 > 0) # Relleno con NA
    long_df <- bind_rows(long_df, long_df |> slice(1:(3 - (nrow(long_df) %% 3))) |> mutate_all(~NA))
  l <- nrow(long_df) %/% 3
  res <- tibble(long_df |> slice(1:l), 
                long_df |> slice((l+1):(2*l)), 
                long_df |> slice((2*l+1):nrow(long_df)), .name_repair="universal") |> 
    suppressMessages()
  return(res)
}

df23gt <- function(long_df, label_col1='Variable', label_col2='Valor', long_min=12) {
  long_df |> 
    rename('col1'=1, 'col2'=2) |> 
    df23df(long_min = long_min) -> tab 
  if (ncol(tab)==2) {
    tab |> 
      gt::gt() |> 
      gt::sub_missing(gt::everything()) |> 
      gt::cols_label(col1 = label_col1, col2 = label_col2) -> res
  } else {
    tab |> 
      gt::gt() |> 
      gt::sub_missing(gt::everything()) |> 
      gt::cols_label(col1...1 = label_col1, col2...2 = label_col2, 
                     col1...3 = label_col1, col2...4 = label_col2, 
                     col1...5 = label_col1, col2...6 = label_col2) -> res
  }
  return(res)
}

df24gt <- function(long_df, label_col1='Variable', label_col2='Valor', long_min=12) {
  long_df |> 
    rename('col1'=1, 'col2'=2) |> 
    df24df(long_min = long_min) -> tab 
  if (ncol(tab)==2) {
    tab |> 
      gt::gt() |> 
      gt::sub_missing(gt::everything()) |> 
      gt::cols_label(col1 = label_col1, col2 = label_col2) -> res
  } else {
    tab |> 
      gt::gt() |> 
      gt::sub_missing(gt::everything()) |> 
      gt::cols_label(col1...1 = label_col1, col2...2 = label_col2, 
                     col1...3 = label_col1, col2...4 = label_col2, 
                     col1...5 = label_col1, col2...6 = label_col2, 
                     col1...7 = label_col1, col2...8 = label_col2) -> res
  }
  res |> 
    tab_options(table.font.size = pct(80), container.width = px(1000),
                table.width = pct(100)) -> res
  return(res)
}


# En desarrollo -----------------------------------------------------------

# Versión que imita a Modeler 
# bin.sf <- function(x, nbins, minpts = floor(length(x)/nbins), nvals.min = 5,
#                    verbose = F) {
#   xtbl <- table(x)
#   xval <- as.numeric(names(xtbl))
#   nvals <- length(xval)
#   if (nvals < nvals.min || is.null(nbins) || floor(minpts)!=minpts) 
#     stop("bins.sf: parametros inconsistentes ", 
#          "\nValores unicos: ", nvals, "<",nvals.min,
#          "\nNumero de bines nulo? ", is.null(nbins), 
#          "\nCasos minimos x bin es entero? ", minpts)
#   binsz <- max(minpts, floor(length(x)/nbins))
#   binacc <- binsz
#   binlo <- vector(nbins, mode = "integer")
#   binhi <- vector(nbins, mode = "integer")
#   binct <- vector(nbins, mode = "integer")
#   startbin <- TRUE
#   k <- 1
#   s <- 0
#   for (i in 1:nvals) {
#     s <- s + xtbl[i]
#     if (startbin) {
#       binlo[k] <- xval[i]
#       startbin <- FALSE
#     }
#     if (s >= binacc) {
#       binhi[k] <- xval[i]
#       binct[k] <- s
#       # Descuenta del tamaño mínimo del bin el sobrante anterior.
#       # Tiene el efecto de que el tamaño acumulado se aproxime al tamaño acumulado por binsz
#       binacc <- binsz - (s- binacc) 
#       k <- k + 1
#       s <- 0
#       startbin <- TRUE
#     }
#   }
#   if (!startbin) {
#     # El último bucket no llegó al mínimo, entonces reseteo el actual y lo sumo al anterior
#     binlo[k] <- 0
#     k <- k - 1
#     binhi[k] <- xval[nvals]
#     binct[k] <- binct[k] + s
#   }
#   binlo <- binlo[1:k]
#   binhi <- binhi[1:k]
#   binct <- binct[1:k]
#   names(binct) <- paste("[", binlo, ", ", binhi, "]", sep = "")
#   if (verbose) 
#     print(binct)
#   return(list(binlo = binlo, binhi = binhi, binct = binct, 
#               xtbl = xtbl, xval = xval))
# }
