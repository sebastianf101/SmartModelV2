# Funciones auxiliares para la Presentación


###  Reporte de medidas desde DF con las columnas apropiadas.
# Devuelve Per_pred con la convención 0=train, 0.5=valid, Z-{0}=OOT
# Ejemplo:
# Data <- BSF_RF_v3_Res_old %>% select(Modelo, Periodo, Per_Pred, truth=Real, prob.Bad=Prob.Bad, prob.Good=Prob.Good,
#                                      response=Predicho)
#
# modelos <- list(list(name="RF_201407", train_per=201407),
#                 list(name="RF_201408", train_per=201408))
# periodos <- list(201407, 201408, 201504)
# res <- medidasxPersyMods(Data, modelos, periodos)
medidasxPeryMod <- function(data, modelo, train_per, periodo, valid=NA, msrs=measures) {
  if (is.na(valid)) {
    data <- data |> dplyr::filter(Modelo == modelo, Periodo == periodo) |>
      dplyr::filter(!is.na(response)) |> dplyr::select(truth, prob.Bad, prob.Good, response) |>
      as.data.frame()
    Per_pred <- diff_months(train_per, periodo)
  }
  else if (valid==0) {
    data <- data |> dplyr::filter(Modelo == modelo, Per_Pred == 0) |>
      dplyr::filter(!is.na(response)) |> dplyr::select(truth, prob.Bad, prob.Good, response) |>
      as.data.frame()
    Per_pred <- 0
  }
  else {
    # asumo valid = 1
    data <- data |> dplyr::filter(Modelo == modelo, Per_Pred == 0.5) |>
      dplyr::filter(!is.na(response)) |> dplyr::select(truth, prob.Bad, prob.Good, response) |>
      as.data.frame()
    Per_pred <- 0.5
  }
  if (nrow(data)==0) {
    error_custom(
      "No hay datos para medir!",
      paste("Modelo:", modelo, "Periodo:", periodo, "Per_pred", Per_pred, "valid:", valid)
    )
  }
  # Construyo un prediction result a mano.  Ojo que hay mucho hardcodeo!
  td <- list(id="BSF", type="classif", target="target", size=368516,
             n.feat=list("numerics"=53, "factors"=1, "ordered"=0),
             has.missings=FALSE, has.weights=FALSE, has.blocking=FALSE,
             class.levels=c("Bad","Good"), positive="Bad", negative="Good")
  class(td) <- c("ClassifTaskDesc", "SupervisedTaskDesc", "TaskDesc")
  pred <- list(predict.type = "prob",
               data = data,
               threshold = c(Bad=0.5, Good=0.5),
               task.desc = td, time = 1234567890,
               error = as.character(NA), dump = NULL)
  class(pred) <- list("PredictionClassif", "Prediction")
  res <- performance(pred, measures = msrs)
  res <- enframe(res, name = "Medida", value = "Valor")
  res <- data.frame(Modelo=modelo, Periodo=periodo, Per_train=train_per,
                    Per_pred=Per_pred, res)
  return(as_data_frame(res))
}

medidasxPersyMods <- function(Data, modelos, periodos) {
  res <- tribble(~Modelo, ~Periodo, ~Per_train, ~Per_pred, ~Medida, ~Valor)
  for (mod in modelos) {
    res <- bind_rows(res, medidasxPeryMod(Data, mod$name, mod$train_per, 0, valid=0))
    res <- bind_rows(res, medidasxPeryMod(Data, mod$name, mod$train_per, 0, valid=1))
    for (per in periodos)
      res <- bind_rows(res, medidasxPeryMod(Data, mod$name, mod$train_per, per, valid=NA))
  }
  return(res)
}

# SF, Calculate months difference between two dates
# requires lubridate package
# Ex: diff_months(201407, 201504)
# Note the format "%Y%m" is not used. So, I didn't change valid formats.
diff_months <- function(init.date, end.date, frmt = "%Y%m") {
  if (frmt=="%Y%m") {
    init.date <- as.Date(paste0(init.date, '01'), format='%Y%m%d')
    end.date <- as.Date(paste0(end.date, '01'), format='%Y%m%d')
  } else {
    init.date <- as.Date(init.date, format=frmt)
    end.date <- as.Date(end.date, format=frmt)
  }
  lubridate::interval(init.date, end.date) %/% months(1)
}


# Tablas por cuantiles y KS -----------------------------------------------
# generación de reportes de performance a cuantiles y con intervalos fijos.
# 20180111: Dejé todos los campos para construir medidas personalizadas
# Asume que df tiene los campos score y target.  Malos = sum(target)
# Ojo! Siguiendo los problemas con coalesce (ver https://github.com/tidyverse/dplyr/issues/2254)
# fuerzo la conversión a integer de Target
genTab_ntil <- function(df,n) {
  res <- df %>%
    arrange(desc(score)) %>%
    mutate(Cuantil=ntile(x=score, n=n), Target=dplyr::coalesce(as.integer(as.character(target)),0L)) %>%
    group_by(Cuantil) %>%
    summarise(Score_Min=min(score), Score_Prom=round(mean(score)), Score_Max=max(score),
              Buenos=sum(1-Target), Malos=sum(Target),
              Tasa_Malos=100*Malos/(Malos+Buenos), Odds=Buenos/Malos) %>%
    arrange(Cuantil) %>%
    mutate(Total=Buenos+Malos, P.Total=100*Total/sum(Total),
           asc.total=cumsum(Total), asc.buenos=cumsum(Buenos), asc.malos=cumsum(Malos),
           desc.total=sum(Total) - asc.total + Total,
           desc.malos=sum(Malos) - asc.malos + Malos,
           P.asc.Total=100*asc.total/sum(Total),
           P.asc.Buenos=100*asc.buenos/sum(Buenos),
           P.asc.Malos=100*asc.malos/sum(Malos),
           Tasa_Malos_desc=100*desc.malos/desc.total,
           asc.Odds=asc.buenos/asc.malos,
           Tasa_Malos_Rel=Tasa_Malos/(sum(Malos)/(sum(Buenos) + sum(Malos))),
           KS=abs(P.asc.Buenos - P.asc.Malos)) %>%
    arrange(desc(Cuantil)) %>%
    select(Cuantil, Score_Min, Score_Max, Total, P.Total, P.asc.Total,
           Malos, P.asc.Malos, Tasa_Malos, Tasa_Malos_Rel, Tasa_Malos_desc,
           KS, Odds)
  return(res)
}

genTab_f <- function(df,Asoc, rev=F, vars_adic=NULL) {
  res <- df %>%
    mutate(Target=dplyr::coalesce(as.integer(as.character(target)),0L), score=round(score)) %>%
    group_by(score) %>% arrange(desc(score)) %>%
    summarise(buenos=sum(1-Target), malos=sum(Target)) %>%
    mutate(Cuantil=Asoc(score)) %>% group_by(Cuantil) %>%
    summarise(Score_Min=min(score), Score_Prom=round(mean(score)), Score_Max=max(score),
              Buenos=sum(buenos), Malos=sum(malos),
              Tasa_Malos=100*Malos/(Malos+Buenos), Odds=Buenos/Malos)
  if (rev) res <- res %>% arrange(desc(Cuantil))
  else  res <- res %>% arrange(Cuantil)
    res <- res %>%
      mutate(Total=Buenos+Malos, P.Total=100*Total/sum(Total),
             asc.total=cumsum(Total), asc.buenos=cumsum(Buenos), asc.malos=cumsum(Malos),
             desc.total=sum(Total) - asc.total + Total,
             desc.malos=sum(Malos) - asc.malos + Malos,
             P.asc.Total=100*asc.total/sum(Total),
             P.asc.Buenos=100*asc.buenos/sum(Buenos),
             P.asc.Malos=100*asc.malos/sum(Malos),
             Tasa_Malos_desc=100*desc.malos/desc.total,
             asc.Odds=asc.buenos/asc.malos,
             Tasa_Malos_Rel=Tasa_Malos/(sum(Malos)/(sum(Buenos) + sum(Malos))),
             KS=abs(P.asc.Buenos - P.asc.Malos))
  if (rev) res <- res %>% arrange(Cuantil)
  else  res <- res %>% arrange(desc(Cuantil))
  if (missing(vars_adic))
    res <- res %>% select(Cuantil, Score_Min, Score_Max, Total, P.Total, P.asc.Total,
           Malos, P.asc.Malos, Tasa_Malos, Tasa_Malos_Rel, Tasa_Malos_desc,
           KS, Odds)
  else res <- res %>%
      select(Cuantil, Score_Min, Score_Max, Total, P.Total, P.asc.Total,
             Malos, P.asc.Malos, Tasa_Malos, Tasa_Malos_Rel, Tasa_Malos_desc,
             KS, Odds, all_of(vars_adic))
  return(res)
}

### Versión con mas campos
# quité el round de mean(score):     summarise(Score_Min=min(score), Score_Prom=round(mean(score)), Score_Max=max(score),
genTab_n_plus <- function(df,n) {
  res <- df %>%
    arrange(desc(score)) %>%
    mutate(Cuantil=ntile(x=score, n=n), Target=dplyr::coalesce(as.integer(as.character(target)),0L)) %>%
    group_by(Cuantil) %>%
    summarise(Score_Min=min(score), Score_Prom=mean(score), Score_Max=max(score),
              Buenos=sum(1-Target), Malos=sum(Target),
              Tasa_Malos=100*Malos/(Malos+Buenos), Odds=Buenos/Malos) %>%
    arrange(Cuantil) %>%
    mutate(Total=Buenos+Malos, P.Total=100*Total/sum(Total),
           asc.total=cumsum(Total), asc.buenos=cumsum(Buenos), asc.malos=cumsum(Malos),
           desc.total=sum(Total) - asc.total + Total,
           desc.malos=sum(Malos) - asc.malos + Malos,
           P.asc.Total=100*asc.total/sum(Total),
           P.asc.Buenos=100*asc.buenos/sum(Buenos),
           P.asc.Malos=100*asc.malos/sum(Malos),
           Tasa_Malos_desc=100*desc.malos/desc.total,
           asc.Odds=asc.buenos/asc.malos,
           Tasa_Malos_Rel=Tasa_Malos/(sum(Malos)/(sum(Buenos) + sum(Malos))),
           KS=abs(P.asc.Buenos - P.asc.Malos)) %>%
    arrange(desc(Cuantil))
    # %>%
    # select(Cuantil, Score_Min, Score_Max, Total, P.Total, P.asc.Total,
    #        Malos, P.asc.Malos, Tasa_Malos, Tasa_Malos_Rel, Tasa_Malos_desc,
    #        KS, Odds)
  return(res)
}


genAsocCuantil <- function(tab, rev=F) {
  res <- tab  %>%  select(Cuantil, Score_Max)
  res[1,2] <- 999
  f <- function(score) {
    if (rev)     cbind(res, score) %>% filter(score <= Score_Max) %>%
      summarise(Cuantil=max(Cuantil))  %>% pull()
    else
    cbind(res, score) %>% filter(score <= Score_Max) %>%
      summarise(Cuantil=min(Cuantil))  %>% pull()
  }
  return(Vectorize(f))
}

# Reports with GT package -------------------------------------------------

# Resume una tabla por la última variable de agrupación, calcula totales por las otras variables
# de agrupación y lo publica en gt
# Asume por lo menos dos variables de agrupación!
# Ej de uso Datos_muestra %>% group_by(.part, Bad) %>% sum_table_2_gt
sum_table_1_gt <- function(tab) {
  ult_group_col <- tab %>% group_vars() %>% tail(1)
  tab_gt <- tab |>
    summarise(Q=n(), W=sum(.weight)) |>
    mutate(PercQ=Q/sum(Q)) |>
    mutate(PercW=W/sum(W)) |>
    gt(rowname_col = ult_group_col, locale = 'es-AR') %>%
    tab_header(title=stringr::str_c('Distribucion de ',ult_group_col)) %>%
    tab_stubhead(label = ult_group_col) %>%
    sub_missing(columns = everything()) %>%
    fmt_number(columns = c(Q,W), decimals = 0) %>%
    fmt_percent(columns = c(PercQ, PercW), decimals = 1) %>%
    cols_align(align = 'right', columns = c(Q, W, PercQ, PercW)) %>%
    tab_style(style = cell_text(align = 'right'), locations = cells_stub()) %>%
    cols_align(align = 'center', columns = ult_group_col) %>%
    cols_label(PercQ=gt::html("% Q")) |>
    cols_label(PercW=gt::html("% W")) |>
    cols_move(columns = PercQ, after = Q) |>
    cols_move(columns = PercW, after = W) |>
    grand_summary_rows(columns = c(Q,W), fns = list(`Total` = ~sum(., na.rm = TRUE)),
                       fmt = ~ fmt_number(., decimals = 0, sep_mark = '')) %>%
    opt_row_striping()
  return(tab_gt)
}

# Resume una tabla por la última variable de agrupación, calcula totales por las otras variables
# de agrupación y lo publica en gt
# Asume por lo menos dos variables de agrupación!
# Ej de uso Datos_muestra %>% group_by(.part, Bad) %>% sum_table_2_gt
sum_table_2_gt <- function(tab) {
  ult_group_col <- tab |> group_vars() |> tail(1)
  other_group_cols <- tab |> group_vars() |> head(-1)
  first_group_col <- tab |> group_vars() |> head(1)

  other_group_cols |> internal_vars_labels() -> subtitle
  stringr::str_c('x ', "**", stringr::str_c(subtitle, collapse = ' y '), "**") -> subtitle

  # Summarise once and compute both subgroup (within-group) percentages and
  # per-group (wrt grand total) summary rows inserted as explicit rows.
  df_sum <- tab |>
    dplyr::summarise(Q = dplyr::n(), W = sum(.weight)) |>
    dplyr::ungroup()

  # Grand totals
  grand_Q <- sum(df_sum$Q)
  grand_W <- sum(df_sum$W)

  # Compute group totals and subgroup percentages (within group)
  df_sum <- df_sum |>
    dplyr::group_by(dplyr::across(dplyr::all_of(first_group_col))) |>
    dplyr::mutate(GroupQ = sum(.data$Q), GroupW = sum(.data$W)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      PercQ = .data$Q / .data$GroupQ,    # subgroup % (within group)
      PercW = .data$W / .data$GroupW
    )

  # Levels for the first grouping variable (to add per-group totals)
  first_group_levels <- df_sum |>
    dplyr::pull(dplyr::all_of(first_group_col)) |>
    unique() |>
    as.character()

  # Build explicit group-summary rows (unlabeled stub) that show group totals
  group_summaries <- df_sum |>
    dplyr::select(dplyr::all_of(first_group_col)) |>
    dplyr::distinct() |>
    dplyr::left_join(
      df_sum |>
        dplyr::group_by(dplyr::across(dplyr::all_of(first_group_col))) |>
        dplyr::summarise(GroupQ = sum(.data$Q), GroupW = sum(.data$W)),
      by = first_group_col
    ) |>
    dplyr::mutate(
      !!ult_group_col := "\u00A0",
      Q = .data$GroupQ,
      W = .data$GroupW,
      PercQ = .data$GroupQ / grand_Q,   # percent wrt Grand Total
      PercW = .data$GroupW / grand_W
    ) |>
    dplyr::select(dplyr::all_of(c(first_group_col, ult_group_col, "Q", "W", "PercQ", "PercW")))

  # Combine sub-rows and summary rows, ordering so summary is last within group
  df_out <- df_sum |>
    dplyr::mutate(!!ult_group_col := as.character(.data[[ult_group_col]])) |>
    dplyr::select(dplyr::all_of(c(first_group_col, ult_group_col, "Q", "W", "PercQ", "PercW"))) |>
    dplyr::bind_rows(group_summaries) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(first_group_col))) |>
    dplyr::mutate(.ord = dplyr::row_number()) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(first_group_col)), .ord) |>
    dplyr::ungroup() |>
    dplyr::select(-.ord)

  tab_gt <- df_out |>
    gt::gt(rowname_col = ult_group_col,
       groupname_col = other_group_cols,
       locale = 'es-AR') |>
    gt::tab_header(title = stringr::str_c('Distribucion de ',ult_group_col),
               subtitle = gt::md(subtitle)) |>
    gt::tab_stubhead(label = ult_group_col) |>
    gt::sub_missing(columns = everything()) |>
    gt::fmt_number(columns = 'Q', decimals = 0) |>
    gt::fmt_number(columns = 'W', decimals = 1) |>
    gt::fmt_percent(columns = c(PercQ, PercW), decimals = 1) |>
    gt::cols_align(align = 'right', columns = c(Q, W, PercQ, PercW)) |>
    gt::tab_style(style = gt::cell_text(align = 'right'), locations = gt::cells_stub()) |>
    gt::cols_align(align = 'center', columns = ult_group_col) |>
    gt::cols_label(PercQ=gt::html("% Q")) |>
    gt::cols_label(PercW=gt::html("% W")) |>
    gt::cols_move(columns = PercQ, after = Q) |>
    gt::cols_move(columns = PercW, after = W) |>
    # Summary rows for the inner groups (existing behaviour)
    gt::summary_rows(groups = TRUE, columns = c(Q), fns = list(Total = ~sum(., na.rm = TRUE)),
                 fmt = ~ gt::fmt_number(., decimals = 0, sep_mark = '')) |>
    gt::summary_rows(groups = TRUE, columns = c(W), fns = list(Total = ~sum(., na.rm = TRUE)),
                 fmt = ~ gt::fmt_number(., decimals = 1, sep_mark = ''))

  tab_gt <- tab_gt |>
    # Use precomputed grand totals so we don't double-count explicit per-group summary rows
    # Use function closures to capture the grand totals' values so they remain available
    # at render time (formulas can evaluate in a different environment and fail).
    gt::grand_summary_rows(columns = Q, fns = list(`Gran Total` = rlang::new_formula(NULL, grand_Q)),
                       fmt = ~ gt::fmt_number(., decimals = 0, sep_mark = '')) |>
    gt::grand_summary_rows(columns = W, fns = list(`Gran Total` = rlang::new_formula(NULL, grand_W)),
                       fmt = ~ gt::fmt_number(., decimals = 0, sep_mark = '')) |>
    gt::opt_row_striping()

  return(tab_gt)
}


internal_vars_labels <- function(vec_char) {
  vec_char |> stringr::str_replace_all(
    c("\\.part" = "Partición", "\\.id" = "Id",
      "\\.group" = "Grupo", "\\.grupo" = "Grupo", "\\.weight" = "Peso",
      "\\.segment" = "Segmento", "\\.segmento" = "Segmento"))
}


# Asume que df continen las columnas Target y score_niv y construye un reportes de performance con ellas.
# Asume que tab_ref_alin contiene las columnas level_name, TM_min y TM_max
# En ... poner las columnas que se quieren seleccionar en la tabla de salida entre
# score_niv, Score_Min, Score_Prom, Score_Max, Buenos, Malos, Tasa_Malos, Odds,
# Total, P.Total, asc.total, asc.buenos, asc.malos, desc.total, desc.malos,
# P.asc.Total, P.asc.Buenos, P.asc.Malos, Tasa_Malos_desc, asc.Odds,
# Tasa_Malos_Rel, KS
# La hice como función auxiliar de tab_niv_2_gt
# Ej. uso: df.scores %>% gentTab_niv %>% gt
# Ej. uso: df.scores %>% tab_niv_2_gt(title='Todos')
gentTab_niv <- function(df, tab_ref_alin = tab_niv, ...) {
  sel_vars <- rlang::enquos(...)
  group_vars <- df %>% group_vars()
  df <- df %>% mutate(Target=dplyr::coalesce(as.integer(as.character(Target)),0L))
  tab <- df %>%
    group_by(score_niv, .add=T) %>%
    summarise(Score_Min=min(score), Score_Prom=mean(score), Score_Max=max(score),
              Buenos=sum(1-Target), Malos=sum(Target),
              Tasa_Malos=Malos/(Malos+Buenos), Odds=Buenos/Malos) %>%
    arrange(desc(Score_Prom)) %>%
    mutate(Total=Buenos+Malos, P.Total=Total/sum(Total),
           asc.total=cumsum(Total), asc.buenos=cumsum(Buenos), asc.malos=cumsum(Malos),
           desc.total=sum(Total) - asc.total + Total,
           desc.malos=sum(Malos) - asc.malos + Malos,
           P.asc.Total=asc.total/sum(Total),
           P.asc.Buenos=asc.buenos/sum(Buenos),
           P.asc.Malos=asc.malos/sum(Malos),
           Tasa_Malos_desc=desc.malos/desc.total,
           Tasa_Malos_asc=asc.malos/asc.total,
           asc.Odds=asc.buenos/asc.malos,
           Tasa_Malos_Rel=Tasa_Malos/(sum(Malos)/(sum(Buenos) + sum(Malos))),
           KS=abs(P.asc.Buenos - P.asc.Malos),
           S_Min=(1-Score_Min/1000), S_Prom=(1-Score_Prom/1000), S_Max=(1-Score_Max/1000))
  tab |> left_join(tab_ref_alin, by = c("score_niv" = "level_name")) |>
    mutate(Alineado = TM_min <= Tasa_Malos & Tasa_Malos <= TM_max) -> tab
  KS <- tab %>% summarise(KS=max(KS), Alineado=all(Alineado),
                          TM_min=min(TM_min), TM_max=max(TM_max))
  ult_fila <- df %>%
    summarise(score_niv='Total', Score_Min=min(score), Score_Prom=mean(score), Score_Max=max(score),
              Buenos=sum(1-Target), Malos=sum(Target),
              Tasa_Malos=Malos/(Malos+Buenos), Odds=Buenos/Malos) %>%
    mutate(Total=Buenos+Malos, P.Total=1,
           asc.total=Total, asc.buenos=Buenos, asc.malos=Malos,
           desc.total=0,
           desc.malos=0,
           P.asc.Total=1,
           P.asc.Buenos=1,
           P.asc.Malos=1,
           Tasa_Malos_desc=Tasa_Malos,
           Tasa_Malos_asc=Tasa_Malos,
           asc.Odds=Odds,
           Tasa_Malos_Rel=1,
           S_Min=(1-Score_Min/1000),
           S_Prom=(1-Score_Prom/1000),
           S_Max=(1-Score_Max/1000)) %>%
    inner_join(KS, by = group_vars)
  tab <- tab %>% bind_rows(ult_fila)
  if (!missing(...)) return(tab %>% select(all_of(group_vars),!!!sel_vars)) else return(tab)
}

gentTab_B <- function(df, par_times = 2000, par_quantiles = c(0.025, 0.5, 0.975), ...) {
  other_args <- rlang::enquos(...)
  group_vars <- df %>% group_vars()
  df %>% bootstraps(times = par_times) -> bt_rs
  df %>%
    gentTab_niv(score_niv, Score_Min, Score_Prom, Score_Max, S_Max, S_Prom, Tasa_Malos, S_Min,
                Alineado, KS, Total, Malos, P.Total, P.asc.Total, Tasa_Malos_asc) -> tab
  bt_rs %>%
    transmute(boot_id=id, tab_niv=map(splits, ~ .x %>% (rsample::analysis) %>%
      gentTab_niv(score_niv, Score_Min, Score_Prom, Score_Max, S_Max, S_Prom, Tasa_Malos, S_Min,
                  Alineado, KS, Total, Malos, P.Total, P.asc.Total, Tasa_Malos_asc))) %>%
    tidyr::unnest(cols = c(tab_niv)) -> tab_bt
  tab_bt %>% group_by(across(all_of(group_vars)), score_niv) %>%
    summarise(B_Cuantil=par_quantiles, Tasa_Malos_B=quantile(Tasa_Malos, probs=par_quantiles),
              KS_B=quantile(KS, probs=par_quantiles),
              Alineado_B=mean(Alineado)) -> tab_bt_sum
  tab_bt_sum %>%
    tidyr::pivot_wider(id_cols = c(Segmento, score_niv, Alineado_B),
                names_from = B_Cuantil, values_from = c(Tasa_Malos_B, KS_B)) -> tab_bt_sum
  res <- tab %>% inner_join(tab_bt_sum, by = c(group_vars, "score_niv"))
  return(res)
}

tab_niv_2_gt <- function(df, title='ALL') {
  res <- df %>%
    gentTab_niv(score_niv, Score_Min, Score_Prom, Score_Max, S_Max, S_Prom, Tasa_Malos, S_Min,
                Alineado, KS, Total, Malos, P.Total, P.asc.Total, Tasa_Malos_asc) %>%
    gt %>%
    tab_header(title = title) %>%
    cols_label(
      score_niv = gt::html("Nivel<br> de Riesgo"),
      Score_Min = gt::html("Score<br> m&iacute;nimo"),
      Score_Prom = gt::html("Score<br> medio"),
      Score_Max = gt::html("Score<br> m&aacute;ximo"),
      S_Min = gt::html("Riesgo S+<br> m&iacute;nimo"),
      S_Prom = gt::html("Riesgo S+<br> medio"),
      Tasa_Malos = gt::html("% M"),
      S_Max = gt::html("Riesgo S+<br> m&aacute;ximo"),
      Alineado = gt::html("<b>Score<br> alineado?<b>"),
      Total = gt::html("#"),
      Malos = gt::html("Malos"),
      P.Total = gt::html("# %"),
      P.asc.Total = gt::html("# %+"),
      Tasa_Malos_asc = gt::html("% M+"),
      KS='KS') %>%
    fmt_number(columns = c(Score_Min, Score_Prom, Score_Max), decimals = 0) %>%
    fmt_percent(columns = c(S_Min, S_Prom, S_Max), decimals = 1) %>%
    fmt_percent(columns = c(P.Total, P.asc.Total, KS), decimals = 0) %>%
    fmt_percent(columns = c(Tasa_Malos, Tasa_Malos_asc), decimals = 1) %>%
    cols_align(
      align = "center",
      columns = c(score_niv, Score_Prom)
    ) %>%
    tab_style(
      cell_text(align = 'center'),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      cell_text(weight = 'bold'),
      locations = cells_body(columns=c(S_Min, Tasa_Malos, S_Max))
    ) %>%
    tab_style(
      cell_text(color = 'red'),
      locations = cells_body(columns=c(Alineado), rows = (Alineado==F))
    ) %>%
    tab_style(
      cell_text(color = 'red'),
      locations = cells_body(columns=c(Tasa_Malos), rows = (Alineado==F))
    ) %>%
    tab_options(
      table.font.size = px(10L)
    ) %>%
    opt_table_lines(extent = 'all') %>%
    opt_row_striping()
  return(res)
}

# Asume que la tabla fue construida por gentTab_B()
tab_niv_bt_gt <- function(tab_bt, title='ALL') {
  res <- tab_bt %>%
    select(all_of(group_vars(tab_bt)),
           score_niv, Score_Min, Score_Prom, Score_Max, S_Max, S_Prom,
           TM_min, Tasa_Malos_B_0.025, Tasa_Malos, Tasa_Malos_B_0.975, TM_max,
           S_Min, Alineado, Alineado_B, KS_B_0.025, KS, KS_B_0.975,
           Total, Malos, P.Total, P.asc.Total, Tasa_Malos_asc) %>%
    gt %>%
    tab_header(title = title) %>%
    cols_label(
      score_niv = gt::html("Nivel<br> de Riesgo"),
      Score_Min = gt::html("Score<br> m&iacute;nimo"),
      Score_Prom = gt::html("Score<br> medio"),
      Score_Max = gt::html("Score<br> m&aacute;ximo"),
      S_Min = gt::html("Riesgo S+<br> m&iacute;nimo"),
      S_Prom = gt::html("Riesgo S+<br> medio"),
      TM_min = gt::html("Tasa de malos<br> m&iacute;nima<br> permitida"),
      Tasa_Malos_B_0.025 = gt::html("2.5% quant<br><b>% M<b>"),
      Tasa_Malos = gt::html("<b>% M<b>"),
      Tasa_Malos_B_0.975 = gt::html("97.5% quant<br><b>% M<b>"),
      TM_max = gt::html("Tasa de malos<br> m&aacute;xima<br> permitida"),
      S_Max = gt::html("Riesgo S+<br> m&aacute;ximo"),
      Alineado = gt::html("Score<br> alineado?"),
      Alineado_B = gt::html("<b>% veces Alineado<b>"),
      Total = gt::html("#"),
      Malos = gt::html("Malos"),
      P.Total = gt::html("# %"),
      P.asc.Total = gt::html("# %+"),
      Tasa_Malos_asc = gt::html("% M+"),
      KS_B_0.025 = gt::html("2.5% quant<br><b>% KS<b>"),
      KS=gt::html('<b>KS<b>'),
      KS_B_0.975 = gt::html("97.5% quant<br><b>% KS<b>")) %>%
    fmt_number(columns = c(Score_Min, Score_Prom, Score_Max), decimals = 0) %>%
    fmt_percent(columns = c(S_Min, S_Prom, S_Max, Alineado_B), decimals = 1) %>%
    fmt_percent(columns = c(P.Total, P.asc.Total, KS, KS_B_0.025, KS_B_0.975), decimals = 0) %>%
    fmt_percent(columns = c(Tasa_Malos, Tasa_Malos_asc, Tasa_Malos_B_0.025,
                               Tasa_Malos_B_0.975, TM_min, TM_max), decimals = 1) %>%
    cols_align(
      align = "center",
      columns = c(score_niv, Score_Prom)
    ) %>%
    tab_style(
      cell_text(align = 'center'),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      cell_text(weight = 'bold'),
      locations = cells_body(columns=c(S_Min, Tasa_Malos, S_Max))
    ) %>%
    tab_style(
      cell_text(weight = 'bold'),
      locations = cells_body(columns=KS, rows = score_niv == "Total")
    ) %>%
    tab_style(
      cell_text(color = 'red'),
      locations = cells_body(columns=c(Alineado), rows = (Alineado==F))
    ) %>%
    tab_style(
      cell_text(color = 'red'),
      locations = cells_body(columns=c(Tasa_Malos), rows = (Alineado==F))
    ) %>%
    tab_options(
      table.font.size = px(10L)
    ) %>%
    opt_table_lines(extent = 'all') %>%
    opt_row_striping()
  return(res)
}

# La sigiente es una función que dada una tabla
# la formatea con gt() en n_main_cols tablas y las pone una al lado de la otra.
# Sirve para que tablas largas no ocupen muchas líneas en un reporte.
# Ejemplo de uso
# tibble(Var=pred.cont) |> long_tab_2_wide_gt(cols = "Var") |>
#   tab_header("Variables de las muestra inicial clasificadas Continuas")
long_tab_2_wide_gt <- function(long_tab, cols = c('Variable'), n_main_cols = 3,
                               n_row_min=10) {

  long_tab |> select(all_of(cols)) |>
    mutate(row_n = row_number()) -> tab

  if (nrow(long_tab) < n_row_min) {
    tab |>
      mutate(id_wth_col = row_n) |>
      select(-row_n) -> tab
  } else {
    long_tab |>
      mutate(row_n = row_number(),
             n_by_col = ceiling(nrow(long_tab) / n_main_cols),
             wide_col = ceiling(row_n / n_by_col),
             id_wth_col = row_n %% n_by_col,
             id_wth_col = if_else(id_wth_col==0, n_by_col, id_wth_col)) |>
      group_by(wide_col) |>
      mutate(row_min = min(row_n),
             row_max=max(row_n),
             main_col = paste0(row_min, " - ", row_max)) |>
      ungroup() |>
      select(-row_n, -n_by_col, -row_min, -row_max, -wide_col) |>
      tidyr::pivot_wider(names_from = main_col, values_from = all_of(cols)) -> tab
  }

  tab |>
    gt(locale = 'es-AR') |>
    cols_label(id_wth_col='Orden') |>
    tab_style( style = "vertical-align:top",
               locations = cells_body(columns = everything())) |>
    # Ojo que por ahora formatea así todas las columnas excepto la primera!
    # Se puede sobreescribir luego. gt toma el último formato.
    fmt_number(columns = where(is.numeric), decimals = 1) |>
    fmt_markdown(columns = everything()) |>
    opt_row_striping()
}


resumen_fwd_gt <- function(logit.fwd.res=res.trad, tab_bins=tab.bins) {

  logit.fwd.res$det$mod.curr |> broom::tidy() |>
    select(term, estimate) |>
    rename(Variable=term, Beta=estimate) |>
    filter(Variable!="(Intercept)") -> vars_mod

  # Use vars.fwd.all.candidates if available, otherwise vars.ajustes.fwd
  if ("vars.fwd.all.candidates" %in% names(logit.fwd.res) &&
      !is.null(logit.fwd.res$vars.fwd.all.candidates) &&
      nrow(logit.fwd.res$vars.fwd.all.candidates) > 0) {
    tab_trios <- logit.fwd.res$vars.fwd.all.candidates |>
      group_by(Ajuste, Paso) |>
      arrange(desc(LRT)) |>
      mutate(puesto=row_number()) |>
      filter(puesto |> between(1,3)) |>
      select(Ajuste, Paso, puesto, LRT, `Pr(>Chi)`, Variable) |>
      arrange(Ajuste, Paso, puesto)
  } else {
    # Fallback: single winner per step
    tab_trios <- logit.fwd.res$vars.ajustes.fwd |>
      mutate(puesto=1) |>
      select(Ajuste, Paso, puesto, LRT, `Pr(>Chi)`, Variable) |>
      arrange(Ajuste, Paso, puesto)
  }

  tab_trios |>
    filter(puesto==1) |>
    ungroup() |>
    mutate(orden=row_number()) |>
    arrange(desc(orden)) |>
    distinct(Variable, .keep_all = TRUE) |>
    arrange(orden) |>
    select(Ajuste, Paso, orden, LRT, `Pr(>Chi)`, Variable) -> tab_elegidas

  tab_trios |>
    select(Ajuste, Paso, puesto, Variable) |>
    tidyr::pivot_wider(names_from = puesto, values_from = Variable,
                       names_prefix = "Variable") |>
    rename(Variable=Variable1) |>
    ungroup() -> tab_compet

  tab_bins |>
    map(~tibble(Variable=.$var_gen, IV=.$iv, Tipo=.$type, Sentido=.$sentido)) |>
    reduce(bind_rows) |>
    inner_join(vars_mod, by="Variable") |>
    mutate(flechas=case_when(
      is.na(Sentido) & Tipo == "Factor"  ~ "circle-arrow-right",
      Sentido == 1 & Tipo == "Continua"  ~ "circle-arrow-up",
      Sentido == -1 & Tipo == "Continua" ~ "circle-arrow-down",
      TRUE                               ~ "circle-question"
    )) -> tab_ivs

  tab_ivs |>
    inner_join(tab_elegidas, by = "Variable") |>
    inner_join(tab_compet, by = c("Ajuste", "Paso", "Variable")) |>
    # Para no tener que explicar las complejidades de Ajuste y Paso creo Orden
    arrange(Ajuste, Paso) |>
    mutate(Orden=row_number()) |>
    arrange(desc(IV)) -> tab_fwd_res

  # Ensure Variable2 and Variable3 columns exist (even if NA)
  if (!"Variable2" %in% names(tab_fwd_res)) {
    tab_fwd_res <- tab_fwd_res |> mutate(Variable2 = NA_character_)
  }
  if (!"Variable3" %in% names(tab_fwd_res)) {
    tab_fwd_res <- tab_fwd_res |> mutate(Variable3 = NA_character_)
  }

  tab_2_gt <- function(tab) {
    tab |> ungroup() |> gt(locale = 'es-AR') |>
      fmt_icon(columns = flechas, fill_color = 'black', stroke_color = 'white') |>
      opt_stylize(style = 5, color = "red") |>
      fmt_number(columns = c(LRT, IV), decimals = 2) |>
      fmt_number(columns = c(Beta), decimals = 2) |>
      fmt_number(columns = c(`Pr(>Chi)`), decimals = 5) |>
      tab_header(title = "Resumen Pasos") |>
      cols_label(Orden="Paso",
                 Beta=gt::html("Coeficiente<br><em>\u03b2</em>"),
                 flechas="Direccion") |>
      tab_style(style = cell_text(align = "center", v_align = "middle"),
                locations = cells_column_labels()) |>
      tab_style(style = cell_text(align = "center", v_align = "middle"),
                locations = cells_body(columns = flechas))

  }

  # Always include competitor columns, even if they're NA
  res <- tab_fwd_res |>
    select(Variable, IV, flechas, Beta, Orden, `Pr(>Chi)`, LRT,
           Variable2, Variable3) |>
    tab_2_gt() |>
    cols_label(Variable2=gt::html("Variable<br>competidora 1"),
               Variable3=gt::html("Variable<br>competidora 2"))

  return(res)

}

det_ivs_cont <- function(var_names, tipo_discr="stp", par_tab.bins=tab.bins) {
  if (length(var_names)==0) return(list(error=T, cod="sin_vars", descr="Lista de nombres vacía!"))
  tabs <- par_tab.bins |>
    keep(~.x$var %in% var_names && .x$type=="Continua" && .x$generada==1)
  if (length(tabs)==0) return(list(error=T, cod="sin_ivts", descr="Lista de variables vacía!"))
  var_names <- tabs |> map_chr(~.x$var)
  if (!(tipo_discr %in% c("stp", "pwl"))) return(list(error=T, cod="tipo_discr_invalid",
                                                      descr=paste("El tipo de discretización", tipo_discr,
                                                                  "es inválido para variables continuas!", collapse = " ")))
  if (tipo_discr=="stp") var_ivt <- tabs |> map(~.x$tab_iv_stp)
  else var_ivt <- tabs |> map(~.x$tab_iv_pwl)
  var_ivt <- set_names(var_ivt, var_names)
  var_direction <- tabs |> map(~.x$sentido)
  return(list(error=F, cod="tab_ivs_cont", nom_ivs=var_names, tab_ivs=var_ivt,
              direct_ivs=var_direction))
}

det_ivs_cont_gt <- function(nom_ivs, tab_ivs) {
  gt_group() -> tables

  nom_ivs |> order() -> index
  for (i in index)
    tables |>
    grp_add(
      det_iv_cont_gt(
        tab_ivs[[i]],
        nom_ivs[[i]]
      )
    ) -> tables
  return(tables)
}

det_iv_cont_gt <- function(ivt, title) {
  tabla_gt <- ivt |> gt(locale = 'es-AR') |>
    tab_header(title = title) |>
    tab_options(table.font.size = pct(80), container.width = px(1000),
                table.width = pct(100)) |>
    fmt_number(c('orden'), decimals = 1,  # Recordar que los nulos tienen decimal .5
               suffixing = T, drop_trailing_zeros = T) |>
    fmt_number(c('cut_lo', 'cut_median', 'cut_hi'), n_sigfig = 3,
               suffixing = T, drop_trailing_zeros = T) |>
    fmt_number(c('WoE', 'woe_est_lo', 'woe_est_hi'), n_sigfig = 3, suffixing = TRUE,
               drop_trailing_zeros = TRUE) |>
    fmt_number(c('IV'), n_sigfig = 2, suffixing = TRUE, drop_trailing_zeros = TRUE) |>
    fmt_percent(c('PctRec', 'PctGood', 'PctBad', 'BadRate'), decimals = 1, scale = F) |>
    cols_label(col_agrup = 'Rango', bin_woe='Rango (WoE)',  woe_est_lo='Min (WoE)', woe_est_hi='Max (WoE)',
               cut_lo = 'Min', cut_median = 'Mediana', cut_hi = 'Max',
               CntRec = '# Obs', CntGood = '# Buenos', CntBad = '# Malos',
               PctRec = '% Obs', PctGood = '% Buenos', PctBad = '% Malos') |>
    sub_missing(columns = everything())
  return(tabla_gt)
}

det_ivs_categ <- function(var_names, par_tab.bins=tab.bins) {
  if (length(var_names)==0) return(list(error=T, cod="sin_vars", descr="Lista de nombres vacía!"))
  tabs <- par_tab.bins |>
    keep(~.x$var %in% var_names && .x$type=="Factor" && .x$generada==1)
  if (length(tabs)==0) return(list(error=T, cod="sin_ivts", descr="Lista de variables vacía!"))
  var_names <- tabs |> map_chr(~.x$var)
  var_ivt <- tabs |> map(~tab_fmt_fct(.x$ivtable))
  var_ivt <- set_names(var_ivt, var_names)
  return(list(error=F, cod="tab_ivs_categ", nom_ivs=var_names, tab_ivs=var_ivt))
}

det_iv_categ_gt <- function(ivt, title) {
  tabla_gt <- ivt |>
    mutate(WoE_anulado = as.logical(WoE_anulado)) |>
    gt(locale = 'es-AR') |>
    tab_header(title = title) |>
    tab_options(table.font.size = pct(80), container.width = px(1000),
                table.width = pct(100)) |>
    cols_move_to_end('groups') |>
    fmt_number(c('WoE','WoE_datos'), n_sigfig = 3, suffixing = T, drop_trailing_zeros = T) |>
    fmt_number(c('IV'), n_sigfig = 2, suffixing = T, drop_trailing_zeros = T) |>
    fmt_percent(c('PctRec', 'PctGood', 'PctBad', 'BadRate'), decimals = 1, scale = F) |>
    cols_label(Cutpoint = 'Grupo', groups = 'Integrantes',
               CntRec = '# Obs', CntGood = '# Buenos', CntBad = '# Malos',
               PctRec = '% Obs', PctGood = '% Buenos', PctBad = '% Malos') |>
    tab_style(
      style = cell_text(color = "red"),
      locations = cells_body(
        columns = WoE_anulado,
        rows = WoE_anulado == TRUE)) |>
    sub_missing(columns = everything())
  return(tabla_gt)
}

det_ivs_categ_gt <- function(nom_ivs, tab_ivs) {
  gt_group() -> tables

  nom_ivs |> order() -> index
  for (i in index)
    tables |>
    grp_add(
      det_iv_categ_gt(
        tab_ivs[[i]],
        nom_ivs[[i]]
      )
    ) -> tables
  return(tables)
}

iv_tab_cont_2_plot <- function(var_name, iv_tab, direction) {
  assertthat::assert_that(direction %in% c(-1,1))

  if (direction==-1) descendente <- TRUE
  else descendente <- FALSE

  iv_tab |>
    filter(col_agrup != "Total") |>
    mutate(col_agrup = case_when(
      col_agrup |> stringr::str_detect("NA") ~ "Nulos ",
      col_agrup |> stringr::str_detect("NULL") ~ "Nulos ",
      col_agrup |> stringr::str_detect("Nulo") ~ "Nulos ",
      col_agrup |> stringr::str_detect("Missing") ~ "Nulos ",
      col_agrup |> stringr::str_detect("Otros_") ~ "Resto ",
      TRUE ~ col_agrup
    )) |>
    # Para gráficos, si hay más de una tabla, elijo col_agrup de la primera
    group_by(orden) |>
    mutate(col_agrup = first(x=col_agrup, order_by=valor_grupo)) |>
    ungroup() |>
    mutate(grupo = forcats::fct_reorder(col_agrup, orden, .desc = descendente)) |>
    mutate(WoE_gr = if_else(woe_est_lo == woe_est_hi & 0 == woe_est_hi, 0, WoE)) |>
    select(grupo, PctRec, WoE_gr) |>
    mutate(PctRecLabel = scales::label_percent(scale = 1, accuracy = 1)(PctRec)) -> tab

  altura <- max(50, tab$PctRec)

  tab |>
    ggplot(aes(x=grupo, y=PctRec, group=1, label = PctRecLabel)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
    coord_cartesian(ylim = c(0, altura)) +
    scale_x_discrete(expand = expansion(add = c(0,1.5))) +
    scale_y_continuous(name = "% Pob",
                       sec.axis = sec_axis(~./12.5-2, name="WoE")) +
    geom_line(aes(y=(2+WoE_gr)*12.5), colour="brown1") +
    geom_point(aes(y=(2+WoE_gr)*12.5), colour="brown1", size=2.5) +
    geom_text(vjust = -0.5, size = 2.5)
}

iv_tab_categ_2_plot <- function(var_name, iv_tab) {
  iv_tab |>
    filter(Cutpoint != "Total") |>
    mutate(col_agrup = case_when(
      Cutpoint |> stringr::str_detect("NA") ~ "Nulos ",
      Cutpoint |> stringr::str_detect("NULL") ~ "Nulos ",
      Cutpoint |> stringr::str_detect("Missing") ~ "Nulos ",
      Cutpoint |> stringr::str_detect("Nulo") ~ "Nulos ",
      Cutpoint |> stringr::str_detect("Otros_") ~ "Resto ",
      TRUE ~ Cutpoint
    )) |>
    mutate(grupo = forcats::fct_reorder(col_agrup, WoE)) |>
    select(grupo, PctRec, WoE) |>
    mutate(PctRecLabel = scales::label_percent(scale = 1, accuracy = 1)(PctRec)) -> tab

  altura <- max(50, tab$PctRec)

  tab |>
    ggplot(aes(x=grupo, y=PctRec, group=1, label = PctRecLabel)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
    coord_cartesian(ylim = c(0, 50)) +
    scale_x_discrete(expand = expansion(add = c(0,1.5))) +
    scale_y_continuous(name = "% Pob",
                       sec.axis = sec_axis(~./12.5-2, name="WoE")) +
    geom_line(aes(y=(2+WoE)*12.5), colour="brown1") +
    geom_point(aes(y=(2+WoE)*12.5), colour="brown1", size=2) +
    geom_text(vjust = -0.5, size = 2.5)
}

iv_tab_cont_2_section <- function(var_name, iv_tab, direction) {
  cat("\n")
  cat("#####", "x", var_name, "\n") # Create sub level headings with the names.
  cat("\n\n")
  cat("<p>", "\n")
  iv_tab_cont_2_plot(var_name, iv_tab, direction) |> print()
  cat("</p>", "\n")
}

iv_tab_categ_2_section <- function(var_name, iv_tab) {
  cat("\n")
  cat("#####", "x", var_name, "\n") # Create sub level headings with the names.
  cat("\n\n")
  cat("<p>", "\n")
  iv_tab_categ_2_plot(var_name, iv_tab) |> print()
  cat("</p>", "\n")
}

iv_grouped_tabs_cont_2_section <- function(df, key) {
  cat("\n")
  cat("#####", key |> pluck(1,1), "\n") # Create sub level headings with the names.
  cat("\n\n")
  cat("<p>", "\n")
  iv_grouped_tab_cont_2_plot(df) |> print()
  cat("</p>", "\n")
}

iv_grouped_tab_cont_2_plot <- function(df) {

  df |>
    summarise(d_mn = min(direction), d_mx=max(direction)) |>
    mutate(dif = d_mx - d_mn ) -> df_dir

  assertthat::assert_that(df_dir |> pluck('dif') == 0,
                          df_dir |> pluck('d_mn') %in% c(-1,1))

  if (df_dir |> pluck('d_mn')==-1) descendente <- TRUE
  else descendente <- FALSE

  df |>
    tidyr::unnest(iv_tab) |>
    filter(col_agrup != "Total") |>
    mutate(col_agrup = case_when(
      col_agrup |> stringr::str_detect("NA") ~ "Nulos ",
      col_agrup |> stringr::str_detect("NULL") ~ "Nulos ",
      col_agrup |> stringr::str_detect("Nulo") ~ "Nulos ",
      col_agrup |> stringr::str_detect("Missing") ~ "Nulos ",
      col_agrup |> stringr::str_detect("Otros_") ~ "Resto ",
      TRUE ~ col_agrup
    )) |>
    group_by(orden) |>
    mutate(col_agrup = first(x=col_agrup, order_by=valor_grupo)) |>
    ungroup() |>
    mutate(grupo = forcats::fct_reorder(col_agrup, orden,
                                        .desc = descendente)) |>
    mutate(WoE_gr = if_else(woe_est_lo == woe_est_hi &
                              0 == woe_est_hi, 0, WoE)) |>
    select(valor_grupo, grupo, PctRec, WoE_gr) |>
    mutate(PctRecLabel = scales::label_percent(scale = 1,
                                               accuracy = 1)(PctRec)) -> tab

  custom_palette <- function(values, palette="Set2") {
    n <- length(unique(values))
    if (n < 3) {
      return(RColorBrewer::brewer.pal(3, palette)[1:n]) }
    return(RColorBrewer::brewer.pal(n, palette))
  }

  altura <- max(50, tab$PctRec)

  tab |>
    ggplot(aes(x=grupo, y=PctRec, label = PctRecLabel,
               group=valor_grupo,
               colour = valor_grupo,
               fill = valor_grupo,
               shape = valor_grupo)) +
    scale_color_manual(values = custom_palette(tab$valor_grupo, 'Set2')) +
    scale_fill_manual(values = custom_palette(tab$valor_grupo, 'Set2')) +
    geom_col(position ='dodge') +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
    coord_cartesian(ylim = c(0, altura)) +
    scale_x_discrete(expand = expansion(add = c(0,1.5))) +
    scale_y_continuous(name = "% Pob",
                       sec.axis = sec_axis(~./12.5-2, name="WoE")) +
    geom_text(vjust = -0.5, size = 2.5,
              position = position_dodge(width = 1)) +
    geom_line(aes(y=(2+WoE_gr)*12.5)) +
    geom_point(aes(y=(2+WoE_gr)*12.5), size=2.5,
               color = 'grey')
}

iv_grouped_tabs_categ_2_section <- function(df, key) {
  cat("\n")
  cat("#####", key |> pluck(1,1), "\n") # Create sub level headings with the names.
  cat("\n\n")
  cat("<p>", "\n")
  iv_grouped_tab_categ_2_plot(df) |> print()
  cat("</p>", "\n")
}

iv_grouped_tab_categ_2_plot <- function(df) {

  df |>
    tidyr::unnest(iv_tab) |>
    filter(Cutpoint != "Total") |>
    mutate(col_agrup = case_when(
      Cutpoint |> stringr::str_detect("NA") ~ "Nulos ",
      Cutpoint |> stringr::str_detect("NULL") ~ "Nulos ",
      Cutpoint |> stringr::str_detect("Missing") ~ "Nulos ",
      Cutpoint |> stringr::str_detect("Nulo") ~ "Nulos ",
      Cutpoint |> stringr::str_detect("Otros_") ~ "Resto ",
      TRUE ~ Cutpoint
    )) |>
    mutate(orden_valor_grupo = case_when(
      stringr::str_detect(valor_grupo, "Train") ~ 1,
      stringr::str_detect(valor_grupo, "Test") ~ 2,
      stringr::str_detect(valor_grupo, "Val") ~ 3,
      TRUE ~ 4 )
    ) |>
    group_by(col_agrup) |>
    mutate(WoE_train = first(x=WoE, order_by=orden_valor_grupo)) |>
    ungroup() |>
    mutate(grupo = forcats::fct_reorder(col_agrup, WoE_train)) |>
    arrange(WoE_train) |>
    mutate(PctRecLabel = scales::label_percent(scale = 1,
                                               accuracy = 1)(PctRec)) |>
    select(valor_grupo, grupo, PctRec, PctRecLabel, WoE) -> tab

  altura <- max(50, tab$PctRec)

  custom_palette <- function(values, palette="Set2") {
    n <- length(unique(values))
    if (n < 3) {
      return(RColorBrewer::brewer.pal(3, palette)[1:n]) }
    return(RColorBrewer::brewer.pal(n, palette))
  }

  tab |>
    ggplot(aes(x=grupo, y=PctRec, label = PctRecLabel,
               group=valor_grupo,
               colour = valor_grupo,
               fill = valor_grupo,
               shape = valor_grupo)) +
    scale_color_manual(values = custom_palette(tab$valor_grupo, 'Set2')) +
    scale_fill_manual(values = custom_palette(tab$valor_grupo, 'Set2')) +
    geom_col(position ='dodge') +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5)) +
    coord_cartesian(ylim = c(0, 50)) +
    scale_x_discrete(expand = expansion(add = c(0,1.5))) +
    scale_y_continuous(name = "% Pob",
                       sec.axis = sec_axis(~./12.5-2, name="WoE")) +
    geom_text(vjust = -0.5, size = 2.5,
              position = position_dodge(width = 1)) +
    geom_line(aes(y=(2+WoE)*12.5)) +
    geom_point(aes(y=(2+WoE)*12.5), size=2.5,
               color = 'grey')
}

iv_grouped_tab_cont_2_plotly <- function(df) {

  df |>
    summarise(d_mn = min(direction), d_mx=max(direction)) |>
    mutate(dif = d_mx - d_mn ) -> df_dir

  assertthat::assert_that(df_dir |> pluck('dif') == 0,
                          df_dir |> pluck('d_mn') %in% c(-1,1))

  if (df_dir |> pluck('d_mn')==-1) descendente <- TRUE
  else descendente <- FALSE

  df |>
    tidyr::unnest(iv_tab) |>
    filter(col_agrup != "Total") |>
    mutate(col_agrup = case_when(
      col_agrup |> stringr::str_detect("NA") ~ "Nulos ",
      col_agrup |> stringr::str_detect("NULL") ~ "Nulos ",
      col_agrup |> stringr::str_detect("Nulo") ~ "Nulos ",
      col_agrup |> stringr::str_detect("Missing") ~ "Nulos ",
      col_agrup |> stringr::str_detect("Otros_") ~ "Resto ",
      TRUE ~ col_agrup
    )) |>
    # Para gráficos, si hay más de una tabla, elijo col_agrup de la primera
    group_by(orden) |>
    mutate(col_agrup = first(x=col_agrup, order_by=valor_grupo)) |>
    ungroup() |>
    mutate(grupo = forcats::fct_reorder(col_agrup, orden, .desc = descendente)) |>
    mutate(WoE_gr = if_else(woe_est_lo == woe_est_hi & 0 == woe_est_hi, 0, WoE)) |>
    select(valor_grupo, grupo, PctRec, WoE_gr) |>
    mutate(PctRecLabel = scales::label_percent(scale = 1, accuracy = 1)(PctRec)) -> tab

  altura <- max(50, tab$PctRec)

  custom_palette <- function(values, palette="Set2") {
    n <- length(unique(values))
    if (n < 3) {
      return(RColorBrewer::brewer.pal(3, palette)[1:n]) }
    return(RColorBrewer::brewer.pal(n, palette))
  }

  colors <- custom_palette(tab$valor_grupo)

  plotly_obj <- plot_ly() |>
    add_trace(x = ~tab$grupo, y = ~tab$PctRec,
              type = 'bar',
              color = ~tab$valor_grupo, text = ~tab$PctRecLabel,
              textposition = 'outside',
              colors = custom_palette(tab$valor_grupo, 'Set2'))

  plotly_obj <- plotly_obj |>
    layout(legend = list(title = list(text = 'Valor Grupo'),
                         x = 1.2, y = 0.5,
                         xanchor = 'center', yanchor = 'middle'),
           xaxis = list(title = "grupo", linewidth = 2),
           yaxis = list(title = "% Pob", range = c(0, altura), linewidth = 2)
    )

  plotly_obj <- plotly_obj |>
    add_trace(x = ~tab$grupo, y = ~((2 + tab$WoE_gr) * 12.5),
              color = ~tab$valor_grupo, type = 'scatter', mode = 'lines',
              showlegend = FALSE, yaxis = "y2"
    )

  plotly_obj <- plotly_obj |>
    add_trace(x = ~tab$grupo, y = ~((2 + tab$WoE_gr) * 12.5),
              marker = list(color = 'Grey'),
              size = 3,
              type = 'scatter', mode = 'markers',
              showlegend = FALSE, yaxis = "y2",
              symbol = ~tab$valor_grupo,
              text = ~paste("Grupo:", tab$grupo, "<br>",
                            "WoE:", round(tab$WoE_gr, 3)),
              hoverinfo = "text"
    )

  plotly_obj <- plotly_obj |>
    layout(yaxis2 = list(title = "WoE",
                         linewidth = 2, side = "right", overlaying = "y", showline = TRUE))


  tickvals <- seq(min(tab$WoE_gr), max(tab$WoE_gr), length.out = 5)
  ticktext <- tickvals |> format(digits = 2, nsmall = 2)
  tickvals <- (2 + tickvals) * 12.5

  plotly_obj <- plotly_obj |>
    layout( yaxis2 = list(
      overlaying = "y", side = "right",
      tickvals = tickvals,
      ticktext = ticktext,
      title = "WoE" )
    )

  plotly_obj <- plotly_obj |>
    config(displaylogo = FALSE)

  return(plotly_obj)
}

iv_grouped_tab_categ_2_plotly <- function(df) {

  df |>
    tidyr::unnest(iv_tab) |>
    filter(Cutpoint != "Total") |>
    mutate(col_agrup = case_when(
      Cutpoint |> stringr::str_detect("NA") ~ "Nulos ",
      Cutpoint |> stringr::str_detect("NULL") ~ "Nulos ",
      Cutpoint |> stringr::str_detect("Missing") ~ "Nulos ",
      Cutpoint |> stringr::str_detect("Nulo") ~ "Nulos ",
      Cutpoint |> stringr::str_detect("Otros_") ~ "Resto ",
      TRUE ~ Cutpoint
    )) |>
    mutate(orden_valor_grupo = case_when(
      stringr::str_detect(valor_grupo, "Train") ~ 1,
      stringr::str_detect(valor_grupo, "Test") ~ 2,
      stringr::str_detect(valor_grupo, "Val") ~ 3,
      TRUE ~ 4 )
    ) |>
    group_by(col_agrup) |>
    mutate(WoE_train = first(x=WoE, order_by=orden_valor_grupo)) |>
    ungroup() |>
    mutate(grupo = forcats::fct_reorder(col_agrup, WoE_train)) |>
    arrange(WoE_train) |>
    mutate(PctRecLabel = scales::label_percent(scale = 1,
                                               accuracy = 1)(PctRec)) |>
    #    select(valor_grupo, col_agrup, grupo, WoE_train, PctRec, WoE) -> tab
    select(valor_grupo, grupo, PctRec, PctRecLabel, WoE) -> tab

  altura <- max(50, tab$PctRec)

  custom_palette <- function(values, palette="Set2") {
    n <- length(unique(values))
    if (n < 3) {
      return(RColorBrewer::brewer.pal(3, palette)[1:n]) }
    return(RColorBrewer::brewer.pal(n, palette))
  }

  plotly_obj <- plot_ly() |>
    add_trace(x = ~tab$grupo, y = ~tab$PctRec,
              type = 'bar',
              color = ~tab$valor_grupo, text = ~tab$PctRecLabel,
              textposition = 'outside',
              colors = custom_palette(tab$valor_grupo, 'Set2'))

  plotly_obj <- plotly_obj |>
    layout(legend = list(title = list(text = 'Valor Grupo'),
                         x = 1.2, y = 0.5,
                         xanchor = 'center', yanchor = 'middle'),
           xaxis = list(title = "grupo", linewidth = 2),
           yaxis = list(title = "% Pob", range = c(0, altura), linewidth = 2)
    )

  plotly_obj <- plotly_obj |>
    layout(yaxis2 = list(title = "WoE",
                         linewidth = 2, side = "right", overlaying = "y", showline = TRUE))

  plotly_obj <- plotly_obj |>
    add_trace(x = ~tab$grupo, y = ~((2 + tab$WoE) * 12.5),
              color = ~tab$valor_grupo, type = 'scatter', mode = 'lines',
              showlegend = FALSE, yaxis = "y2"
    )

  plotly_obj <- plotly_obj |>
    add_trace(x = ~tab$grupo, y = ~((2 + tab$WoE) * 12.5),
              marker = list(color = 'Grey'),
              size = 3,
              type = 'scatter', mode = 'markers',
              showlegend = FALSE, yaxis = "y2",
              symbol = ~tab$valor_grupo,
              text = ~paste("Grupo:", tab$grupo, "<br>",
                            "WoE:", round(tab$WoE, 3)),
              hoverinfo = "text"
    )

  tickvals <- seq(min(tab$WoE), max(tab$WoE), length.out = 5)
  ticktext <- tickvals |> format(digits = 2, nsmall = 2)
  tickvals <- (2 + tickvals) * 12.5

  plotly_obj <- plotly_obj |>
    layout( yaxis2 = list(
      overlaying = "y", side = "right",
      tickvals = tickvals,
      ticktext = ticktext,
      title = "WoE" )
    )

  plotly_obj <- plotly_obj |>
    config(displaylogo = FALSE)

  return(plotly_obj)
}

# Stability -------------------------------------------------------------------

# tab_woes_cuts <- tab.bins |> tab_woes_cuts()
# tab_bins_df <- tab.bins |> tab_woes_cuts()
tab_woes_cuts <- function(tab.bins) {
  if (par_discret==0) {
    woes_type_cut <- "woes_stp_cuts"
    tab_type_cut <- "tab_iv_stp"
  }
  else {
    woes_type_cut <- "woes_pwl_cuts"
    tab_type_cut <- "tab_iv_pwl"
  }
  tab.bins |> keep(~ .x$generada==1) |>
    map_if(~ (.x$type=="Continua"),
           ~ list(var=.x$var, var_gen=.x$var_gen, type=.x$type,
                  w=.x[[woes_type_cut]], t=.x[[tab_type_cut]],
                  n=.x$woe_nulos, a=.x$WoE_anulado, s=.x$sentido)) |>
    map_if(~ (.x$type=="Factor"),
           ~ list(var=.x$var, var_gen=.x$var_gen, type=.x$type,
                  w=.x$woes_tab, t=.x$ivtable, n=.x$woe_nulos,
                  a=.x$WoE_anulado)) |>
    tibble::enframe('idx', 'lst') |>
    transmute(tipo=map_chr(lst, ~.x$type), var=map_chr(lst, ~.x$var),
              var_gen=map_chr(lst, ~.x$var_gen), woes_tab=map(lst, ~.x$w),
              woe_nulos=map(lst, ~.x$n), woe_anulado=map(lst, ~.x$a),
              sentido=map(lst, ~.x$s), tab_iv=map(lst, ~.x$t)) -> tab_bins_df
  return(tab_bins_df)
}

# Dado un vector, devuelve el porcentaje de posiciones que coinciden con el mismo vector ordenado.
porc_sorted <- function(vec, asc=T) {
  vec_sorted <- sort(vec, decreasing = !asc)
  q_sorted <- sum(vec==vec_sorted)
  return(q_sorted/length(vec))
}

# Calcula los porcentajes de cuantiles ordenados solo considerando aquellos con por lo menos q_gb_min buenos y malos.
iv_tab_porc_sorted <- function(iv_tab, q_gb_min=50, tipo_var = 'Continua', asc=F) {
  assertthat::assert_that(tipo_var %in% c('Continua', 'Factor'), msg = 'iv_tab_porc_sorted(): Los tipos de variables admitidos son Continua o Factor!')
  if (tipo_var=='Continua') {
    iv_tab |> # Asume que ya asc es el orden de los los WoEs de Train, por BadRate según Train es lo opuesto.
      filter(col_agrup != 'Total', !is.na(cut_hi), CntGood >= q_gb_min, CntBad >= q_gb_min) |>
      summarise(monot_fl=is.monotone(BadRate)$res, p_sorted=porc_sorted(BadRate, asc = !asc), q_measured=length(BadRate))
  } else {
    iv_tab |>
      filter(Cutpoint != 'Total', !is.na(Cutpoint), CntGood >= q_gb_min, CntBad >= q_gb_min) |>
      summarise(monot_fl=is.monotone(BadRate)$res, p_sorted=porc_sorted(BadRate, asc = asc), q_measured=length(BadRate))
  }
}

iv_x_grupos <- function(df, tipo, col, col_agrup, woes_tab, woe_nulos, sentido,
                        var_grupo, grupos, detailed=FALSE) {
  assertthat::assert_that(exists("par_iv_cuantiles_gb_min"), exists("par_iv_tot_min"),
                          exists("par_iv_tot_gb_min"))

  assertthat::assert_that(0 < par_iv_cuantiles_gb_min,
                          par_iv_cuantiles_gb_min < par_iv_tot_gb_min,
                          par_iv_tot_gb_min < par_iv_tot_min)


  ivs <- tibble()

  iv_tot_init <- tibble(
    variable=col, tipo=tipo, var_grupo=var_grupo,
    detailed = detailed, sentido = sentido
  )

  for (v in grupos) {
    iv_tot <- iv_tot_init |>
      mutate(error = FALSE, !!var_grupo:=v, valor_grupo=v)
    if (tipo=='Continua') {
      assertthat::assert_that(is.monotone(woes_tab)$res)
      asc <- is.monotone(woes_tab)$incr
      #print(paste("Tipo Continua Grupo", v, "col", col, "col_agrup", col_agrup, "woes_tab", woes_tab))
      res <- df |> filter(.data[[var_grupo]] == v) |>
        mutate(Good = as.integer(as.character(Good))) |>
        group_2_ivtab_cont(col, col_agrup, woes_breaks = woes_tab, woe_nulos, sentido)

      if (res$error) {
        res$error_detalle <- cli::format_error(c("Hubo un error en el calculo del IV x grupos",
                                                 "i" = "para la variable {col} en el grupo {v}",
                                                 " " = res$error_detalle))
        message(res$error_detalle)
        iv_tot <- iv_tot |>
          mutate(error = TRUE, error_detail = res$error_detalle)
      } else {
        iv <- res |> pluck('tab')

        iv_tot <- iv_tot |>
          bind_cols(
            iv |> slice(n()) |>                  # La ultima fila es la que tiene los totales
              rename(TotRec=CntRec, TotGood=CntGood, TotBad=CntBad)) |>
          bind_cols(iv |> summarise(bines=n()-1)) |>
          bind_cols(iv |>
                      iv_tab_porc_sorted(tipo_var = tipo, q_gb_min = par_iv_cuantiles_gb_min, asc=asc)) |>
          select(-col_agrup, -PctRec, -PctGood, -PctBad, -WoE)
        if (detailed) iv_tot <- iv_tot |> mutate(iv_tab=list(iv))
      }
    }
    else if (tipo=='Factor') {
      # A diferencias de la versión Continua siempre agrupamos por col_agrup. Ese es el mapeo obtenido en train.
      res <- df |> filter(.data[[var_grupo]] == v) |>
        mutate(Good = as.integer(as.character(Good))) |>
        # Desactivo minpts porque ya se discretizó la variable.
        # El control debe ser externo
        group_2_ivtab_factor(col, col_agrup, minpts = 0)
      if (!res$error) {
        iv <- res |>
          pluck('tab') |>
          dplyr::mutate(newCutpoint = suppressWarnings(as.numeric(Cutpoint)),
                 ok_new = if_else(!is.na(newCutpoint), TRUE, FALSE)) |>
          dplyr::mutate(Cutpoint = case_when(
            ok_new & stringr::str_detect(groups, ',') ~ '_Otros',
            ok_new & (groups == "'NA'") ~ '_Missing',
            ok_new ~ groups,
            TRUE ~ Cutpoint
          )) |>
          tab_fmt_fct() # Ordena por desc(col_agrup) que son los WoEs de Train.
        iv_tot <- iv_tot |>
          bind_cols(iv |> slice(-n()) |>
                      dplyr::summarise(IV = sum(IV, na.rm = TRUE),
                                TotRec = sum(CntRec), TotGood = sum(CntGood), TotBad = sum(CntBad),
                                BadRate = 100 * TotBad / TotRec)) |>
          bind_cols(iv |> dplyr::summarise(bines = n() - 1)) |>
          bind_cols(iv |> iv_tab_porc_sorted(tipo_var = tipo,
                                             q_gb_min = par_iv_cuantiles_gb_min, asc = TRUE)) # al ordenar por desc(WoE) de Train, ordena por BadRate asc de Train
        if (detailed) iv_tot <- iv_tot |> mutate(iv_tab=list(iv))
      } else {
        res$error_detalle <- cli::format_error(c("Hubo un error en el calculo del IV x grupos",
                                                 "i" = "para la variable {col} en el grupo {v}",
                                                 " " = res$error_detalle))
        message(res$error_detalle)
        iv_tot <- iv_tot |>
          mutate(error = TRUE, error_detail = res$error_detalle)
      }
    }
    else cli::cli_alert(c("Tipo inesperado en el calculo del IV x grupos",
                          "i" = "Se esperaba Continua o Factor y fue {tipo}"))
    ivs <- ivs |> bind_rows(iv_tot)
  }
  return(ivs)
}

estab_x_grupo <- function(df, var_grupo, tab_woes, detailed=FALSE) {
  df <- df |> mutate("{var_grupo}":=coalesce(.data[[var_grupo]], "Missing"),
                     Good=as.integer(as.character(Good)))

  grupos <- df |> pull(var_grupo) |> unique()

  xvars <- tab_woes |> purrr::pmap_dfr(
    ~ iv_x_grupos(df=df, tipo=..1, col=..2, col_agrup=..3, woes_tab=..4,
                  woe_nulos = ..5, sentido = ..7, var_grupo=var_grupo,
                  grupos=grupos, detailed=detailed))

  xgrupos <- xvars |>
    filter(!error) |>
    group_by(valor_grupo) |>
    summarise(across(c(TotRec, TotGood, TotBad, BadRate), first)) |>
    arrange(desc(TotRec))

  return(list(var_grupo=var_grupo, xvars=xvars, xgrupos=xgrupos, tab_woes=tab_woes))
}

estab_tab_2_gt <- function(res_estab, var_list=NULL) {

  res_estab |>
    purrr::pluck("var_grupo") |>
    internal_vars_labels() -> subtitle

  stringr::str_c("x ", subtitle) -> subtitle

  xgrupo_tab <- res_estab |>
    purrr::pluck("xgrupos") |>
    gt::gt(locale = 'es-AR') |> #view("x Prov")
    gt::tab_header(title = gt::html("Estad&iacute;sticas por Grupo"),
                   subtitle = subtitle) |>
    gt::fmt_number(c('TotRec', 'TotGood', 'TotBad'), decimals = 0,
                   suffixing = T, drop_trailing_zeros = T) |>
    gt::fmt_percent(c('BadRate'), decimals = 1, scale = F) |>
    gt::cols_label(valor_grupo = "Grupo", TotRec = gt::html('#<br>Obs'),
                   TotGood = gt::html('#<br>Buenos'), TotBad =gt::html('#<br>Malos'),
                   BadRate = gt::html('Tasa<br>de Malos'))|>
    gt::sub_missing(columns = everything()) |>
    gt::opt_row_striping()

  tab_top_10 <- res_estab |> purrr::pluck("xgrupos") |> slice_max(order_by = TotRec, n = 10) |>
    mutate(sufic_datos = TotRec >= par_iv_tot_min & TotGood >= par_iv_tot_gb_min & TotBad >= par_iv_tot_gb_min)

  errors_tab <- res_estab |> purrr::pluck("xvars") |>
    filter(error) |>
    select(valor_grupo, variable) |>
    gt::gt(locale = 'es-AR') |>
    gt::tab_header(title = gt::html("Errores encontrados"))

  xvar_tab <- res_estab |> purrr::pluck("xvars") |>
    filter(!error) |>
    # Se usa este argumento para restringir la tabla a variables del modelo por ejemplo
    filter(is.null(var_list) | variable %in% var_list) |>
    filter(valor_grupo %in% (tab_top_10 |> pull(valor_grupo))) |>
    mutate(p_measured=q_measured/bines) |>
    arrange(variable, desc(TotRec)) |>
    select(variable, valor_grupo, IV, p_measured, p_sorted) |>
    rename(`% Bines c/ datos`=p_measured, `% Ordenados`=p_sorted) |>
    tidyr::pivot_wider(names_from = valor_grupo, values_from = c(IV, `% Bines c/ datos`, `% Ordenados`), names_sep = ".") |>
    gt::gt(locale = 'es-AR') |>
    gt::tab_header(title = gt::html("Estad&iacute;sticas por Variable")) |>
    gt::tab_spanner_delim(delim = ".") |>
    gt::cols_label(variable="Variable") |>
    gt::fmt_number(columns = starts_with("IV"), n_sigfig = 2, suffixing = T, drop_trailing_zeros = T) |>
    gt::fmt_percent(columns = starts_with("% Bines c/ datos") | starts_with("% Ordenados"), decimals = 0, scale = T) |>
    gt::sub_missing(columns = gt::everything()) |>
    gt::tab_style(locations = gt::cells_column_labels(columns = gt::ends_with(tab_top_10 |> filter(!sufic_datos) |> pull(valor_grupo))),
                  style = list(gt::cell_text(color = "orange"))) |>
    gt::opt_row_striping() |>
    gt::tab_footnote(locations = gt::cells_column_spanners(spanners = starts_with("% Bines c/ datos")),
                     footnote = glue::glue("% de bines con por lo menos ", par_iv_cuantiles_gb_min, " buenos y malos")) |>
    gt::tab_footnote(locations = gt::cells_column_spanners(spanners = starts_with("% Ordenados")),
                     footnote = "% de bines que conservan el puesto original al ordenar por tasa de malos") |>
    gt::tab_source_note(source_note = gt::html(glue::glue("Los grupos en ", "<span style='color:orange'>naranja</span>" ,
                                                          " no cumplen que haya por lo menos ", par_iv_tot_min,
                                                          " casos o que por lo menos hayan ", par_iv_tot_gb_min, " buenos y malos en total.")))
  return(list(xgrupo_tab=xgrupo_tab, xvar_tab=xvar_tab, errors_tab=errors_tab))
}

# Auxiliares para publicar las tablas de estabilidad en gt
details_tag <- function(html_content, html_head) {
  glue::glue("<details><summary>{html_head}</summary><p>{html_content}<p></details>") |>
    gt::html()
}

# 11 representa el lugar de la variable de segmento
ivs_det_2_gt <- function(df) {
  df |>
    select(11, TotRec, TotGood, TotBad, IV, BadRate, q_measured, p_sorted) |>
    arrange(desc(q_measured)) |>
    slice_head(n = 10) |> gt::gt(locale = 'es-AR') |>
    gt::tab_options(table.font.size = pct(80), container.width = px(1000),
                    table.width = pct(100)) |>
    gt::cols_label(TotRec = '#', TotGood = gt::html('Buenos'),
                   TotBad = gt::html('Malos'), BadRate = gt::html('Tasa<br>Malos'),
                   q_measured = gt::html('Cuantiles<br>medidos'),
                   p_sorted = gt::html('Cuantiles<br>ordenados')) |>
    gt::fmt_number(columns = c('IV'), decimals = 1) |>
    gt::fmt_percent(columns = c('BadRate'), decimals = 1, scale = F) |>
    gt::fmt_percent(p_sorted, decimals = 1) |>
    gt::sub_missing(p_sorted)

}

drift_x_bines <- function(df, tipo, col, col_agrup, woes_breaks,
                          woe_nulos, sentido, tab_iv_ref,
                          var_grupo=var_grupo, grupos=grupos, detailed=detailed) {

  # Uso una tolerancia por errores de redondeo al leer csv y usar SQL.
  # WoE en tab_nue puede tener algunas diferencias.
  TOL <- .Machine$double.eps^0.5 # 1.49e-08

  df |>
    select(.weight, all_of(c(var_grupo, col, col_agrup))) -> df

  df |>
    filter(!is.na(.data[[col]])) |>
    summarise(mn=min(.data[[col_agrup]])) |> pull(mn) -> mn_nonulos

  if (tipo=='Continua') {
    # Ajuste x redondeos
    sort(woes_breaks) -> woes_breaks  # redundante pero defensivo
    assertthat::assert_that(woes_breaks[1] - 1E-7 < mn_nonulos ,
                            msg = paste("Valores de ", col_agrup, "fuera del rango esperado!",
                                        "El mínimo fue:", mn_nonulos, "y se esperaba", woes_breaks[1]))
    min(woes_breaks[1], mn_nonulos) ->  woes_breaks[1]

    # + TOL, garantiza superar los errores de redondeo que provienen del SQL
    df |>
      filter(!is.na(.data[[col]])) |>
      mutate(bin_woe=cut(.data[[col_agrup]] + TOL, woes_breaks, include.lowest = T,
                         ordered_result = T, right=F,
                         labels = NULL)) -> df_nonulos

    if (nrow(df |> filter(is.na(.data[[col]])) ) > 0) {

      df |>
        filter(is.na(.data[[col]])) -> df_nulos

      df_nulos |>
        summarise(mn=min(.data[[col_agrup]])) |> pull(mn) -> mn_nulos
      assertthat::assert_that(woe_nulos - 1E-7 < mn_nulos ,
                              msg = paste("Valores de ", col_agrup, "fuera del rango esperado!",
                                          "El mínimo para nulos fue:", mn_nulos, "y se esperaba", woe_nulos))
      min(woe_nulos, mn_nulos) ->  woe_nulos

      df_nulos |>
        mutate(bin_woe = paste('[', round(woe_nulos,3), ']')) -> df_nulos

      df_nonulos |>
        bind_rows(df_nulos) -> df_tot

    } else { df_nonulos -> df_tot }

    df_tot |>
      group_by(.data[[var_grupo]], bin_woe) |>
      # CntRec debería llamarse Weight o Peso directamente. Se mantiene x compat
      summarise(CntRec = sum(.weight)) |>
      # por cada valor de var_grupo se calculan los porcentajes
      mutate(PctRec = CntRec/sum(CntRec),
             PctRecAdj = if_else(PctRec>0,PctRec, 0.5/sum(CntRec))) -> tab_nue

    # El PSI no es más que el IV entre la distribución de referencia y la nueva
    tab_iv_ref |>
      filter(col_agrup != 'Total') |>
      select(col_agrup, orden, bin_woe, CntRec, PctRec) |>
      rename(CntRecRef = CntRec, PctRecRef = PctRec) |>
      mutate(PctRecRef = PctRecRef/100) -> tab_ref

    # Está bien drop porque las pérdidas se reflejan en los porcentajes
    tab_ref |>
      right_join(tab_nue, by = join_by(bin_woe),
                 unmatched = 'drop',
                 relationship = 'one-to-many') |>
      arrange(.data[[var_grupo]], orden) |>
      select(-orden, -bin_woe) -> tab_res

  } else if (tipo=='Factor') {
    # Como no hay interpolación simplemente agrupo por la columna agrupada
    df |>
      mutate(WoE = .data[[col_agrup]]) |>
      group_by(.data[[var_grupo]], WoE) |>
      # CntRec debería llamarse Weight o Peso directamente. Se mantiene x compat
      summarise(CntRec = sum(.weight)) |>
      # por cada valor de var_grupo se calculan los porcentajes
      mutate(PctRec = CntRec/sum(CntRec),
             PctRecAdj = if_else(PctRec>0,PctRec, 0.5/sum(CntRec))) -> tab_nue

    # El PSI no es más que el IV entre la distribución de referencia y la nueva
    tab_iv_ref |>
      filter(Cutpoint != 'Total') |>
      # El WoE se usa para ordenar y joinear
      select(CntRec, PctRec, WoE) |>
      # Ojo que asume PctRec en escala 0-100.
      rename(CntRecRef = CntRec, PctRecRef = PctRec) |>
      mutate(PctRecRef = PctRecRef/100) -> tab_ref

    tab_nue |>
      inner_join(
        tab_ref |>
          mutate(WoEl = WoE - TOL, WoEu = WoE + TOL),
        by = join_by(between(x$WoE, y$WoEl, y$WoEu)),
        unmatched = 'drop',
        relationship = "many-to-one" ) |>
      arrange(.data[[var_grupo]], WoE.y) |>
      select(-WoE.x, -WoE.y, -WoEl, -WoEu) -> tab_res

  } else abort("Tipo inesperado. Se esperaba Continua o Factor y fue:", "error_func_aux", tipo)

  tab_res |>
    # Solo el PSI usa el pct ajustado para ceros
    mutate(col = col, col_agrup = {{col_agrup}},
           abs_dif_pct=abs(PctRecRef - PctRec),
           psi=(PctRecRef - PctRecAdj)*log(PctRecRef / PctRecAdj)) -> tab_res

  tab_res |>
    # Sutileza para unificar nombres.  Asume que no colisiona con col.
    rename(var_grupo=all_of(var_grupo)) |>
    relocate(col, col_agrup, var_grupo) -> tab_res

  tab_res |>
    group_by(var_grupo) |>
    summarise(TotPctRec = sum(PctRec)) |>
    summarise(min_pct = min(TotPctRec),
              max_pct = max(TotPctRec)) -> tab_ctrl

  # Sorprendentemente no anduvo == 1 por errores de redondeo!
  assertthat::assert_that(assertthat::are_equal(tab_ctrl$min_pct, 1),
                          assertthat::are_equal(tab_ctrl$max_pct, 1),
                          msg = paste("Falló el calculo de estabilidad para la variable", col))

  return(tab_res)

}

drift_x_vars <- function(df, var_grupo, tab_bins_df, detailed=FALSE) {

  # Defensivo, no debería pasar
  df <- df |>
    mutate("{var_grupo}":=coalesce(.data[[var_grupo]], "Missing"))

  grupos <- df |> pull(var_grupo) |> unique()

  x_bines <- tab_bins_df |> purrr::pmap_dfr(
    ~ drift_x_bines(df=df, tipo=..1, col=..2, col_agrup=..3, woes_breaks=..4,
                    woe_nulos = ..5, sentido = ..6, tab_iv_ref = ..8,
                    var_grupo=var_grupo, grupos=grupos, detailed=detailed))

  x_vars_y_grupos <- x_bines |>
    group_by(col, var_grupo) |>
    summarise(CntRec=sum(CntRec), Linf=max(abs_dif_pct), PSI=sum(psi))

  x_vars <- x_vars_y_grupos |>
    group_by(col) |>
    summarise(CntRecMin=min(CntRec), CntRecMax=max(CntRec),
              Linf_max=max(Linf), var_grupo_Linf_max=var_grupo[which.max(Linf)],
              PSI_max=max(PSI), var_grupo_PSI_max=var_grupo[which.max(PSI)])

  ctrl <- x_vars_y_grupos |>
    group_by(var_grupo) |>
    summarise(CntRecMin=min(CntRec), CntRecMax=max(CntRec)) |>
    mutate(dif=CntRecMax - CntRecMin) |>
    summarise(max_dif = max(dif)) |>
    pull(max_dif)


  # Defensivo, no debería pasar
  assertthat::assert_that(ctrl==0,
                          msg = "Casos perdidos en el cálculo de estabilidad")

  x_grupos <- x_vars_y_grupos |>
    group_by(var_grupo) |>
    summarise(CntRec=min(CntRec),
              Linf_max=max(Linf), var_grupo_Linf_max=col[which.max(Linf)],
              PSI_max=max(PSI), var_grupo_PSI_max=col[which.max(PSI)])

  return(list(var_grupo=var_grupo, x_bines=x_bines,
              x_vars_y_grupos=x_vars_y_grupos,
              x_vars=x_vars, x_grupos=x_grupos))
}

estab_tab_dist_2_gt <- function(res_estab, var_list=NULL) {

  # Anda en la corrida por chunks pero no anda en el tejido la expresión latex:
  #  L_inf_latex <- "$\\mathcal{L}_\\infty$"
  L_inf_latex <- "L_inf"

  res_estab |>
    purrr::pluck("var_grupo") |>
    internal_vars_labels() -> subtitle

  stringr::str_c("x ", subtitle) -> subtitle

  x_vars_y_grupos_tab <- res_estab |>
    purrr::pluck("x_vars_y_grupos") |>
    # Necesario para que no se agregue x defecto col_agrup
    # Para identificar la variable basta con col
    gt::gt(locale = 'es-AR') |> #view("x Prov")
    gt::tab_header(title = gt::html("Estad&iacute;sticas por Variable y Grupos"),
                   subtitle = subtitle) |>
    gt::fmt_number(c('CntRec'), decimals = 0,
                   suffixing = T, drop_trailing_zeros = T) |>
    gt::fmt_percent(c('Linf', 'PSI'), decimals = 3, scale = F) |>
    gt::sub_missing(columns = everything()) |>
    gt::opt_row_striping() |>
    # Ojo que fmt_markdown afecta a las filas siguientes!
    gt::cols_label(var_grupo = "Grupo", CntRec = gt::html('# Obs'),
                   Linf=gt::md(L_inf_latex), PSI='PSI')

  x_vars_tab <- res_estab |>
    purrr::pluck("x_vars") |>
    ungroup() |>
    # Para identificar la variable basta con col
    gt::gt(locale = 'es-AR') |> #view("x Prov")
    cols_hide(c("CntRecMin", "CntRecMax")) |>
    gt::tab_header(title = gt::html("Estad&iacute;sticas por Variable"),
                   subtitle = subtitle) |>
    gt::fmt_number(c('CntRecMin', 'CntRecMax'), decimals = 0,
                   suffixing = T, drop_trailing_zeros = T) |>
    gt::fmt_percent(c('Linf_max', 'PSI_max'), decimals = 3, scale = F) |>
    gt::sub_missing(columns = everything()) |>
    gt::opt_row_striping() |>
    # Ojo que fmt_markdown afecta a las filas siguientes!
    gt::cols_label(col = "Variable",
                   var_grupo_Linf_max =
                     gt::md(glue::glue("Grupo con<br> {L_inf_latex} max")),
                   var_grupo_PSI_max = gt::md("Grupo con<br> PSI max"),
                   CntRecMin = gt::html('# Obs Min'),
                   CntRecMax = gt::html('# Obs Max'),
                   Linf_max=gt::md(L_inf_latex),
                   PSI_max=gt::md('PSI<br> '))

  x_grupos_tab <- res_estab |>
    purrr::pluck("x_grupos") |>
    ungroup() |>
    # Para identificar la variable basta con col
    gt::gt(locale = 'es-AR') |> #view("x Prov")
    gt::tab_header(title = gt::html("Estad&iacute;sticas por Grupo"),
                   subtitle = subtitle) |>
    gt::fmt_number(c('CntRec'), decimals = 0,
                   suffixing = T, drop_trailing_zeros = T) |>
    gt::fmt_percent(c('Linf_max', 'PSI_max'), decimals = 3, scale = F) |>
    gt::sub_missing(columns = everything()) |>
    gt::opt_row_striping() |>
    # Ojo que fmt_markdown afecta a las filas siguientes!
    gt::cols_label(var_grupo_Linf_max =
                     gt::md(glue::glue("Grupo con<br> {L_inf_latex} max")),
                   var_grupo_PSI_max = gt::md("Grupo con<br> PSI max"),
                   CntRec = gt::html('# Obs'),
                   Linf_max=gt::md(L_inf_latex),
                   PSI_max=gt::md('PSI<br> '))

  return(list(x_vars_y_grupos_tab=x_vars_y_grupos_tab,
              x_vars_tab=x_vars_tab, x_grupos_tab=x_grupos_tab))
}

# Dadas dos tablas de performance calcula la tabla PSI
tabPSI <- function(tab_orig, tab_nue) {
  tab_orig |>
    select(Cuantil, Score_Min, Score_Max, Total, P.Total) |>
    rename(CntRecRef = Total, PctRecRef = P.Total) |>
    mutate(PctRecRef = PctRecRef/100) -> tab_ref

  tab_nue |>
    select(Cuantil, Total, P.Total) |>
    rename(CntRec = Total, PctRec = P.Total) |>
    mutate(PctRec = PctRec/100,
           PctRecAdj = if_else(PctRec>0,PctRec, 0.5/sum(CntRec))) -> tab_nue

  tab_ref |>
    left_join(tab_nue, by = 'Cuantil',
              unmatched = 'error',
              relationship = 'one-to-one') |>
    arrange(Cuantil) -> tab_res

  tab_res |>
    # Solo el PSI usa el pct ajustado para ceros
    mutate(abs_dif_pct=abs(PctRecRef - PctRec),
           psi=(PctRecRef - PctRecAdj)*log(PctRecRef / PctRecAdj)) -> tab_res

  tab_res |>
    summarise(TotPctRec = sum(PctRec)) |>
    summarise(min_pct = min(TotPctRec),
              max_pct = max(TotPctRec)) -> tab_ctrl

  assertthat::assert_that(tab_ctrl$min_pct == 1, tab_ctrl$max_pct == 1,
                          msg = paste("Falló el calculo de estabilidad para la variable", col))

  return(tab_res)
}

# Uso:
# tabPSI(tab.orig, tab.nue) |> repPSI()
#
repPSI <- function(tab) {

  L_inf_latex <- "L_inf"  # No anduvo gt::md("$\\mathcal{L}_\\infty$")

  tab <- tab |> mutate(Cuantil=as.character(Cuantil)) |>
    bind_rows(tab |> summarise(Cuantil="Total", Score_Min=min(Score_Min), Score_Max=max(Score_Max),
                               CntRecRef=sum(CntRecRef), PctRecRef=sum(PctRecRef),
                               CntRec=sum(CntRec), PctRec=sum(PctRec), PctRecAdj=sum(PctRecAdj),
                               abs_dif_pct=max(abs_dif_pct), psi=sum(psi)))
  tab |> gt(locale = "es-AR") |>
    tab_style(locations = cells_body(rows = Cuantil == "Total"),
              style = cell_borders(sides = "top", weight = px(2)) ) |>
    opt_row_striping() |>
    tab_style(locations = cells_body(columns = abs_dif_pct, rows = abs_dif_pct == max(abs_dif_pct)),
              style = cell_text(weight = "bold")) |>
    # Remarco el máximo PSI (además del Total, que es el verdadero máximo).
    tab_style(locations = cells_body(columns = psi,
                                     rows = (psi == psi[order(psi, decreasing = TRUE)][2])),
              style = cell_text(weight = "bold")) |>
    tab_style(locations = cells_body(columns = psi,
                                     rows = psi == max(psi)),
              style = cell_text(weight = "bold")) |>
    gt::fmt_number(c('Score_Min', 'Score_Max', 'CntRec', 'CntRecRef'), decimals = 0,
                   drop_trailing_zeros = T) |>
    gt::fmt_percent(c('PctRecRef', 'PctRec', 'PctRecAdj'), decimals = 1, scale = F) |>
    gt::fmt_percent(c('abs_dif_pct', 'psi'), decimals = 3, scale = F) |>
    gt::sub_missing(columns = everything()) |>
    cols_label(Cuantil='Cuantil', Score_Min='Score Min', Score_Max='Score Max',
               CntRecRef='Total Orig', PctRecRef = '% Total Orig',
               CntRec='Total Val', PctRec = '% Total Val', PctRecAdj = '% Total Val Adj.',
               abs_dif_pct=gt::md(L_inf_latex), psi='PSI')
}

# Other Reports -----------------------------------------------------------

tab_iv_2_DT <- function(tab) {
  tab |> DT::datatable(extensions = 'Buttons',
                       options = list(dom = 'Blfrtip',
                                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                      lengthMenu = list(c(10,25,50,-1), c(10,25,50,"All")))) |>
    DT::formatSignif(c('cut_lo', 'cut_median', 'cut_hi', 'PctRec', 'PctGood', 'PctBad', 'BadRate', 'WoE', 'IV'), digits = 3)
}

cortes_a_sql <- function(x_var, x_var_gen, cortes) {
  res <- "case"
  res <- paste(res, "when",x_var, "is null then", "'bin_nulos'")
  res <- paste(res, "when", x_var, "<=", cortes[1], "then", "'bin_1'")
  anteult <- length(cortes) - 1
  for (i in 2:anteult)
    res <- paste(res, "when", x_var, "<", cortes[i], "then",
                 paste0("'",'bin_',i, "'"))
  res <- paste(res,"else", paste0("'", 'bin_',anteult+1, "'"))
  res <- paste0(res, " end as bin_", x_var)
  res
}

# Excel Reports -----------------------------------------------------------

skim_2_excel <- function(res, par_file_name=file_name) {
  res_part <- res |> skimr::partition()
  options('openxlsx.numFmt' = '#,#0.00')
  Boldstyle <- openxlsx::createStyle(textDecoration = "bold")
  wb <- openxlsx::write.xlsx(x = res_part |> map(~ as_tibble(.x) |> rename(variable=skim_variable)),
                             file = par_file_name, asTable = T, tableStyle = "TableStyleLight9",
                             overwrite = T, creator = 'BeSmart',
                             startRow = 1, startCol = 1, gridLines = F)
  res |> count(skim_type) |> pull(skim_type) -> sk_types
  sk_types |> walk(~ openxlsx::setColWidths(wb = wb, sheet = .x,
                                            cols = 1:20, widths = 'auto'))
  sk_types |> walk(~ openxlsx::freezePane(wb = wb, sheet = .x,
                                          firstRow = TRUE))
  openxlsx::addWorksheet(wb, sheetName = 'Gral', gridLines = F)
  openxlsx::activeSheet(wb) <- "Gral"
  openxlsx::addStyle(wb = wb, sheet = 'Gral',  style = Boldstyle, rows = 1:4, cols = 1, gridExpand = T)
  openxlsx::setColWidths(wb = wb, sheet = 'Gral', cols = 1:2, widths = 20)
  wb |> openxlsx::writeData(sheet = 'Gral', startRow = 1, startCol = 1,
                            x = 'Descripción general de la tabla de observaciones')
  wb |> openxlsx::writeData(sheet = 'Gral', startRow = 3, startCol = 1,
                            x = 'En las hojas siguientes de detallan las distribuciones de las variables predictoras')
  wb |> openxlsx::writeData(x = paste('Variables no predictoras:              ', paste(cols_no_predictoras, collapse = ", ")), sheet = 'Gral', startRow = 5)
  wb |> openxlsx::writeData(x = paste('Variables forzadas a predictoras:              ', paste(cols_forzadas_a_predictoras, collapse = ", ")), sheet = 'Gral', startRow = 6)
  wb |> openxlsx::writeData(x = paste('Variables forzadas a categóricas: ', paste(cols_forzadas_a_cat, collapse = ", ")), sheet = 'Gral', startRow = 7)
  wb |> openxlsx::writeData(x = paste('Variables con nulos adicionales:  ', paste(cols_nulos_adic, collapse = ", ")), sheet = 'Gral', startRow = 8)
  wb |> openxlsx::writeData(x = utils::capture.output(res |> summary()), sheet = 'Gral', startRow = 10)
  openxlsx::worksheetOrder(wb) <- openxlsx::worksheetOrder(wb) |> last2first()
  res <- wb |> openxlsx::saveWorkbook(par_file_name, overwrite = TRUE, returnValue = TRUE)
  return(res)
}


# Chequeo alineación ------------------------------------------------------

# Esta función usa la salida de  bootstraps(apparent = T) en el argumento bt_rs
# Uso la opción apparent para obtener el dataset completo pero no sé si es rebuscado.
bt_rs_2_tab_bt_sum <- function(bt_rs, par_quantiles = c(0.025, 0.5, 0.975),
                               score_var = score, score_niv_var = score_niv,
                               group_vars = ".part", tab_ref_alin, ...) {
  other_args <- rlang::enquos(...)
  corte_2_exprs(group_vars) -> group_vars_exprs
  bt_rs %>%
    transmute(boot_id=id, tab_niv=
                map(splits, ~ .x %>% (rsample::analysis) %>%
                      mutate(score={{score_var}}, score_niv={{score_niv_var}}) %>%
                      group_by(!!!group_vars_exprs) %>%
                      gentTab_niv(tab_ref_alin, score_niv, Score_Min, Score_Prom, Score_Max,
                                  TM_min, S_Max, S_Prom, Tasa_Malos, S_Min, TM_max,
                                  Alineado, KS, Total, Malos, P.Total, P.asc.Total, Tasa_Malos_asc))) %>%
    tidyr::unnest(cols = c(tab_niv)) -> tab_bt
  tab_bt %>% filter(boot_id=='Apparent') %>% group_by(!!!group_vars_exprs) -> tab
  tab_bt %>% filter(boot_id!='Apparent') %>% group_by(!!!group_vars_exprs, score_niv) %>%
    reframe(B_Cuantil=par_quantiles,
            Tasa_Malos_B=quantile(Tasa_Malos, probs=par_quantiles, na.rm = TRUE),
            KS_B=quantile(KS, probs=par_quantiles, na.rm = TRUE),
            Alineado_B=mean(Alineado)) -> tab_bt_sum
  tab_bt_sum %>%
    tidyr::pivot_wider(id_cols = c(!!!group_vars_exprs, score_niv, Alineado_B),
                       names_from = B_Cuantil, values_from = c(Tasa_Malos_B, KS_B)) -> tab_bt_sum
  res <- tab %>% inner_join(tab_bt_sum, by = join_by(!!!group_vars_exprs, score_niv))
  return(res)
}

tab_ref_niv_alin_gt_print <- function(tab_ref, title = 'Modelo Obtenido',
                                      subtitle = 'Límites para Tasas de malos') {
  tab_ref %>%
    gt %>%
    tab_header(title = title, subtitle = subtitle) %>%
    cols_label(
      score_niv = gt::html("Nivel<br> de Riesgo"),
      TM_min = gt::html("<b>% M<b><br> m&iacute;nimo"),
      TM_max = gt::html("<b>% M<b><br> m&aacute;ximo")
    ) %>%
    fmt_percent(columns = vars(TM_min, TM_max), decimals = 1) %>%
    cols_align(
      align = "center",
      columns = vars(score_niv)
    ) %>%
    tab_style(
      cell_text(align = 'center'),
      locations = cells_column_labels(everything())
    ) %>%
    opt_table_lines(extent = 'all') %>%
    opt_row_striping()
}

# Función para obtener una tabla de referencia para comprobar la alienación de cada nivel de riesgo.
df_2_tab_ref_niv_alin <- function(df, score_niv_var=score_niv, score_var = score) {
  df %>%
    mutate(Target=dplyr::coalesce(as.integer(as.character(Target)),0L)) %>%
    mutate(score={{score_var}}, score_niv={{score_niv_var}}) %>%
    group_by(score_niv, .add=T) %>%
    summarise(Score_Min=min(score), Score_Prom=mean(score), Score_Max=max(score),
              Buenos=sum(1-Target), Malos=sum(Target),
              Tasa_Malos=Malos/(Malos+Buenos), Odds=Buenos/Malos) %>%
    arrange(desc(Score_Prom)) %>%
    mutate(TM_min=lag(Tasa_Malos, default = 0),
           TM_max=lead(Tasa_Malos, default = 1)) %>%
    select(score_niv, TM_min, TM_max)
}

# Función auxiliar para calcular alineación con respecto a una tabla de referencia
# Por ahora se asume una tabla basada en score_niv y donde se compara la tasa de Malos.
# tab_ref
nivel_alineado_Train <- function(tab, tab_ref) {
  tab %>% inner_join(tab_ref, by = 'score_niv') %>%
    mutate(Alin_niv_Train=(TM_min <= Tasa_Malos & Tasa_Malos <= TM_max))
}

comp_2_scores_niv_gt <- function(bt_rs, group_vars) {
  bt_rs %>% bt_rs_2_tab_bt_sum(score_var = score, score_niv_var = score_niv,
                               group_vars = !!enquo(group_vars), tab_ref_alin = tab_ref_niv_alin_obtenido) %>%
    tab_niv_bt_gt(title='Modelo Obtenido') %>% print()
  bt_rs %>% bt_rs_2_tab_bt_sum(score_var = score_inic, score_niv_var = score_inic_niv,
                               group_vars = !!enquo(group_vars), tab_ref_alin = tab_ref_niv_alin_inic) %>%
    tab_niv_bt_gt(title='Modelo Inicial') %>% print()
}


# Performance -------------------------------------------------------------

# Resumen en una sola fila de una tabla de performance.
# Calcula KS, Gini, bad rate cortando los peores 10,20,30%.
# Es aproximado porque usa tablas de peformance pero anda bien a 20 cuantiles.
# Ej Uso
# tab.train <- df.scores |> filter(.part=="1_Train") |> select(-.part) |>
#   genTab_f(genAsocCuantil(tabOrig), vars_adic = "P.asc.Buenos")
# tab.train |> tab_perf_resumen() |> tidyr::pivot_longer(everything())
tab_perf_resumen <- function(tab_perf) {
  c("Cuantil", "P.asc.Total", "P.asc.Malos", "P.asc.Buenos", "Score_Min", "Score_Max",
    "Total", "Malos", "P.Total", "Tasa_Malos_desc") %in% names(tab_perf) -> cond
  res <- assertthat::assert_that(all(cond))
  tab_perf |>
    arrange(desc(Cuantil)) |>
    summarise(
      Cuantiles=n(),
      br_corte_peor_10 = br_corte_peor_Xp(P.asc.Total, Tasa_Malos_desc, Xporc=10),
      br_corte_peor_20 = br_corte_peor_Xp(P.asc.Total, Tasa_Malos_desc, Xporc=20),
      br_corte_peor_30 = br_corte_peor_Xp(P.asc.Total, Tasa_Malos_desc, Xporc=30),
      malos_prim_cuant=first(Malos), malos_ult_cuant=last(Malos), peor_mejor_cuantil=malos_ult_cuant/malos_prim_cuant,
      KS=max(KS), auc = tab_perf_2_auc_cols(Cuantil, P.asc.Malos, P.asc.Buenos),
      # Ojo! Para evitar conflictos por reusar nombres, lo hago aquí. Después de los cálculos que NO usan la información resumida.
      Score_Min=min(Score_Min), Score_Max=max(Score_Max),
      Total=sum(Total), `P.Total`=sum(`P.Total`), `P.asc.Total`=first(`P.asc.Total`),
      Malos=sum(Malos), `P.asc.Malos`=first(`P.asc.Malos`), Tasa_Malos_desc=last(Tasa_Malos_desc),
      # Aqui ya uso los resúmenes.
      Buenos=Total - Malos, Odds=(Total-Malos)/Malos, Tasa_Malos_Rel=100, Tasa_Malos=100*Malos/Total,
      Gini = 2*auc-1, Gini = 100*Gini, auc = 100*auc) |>
    relocate(c("Total", "Malos", "Tasa_Malos", "Odds", "br_corte_peor_10", "br_corte_peor_20", "br_corte_peor_30",
               "peor_mejor_cuantil", "KS", "auc", "Gini"))
}

# Resumen x grupos.
# Ej uso df.scores |> perf_resumen_x_grupos(tabOrig, .part) -> res
perf_resumen_x_grupos <- function(scores, tab_cortes, var_grupo, sel_cols) {
  if (missing(sel_cols)) {
    c("Cuantiles", "Total", "Malos", "Tasa_Malos", "Odds",
      "br_corte_peor_10", "br_corte_peor_20", "br_corte_peor_30", "peor_mejor_cuantil",
      "KS", "auc", "Gini") -> sel_cols
  }
  scores |> group_by({{var_grupo}}) |>
    group_map(~ bind_cols(.y, genTab_f(.x, genAsocCuantil(tab_cortes), vars_adic = c("P.asc.Buenos")) |>
                            tab_perf_resumen() |>
                            select(all_of(sel_cols)))) |>
    bind_rows() |>
    tidyr::pivot_longer(cols = !{{var_grupo}}) |>
    tidyr::pivot_wider(names_from = {{var_grupo}}, values_from = value) |>
    rename(estad_fmt=name)
}

# Publica con gt la tabla de resúmenes de performance por grupo
# Ej uso:
# df.scores |> perf_resumen_x_grupos(tabOrig, .part) |> res_x_grupos_2_gt("Resumen de Performance x Partición")
res_x_grupos_2_gt <- function(res_x_grupos, title) {
  res_x_grupos |> gt(locale = "es-AR") |>
    text_transform(locations = cells_body(columns = c(estad_fmt)),
                   fn = function(x) {case_when(
                     x == "br_corte_peor_10" ~ "Tasa de Malos<br>cortando el 10%",
                     x == "br_corte_peor_20" ~ "Tasa de Malos<br>cortando el 20%",
                     x == "br_corte_peor_30" ~ "Tasa de Malos<br>cortando el 30%",
                     x == "Tasa_Malos" ~ "Tasa de Malos",
                     x == "peor_mejor_cuantil" ~ "<small># Malos mejor cuantil /<br> # Malos peor</small>",
                     x == "auc" ~ "AUC",
                     x == "Gini" ~ "Índice Gini",
                     T ~ x) |> map(~ gt::html(.x))}) |>
    fmt_number(columns = !estad_fmt, decimals = 1,
               drop_trailing_zeros = TRUE, drop_trailing_dec_mark = TRUE) |>
    cols_label(estad_fmt = "Estadístico") |>
    opt_row_striping() |>
    tab_header(title = title)
}

# % de Malos al dejar Xporc afuera.  Es aproximado xq usa tablas de performance
# Ej uso.
# tab.train |>
#   summarise(br_corte_peor_10 = br_corte_peor_Xp(P.asc.Total, Tasa_Malos_desc, Xporc=10),
#             br_corte_peor_20 = br_corte_peor_Xp(P.asc.Total, Tasa_Malos_desc, Xporc=20),
#             br_corte_peor_30 = br_corte_peor_Xp(P.asc.Total, Tasa_Malos_desc, Xporc=30))
br_corte_peor_Xp <- function(P.asc.Total, Tasa_Malos_desc, Xporc=30) {
  tibble(P.asc.Total, Tasa_Malos_desc) -> tab
  tab |> arrange(desc(P.asc.Total)) |>
    dplyr::mutate(pat_sig = dplyr::lead(P.asc.Total, default = 0)) |>
    dplyr::filter((pat_sig |> round()) >= Xporc) |>
    dplyr::summarise(malos_corte_peor_Xp = dplyr::last(Tasa_Malos_desc))
  res <- dplyr::pull(res, malos_corte_peor_Xp)
}


# de https://www.r-bloggers.com/2016/11/calculating-auc-the-area-under-a-roc-curve/
# Ej uso: tab.train |> tab_perf_2_auc()
# Con una tabla de 20 cuantiles es bastante preciso.
tab_perf_2_auc <- function(tab_perf) {
  assertthat::assert_that(tab_perf |> is_tibble(), msg = "Para calcular el auc se necesita una tabla del tipo tibble!")
  assertthat::assert_that(c("Cuantil", "P.asc.Malos", "P.asc.Buenos") %in% (tab_perf |> names()) |> all(),
                          msg = "La tabla de performance para calcular el auc necesita las columnas Cuantil, P.asc.Malos, P.asc.Buenos")
  tab_perf |> dplyr::arrange(Cuantil) |>
    dplyr::select(P.asc.Malos, P.asc.Buenos) |>
    dplyr::rename(TPR = P.asc.Malos, FPR = P.asc.Buenos) -> tab
  bind_rows(tibble(TPR = 0, FPR = 0), tab) |>
    dplyr::mutate(TPR = TPR / 100, FPR = FPR / 100) |>
    dplyr::mutate(dFPR = dplyr::lead(FPR, default = 1) - FPR, dTPR = dplyr::lead(TPR, default = 1) - TPR) |>
    dplyr::summarise(auc = sum(TPR * dFPR) + sum(dTPR * dFPR) / 2) -> res
  res <- dplyr::pull(res, auc)
  res
}

# de https://www.r-bloggers.com/2016/11/calculating-auc-the-area-under-a-roc-curve/
# Ej Uso: tab.train |> summarise(auc = tab_perf_2_auc_cols(Cuantil, P.asc.Malos, P.asc.Buenos))
# Con una tabla de 20 cuantiles es bastante preciso.
tab_perf_2_auc_cols <- function(Cuantil, TPR, FPR) {
  assertthat::assert_that(length(Cuantil) == length(TPR), length(Cuantil) == length(FPR))
  tibble(Cuantil, TPR, FPR) -> tab
  tab |> dplyr::arrange(Cuantil) |> dplyr::select(TPR, FPR) -> tab
  bind_rows(tibble(TPR = 0, FPR = 0), tab) |>
    dplyr::mutate(TPR = TPR / 100, FPR = FPR / 100) |>
    dplyr::mutate(dFPR = dplyr::lead(FPR, default = 1) - FPR, dTPR = dplyr::lead(TPR, default = 1) - TPR) |>
    dplyr::summarise(auc = sum(TPR * dFPR) + sum(dTPR * dFPR) / 2) -> res
  res <- dplyr::pull(res, auc)
  res
}


plotROC_Pred1vs2y3 <- function(Pred1, Pred2, Pred3, per, main="ROC") {
  # TO DO:  fix hardcoded legend.names
  legend.colors <- array(data = NA, 4)
  legend.lty <- array(data = NA, dim = 4)
  legend.names <- array(data = NA, dim = 4)
  plot(c(0, 1), c(0, 1), type = "l", lty = 4, col = "black",
       xlab = "FPR (1-F0)", ylab = "TPR (1-F1)", main = main)
  legend.lty[1] <- 4
  legend.colors[1] <- "black"
  legend.names[1] <- "trivial"
  # Pred1 results
  pred <- Pred1 |> dplyr::filter(Periodo == per)
  results <- hmeasure::HMeasure(pred$truth, pred$score)
  data <- attr(results, "data")
  data.now <- data[[1]]
  lines(1 - data.now$F0, 1 - data.now$F1, type = "l",
        lty = 1, col = "blue")
  lines(data.now$G0, data.now$G1, type = "l", lty = 3,
        col = "blue")
  legend.lty[2] <- 1
  legend.colors[2] <- "blue"
  legend.names[2] <- "LR Reaj"
  # Pred2 results
  pred <- Pred2 |> dplyr::filter(Periodo == per)
  results <- hmeasure::HMeasure(pred$truth, pred$score)
  data <- attr(results, "data")
  data.now <- data[[1]]
  lines(1 - data.now$F0, 1 - data.now$F1, type = "l",
        lty = 1, col = "red")
  lines(data.now$G0, data.now$G1, type = "l", lty = 3,
        col = "red")
  legend.lty[3] <- 1
  legend.colors[3] <- "red"
  legend.names[3] <- "RF Reaj"

  # Hardcoded! LR Orig per=201408
  pred <- Pred3 |> dplyr::filter(Periodo == per)
  results <- hmeasure::HMeasure(pred$truth, pred$score)
  data <- attr(results, "data")
  data.now <- data[[1]]
  lines(1 - data.now$F0, 1 - data.now$F1, type = "l",
        lty = 1, col = "green")
  lines(data.now$G0, data.now$G1, type = "l", lty = 3,
        col = "green")
  legend.lty[4] <- 1
  legend.colors[4] <- "green"
  legend.names[4] <- "LR Orig"

  legend("bottomright", legend = legend.names, lty = legend.lty,
         col = legend.colors)
}


tab_KS <- function(df, bins=20, var_target="Bad") {
  df <- df |> mutate(target=df[[var_target]])
  tabOrig <- df  |> genTab_ntil(bins)
  tab <- df |> genTab_f(genAsocCuantil(tabOrig))
  return(tab)
}

repKS <- function(tab, caption, totales=F) {
  if (totales)
    tab <- tab |> mutate(Cuantil=as.character(Cuantil)) |>
      bind_rows(tab |> summarise(Cuantil="Total", Score_Min=min(Score_Min), Score_Max=max(Score_Max),
                                 Total=sum(Total), `P.Total`=sum(`P.Total`), `P.asc.Total`=max(`P.asc.Total`),
                                 Malos=sum(Malos), `P.asc.Malos`=max(`P.asc.Malos`), Tasa_Malos=100*Malos/Total,
                                 Tasa_Malos_Rel=100, Tasa_Malos_desc=Tasa_Malos, KS=max(KS), Odds=(Total-Malos)/Malos))
  tab |> gt(locale = "es-AR") |>
    tab_style(locations = cells_body(rows = Cuantil == "Total"),
              style = cell_borders(sides = "top", weight = px(2)) ) |>
    tab_header(title = caption) |>
    opt_row_striping() |>
    tab_style(locations = cells_body(columns = KS, rows = KS == max(KS)),
              style = cell_text(weight = "bold")) |>
    tab_style(locations = cells_body(columns = Tasa_Malos_desc,
                                     rows = Tasa_Malos_desc==max(Tasa_Malos_desc)),
              style = cell_text(weight = "bold")) |>
    tab_style(locations = cells_body(columns = Tasa_Malos,
                                     rows = Tasa_Malos==min(Tasa_Malos) | Tasa_Malos==max(Tasa_Malos)),
              style = cell_text(weight = "bold")) |>
    fmt_number(columns = c(P.Total, P.asc.Total, P.asc.Malos, Tasa_Malos, Tasa_Malos_Rel, Tasa_Malos_desc, KS, Odds),
               decimals = 1) |>
    fmt_number(decimals = 0, columns = c(Score_Min, Score_Max, Total, Malos)) |>
    cols_label(Cuantil='Cuantil', Score_Min='Score Min', Score_Max='Score Max', Total='Total',
               P.Total='% Total', P.asc.Total='% Asc Total', Malos='Malos', P.asc.Malos='% Asc Malos',
               Tasa_Malos='Tasa Malos', Tasa_Malos_Rel='Tasa Malos Rel', Tasa_Malos_desc='Tasa Malos desc',
               KS='KS', Odds='Odds')
}



# Reportes de resúmenes por skim -------------------

top_percents <- function(vec, n = 10) {
  skimr::sorted_count(vec)/length(vec) -> vec
  min(n, length(vec)) -> n
  vec[1:n] %>% {100*round(., digits = 4)} -> vec
  paste(names(vec), ': ', vec, '%', sep = '', collapse = ' | ')
}

my_skim <- skimr::skim_with(
  base = skimr::sfl(
    q = length,
    q_nulos = skimr::n_missing,
    porc_compl = skimr::complete_rate
  ),
  character = skimr::sfl(
    q_vacios = ~ skimr::n_empty(.x),
    q_blancos = ~ skimr::n_whitespace(.x),
    q_unicos = ~ skimr::n_unique(.x),
    long_min = ~ skimr::min_char(.x),
    long_max = ~ skimr::max_char(.x),
    mostFreq20 = ~ top_percents(.x, n = 20)
  ),
  numeric = skimr::sfl(
    avg = ~ mean(.x, na.rm = T),
    sd = ~ sd(.x, na.rm = T),
    min = ~ min_na(.x),
    p01 = ~ quantile(.x, probs = .01, na.rm = T),
    p05 = ~ quantile(.x, probs = .05, na.rm = T),
    p25 = ~ quantile(.x, probs = .25, na.rm = T),
    mediana = ~ quantile(.x, probs = .50, na.rm = T),
    p75 = ~ quantile(.x, probs = .75, na.rm = T),
    p95 = ~ quantile(.x, probs = .95, na.rm = T),
    p99 = ~ quantile(., probs = .99, na.rm = T),
    max = ~ max_na(.x),
    hist = ~ skimr::inline_hist(.x, n_bins = 8) # Parametrizar?
  ),
  append = F
)


