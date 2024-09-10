#- Rounding on 2 Digits --------------------------------------------------------

scaleFUN <- function(x) sprintf("%.2f", x)

compute_all_cis <- function(boot_object) {
  index <- seq_len(ncol(boot_object$t))
  
  compute_ci <- function(i) {
    ci <- boot::boot.ci(boot_object, type = "perc", index = i, conf = 0.95)$percent
    result <- data.frame(ci_lower = ci[(length(ci)-1)],
                         ci_higher = ci[length(ci)])
  }
  
  ci_results <- bind_rows(lapply(index, compute_ci))
  
  return(ci_results)
}

#- Bootstrap to Modelsummary ---------------------------------------------------


bootstrap_to_modelsummary <- function(bootstrap, TFE = 0, UFE = 0){
  
  results <-  bootstrap |> 
    rename(estimate = original,
           std.error = bootSE,
           term = indicator,
           iterations = R,
           conf.low = ci_lower,
           conf.high = ci_higher)
  
  results <- list(
    tidy = select(results, c("term","estimate", "std.error", "conf.low", "conf.high")),
    glance = data.frame(
      Iterations = "1000",
      Time_fixed_effects = if_else(TFE == 1, "X", ""),
      Unit_fixed_effects = if_else(UFE == 1, "X", ""))
  )
  
  return(results)
  
}

#- Mediation Functions ---------------------------------------------------------

mediation_function_partyleader <- function(data, indices) {
  d <- data[indices,]
  
  model_m <- fixest::fepois(n ~ treatment + n_lag1 | classification, data = d)
  model_y <- fixest::fepois(n_speeches ~ treatment + n*party_leader_2 + government + lag_n_speeches| classification, data = d)
  
  a_path = coef(model_m)["treatment"]
  b_path = coef(model_y)["n"]
  c_path = coef(model_y)["treatment"]
  interaction = coef(model_y)["n:party_leader_21"]
  
  b_moderated_1 <- b_path + interaction * 0
  b_moderated_2 <- b_path + interaction * 1
  
  indirect_1 <- b_moderated_1 * a_path
  indirect_2 <- b_moderated_2 * a_path
  
  total_effect_1 <- indirect_1 + c_path
  total_effect_2 <- indirect_2 + c_path

  c_path_exp <- exp(c_path)
  indirect_1_exp <- exp(indirect_1)
  indirect_2_exp <- exp(indirect_2)
  total_effect_1_exp <- exp(total_effect_1)
  total_effect_2_exp <- exp(total_effect_2)
  
  prop_indirect_1 <- (indirect_1 / total_effect_1) * 100
  prop_indirect_2 <- (indirect_2 / total_effect_2) * 100
  
  return(c(c_path_exp,
           indirect_1_exp,
           indirect_2_exp,
           prop_indirect_1,
           prop_indirect_2))
}

# Party Leader 2WFE
mediation_function_partyleader_2WFE <- function(data, indices) {
  d <- data[indices,]
  
  model_m <- fixest::fepois(n ~ treatment + n_lag1 | classification + date, data = d)
  model_y <- fixest::fepois(n_speeches ~ treatment + n*party_leader_2 + government + lag_n_speeches| classification + date, data = d)
  
  # Koeffizienten extrahieren
  a_path = coef(model_m)["treatment"]
  b_path = coef(model_y)["n"]
  c_path = coef(model_y)["treatment"]
  interaction = coef(model_y)["n:party_leader_21"]

  b_moderated_1 <- b_path + interaction * 0
  b_moderated_2 <- b_path + interaction * 1

  indirect_1 <- b_moderated_1 * a_path
  indirect_2 <- b_moderated_2 * a_path
  
  total_effect_1 <- indirect_1 + c_path
  total_effect_2 <- indirect_2 + c_path
  
  c_path_exp <- exp(c_path)
  indirect_1_exp <- exp(indirect_1)
  indirect_2_exp <- exp(indirect_2)
  total_effect_1_exp <- exp(total_effect_1)
  total_effect_2_exp <- exp(total_effect_2)
  
  prop_indirect_1 <- (indirect_1 / total_effect_1) * 100
  prop_indirect_2 <- (indirect_2 / total_effect_2) * 100
  
  return(c(c_path_exp,
           indirect_1_exp,
           indirect_2_exp,
           prop_indirect_1,
           prop_indirect_2))
}

# Difference in policy position
mediation_function_poldiff <- function(data, indices) {
  d <- data[indices,]
  
  model_m <- fixest::fepois(n ~ treatment + n_lag1 | classification, data = d)
  model_y <- fixest::fepois(n_speeches ~ treatment + n*pol_position_diff_party + government + lag_n_speeches| classification, data = d)
  
  a_path = coef(model_m)["treatment"]
  b_path = coef(model_y)["n"]
  c_path = coef(model_y)["treatment"]
  
  results = list()
  c_path_exp = exp(c_path)
  results[["direct"]] = c_path_exp
  
  for (level in levels(data$pol_position_diff_party)){
    interaction_term = paste0("n:pol_position_diff_party", level)
    if (interaction_term %in% names(coef(model_y))) {
      interaction = coef(model_y)[interaction_term]
    } else {
      interaction = 0  
    }
    
    b_moderated <- b_path + interaction
    indirect <- a_path * b_path
    total_effect <- indirect + c_path
    
    prop_indirect <- (indirect / total_effect) * 100
    
    total_effect <- exp(total_effect)
    indirect <- exp(indirect)
    
    results[[paste0("indirect_level_", level)]] <- indirect
    results[[paste0("prop_indirect_level_", level)]] <- prop_indirect
  }
  
  result_vector = unlist(results)
  names(result_vector) <- names(results)
  
  return(result_vector)
}

# Poldifference 2WFE
mediation_function_poldiff_2WEF <- function(data, indices) {
  d <- data[indices,]
  
  model_m <- fixest::fepois(n ~ treatment + n_lag1 | classification + date, data = d)
  model_y <- fixest::fepois(n_speeches ~ treatment + n*pol_position_diff_party + government + lag_n_speeches| classification + date, data = d)
  
  a_path = coef(model_m)["treatment"]
  b_path = coef(model_y)["n"]
  c_path = coef(model_y)["treatment"]
  
  results = list()
  c_path_exp = exp(c_path)
  results[["direct"]] = c_path_exp
  
  for (level in levels(data$pol_position_diff_party)){
    interaction_term = paste0("n:pol_position_diff_party", level)
    if (interaction_term %in% names(coef(model_y))) {
      interaction = coef(model_y)[interaction_term]
    } else {
      interaction = 0  
    }
    
    b_moderated <- b_path + interaction
    indirect <- a_path * b_path
    total_effect <- indirect + c_path
    
    prop_indirect <- (indirect / total_effect) * 100
    
    total_effect <- exp(total_effect)
    indirect <- exp(indirect)

    results[[paste0("indirect_level_", level)]] <- indirect
    results[[paste0("prop_indirect_level_", level)]] <- prop_indirect
  }
  
  result_vector = unlist(results)
  names(result_vector) <- names(results)
  
  return(result_vector)
}

#- Predicted Probs with Bootstrapping -------------------------------------------

# Bootstrapping Party Leader
boot_function_party_leader <- function(data, indices) {
  sample_data <- data[indices, ]  
  
  model <- update(model, data = sample_data)
  
  rel_freq <- sample_data |> 
    select(treated_topic_lag1, politician_id, treatment, party_leader_2) |> 
    na.omit() |> 
    arrange(treated_topic_lag1, politician_id, treatment, party_leader_2) |> 
    group_by_all() |> 
    count() |> 
    ungroup() |> 
    mutate(rel_freq = n / sum(n)) |> 
    crossing(
      treated_media = seq(0.06, 0.25, length.out = 50)
    )
  
  rel_freq['predicted_prob'] <- predict(model, newdata = rel_freq, type = "response")
  
  pred_data <- rel_freq |> 
    na.omit() |> 
    group_by(party_leader_2, treated_media) |> 
    mutate(
      predicted_prob = sum(rel_freq*predicted_prob),
    ) |> 
    ungroup() |> 
    select(predicted_prob, treated_media, party_leader_2) |> 
    distinct() |> 
    arrange(party_leader_2)
  
  
  return(pred_data$predicted_prob)
}

# Bootstrapping Ideological Distance
boot_function_ideological_distance <- function(data, indices) {
  
  sample_data <- data[indices, ]
  
  model <- update(models_pred_prob, data = sample_data)
  
  print(length(model$coefficients))
  
  if(length(model$coefficients) == 13){
    rel_freq <- sample_data |> 
      select(treated_topic_lag1, politician_id, treatment, pol_position_diff_party) |> 
      na.omit() |> 
      arrange(treated_topic_lag1, politician_id, treatment, pol_position_diff_party) |> 
      group_by_all() |> 
      count() |> 
      ungroup() |> 
      mutate(rel_freq = n / sum(n)) |> 
      crossing(
        treated_media = seq(0.06, 0.25, length.out = 50)
      )
    
    rel_freq['predicted_prob'] <- predict(model, newdata = rel_freq, type = "response")
    
    pred_data <- rel_freq |> 
      na.omit() |> 
      group_by(pol_position_diff_party, treated_media) |> 
      mutate(
        predicted_prob = sum(rel_freq*predicted_prob)
      ) |> 
      ungroup() |> 
      select(predicted_prob, treated_media, pol_position_diff_party) |> 
      distinct() |> 
      arrange(pol_position_diff_party, treated_media)
    
    print(nrow(pred_data))
    return(pred_data$predicted_prob) 
  } else {
    return(rep(NA, 450))
  }
}

#- Summary Table ---------------------------------------------------------------

remove_na_columns <- function(df) {
  df <- df |>  select_if(~ !all(is.na(.)))
  return(df)
}

create_summary_table <- function(data, type) {
  
  full_summary <- data |>
    skimr::skim_without_charts() |>
    as.data.frame() 
  
  if(type == "numeric"){
    summary <- full_summary |> 
      filter(skim_type == "numeric") |> 
      select(-skim_type, -complete_rate) |> 
      remove_na_columns()
  }
  
  if(type == "factor"){
    summary <- full_summary |> 
      filter(skim_type == "character") |> 
      select(-skim_type) |> 
      remove_na_columns()
  }
  
  if(type == "date"){
    summary <- full_summary |> 
      filter(skim_type == "Date") |> 
      select(-skim_type) |> 
      remove_na_columns()
  }
  
  return(summary)
}

#- Statistical Tests Bootstrapping ---------------------------------------------

check_overlap_indirect <- function(data) {
  data <- data |> 
    filter(str_detect(indicator, "^indirect")) |> 
    select(indicator, contains("ci"))
  
  ci1_upper <- data$ci_higher[1]
  ci1_lower <- data$ci_lower[1]
  ci2_upper <- data$ci_higher[2]
  ci2_lower <- data$ci_lower[2]
  
  if (ci1_upper >= ci2_lower && ci2_upper >= ci1_lower) {
    return(TRUE)
  } else {
    return(FALSE)  
  }
}

test_for_significance <- function(data) {
  data <- data |> 
    filter(str_detect(indicator, "^indirect|^direct")) |> 
    select(indicator, contains("ci")) |> 
    mutate(ci_higher = ci_higher - 1,
           ci_lower = ci_lower -1) |> 
    mutate(sini = if_else(ci_higher * ci_lower > 0, TRUE, FALSE))
  
  for(i in 1:nrow(data)){
    print(paste0(data$indicator[i], ": ", data$sini[i]))
  }
}

#- Read Latest Data Set --------------------------------------------------------

latest_csv = function(directory = getwd(), name){
  old_wd <- getwd()
  setwd(directory)
  files_names <- list.files()
  files_correct_name <- files_names[str_detect(files_names, name)] 
  dates <- as.integer(str_replace_all(files_correct_name, "_.*", ""))
  latest_date <- as.character(max(dates))
  latest_file <- files_correct_name[str_detect(files_correct_name, latest_date)]
  data <- read_csv(latest_file)
  setwd(old_wd)
  return(data)
}

latest_xls = function(directory = getwd(), name){
  old_wd <- getwd()
  setwd(directory)
  files_names <- list.files()
  files_correct_name <- files_names[str_detect(files_names, name)] 
  dates <- as.integer(str_replace_all(files_correct_name, "_.*", ""))
  latest_date <- as.character(max(dates))
  latest_file <- files_correct_name[str_detect(files_correct_name, latest_date)]
  data <- read_excel(latest_file)
  setwd(old_wd)
  return(data)
}

#- Panel Occurrence ------------------------------------------------------------

create_panel_ocurrence = function(date, seq = 1, id){
  date_min <- min(date)
  date_max <- max(date)
  id = unique(id)
  
  panel_frame <- expand.grid(date = seq(min(date_min), max(date_max), by = seq),   
                             id = id) 
  return(panel_frame)
}

#- Unique Filter  --------------------------------------------------------------

unique_filter <- function(df,...){
  grouping <- quos(...)
  df <-  df |> group_by(!!!grouping) |>
    mutate(n = n()) |>
    ungroup() |>
    filter(n == 1) |>
    select(-n)
  
  return(df)
}

#- Test for duplicates  --------------------------------------------------------

test_for_duplicates <- function(x,y){
  
  vec  <- !(duplicated(x) | duplicated(y) | duplicated(x, fromLast = TRUE)|  duplicated(y, fromLast = TRUE))
  
  return(vec)
}

#- Stepwise Merging  -----------------------------------------------------------

schrittweiser_merge <- function(daten1, daten2, variablen) {
  
  daten1 <- daten1 |> mutate(id_x = row_number())
  daten2 <- daten2 |> mutate(id_y = row_number())
  final <- data.frame()
  
  # round 1
  merged_data <- inner_join(daten1, daten2, by = variablen[1], suffix = c("", ".y")) |>
    filter(T == test_for_duplicates(id_x, id_y))
  
  daten1 <- daten1 |> filter(! id_x %in% merged_data$id_x)
  daten2 <- daten2 |> filter(! id_y %in% merged_data$id_y)
  final <- bind_rows(final, merged_data)
  
  # round 2-x
  for (i in 2:length(variablen)) {
    merged_data <- inner_join(daten1, daten2, by = variablen[1:i], suffix = c("", ".y")) |>
      filter(T == test_for_duplicates(id_x, id_y))
    
    daten1 <- daten1 |> filter(! id_x %in% merged_data$id_x)
    daten2 <- daten2 |> filter(! id_y %in% merged_data$id_y)
    final <- bind_rows(final, merged_data)
  }
  
  return(final)
}


# Mediation Functions Party Leader 3--------------------------------------------

# Partyleader with party leader 3 Unit fixed
mediation_function_partyleader <- function(data, indices) {
  # Stichprobe extrahieren
  d <- data[indices,]
  
  # Modelle fiten
  model_m <- fixest::fepois(n ~ treatment + n_lag1 | classification, data = d)
  model_y <- fixest::fepois(n_speeches ~ treatment + n*party_leader_3 + government + lag_n_speeches| classification, data = d)
  
  # Koeffizienten extrahieren
  a_path = coef(model_m)["treatment"]
  b_path = coef(model_y)["n"]
  c_path = coef(model_y)["treatment"]
  interaction = coef(model_y)["n:party_leader_31"]
  
  # Interaktion und Modulation einbeziehen
  b_moderated_1 <- b_path + interaction * 0
  b_moderated_2 <- b_path + interaction * 1
  
  # Indirekte und totale Effekte für beide Moderationsbedingungen
  indirect_1 <- b_moderated_1 * a_path
  indirect_2 <- b_moderated_2 * a_path
  
  total_effect_1 <- indirect_1 + c_path
  total_effect_2 <- indirect_2 + c_path
  
  # Berechnungen anpassen für log odds
  c_path_exp <- exp(c_path)
  indirect_1_exp <- exp(indirect_1)
  indirect_2_exp <- exp(indirect_2)
  total_effect_1_exp <- exp(total_effect_1)
  total_effect_2_exp <- exp(total_effect_2)
  
  # Proportionen
  prop_indirect_1 <- (indirect_1 / total_effect_1) * 100
  prop_indirect_2 <- (indirect_2 / total_effect_2) * 100
  
  
  # Vektor zurückgeben
  return(c(c_path_exp,
           indirect_1_exp,
           indirect_2_exp,
           prop_indirect_1,
           prop_indirect_2))
}

# Party Leader 2WFE party leader 3
mediation_function_partyleader_2WFE <- function(data, indices) {
  # Stichprobe extrahieren
  d <- data[indices,]
  
  # Modelle fiten
  model_m <- fixest::fepois(n ~ treatment + n_lag1 | classification + date, data = d)
  model_y <- fixest::fepois(n_speeches ~ treatment + n*party_leader_3 + government + lag_n_speeches| classification + date, data = d)
  
  # Koeffizienten extrahieren
  a_path = coef(model_m)["treatment"]
  b_path = coef(model_y)["n"]
  c_path = coef(model_y)["treatment"]
  interaction = coef(model_y)["n:party_leader_31"]
  
  # Interaktion und Modulation einbeziehen
  b_moderated_1 <- b_path + interaction * 0
  b_moderated_2 <- b_path + interaction * 1
  
  # Indirekte und totale Effekte für beide Moderationsbedingungen
  indirect_1 <- b_moderated_1 * a_path
  indirect_2 <- b_moderated_2 * a_path
  
  total_effect_1 <- indirect_1 + c_path
  total_effect_2 <- indirect_2 + c_path
  
  # Berechnungen anpassen für log odds
  c_path_exp <- exp(c_path)
  indirect_1_exp <- exp(indirect_1)
  indirect_2_exp <- exp(indirect_2)
  total_effect_1_exp <- exp(total_effect_1)
  total_effect_2_exp <- exp(total_effect_2)
  
  # Proportionen
  prop_indirect_1 <- (indirect_1 / total_effect_1) * 100
  prop_indirect_2 <- (indirect_2 / total_effect_2) * 100
  
  
  # Vektor zurückgeben
  return(c(c_path_exp,
           indirect_1_exp,
           indirect_2_exp,
           prop_indirect_1,
           prop_indirect_2))
}




# Predicted Probs with Bootstrapping party leader 3-----------------------------

boot_function_party_leader <- function(data, indices) {
  sample_data <- data[indices, ]  
  
  model <- update(model, data = sample_data)
  
  rel_freq <- sample_data |>
    select(treated_topic_lag1, politician_id, treatment, party_leader_3) |>
    na.omit() |>
    arrange(treated_topic_lag1, politician_id, treatment, party_leader_3) |>
    group_by_all() |>
    count() |>
    ungroup() |>
    mutate(rel_freq = n / sum(n)) |>
    crossing(
      treated_media = seq(0.06, 0.25, length.out = 50)
    )
  
  rel_freq['predicted_prob'] <- predict(model, newdata = rel_freq, type = "response")
  
  pred_data <- rel_freq |>
    na.omit() |>
    group_by(party_leader_3, treated_media) |>
    mutate(
      predicted_prob = sum(rel_freq*predicted_prob),
    ) |>
    ungroup() |>
    select(predicted_prob, treated_media, party_leader_3) |>
    distinct()
  
  
  return(pred_data$predicted_prob)
}

# Mediation Functions party leader 1 -------------------------------------------

# Partyleader Unit fixed
mediation_function_partyleader <- function(data, indices) {
  d <- data[indices,]
  
  model_m <- fixest::fepois(n ~ treatment + n_lag1 | classification, data = d)
  model_y <- fixest::fepois(n_speeches ~ treatment + n*party_leader_1 + government + lag_n_speeches| classification, data = d)
  
  a_path = coef(model_m)["treatment"]
  b_path = coef(model_y)["n"]
  c_path = coef(model_y)["treatment"]
  interaction = coef(model_y)["n:party_leader_11"]
  
  b_moderated_1 <- b_path + interaction * 0
  b_moderated_2 <- b_path + interaction * 1
  
  indirect_1 <- b_moderated_1 * a_path
  indirect_2 <- b_moderated_2 * a_path
  
  total_effect_1 <- indirect_1 + c_path
  total_effect_2 <- indirect_2 + c_path
  
  c_path_exp <- exp(c_path)
  indirect_1_exp <- exp(indirect_1)
  indirect_2_exp <- exp(indirect_2)
  total_effect_1_exp <- exp(total_effect_1)
  total_effect_2_exp <- exp(total_effect_2)
  
  prop_indirect_1 <- (indirect_1 / total_effect_1) * 100
  prop_indirect_2 <- (indirect_2 / total_effect_2) * 100
  
  return(c(c_path_exp,
           indirect_1_exp,
           indirect_2_exp,
           prop_indirect_1,
           prop_indirect_2))
}

# Party Leader 2WFE party leader 1
mediation_function_partyleader_2WFE <- function(data, indices) {
  d <- data[indices,]
  
  model_m <- fixest::fepois(n ~ treatment + n_lag1 | classification + date, data = d)
  model_y <- fixest::fepois(n_speeches ~ treatment + n*party_leader_1 + government + lag_n_speeches| classification + date, data = d)
  
  a_path = coef(model_m)["treatment"]
  b_path = coef(model_y)["n"]
  c_path = coef(model_y)["treatment"]
  interaction = coef(model_y)["n:party_leader_11"]
  
  b_moderated_1 <- b_path + interaction * 0
  b_moderated_2 <- b_path + interaction * 1
  
  indirect_1 <- b_moderated_1 * a_path
  indirect_2 <- b_moderated_2 * a_path
  
  total_effect_1 <- indirect_1 + c_path
  total_effect_2 <- indirect_2 + c_path
  
  c_path_exp <- exp(c_path)
  indirect_1_exp <- exp(indirect_1)
  indirect_2_exp <- exp(indirect_2)
  total_effect_1_exp <- exp(total_effect_1)
  total_effect_2_exp <- exp(total_effect_2)
  
  prop_indirect_1 <- (indirect_1 / total_effect_1) * 100
  prop_indirect_2 <- (indirect_2 / total_effect_2) * 100
  
  return(c(c_path_exp,
           indirect_1_exp,
           indirect_2_exp,
           prop_indirect_1,
           prop_indirect_2))
}

# Predicted Probs with Bootstrapping Party Leader 1-----------------------------

boot_function_party_leader <- function(data, indices) {
  sample_data <- data[indices, ]  
  
  model <- update(model, data = sample_data)
  
  rel_freq <- sample_data |>
    select(treated_topic_lag1, politician_id, treatment, party_leader_1) |>
    na.omit() |>
    arrange(treated_topic_lag1, politician_id, treatment, party_leader_1) |>
    group_by_all() |>
    count() |>
    ungroup() |>
    mutate(rel_freq = n / sum(n)) |>
    crossing(
      treated_media = seq(0.06, 0.25, length.out = 50)
    )
  
  rel_freq['predicted_prob'] <- predict(model, newdata = rel_freq, type = "response")
  
  pred_data <- rel_freq |>
    na.omit() |>
    group_by(party_leader_1, treated_media) |>
    mutate(
      predicted_prob = sum(rel_freq*predicted_prob),
    ) |>
    ungroup() |>
    select(predicted_prob, treated_media, party_leader_1) |>
    distinct()
  
  
  return(pred_data$predicted_prob)
}


# Part A of fig. 2
# Prediction with Bootstrapping

boot_function_ideological_distance <- function(data, indices) {
  
  sample_data <- data[indices, ]
  
  model <- update(models_pred_prob, data = sample_data)
  
  rel_freq <- sample_data |>
    select(treated_topic_lag1, politician_id, treatment, pol_position_diff_party) |>
    na.omit() |>
    arrange(treated_topic_lag1, politician_id, treatment, pol_position_diff_party) |>
    group_by_all() |>
    count() |>
    ungroup() |>
    mutate(rel_freq = n / sum(n)) |>
    crossing(
      treated_media = seq(0.06, 0.25, length.out = 50)
    )
  
  rel_freq['predicted_prob'] <- predict(model, newdata = rel_freq, type = "response")
  
  pred_data <- rel_freq |>
    na.omit() |>
    group_by(pol_position_diff_party, treated_media) |>
    mutate(
      predicted_prob = sum(rel_freq*predicted_prob)
    ) |>
    ungroup() |>
    select(predicted_prob, treated_media, pol_position_diff_party) |>
    distinct() |>
    filter(pol_position_diff_party %in% c(0, 1, 2))
  
  return(pred_data$predicted_prob)
}

