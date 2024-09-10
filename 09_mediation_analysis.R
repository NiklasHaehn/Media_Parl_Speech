#- Set up ----------------------------------------------------------------------

set.seed(1234)
rm(list = ls())
options(scipen = 999)

pacman::p_load(dplyr,
               readr,
               tidyr,
               stringr,
               ggplot2,
               grid,
               lubridate,
               boot,
               gt,
               fixest) 

#- Load Data -------------------------------------------------------------------

load("data_analysis.RData")


# Data Wrangling  --------------------------------------------------------------
# Aggregate Analysis 

media_war <- media_war |> 
  group_by(date) |> 
  mutate(n_lag1 = lag(n)) |> 
  ungroup() |> 
  mutate(post_treatment = ifelse(date >= as.Date("2022-02-24"),1,0)) |> 
  mutate(treatment = ifelse(post_treatment == 1 & classification %in% c("Defense", "International", "Foreign", "Immigration"),1,0))

media_gau <- media_gau |> 
  group_by(date) |> 
  mutate(n_lag1 = lag(n)) |> 
  ungroup() |> 
  mutate(post_treatment = ifelse(date >= as.Date("2011-03-11"),1,0)) |> 
  mutate(treatment = ifelse(post_treatment == 1 & classification %in% c("Energy"),1,0))

media_data_pooled <- media_gau |> 
  bind_rows(media_war)

grid <- expand.grid(date = unique(data_war$date),
                    party_leader_2 = unique(data_war$party_leader_2),
                    classification = unique(data_war$classification),
                    government = unique(data_war$government))

data_war_agg <- data_war |> 
  group_by(
    date, party_leader_2, classification, government
  ) |> 
    summarise(n_speeches = n()) |> 
  ungroup() |> 
  right_join(grid, by = c("date", "party_leader_2", "classification", "government")) |> 
  mutate(n_speeches = ifelse(is.na(n_speeches), 0, n_speeches)) |> 
  left_join(media_war, by = c("date", "classification")) |> 
  group_by(classification, party_leader_2, government) |> 
  arrange(date) |> 
  mutate(lag_n_speeches = lag(n_speeches)) |> 
  ungroup()

grid <- expand.grid(date = unique(data_gau$date),
                    party_leader_2 = unique(data_gau$party_leader_2),
                    classification = unique(data_gau$classification),
                    government = unique(data_war$government))

data_gau_agg <- data_gau |> 
  group_by(
    date, party_leader_2, classification, government
  ) |> 
  summarise(n_speeches = n()) |> 
  ungroup() |> 
  right_join(grid, by = c("date", "party_leader_2", "classification", "government")) |> 
  mutate(n_speeches = ifelse(is.na(n_speeches), 0, n_speeches)) |> 
  left_join(media_gau, by = c("date", "classification")) |> 
  group_by(classification, party_leader_2, government) |> 
  arrange(date) |> 
  mutate(lag_n_speeches = lag(n_speeches)) |> 
  ungroup()

data_pool_agg <- data_gau_agg |> 
  bind_rows(data_war_agg)

grid <- expand.grid(date = unique(data_war$date),
                    pol_position_diff_party = unique(as.numeric(as.character(data_war$pol_position_diff_party))),
                    classification = unique(data_war$classification),
                    government = unique(data_war$government)) |> 
  na.omit()

data_war_agg_polpos <- data_war |> 
  mutate(pol_position_diff_party = as.numeric(as.character(data_war$pol_position_diff_party))) |> 
  group_by(
    date, pol_position_diff_party, classification, government
  ) |> 
  summarise(n_speeches = n()) |> 
  ungroup() |> 
  right_join(grid, by = c("date", "pol_position_diff_party", "classification", "government")) |> 
  mutate(n_speeches = ifelse(is.na(n_speeches), 0, n_speeches)) |> 
  left_join(media_war, by = c("date", "classification")) |> 
  group_by(classification, pol_position_diff_party, government) |> 
  arrange(date) |> 
  mutate(lag_n_speeches = lag(n_speeches)) |> 
  ungroup() |> 
  mutate(pol_position_diff_party = as.factor(pol_position_diff_party))

grid <- expand.grid(date = unique(data_war$date),
                    pol_position_diff_party = unique(as.numeric(as.character(data_war$pol_position_diff_party))),
                    classification = unique(data_war$classification),
                    government = unique(data_war$government)) |> 
  na.omit()

data_war_agg_polpos <- data_war |> 
  mutate(pol_position_diff_party = as.numeric(as.character(pol_position_diff_party))) |> 
  group_by(
    date, pol_position_diff_party, classification, government
  ) |> 
  summarise(n_speeches = n()) |> 
  ungroup() |> 
  right_join(grid, by = c("date", "pol_position_diff_party", "classification", "government")) |> 
  mutate(n_speeches = ifelse(is.na(n_speeches), 0, n_speeches)) |> 
  left_join(media_war, by = c("date", "classification")) |> 
  group_by(classification, pol_position_diff_party, government) |> 
  arrange(date) |> 
  mutate(lag_n_speeches = lag(n_speeches)) |> 
  ungroup() |> 
  mutate(pol_position_diff_party = as.factor(pol_position_diff_party))

grid <- expand.grid(date = unique(data_gau$date),
                    pol_position_diff_party = unique(as.numeric(as.character(data_gau$pol_position_diff_party))),
                    classification = unique(data_gau$classification),
                    government = unique(data_gau$government)) |> 
  na.omit()

data_gau_agg_polpos <- data_gau |> 
  mutate(pol_position_diff_party = as.numeric(as.character(pol_position_diff_party))) |> 
  group_by(
    date, pol_position_diff_party, classification, government
  ) |> 
  summarise(n_speeches = n()) |> 
  ungroup() |> 
  right_join(grid, by = c("date", "pol_position_diff_party", "classification", "government")) |> 
  mutate(n_speeches = ifelse(is.na(n_speeches), 0, n_speeches)) |> 
  left_join(media_gau, by = c("date", "classification")) |> 
  group_by(classification, pol_position_diff_party, government) |> 
  arrange(date) |> 
  mutate(lag_n_speeches = lag(n_speeches)) |> 
  ungroup() |> 
  mutate(pol_position_diff_party = as.factor(pol_position_diff_party))

data_pool_agg_polpos <- data_gau_agg_polpos |> 
  bind_rows(data_war_agg_polpos)

#---- binarized data

grid <- expand.grid(date = unique(data_war$date),
                    pol_position_diff_party = c(1:2),
                    classification = unique(data_war$classification),
                    government = unique(data_war$government)) |> 
  na.omit()

data_war_bin_polpos <- data_war |> 
  mutate(pol_position_diff_party = as.numeric(as.character(pol_position_diff_party))) |> 
  mutate(pol_position_diff_party = ifelse(pol_position_diff_party > 0, 1, 0)) |> 
  group_by(
    date, pol_position_diff_party, classification, government
  ) |> 
  summarise(n_speeches = n()) |> 
  ungroup() |> 
  right_join(grid, by = c("date", "pol_position_diff_party", "classification", "government")) |> 
  mutate(n_speeches = ifelse(is.na(n_speeches), 0, n_speeches)) |> 
  left_join(media_war, by = c("date", "classification")) |> 
  group_by(classification, pol_position_diff_party, government) |> 
  arrange(date) |> 
  mutate(lag_n_speeches = lag(n_speeches)) |> 
  ungroup() |> 
  mutate(pol_position_diff_party = as.factor(pol_position_diff_party))

grid <- expand.grid(date = unique(data_gau$date),
                    pol_position_diff_party = c(1:2),
                    classification = unique(data_gau$classification),
                    government = unique(data_gau$government)) |> 
  na.omit()

data_gau_bin_polpos <- data_gau |> 
  mutate(pol_position_diff_party = as.numeric(as.character(pol_position_diff_party))) |> 
  mutate(pol_position_diff_party = ifelse(pol_position_diff_party > 0, 1, 0)) |> 
  group_by(
    date, pol_position_diff_party, classification, government
  ) |> 
  summarise(n_speeches = n()) |> 
  ungroup() |> 
  right_join(grid, by = c("date", "pol_position_diff_party", "classification", "government")) |> 
  mutate(n_speeches = ifelse(is.na(n_speeches), 0, n_speeches)) |> 
  left_join(media_gau, by = c("date", "classification")) |> 
  group_by(classification, pol_position_diff_party, government) |> 
  arrange(date) |> 
  mutate(lag_n_speeches = lag(n_speeches)) |> 
  ungroup() |> 
  mutate(pol_position_diff_party = as.factor(pol_position_diff_party))

data_pool_bin_polpos <- data_gau_bin_polpos |> 
  bind_rows(data_war_bin_polpos)

#- Path Analysis Aggregated Level Party Leader ---------------------------------

##- A Path ---------------------------------------------------------------------

models1 <- list()
models1[['1.1']] <- fixest::fepois(n ~ treatment + n_lag1 | classification, data = data_war_agg)
models1[['1.2']] <- fixest::fepois(n ~ treatment + n_lag1 | classification + date, data = data_war_agg)

models1[['1.3']] <- fixest::fepois(n ~ treatment + n_lag1 | classification, data = data_gau_agg)
models1[['1.4']] <- fixest::fepois(n ~ treatment + n_lag1 | classification + date, data = data_gau_agg)

models1[['1.5']] <- fixest::fepois(n ~ treatment + n_lag1 | classification, data = data_pool_agg)
models1[['1.6']] <- fixest::fepois(n ~ treatment + n_lag1 | classification + date, data = data_pool_agg)

##- B Path ---------------------------------------------------------------------

models2 <- list()
models2[['1.1']] <- fixest::fepois(n_speeches ~ treatment + n*party_leader_2 + government + lag_n_speeches | classification, data = data_war_agg)
models2[['1.2']] <- fixest::fepois(n_speeches ~ treatment + n*party_leader_2 + government + lag_n_speeches | classification + date, data = data_war_agg)

models2[['1.3']] <- fixest::fepois(n_speeches ~ treatment + n*party_leader_2 + government + lag_n_speeches | classification, data = data_gau_agg)
models2[['1.4']] <- fixest::fepois(n_speeches ~ treatment + n*party_leader_2 + government + lag_n_speeches | classification + date, data = data_gau_agg)

models2[['1.5']] <- fixest::fepois(n_speeches ~ treatment + n*party_leader_2 + government + lag_n_speeches | classification, data = data_pool_agg)
models2[['1.6']] <- fixest::fepois(n_speeches ~ treatment + n*party_leader_2 + government + lag_n_speeches | classification + date, data = data_pool_agg)

##- C Path ---------------------------------------------------------------------

models3 <- list()
models3[['1.1']] <- fixest::fepois(n_speeches ~ treatment  + government + lag_n_speeches | classification, data = data_war_agg)
models3[['1.2']] <- fixest::fepois(n_speeches ~ treatment  + government + lag_n_speeches | classification + date, data = data_war_agg)

models3[['1.3']] <- fixest::fepois(n_speeches ~ treatment  + government + lag_n_speeches | classification, data = data_gau_agg)
models3[['1.4']] <- fixest::fepois(n_speeches ~ treatment  + government + lag_n_speeches | classification + date, data = data_gau_agg)

models3[['1.5']] <- fixest::fepois(n_speeches ~ treatment + government + lag_n_speeches | classification, data = data_pool_agg)
models3[['1.6']] <- fixest::fepois(n_speeches ~ treatment + government + lag_n_speeches | classification + date, data = data_pool_agg)

modelsummary::modelsummary(models1)

##- Model Output ---------------------------------------------------------------

# Table 1 agg - Party Leader
msum <- modelsummary::modelsummary(models1,
                                   output = "gt",
                                   stars = c("*"=0.05, "**"=0.01, "***"=0.001),
                                   coef_map = cm,
                                   gof_map = gm,
                                   fmt = scaleFUN,
                                   title = "",
                                   notes = list('A Path (H3): Poisson regression at the Aggregated Level.',
                                                'Robust standard errors clustered in parentheses.')
)

table1 <- msum |> 
  gt::tab_spanner(
    label = 'Ukraine War',
    columns = c("1.1", "1.2")) |> 
  gt::tab_spanner(
    label = 'Fukushima Nuclear Catastrophe',
    columns = c("1.3", "1.4")) |> 
  gt::tab_spanner(
    label = 'Pooled',
    columns = c("1.5", "1.6"))

gtExtras::gtsave_extra(table1, "table_a_path_pois_party_leader.png", path = "tables", zoom = 10, expand = 5)

modelsummary::modelsummary(models2)

# Table 2 agg - Party Leader
msum <- modelsummary::modelsummary(models2,
                                   output = "gt",
                                   stars = c("*"=0.05, "**"=0.01, "***"=0.001),
                                   coef_map = cm,
                                   gof_map = gm,
                                   fmt = scaleFUN,
                                   title = "",
                                   notes = list('B Path (H3): Poisson regression at the Aggregated Level.',
                                                'Robust standard errors clustered in parentheses.')
)

table1 <- msum |> 
  gt::tab_spanner(
    label = 'Ukraine War',
    columns = c("1.1", "1.2")) |> 
  gt::tab_spanner(
    label = 'Fukushima Nuclear Catastrophe',
    columns = c("1.3", "1.4")) |> 
  gt::tab_spanner(
    label = 'Pooled',
    columns = c("1.5", "1.6"))

gtExtras::gtsave_extra(table1, "table_b_path_pois_party_leader.png", path = "tables", zoom = 10, expand = 5)

modelsummary::modelsummary(models3)
# Table 3 agg - Party Leader
msum <- modelsummary::modelsummary(models3,
                                   output = "gt",
                                   stars = c("*"=0.05, "**"=0.01, "***"=0.001),
                                   coef_map = cm,
                                   gof_map = gm,
                                   fmt = scaleFUN,
                                   title = "",
                                   notes = list('C Path (H3): Poisson regression at the Aggregated Level.',
                                                'Robust standard errors clustered in parentheses.')
)

table1 <- msum |> 
  gt::tab_spanner(
    label = 'Ukraine War',
    columns = c("1.1", "1.2")) |> 
  gt::tab_spanner(
    label = 'Fukushima Nuclear Catastrophe',
    columns = c("1.3", "1.4")) |> 
  gt::tab_spanner(
    label = 'Pooled',
    columns = c("1.5", "1.6"))

gtExtras::gtsave_extra(table1, "table_c_path_pois_party_leader.png", path = "tables", zoom = 10, expand = 5)

#- Path Analysis Aggregated Level Ideological Differences ----------------------

##- A Path ---------------------------------------------------------------------

models1 <- list()
models1[['2.1']] <- fixest::fepois(n ~ treatment + n_lag1 | classification, data = data_war_agg_polpos)
models1[['2.2']] <- fixest::fepois(n ~ treatment + n_lag1 | classification + date, data = data_war_agg_polpos)

models1[['2.3']] <- fixest::fepois(n ~ treatment + n_lag1 | classification, data = data_gau_agg_polpos)
models1[['2.4']] <- fixest::fepois(n ~ treatment + n_lag1 | classification + date, data = data_gau_agg_polpos)

models1[['2.5']] <- fixest::fepois(n ~ treatment + n_lag1 | classification, data = data_pool_agg_polpos)
models1[['2.6']] <- fixest::fepois(n ~ treatment + n_lag1 | classification + date, data = data_pool_agg_polpos)

##- B Path ---------------------------------------------------------------------

models2 <- list()
models2[['2.1']] <- fixest::fepois(n_speeches ~ treatment + n*pol_position_diff_party + government + lag_n_speeches | classification, data = data_war_agg_polpos)
models2[['2.2']] <- fixest::fepois(n_speeches ~ treatment + n*pol_position_diff_party + government + lag_n_speeches | classification + date, data = data_war_agg_polpos)

models2[['2.3']] <- fixest::fepois(n_speeches ~ treatment + n*pol_position_diff_party + government + lag_n_speeches | classification, data = data_gau_agg_polpos)
models2[['2.4']] <- fixest::fepois(n_speeches ~ treatment + n*pol_position_diff_party + government + lag_n_speeches | classification + date, data = data_gau_agg_polpos)

models2[['2.5']] <- fixest::fepois(n_speeches ~ treatment + n*pol_position_diff_party + government + lag_n_speeches | classification, data = data_pool_agg_polpos)
models2[['2.6']] <- fixest::fepois(n_speeches ~ treatment + n*pol_position_diff_party + government + lag_n_speeches | classification + date, data = data_pool_agg_polpos)

##- B Path Bin -----------------------------------------------------------------

models2_bin <- list()
models2_bin[['2.1']] <- fixest::fepois(n_speeches ~ treatment + n*pol_position_diff_party + government + lag_n_speeches | classification, data = data_war_bin_polpos)
models2_bin[['2.2']] <- fixest::fepois(n_speeches ~ treatment + n*pol_position_diff_party + government + lag_n_speeches | classification + date, data = data_war_bin_polpos)

models2_bin[['2.3']] <- fixest::fepois(n_speeches ~ treatment + n*pol_position_diff_party + government + lag_n_speeches | classification, data = data_gau_bin_polpos)
models2_bin[['2.4']] <- fixest::fepois(n_speeches ~ treatment + n*pol_position_diff_party + government + lag_n_speeches | classification + date, data = data_gau_bin_polpos)

models2_bin[['2.5']] <- fixest::fepois(n_speeches ~ treatment + n*pol_position_diff_party + government + lag_n_speeches | classification, data = data_pool_bin_polpos)
models2_bin[['2.6']] <- fixest::fepois(n_speeches ~ treatment + n*pol_position_diff_party + government + lag_n_speeches | classification + date, data = data_pool_bin_polpos)


##- C Path ---------------------------------------------------------------------

models3 <- list()
models3[['2.1']] <- fixest::fepois(n_speeches ~ treatment  + government + lag_n_speeches | classification, data = data_war_agg_polpos)
models3[['2.2']] <- fixest::fepois(n_speeches ~ treatment  + government + lag_n_speeches | classification + date, data = data_war_agg_polpos)

models3[['2.3']] <- fixest::fepois(n_speeches ~ treatment  + government + lag_n_speeches | classification, data = data_gau_agg_polpos)
models3[['2.4']] <- fixest::fepois(n_speeches ~ treatment  + government + lag_n_speeches | classification + date, data = data_gau_agg_polpos)

models3[['2.5']] <- fixest::fepois(n_speeches ~ treatment + government + lag_n_speeches | classification, data = data_pool_agg_polpos)
models3[['2.6']] <- fixest::fepois(n_speeches ~ treatment + government + lag_n_speeches | classification + date, data = data_pool_agg_polpos)

##- Model Output ---------------------------------------------------------------

modelsummary::modelsummary(models1, stars =  T)
modelsummary::modelsummary(models2, stars =  T)
modelsummary::modelsummary(models3, stars =  T)


modelsummary::modelsummary(models1)
# Table 1 agg - Ideological Distances
msum <- modelsummary::modelsummary(models1,
                                   output = "gt",
                                   stars = c("*"=0.05, "**"=0.01, "***"=0.001),
                                   coef_map = cm,
                                   gof_map = gm,
                                   fmt = scaleFUN,
                                   title = "",
                                   notes = list('A Path (H4): Poisson regression at the Aggregated Level.',
                                                'Robust standard errors clustered in parentheses.')
)

table1 <- msum |> 
  gt::tab_spanner(
    label = 'Ukraine War',
    columns = c("2.1", "2.2")) |> 
  gt::tab_spanner(
    label = 'Fukushima Nuclear Catastrophe',
    columns = c("2.3", "2.4")) |> 
  gt::tab_spanner(
    label = 'Pooled',
    columns = c("2.5", "2.6"))

gtExtras::gtsave_extra(table1, "table_a_path_pois_distance.png", path = "tables", zoom = 10, expand = 5)


modelsummary::modelsummary(models2)
# Table 2 agg - Ideological Distances
msum <- modelsummary::modelsummary(models2,
                                   output = "gt",
                                   stars = c("*"=0.05, "**"=0.01, "***"=0.001),
                                   coef_map = cm,
                                   gof_map = gm,
                                   fmt = scaleFUN,
                                   title = "",
                                   notes = list('B Path (H4): Poisson regression at the Aggregated Level.',
                                                'Robust standard errors clustered in parentheses.')
)

table1 <- msum |> 
  gt::tab_spanner(
    label = 'Ukraine War',
    columns = c("2.1", "2.2")) |> 
  gt::tab_spanner(
    label = 'Fukushima Nuclear Catastrophe',
    columns = c("2.3", "2.4")) |> 
  gt::tab_spanner(
    label = 'Pooled',
    columns = c("2.5", "2.6"))

gtExtras::gtsave_extra(table1, "table_b_path_pois_distance.png", path = "tables", zoom = 5, expand = 5)

modelsummary::modelsummary(models3)
# Table 3 agg - Ideological Distances
msum <- modelsummary::modelsummary(models3,
                                   output = "gt",
                                   stars = c("*"=0.05, "**"=0.01, "***"=0.001),
                                   coef_map = cm,
                                   gof_map = gm,
                                   fmt = scaleFUN,
                                   title = "",
                                   notes = list('C Path (H4): Poisson regression at the Aggregated Level.',
                                                'Robust standard errors clustered in parentheses.')
)

table1 <- msum |> 
  gt::tab_spanner(
    label = 'Ukraine War',
    columns = c("2.1", "2.2")) |> 
  gt::tab_spanner(
    label = 'Fukushima Nuclear Catastrophe',
    columns = c("2.3", "2.4")) |> 
  gt::tab_spanner(
    label = 'Pooled',
    columns = c("2.5", "2.6"))

gtExtras::gtsave_extra(table1, "table_c_path_pois_distance.png", path = "tables", zoom = 10, expand = 5)

##rename and resorts variables
cm <- c("n" = "Number of Media Articles",
        "treatment" = "Exogenous Shock", 
        "post_treatment" = "Post Treatment Period",
        "government" = "Government",
        "pol_position_diff_party2" = "Ideological Distance",
        "n:pol_position_diff_party2" = "Number of Media Articles x Ideological Distance",
        "n_lag1" = "Number of Media Articles (t-1)",
        "lag_n_speeches" = "Number of Speeches (t-1)")

modelsummary::modelsummary(models2_bin)
# Table 1 agg - Ideological Distances
msum <- modelsummary::modelsummary(models2_bin,
                                   output = "gt",
                                   stars = c("*"=0.05, "**"=0.01, "***"=0.001),
                                   coef_map = cm,
                                   gof_map = gm,
                                   fmt = scaleFUN,
                                   title = "",
                                   notes = list('B Path (H4): Aggregated Level, Binarized.',
                                                'Robust standard errors clustered in parentheses.')
)

table1 <- msum |> 
  gt::tab_spanner(
    label = 'Ukraine War',
    columns = c("2.1", "2.2")) |> 
  gt::tab_spanner(
    label = 'Fukushima Nuclear Catastrophe',
    columns = c("2.3", "2.4")) |> 
  gt::tab_spanner(
    label = 'Pooled',
    columns = c("2.5", "2.6"))

gtExtras::gtsave_extra(table1, "table_b_path_pois_distance_bin.png", path = "tables", zoom = 10, expand = 5)

#- Moderated Mediation Analysis-------------------------------------------------

mediation1 <- list()
data_war_agg$party_leader_2 <- as.factor(data_war_agg$party_leader_2)
data_gau_agg$party_leader_2 <- as.factor(data_gau_agg$party_leader_2)
data_pool_agg$party_leader_2 <- as.factor(data_pool_agg$party_leader_2)

indicators <- c("direct",
                "indirect_1",
                "indirect_2",
                "prop_indirect_1",
                "prop_indirect_2")

##- War Models -----------------------------------------------------------------

# 1.1
results <- boot(data = data_war_agg, statistic = mediation_function_partyleader, R = 1000, parallel = "multicore")
results_boot_1.1 <- as.data.frame(summary(results))  |>  mutate(index = rownames(.)) |> 
  bind_cols(compute_all_cis(results))

results_boot_1.1$indicator <- indicators



# 1.3
results <- boot(data = data_gau_agg, statistic = mediation_function_partyleader, R = 1000, parallel = "multicore")
results_boot_1.3 <- as.data.frame(summary(results))  |>  mutate(index = rownames(.)) |> 
  bind_cols(compute_all_cis(results))

results_boot_1.3$indicator <- indicators

# 1.5
results <- boot(data = data_pool_agg, statistic = mediation_function_partyleader, R = 1000, parallel = "multicore")
results_boot_1.5 <- as.data.frame(summary(results))  |>  mutate(index = rownames(.)) |> 
  bind_cols(compute_all_cis(results))

results_boot_1.5$indicator <-  indicators


# 1.2
results <- boot(data = data_war_agg, statistic = mediation_function_partyleader_2WFE, R = 1000, parallel = "multicore")
results_boot_1.2 <- as.data.frame(summary(results)) |>  mutate(index = rownames(.)) |> 
  bind_cols(compute_all_cis(results))

results_boot_1.2$indicator <- indicators


mediation1 <- list()

# 1.1
mediation1[["1.1"]] <- bootstrap_to_modelsummary(results_boot_1.1, UFE = 1)
class(mediation1[["1.1"]]) <- "modelsummary_list"

# 1.2
mediation1[["1.2"]] <- bootstrap_to_modelsummary(results_boot_1.2, UFE = 1, TFE = 1)
class(mediation1[["1.2"]]) <- "modelsummary_list"

# 1.3
mediation1[["1.3"]] <- bootstrap_to_modelsummary(results_boot_1.3, UFE = 1)
class(mediation1[["1.3"]]) <- "modelsummary_list"

# 1.5
mediation1[["1.5"]] <- bootstrap_to_modelsummary(results_boot_1.5, UFE = 1)
class(mediation1[["1.5"]]) <- "modelsummary_list"

###- Output --------------------------------------------------------------------

##rename and resorts variables
cm <- c("direct" = "Direct Path",
        "indirect_1" = "Indirect Path (Backbencher)",
        "indirect_2" = "Indirect Path (Party Leader)",
        "prop_indirect_1" = "Proportion Mediation (Backbencher)",
        "prop_indirect_2" = "Proportion Mediation (Party Leader)")

modelsummary::modelplot(mediation1)

# Modelplot
modelsummary::modelplot(mediation1,
                        coef_map = cm,
                        coef_omit = "prop") +
                        xlab("Rate Ratios and 95% confidence intervals") +
                        geom_vline(xintercept = 1, color = "grey", linetype = 2)

ggsave(filename="Mediation_Analysis_Estimates_party_leader.png", path = "graph", 
       width = 8, height = 5, dpi = 900)

results_boot_1.1 |>
  mutate(id = "1.1") |>
  bind_rows(results_boot_1.2 |>  mutate(id = "1.2")) |> 
  bind_rows(results_boot_1.3 |>  mutate(id = "1.3")) |> 
  bind_rows(results_boot_1.5 |>  mutate(id = "1.5")) |> 
  filter(str_detect(indicator , "prop")) |> 
  mutate(index = paste0(id, indicator)) |>
ggplot() +
  geom_col(aes(x = indicator, y = original, fill = id), position = "dodge" ,width = .4) +
  geom_hline(yintercept = 0, color = "grey", linetype = 2) +
  scale_x_discrete(labels = c("prop_indirect_1" = "Backbencher", 
                              "prop_indirect_2" = "Party Leader")) + 
  theme_minimal() +
  labs(fill = "", x = "", y = "Proportion of the Indirect Path\nin Percentage Points") +
  coord_flip()

ggsave(filename="Mediation_Analysis_Props_party_leader.png", path = "graph", 
       width = 8, height = 5, dpi = 900)

##- Ideological Differences Models ---------------------------------------------

# 2.1
results <- boot(data = data_war_agg_polpos, statistic = mediation_function_poldiff, R = 1000, parallel = "multicore")
results_boot_2.1 <- summary(results)
results_boot_2.1$indicator <- names(results$t0)
results_boot_2.1 <- results_boot_2.1 |> bind_cols(compute_all_cis(results))

# 2.3
results <- boot(data = data_gau_agg_polpos, statistic = mediation_function_poldiff, R = 1000, parallel = "multicore")
results_boot_2.3 <- summary(results)
results_boot_2.3$indicator <- names(results$t0)
results_boot_2.3 <- results_boot_2.3 |> bind_cols(compute_all_cis(results))

# 2.5
results <- boot(data = data_pool_agg_polpos, statistic = mediation_function_poldiff, R = 1000, parallel = "multicore")
results_boot_2.5 <- summary(results)
results_boot_2.5$indicator <- names(results$t0)
results_boot_2.5 <- results_boot_2.5 |> bind_cols(compute_all_cis(results))

# 2.2
results <- boot(data = data_war_agg_polpos, statistic = mediation_function_poldiff_2WEF, R = 1000, parallel = "multicore")
results_boot_2.2 <- summary(results)
results_boot_2.2$indicator <- names(results$t0)
results_boot_2.2 <- results_boot_2.2 |> bind_cols(compute_all_cis(results))


mediation1 <- list()

# 2.1
mediation1[["2.1"]] <- bootstrap_to_modelsummary(results_boot_2.1, UFE = 1)
class(mediation1[["2.1"]]) <- "modelsummary_list"

# 2.2
mediation1[["2.2"]] <- bootstrap_to_modelsummary(results_boot_2.2, UFE = 1, TFE = 1)
class(mediation1[["2.2"]]) <- "modelsummary_list"

# 2.3
mediation1[["2.3"]] <- bootstrap_to_modelsummary(results_boot_2.3, UFE = 1)
class(mediation1[["2.3"]]) <- "modelsummary_list"


# 2.5
mediation1[["2.5"]] <- bootstrap_to_modelsummary(results_boot_2.5, UFE = 1)
class(mediation1[["2.5"]]) <- "modelsummary_list"

###- Output --------------------------------------------------------------------

##rename and resorts variables
cm <- c("direct" = "Direct Path",
        "indirect_level_0" = "Indirect Path (Ideological Difference = 0)",
        "indirect_level_1" = "Indirect Path (Ideological Difference = 1)",
        "indirect_level_2" = "Indirect Path (Ideological Difference = 2)",
        "indirect_level_3" = "Indirect Path (Ideological Difference = 3)",
        "indirect_level_4" = "Indirect Path (Ideological Difference = 4)",
        "indirect_level_5" = "Indirect Path (Ideological Difference = 5)",
        "indirect_level_6" = "Indirect Path (Ideological Difference = 6)",
        "indirect_level_7" = "Indirect Path (Ideological Difference = 7)",
        "indirect_level_8" = "Indirect Path (Ideological Difference = 8)",
        "indirect_level_9" = "Indirect Path (Ideological Difference = 9)",
        "prop_indirect_level_0" = "Proportion Mediation (Ideological Difference = 0)",
        "prop_indirect_level_1" = "Proportion Mediation (Ideological Difference = 1)",
        "prop_indirect_level_2" = "Proportion Mediation (Ideological Difference = 2)",
        "prop_indirect_level_3" = "Proportion Mediation (Ideological Difference = 3)",
        "prop_indirect_level_4" = "Proportion Mediation (Ideological Difference = 4)",
        "prop_indirect_level_5" = "Proportion Mediation (Ideological Difference = 5)",
        "prop_indirect_level_6" = "Proportion Mediation (Ideological Difference = 6)",
        "prop_indirect_level_7" = "Proportion Mediation (Ideological Difference = 7)",
        "prop_indirect_level_8" = "Proportion Mediation (Ideological Difference = 8)",
        "prop_indirect_level_9" = "Proportion Mediation (Ideological Difference = 9)")


modelsummary::modelplot(mediation1)

modelsummary::modelplot(mediation1,
                 coef_map = cm,
                 coef_omit = "prop") +
  xlab("Rate Ratios and 95% confidence intervals") +
  geom_vline(xintercept = 1, color = "grey", linetype = 2)

ggsave(filename="Mediation_Analysis_Estimates_diff.png", path = "graph",
       width = 10, height = 6, dpi = 900)

results_boot_2.1 |>
  mutate(id = "2.1") |>
  bind_rows(results_boot_2.2 |>  mutate(id = "2.2")) |> 
  bind_rows(results_boot_2.3 |>  mutate(id = "2.3")) |> 
  bind_rows(results_boot_2.5 |>  mutate(id = "2.5")) |> 
  filter(str_detect(indicator , "prop")) |> 
  mutate(index = paste0(id, indicator)) |>
  ggplot() +
  geom_col(aes(x = indicator, y = original, fill = id), position = "dodge" ,width = .4) +
  geom_hline(yintercept = 0, color = "grey", linetype = 2) +
  scale_x_discrete(labels = c(        "prop_indirect_level_0" = "Proportion Mediation (Ideological Difference = 0)",
                                      "prop_indirect_level_1" = "Proportion Mediation (Ideological Difference = 1)",
                                      "prop_indirect_level_2" = "Proportion Mediation (Ideological Difference = 2)",
                                      "prop_indirect_level_3" = "Proportion Mediation (Ideological Difference = 3)",
                                      "prop_indirect_level_4" = "Proportion Mediation (Ideological Difference = 4)",
                                      "prop_indirect_level_5" = "Proportion Mediation (Ideological Difference = 5)",
                                      "prop_indirect_level_6" = "Proportion Mediation (Ideological Difference = 6)",
                                      "prop_indirect_level_7" = "Proportion Mediation (Ideological Difference = 7)",
                                      "prop_indirect_level_8" = "Proportion Mediation (Ideological Difference = 8)",
                                      "prop_indirect_level_9" = "Proportion Mediation (Ideological Difference = 9)")) + 
  theme_minimal() +
  labs(fill = "", x = "", y = "Proportion of the Indirect Path\nin Percentage Points") +
  coord_flip()

ggsave(filename="Mediation_Analysis_Props_difference.png", path = "graph",
       width = 8, height = 5, dpi = 900)

##- Ideological Differences Models Bin -----------------------------------------

# 3.1
results <- boot(data = data_war_bin_polpos, statistic = mediation_function_poldiff, R = 1000, parallel = "multicore")
results_boot_3.1 <- summary(results)
results_boot_3.1$indicator <- names(results$t0)
results_boot_3.1 <- results_boot_3.1 |> bind_cols(compute_all_cis(results))

# 3.3
results <- boot(data = data_gau_bin_polpos, statistic = mediation_function_poldiff, R = 1000, parallel = "multicore")
results_boot_3.3 <- summary(results)
results_boot_3.3$indicator <- names(results$t0)
results_boot_3.3 <- results_boot_3.3 |> bind_cols(compute_all_cis(results))

# 3.5
results <- boot(data = data_pool_bin_polpos, statistic = mediation_function_poldiff, R = 1000, parallel = "multicore")
results_boot_3.5 <- summary(results)
results_boot_3.5$indicator <- names(results$t0)
results_boot_3.5 <- results_boot_3.5 |> bind_cols(compute_all_cis(results))

# 3.2
results <- boot(data = data_war_bin_polpos, statistic = mediation_function_poldiff_2WEF, R = 1000, parallel = "multicore")
results_boot_3.2 <- summary(results)
results_boot_3.2$indicator <- names(results$t0)
results_boot_3.2 <- results_boot_3.2 |> bind_cols(compute_all_cis(results))

# 3.4
results <- boot(data = data_gau_bin_polpos, statistic = mediation_function_poldiff_2WEF, R = 1000, parallel = "multicore")
results_boot_3.4 <- summary(results)
results_boot_3.4$indicator <- names(results$t0)
results_boot_3.4 <- results_boot_3.4 |> bind_cols(compute_all_cis(results))

mediation1 <- list()

# 3.1
mediation1[["3.1"]] <- bootstrap_to_modelsummary(results_boot_3.1, UFE = 1)
class(mediation1[["3.1"]]) <- "modelsummary_list"

# 3.2
mediation1[["3.2"]] <- bootstrap_to_modelsummary(results_boot_3.2, UFE = 1, TFE = 1)
class(mediation1[["3.2"]]) <- "modelsummary_list"

# 3.3
mediation1[["3.3"]] <- bootstrap_to_modelsummary(results_boot_3.3, UFE = 1)
class(mediation1[["3.3"]]) <- "modelsummary_list"


# 3.4
# mediation1[["3.4"]] <- bootstrap_to_modelsummary(results_boot_3.4, UFE = 1, TFE = 1)
# class(mediation1[["3.4"]]) <- "modelsummary_list"


# 3.5
mediation1[["3.5"]] <- bootstrap_to_modelsummary(results_boot_3.5, UFE = 1)
class(mediation1[["3.5"]]) <- "modelsummary_list"

###- Output --------------------------------------------------------------------
##rename and resorts variables
cm <- c("direct" = "Direct Path",
        "indirect_level_1" = "Indirect Path (Ideological Difference = 0)",
        "indirect_level_2" = "Indirect Path (Ideological Difference ≥ 1)")

# Modelplot
modelsummary::modelplot(mediation1)
modelsummary::modelplot(mediation1,
                        coef_map = cm,
                        coef_omit = "prop") +
                    xlab("Rate Ratios and 95% confidence intervals") +
                    geom_vline(xintercept = 1, color = "grey", linetype = 2)

ggsave(filename="Mediation_Analysis_Estimates_difference_bin.png", path = "graph", 
       width = 8, height = 5, dpi = 900)

results_boot_3.1 |>
  mutate(id = "3.1") |>
  bind_rows(results_boot_3.2 |>  mutate(id = "3.2")) |> 
  bind_rows(results_boot_3.3 |>  mutate(id = "3.3")) |> 
  bind_rows(results_boot_3.5 |>  mutate(id = "3.5")) |> 
  filter(str_detect(indicator , "prop")) |> 
  mutate(index = paste0(id, indicator)) |>
  ggplot() +
  geom_col(aes(x = indicator, y = original, fill = id), position = "dodge" ,width = .4) +
  geom_hline(yintercept = 0, color = "grey", linetype = 2) +
  scale_x_discrete(labels = c("prop_indirect_level_1" = "Proportion Mediation (Ideological Difference = 0)",
                               "prop_indirect_level_2" = "Proportion Mediation (Ideological Difference ≥ 1)")) + 
  theme_minimal() +
  labs(fill = "", x = "", y = "Proportion of the Indirect Path\nin Percentage Points") +
  coord_flip()

ggsave(filename="Mediation_Analysis_Props_bin.png", 
       path = "graph", width = 8, height = 5, dpi = 900)

#- Statistical Tests -----------------------------------------------------------

objects_results_boot <- grep("^results_boot", ls(), value = TRUE)

for(i in objects_results_boot){
  print(i)
  data_object <- get(i)
  result <- check_overlap_indirect(data_object)
  print(paste0("Overlap: ", result))
  result <- test_for_significance(data_object)
}



