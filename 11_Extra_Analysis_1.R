rm(list = ls())
set.seed(1234)
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


#- Data Wrangling---------------------------------------------------------------

data_war <- data_war |>
  rename(treated_media = treated_media_war,
         treated_topic_lag1 = treated_topic_war_lag1,
         treated_topic = treated_topic_war,
         post_treatment = post_treatment_war,
         treatment = treatment_war) |>
  mutate(party_leader_1 = as.factor(party_leader_1),
         party_leader_2 = as.factor(party_leader_2),
         party_leader_3 = as.factor(party_leader_3))

data_gau <- data_gau |>
  rename(treated_media = treated_media_gau,
         treated_topic_lag1 = treated_topic_gau_lag1,
         treated_topic = treated_topic_gau,
         post_treatment = post_treatment_gau,
         treatment = treatment_gau) |>
  mutate(party_leader_1 = as.factor(party_leader_1),
         party_leader_2 = as.factor(party_leader_2),
         party_leader_3 = as.factor(party_leader_3))

data_pooled <- data_war |>
  bind_rows(data_gau)

##- Media Articles -------------------------------------------------------------

media_articles_war <- media_articles_war |>
  rename(treated_topic = treated_topic_war,
         treatment = treatment_war) |>
  mutate(post_treatment = ifelse(date >= as.Date("2022-02-24"),1,0)) |>
  mutate(spiegel = ifelse(newspaper == "spiegel", 1,0),
         welt = ifelse(newspaper == "welt", 1,0),
         zeit = ifelse(newspaper == "zeit", 1,0),
         taz = ifelse(newspaper == "taz", 1,0),
         sz = ifelse(newspaper == "sz", 1,0)) |>
  mutate(newspaper = as.factor(newspaper))

media_articles_gau <- media_articles_gau |>
  rename(treated_topic = treated_topic_gau,
         treatment = treatment_gau) |>
  mutate(post_treatment = ifelse(date >= as.Date("2011-03-11"),1,0)) |>
  mutate(spiegel = ifelse(newspaper == "spiegel", 1,0),
         welt = ifelse(newspaper == "welt", 1,0),
         zeit = ifelse(newspaper == "zeit", 1,0),
         taz = ifelse(newspaper == "taz", 1,0),
         sz = ifelse(newspaper == "sz", 1,0)) |>
  mutate(newspaper = as.factor(newspaper))

media_articles_pooled <- media_articles_war |>
  bind_rows(media_articles_gau)

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

#- Rename Variables ------------------------------------------------------------

##rename and resorts variables
cm <- c("n" = "Number of Media Articles",
        "n_lag1" = "Number of Media Articles (t-1)",
        "lag_n_speeches" = "Number of Speeches (t-1)",
        "treatment" = "Exogenous Shock",
        "government" = "Government",
        "party_leader_3" = "Party Leader (3)",
        "post_treatment" = "Post Treatment Period",
        "newspapersz" = "SZ (ref. Spiegel)",
        "newspapertaz" = "Taz (ref. Spiegel)",
        "newspaperwelt" = "Welt (ref. Spiegel)",
        "newspaperzeit" = "Zeit (ref. Spiegel)",
        "post_treatment 1 - 0" = "Post Treatment Period",
        "newspaper welt - spiegel" = "Welt (ref. Spiegel)",
        "newspaper zeit - spiegel" = "Zeit (ref. Spiegel)",
        "newspaper sz - spiegel" = "SZ (ref. Spiegel)",
        "newspaper taz - spiegel" = "Taz (ref. Spiegel)",
        "treated_media" = "Media Salience (Treated)",
        "treated_topic_lag1" = "Topic of Legislative Speech (t-1)",
        "party_leader_31 1 - 0" = "Party Leader (3)",
        "treated_media dY/dX" = "Media Salience (Treated)",
        "treated_topic_lag1 1 - 0" = "Topic of Legislative Speech (t-1)",
        "treatment 1 - 0" = "Exogenous Shock",
        "party_leader_3 1 - 0" = "Party Leader (3)",
        "party_leader_31" = "Party Leader (3)",
        "pol_position_diff_party" = "Ideological Distance",
        "pol_position_diff_party 1 - 0" = "Ideological Distance (1, ref. = 0)",
        "pol_position_diff_party 2 - 0" = "Ideological Distance (2, ref. = 0)",
        "pol_position_diff_party 3 - 0" = "Ideological Distance (3, ref. = 0)",
        "pol_position_diff_party 4 - 0" = "Ideological Distance (4, ref. = 0)",
        "pol_position_diff_party 5 - 0" = "Ideological Distance (5, ref. = 0)",
        "pol_position_diff_party 6 - 0" = "Ideological Distance (6, ref. = 0)",
        "pol_position_diff_party 7 - 0" = "Ideological Distance (7, ref. = 0)",
        "pol_position_diff_party 8 - 0" = "Ideological Distance (8, ref. = 0)",
        "pol_position_diff_party 9 - 0" = "Ideological Distance (9, ref. = 0)",
        
        "treatment mean(1) - mean(0)" = "Exogenous Shock",
        "newspaper mean(sz) - mean(spiegel)" = "SZ (ref. = Spiegel)",
        "newspaper mean(taz) - mean(spiegel)" = "Taz (ref. = Spiegel)",
        "newspaper mean(welt) - mean(spiegel)" = "Welt (ref. = Spiegel)",
        "newspaper mean(zeit) - mean(spiegel)" = "Zeit (ref. = Spiegel)",

        "pol_position_diff_party_bin" = "Ideological Distance",
        "pol_position_diff_party1" = "Ideological Distance (1)",
        "pol_position_diff_party2" = "Ideological Distance (2)",
        "pol_position_diff_party3" = "Ideological Distance (3)",
        "pol_position_diff_party4" = "Ideological Distance (4)",
        "pol_position_diff_party5" = "Ideological Distance (5)",
        "pol_position_diff_party6" = "Ideological Distance (6)",
        "pol_position_diff_party7" = "Ideological Distance (7)",
        "pol_position_diff_party8" = "Ideological Distance (8)",
        "pol_position_diff_party9" = "Ideological Distance (9)",
    
        "n:party_leader_3" = "Number of Media Articles x Party Leader (3)",
        "post_treatment:newspapersz" = "Post Treatment Period x SZ",
        "post_treatment:newspapertaz" = "Post Treatment Period x Taz",
        "post_treatment:newspaperwelt" = "Post Treatment Period x Welt",
        "post_treatment:newspaperzeit" = "Post Treatment Period x Zeit",
        "treated_media:party_leader_31" = "Media Salience x Party Leader (3)",
        "treated_media:pol_position_diff_party" = "Media Salience x Ideological Distance",
        "n:pol_position_diff_party1" = "Number of Media Articles x Ideological Distance (1)",
        "n:pol_position_diff_party2" = "Number of Media Articles x Ideological Distance (2)",
        "n:pol_position_diff_party3" = "Number of Media Articles x Ideological Distance (3)",
        "n:pol_position_diff_party4" = "Number of Media Articles x Ideological Distance (4)",
        "n:pol_position_diff_party5" = "Number of Media Articles x Ideological Distance (5)",
        "n:pol_position_diff_party6" = "Number of Media Articles x Ideological Distance (6)",
        "n:pol_position_diff_party7" = "Number of Media Articles x Ideological Distance (7)",
        "n:pol_position_diff_party8" = "Number of Media Articles x Ideological Distance (8)",
        "n:pol_position_diff_party9" = "Number of Media Articles x Ideological Distance (9)",
        "treated_media:pol_position_diff_party1" = "Media Salience x Ideological Distance (1)",
        "treated_media:pol_position_diff_party2" = "Media Salience x Ideological Distance (2)",
        "treated_media:pol_position_diff_party3" = "Media Salience x Ideological Distance (3)",
        "treated_media:pol_position_diff_party4" = "Media Salience x Ideological Distance (4)",
        "treated_media:pol_position_diff_party5" = "Media Salience x Ideological Distance (5)",
        "treated_media:pol_position_diff_party6" = "Media Salience x Ideological Distance (6)",
        "treated_media:pol_position_diff_party7" = "Media Salience x Ideological Distance (7)",
        "treated_media:pol_position_diff_party9" = "Media Salience x Ideological Distance (9)",
        "treatment:party_leader_31" = "Exogenous Shock x Party Leader (3)",
        "treatment:newspapersz" = "Exogenous Shock x SZ",
        "treatment:newspapertaz" = "Exogenous Shock x Taz",
        "treatment:newspaperwelt" = "Exogenous Shock x Welt",
        "treatment:newspaperzeit" = "Exogenous Shock x Zeit",
        "treatment:pol_position_diff_party" = "Exogenous Shock x Ideological Distance",
        "n:party_leader_31" = "Number of Media Articles x Party Leader (3)",
    
        "(Intercept)" = "Intercept",
        "date" = "Date",
        "session_id" = "Parliamentary Session",
        "politician_id" = "Politician ID",
        "classification" = "Topic (CAP)"
        )

gm <- list(
  list("raw" = "nobs", "clean" = "Sample size", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "R2", "fmt" = 2),
  list("raw" = "adj.r.squared", "clean" = "Adjusted R2", "fmt" = 2),
  list("raw" = "logLik", "clean" = "Log Likelihood", "fmt" = 2),
  list("raw" = "FE: date", "clean" = "Time fixed effects", "fmt" = 0),
  list("raw" = "FE..date", "clean" = "Time fixed effects", "fmt" = 0),
  list("raw" = "FE: classification", "clean" = "Topic fixed effects", "fmt" = 0),
  list("raw" = "FE..classification", "clean" = "Topic fixed effects", "fmt" = 0),
  list("raw" = "FE: politician_id", "clean" = "Unit fixed effects", "fmt" = 0),
  list("raw" = "FE..politician_id", "clean" = "Unit fixed effects", "fmt" = 0),
  list("raw" = "FE: session_id", "clean" = "Time fixed effects", "fmt" = 0),
  list("raw" = "FE..session_id", "clean" = "Time fixed effects", "fmt" = 0),
  list("raw" = "Time_fixed_effects", "clean" = "Time fixed effects", "fmt" = 0),
  list("raw" = "Unit_fixed_effects", "clean" = "Topic fixed effects", "fmt" = 0),
  list("raw" = "Iterations", "clean" = "Bootstrap Iterations", "fmt" = 0)
  )

#- Path Analysis ---------------------------------------------------------------

##- Analysis, b path (H1)--------------------------------------------------------

# FE Models
models1 <- list()
models1[['1.1']] <- fixest::feglm(treated_topic ~ treated_media*party_leader_3 + treated_topic_lag1|politician_id, data = data_war, family = binomial(link = "logit"))
models1[['1.2']] <- fixest::feglm(treated_topic ~ treated_media*party_leader_3 + treated_topic_lag1  + treatment |politician_id,  data = data_war, family = binomial(link = "logit"))
models1[['1.3']] <- fixest::feglm(treated_topic ~ treated_media*party_leader_3 + treated_topic_lag1| politician_id, data = data_gau, family = binomial(link = "logit"))
models1[['1.4']] <- fixest::feglm(treated_topic ~ treated_media*party_leader_3 + treated_topic_lag1 + treatment | politician_id, data = data_gau, family = binomial(link = "logit"))
models1[['1.5']] <- fixest::feglm(treated_topic ~ treated_media*party_leader_3 + treated_topic_lag1| politician_id, data = data_pooled, family = binomial(link = "logit"))
models1[['1.6']] <- fixest::feglm(treated_topic ~ treated_media*party_leader_3 + treated_topic_lag1 + treatment | politician_id, data = data_pooled, family = binomial(link = "logit"))

modelsummary::modelsummary(models1)
# table 3 (Appendix)
msum <- modelsummary::modelsummary(models1,
                                   output = "gt",
                                   stars = c("*"=0.05, "**"=0.01, "***"=0.001),
                                   coef_map = cm,
                                   gof_map = gm,
                                   fmt = scaleFUN,
                                   title = "",
                                   notes = list('B Path (H1): Effect of media reporting on speech behavior. Reported are log odds of a logit model with fixed effects.',
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

gtExtras::gtsave_extra(table1, "table_h1_b_path_lo_partyleader2.png", path = "tables", zoom = 10, expand = 5)


###- AME -----------------------------------------------------------------------

models2 <- list()
models2[['1.1']] <- marginaleffects::avg_slopes(models1[['1.1']])
models2[['1.2']] <- marginaleffects::avg_slopes(models1[['1.2']])
models2[['1.3']] <- marginaleffects::avg_slopes(models1[['1.3']])
models2[['1.4']] <- marginaleffects::avg_slopes(models1[['1.4']])
models2[['1.5']] <- marginaleffects::avg_slopes(models1[['1.5']])
models2[['1.6']] <- marginaleffects::avg_slopes(models1[['1.6']])

modelsummary::modelsummary(models2, stars = T, shape = term : contrast ~ model)

# table 2
msum <- modelsummary::modelsummary(models2,
                                   output = "gt",
                                   stars = c("*"=0.05, "**"=0.01, "***"=0.001),
                                   coef_map = cm,
                                   gof_map = gm,
                                   fmt = scaleFUN,
                                   title = "",
                                   notes = list('B Path (H1): Effect of media reporting on speech behavior. Reported are Average Marginal Effects (AME) of a logit model with fixed effects.',
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

gtExtras::gtsave_extra(table1, "table_h1_b_path_ame_partyleader2.png", path = "tables", zoom = 10, expand = 5)


##- Analysis, c path H1 --------------------------------------------------------
models1 <- list()
models1[['1.1']]  <- fixest::feglm(treated_topic ~ treatment*party_leader_3 | politician_id + session_id, data = data_war, family = binomial(link = "logit"))
models1[['1.2']]  <- fixest::feglm(treated_topic ~ treatment*party_leader_3 + treated_topic_lag1 | politician_id + session_id, data = data_war, family = binomial(link = "logit"))

models1[['1.3']]  <- fixest::feglm(treated_topic ~ treatment*party_leader_3 | politician_id + session_id, data = data_gau, family = binomial(link = "logit"))
models1[['1.4']]  <- fixest::feglm(treated_topic ~ treatment*party_leader_3 + treated_topic_lag1 | politician_id + session_id, data = data_gau, family = binomial(link = "logit"))

models1[['1.5']]  <- fixest::feglm(treated_topic ~ treatment*party_leader_3 | politician_id + session_id, data = data_gau, family = binomial(link = "logit"))
models1[['1.6']]  <- fixest::feglm(treated_topic ~ treatment*party_leader_3 + treated_topic_lag1 | politician_id + session_id, data = data_gau, family = binomial(link = "logit"))

modelsummary::modelsummary(models1)

msum <- modelsummary::modelsummary(models1,
                                   output = "gt",
                                   stars = c("*"=0.05, "**"=0.01, "***"=0.001),
                                   coef_map = cm,
                                   gof_map = gm,
                                   fmt = scaleFUN,
                                   title = "",
                                   notes = list('C Path (H1): Effect of treatment on speech behavior. Reported are log odds of a logit model with fixed effects.',
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

gtExtras::gtsave_extra(table1, "table_h1_c_path_lo_partyleader2.png", path = "tables", zoom = 10, expand = 5)

#- AME -------------------------------------------------------------------------

models2 <- list()
models2[['1.1']] <- marginaleffects::avg_slopes(models1[['1.1']])
models2[['1.2']] <- marginaleffects::avg_slopes(models1[['1.2']])
models2[['1.3']] <- marginaleffects::avg_slopes(models1[['1.3']])
models2[['1.4']] <- marginaleffects::avg_slopes(models1[['1.4']])
models2[['1.5']] <- marginaleffects::avg_slopes(models1[['1.5']])
models2[['1.6']] <- marginaleffects::avg_slopes(models1[['1.6']])

modelsummary::modelsummary(models2)

# table 2
msum <- modelsummary::modelsummary(models2,
                                   output = "gt",
                                   stars = c("*"=0.05, "**"=0.01, "***"=0.001),
                                   coef_map = cm,
                                   gof_map = gm,
                                   fmt = scaleFUN,
                                   title = "",
                                   notes = list('C Path (H1): ffect of treatment on speech behavior. Reported are Average Marginal Effects (AME) of a logit model with fixed effects.',
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

gtExtras::gtsave_extra(table1, "table_h1_c_path_ame_partyleader2.png", path = "tables", zoom = 10, expand = 5)

# Data for Bootstrapping -------------------------------------------------------
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
                    party_leader_3 = unique(data_war$party_leader_3),
                    classification = unique(data_war$classification),
                    government = unique(data_war$government))

data_war_agg <- data_war |>
  group_by(
    date, party_leader_3, classification, government
  ) |>
    summarise(n_speeches = n()) |>
  ungroup() |>
  right_join(grid, by = c("date", "party_leader_3", "classification", "government")) |>
  mutate(n_speeches = ifelse(is.na(n_speeches), 0, n_speeches)) |>
  left_join(media_war, by = c("date", "classification")) |>
  group_by(classification, party_leader_3, government) |>
  arrange(date) |>
  mutate(lag_n_speeches = lag(n_speeches)) |>
  ungroup()

grid <- expand.grid(date = unique(data_gau$date),
                    party_leader_3 = unique(data_gau$party_leader_3),
                    classification = unique(data_gau$classification),
                    government = unique(data_war$government))

data_gau_agg <- data_gau |>
  group_by(
    date, party_leader_3, classification, government
  ) |>
  summarise(n_speeches = n()) |>
  ungroup() |>
  right_join(grid, by = c("date", "party_leader_3", "classification", "government")) |>
  mutate(n_speeches = ifelse(is.na(n_speeches), 0, n_speeches)) |>
  left_join(media_gau, by = c("date", "classification")) |>
  group_by(classification, party_leader_3, government) |>
  arrange(date) |>
  mutate(lag_n_speeches = lag(n_speeches)) |>
  ungroup()

data_pool_agg <- data_gau_agg |>
  bind_rows(data_war_agg)

#- Analysis B Path -------------------------------------------------------------

# B Path
models2 <- list()
models2[['1.1']] <- fixest::fepois(n_speeches ~ treatment + n*party_leader_3 + government + lag_n_speeches | classification, data = data_war_agg)
models2[['1.2']] <- fixest::fepois(n_speeches ~ treatment + n*party_leader_3 + government + lag_n_speeches | classification + date, data = data_war_agg)

models2[['1.3']] <- fixest::fepois(n_speeches ~ treatment + n*party_leader_3 + government + lag_n_speeches | classification, data = data_gau_agg)
models2[['1.4']] <- fixest::fepois(n_speeches ~ treatment + n*party_leader_3 + government + lag_n_speeches | classification + date, data = data_gau_agg)

models2[['1.5']] <- fixest::fepois(n_speeches ~ treatment + n*party_leader_3 + government + lag_n_speeches | classification, data = data_pool_agg)
models2[['1.6']] <- fixest::fepois(n_speeches ~ treatment + n*party_leader_3 + government + lag_n_speeches | classification + date, data = data_pool_agg)

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

gtExtras::gtsave_extra(table1, "table_b_path_pois_party_leader2.png", path = "tables", zoom = 10, expand = 5)


#- Moderated Mediation Analysis ------------------------------------------------

mediation1 <- list()
data_war_agg$party_leader_3 <- as.factor(data_war_agg$party_leader_3)
data_gau_agg$party_leader_3 <- as.factor(data_gau_agg$party_leader_3)
data_pool_agg$party_leader_3 <- as.factor(data_pool_agg$party_leader_3)

indicators <- c("direct",
                "indirect_1",
                "indirect_2",
                "prop_indirect_1",
                "prop_indirect_2")

# 1.1
results <- boot(data = data_war_agg, statistic = mediation_function_partyleader, R = 1000, parallel = "multicore")
results_boot_1.1 <- as.data.frame(summary(results))  |> mutate(index = rownames(.)) |>
  bind_cols(compute_all_cis(results))

results_boot_1.1$indicator <- indicators


# 1.3
results <- boot(data = data_gau_agg, statistic = mediation_function_partyleader, R = 1000, parallel = "multicore")
results_boot_1.3 <- as.data.frame(summary(results))  |> mutate(index = rownames(.)) |>
  bind_cols(compute_all_cis(results))

results_boot_1.3$indicator <- indicators

# 1.5
results <- boot(data = data_pool_agg, statistic = mediation_function_partyleader, R = 1000, parallel = "multicore")
results_boot_1.5 <- as.data.frame(summary(results))  |> mutate(index = rownames(.)) |>
  bind_cols(compute_all_cis(results))

results_boot_1.5$indicator <-  indicators


# 1.2
results <- boot(data = data_war_agg, statistic = mediation_function_partyleader_2WFE, R = 1000, parallel = "multicore")
results_boot_1.2 <- as.data.frame(summary(results)) |> mutate(index = rownames(.)) |>
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


##- Output ---------------------------------------------------------------------


##rename and resorts variables
cm <- c("direct" = "Direct Path",
        "indirect_1" = "Indirect Path (Backbencher)",
        "indirect_2" = "Indirect Path (Party Leader)",
        "prop_indirect_1" = "Proportion Mediation (Backbencher)",
        "prop_indirect_2" = "Proportion Mediation (Party Leader)")

# Modelplot
modelsummary::modelplot(mediation1,
                        coef_map = cm,
                        coef_omit = "prop") +
                        xlab("Rate Ratios and 95% confidence intervals") +
                        geom_vline(xintercept = 1, color = "grey", linetype = 2)

ggsave(filename="Mediation_Analysis_Estimates_1_partyleader2.png", path = "graph", width = 10, height = 6)

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
 # geom_errorbar(aes(x = indicator, ymin = ci_lower, ymax = ci_higher, group = id), 
#                position = position_dodge(width = 0.4), width = 0.2, alpha = .5) +
  scale_x_discrete(labels = c("prop_indirect_1" = "Party Leader", 
                              "prop_indirect_2" = "Backbencher")) + 
  theme_minimal() +
  labs(fill = "", x = "", y = "Proportion of the Indirect Path\nin Percentage Points") +
  coord_flip()

ggsave(filename="Mediation_Analysis_Props_1_partyleader2.png", path = "graph", width = 10, height = 6)

