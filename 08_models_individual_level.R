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

#- Data Wrangling --------------------------------------------------------------

data_war <- data_war  |> 
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

# rename and resorts variables
cm <- c("n" = "Number of Media Articles",
        "n_lag1" = "Number of Media Articles (t-1)",
        "lag_n_speeches" = "Number of Speeches (t-1)",
        "treatment" = "Exogenous Shock",
        "government" = "Government",
        "party_leader_2" = "Party Leader",
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
        "party_leader_21 1 - 0" = "Party Leader",
        "treated_media dY/dX" = "Media Salience (Treated)",
        "treated_topic_lag1 1 - 0" = "Topic of Legislative Speech (t-1)",
        "treatment 1 - 0" = "Exogenous Shock",
        "party_leader_2 1 - 0" = "Party Leader",
        "party_leader_21" = "Party Leader",
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
        
        "n:party_leader_2" = "Number of Media Articles x Party Leader",
        "post_treatment:newspapersz" = "Post Treatment Period x SZ",
        "post_treatment:newspapertaz" = "Post Treatment Period x Taz",
        "post_treatment:newspaperwelt" = "Post Treatment Period x Welt",
        "post_treatment:newspaperzeit" = "Post Treatment Period x Zeit",
        "treated_media:party_leader_21" = "Media Salience x Party Leader",
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
        "treatment:party_leader_21" = "Exogenous Shock x Party Leader",
        "treatment:newspapersz" = "Exogenous Shock x SZ",
        "treatment:newspapertaz" = "Exogenous Shock x Taz",
        "treatment:newspaperwelt" = "Exogenous Shock x Welt",
        "treatment:newspaperzeit" = "Exogenous Shock x Zeit",
        "treatment:pol_position_diff_party" = "Exogenous Shock x Ideological Distance",
        "n:party_leader_21" = "Number of Media Articles x Party Leader",
        
        "(Intercept)" = "Intercept",
        "date" = "Date",
        "session_id" = "Parliamentary Session",
        "politician_id" = "Politician ID",
        "classification" = "Topic (CAP)",
        "party_leader_1" = "Party Leader (2)",
        "party_leader_3" = "Party Leader (3)"
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


#- Path Analysis  --------------------------------------------------------------

##- A Path Individual Level ----------------------------------------------------

# Treatment effect with respect to the outlet
# Logit Models
models1 <- list()
models2 <- list()

models1[['1.1']] <- glm(treated_topic ~ treatment ,  data = media_articles_war, family = binomial(link = "logit"))
models1[['1.2']] <- fixest::feglm(treated_topic ~ treatment*newspaper | date,  data = media_articles_war, family = binomial(link = "logit"))

models1[['1.3']] <- glm(treated_topic ~ treatment,  data = media_articles_gau, family = binomial(link = "logit"))
models1[['1.4']] <- fixest::feglm(treated_topic ~  treatment*newspaper  | date,  data = media_articles_gau, family = binomial(link = "logit"))

models1[['1.5']] <- glm(treated_topic ~ treatment,  data = media_articles_pooled, family = binomial(link = "logit"))
models1[['1.6']] <- fixest::feglm(treated_topic ~ treatment*newspaper  | date,  data = media_articles_pooled, family = binomial(link = "logit"))

# Average Marginal Effects (AME)
models2[['1.1']] <- marginaleffects::avg_slopes(models1[['1.1']])
models2[['1.2']] <- marginaleffects::avg_slopes(models1[['1.2']])
models2[['1.3']] <- marginaleffects::avg_slopes(models1[['1.3']])
models2[['1.4']] <- marginaleffects::avg_slopes(models1[['1.4']])
models2[['1.5']] <- marginaleffects::avg_slopes(models1[['1.5']])
models2[['1.6']] <- marginaleffects::avg_slopes(models1[['1.6']])

modelsummary::modelsummary(models1)

msum <- modelsummary::modelsummary(models1,
                                   output = "gt",
                                   stars = c("*"=0.05, "**"=0.01, "***"=0.001),
                                   coef_map = cm,
                                   gof_map = gm,
                                   fmt = scaleFUN,
                                   title = "",
                                   notes = list('A Path: Effect of treatment on media reporting (aggregated). Reported are log odds of a poisson-Models with fixed effects.',
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


gtExtras::gtsave_extra(table1, "table_ind_a_path_lo.png", path = "tables", zoom = 10, expand = 5)

modelsummary::modelsummary(models2,
                           shape = term : contrast ~ model)

msum <- modelsummary::modelsummary(models2,
                                   shape = term : contrast ~ model,
                                   output = "gt",
                                   stars = c("*"=0.05, "**"=0.01, "***"=0.001),
                                   coef_map = cm,
                                   gof_map = gm,
                                   fmt = scaleFUN,
                                   title = "",
                                   notes = list('A Path: Effect of treatment on media reporting (individual). Reported are average marginal effects (AME) of a Poisson-Models with fixed effects.',
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

gtExtras::gtsave_extra(table1, "table_ind_a_path_ame.png", path = "tables", zoom = 10, expand = 5)

##- Analysis, b path (H1) ------------------------------------------------------

# FE Models
models1 <- list()
models1[['1.1']] <- fixest::feglm(treated_topic ~ treated_media*party_leader_2 + treated_topic_lag1|politician_id, data = data_war, family = binomial(link = "logit"))
models1[['1.2']] <- fixest::feglm(treated_topic ~ treated_media*party_leader_2 + treated_topic_lag1  + treatment |politician_id,  data = data_war, family = binomial(link = "logit"))
models1[['1.3']] <- fixest::feglm(treated_topic ~ treated_media*party_leader_2 + treated_topic_lag1| politician_id, data = data_gau, family = binomial(link = "logit"))
models1[['1.4']] <- fixest::feglm(treated_topic ~ treated_media*party_leader_2 + treated_topic_lag1 + treatment | politician_id, data = data_gau, family = binomial(link = "logit"))
models1[['1.5']] <- fixest::feglm(treated_topic ~ treated_media*party_leader_2 + treated_topic_lag1| politician_id, data = data_pooled, family = binomial(link = "logit"))
models1[['1.6']] <- fixest::feglm(treated_topic ~ treated_media*party_leader_2 + treated_topic_lag1 + treatment | politician_id, data = data_pooled, family = binomial(link = "logit"))

modelsummary::modelsummary(models1)

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

gtExtras::gtsave_extra(table1, "table_h1_b_path_lo.png", path = "tables", zoom = 10, expand = 5)

###- Visualization Predicted Probs----------------------------------------------

# Bootstrapping
# model 1.6
model <- fixest::feglm(treated_topic ~ treated_media*party_leader_2 + treated_topic_lag1 + treatment | politician_id, data = data_pooled, family = binomial(link = "logit"))

boot_data <- data_pooled |> 
  filter(politician_id %in% names(fixef(model)[[1]])) |> 
  filter(!is.na(party_leader_2))

boot_results <- boot(data = boot_data, statistic = boot_function_party_leader, R = 1000, strata = boot_data$party_leader_2) 

results <- summary(boot_results) |> 
  bind_cols(compute_all_cis(boot_results)) |> 
  mutate(party_leader_2 = if_else(row_number() <= 50, 1, 0)) |> 
  group_by(party_leader_2) |> 
  mutate(treated_media = seq(0.06, 0.25, length.out = 50)) |> 
  ungroup()

# Plot
ggplot(results) +
  geom_line(aes(x = treated_media, y = original, color = as.factor(party_leader_2))) +
  geom_errorbar(aes(x = treated_media, y = original,
                    ymin = ci_lower, ymax = ci_higher, color = as.factor(party_leader_2)), alpha = .2) +
  labs(
    x = "Media Salience (H1)",
    y = "Predicted Probability and 95% Confidence Intervals",
    fill = "Hierarchical Position",
    color = "Hierarchical Position"
  ) +
  scale_fill_manual(labels = c("Party Leader", "Backbencher"),
                    values = c("#377eb8", "#e41a1c"))+
  scale_color_manual(labels = c("Party Leader", "Backbencher"),
                     values = c("#377eb8", "#e41a1c"))+
  theme_minimal()

ggsave(file = "graph/Pred_Probs_h1.png",
       width = 7, height = 4, dpi = 900)

###- AME -----------------------------------------------------------------------

models2 <- list()
models2[['1.1']] <- marginaleffects::avg_slopes(models1[['1.1']])
models2[['1.2']] <- marginaleffects::avg_slopes(models1[['1.2']])
models2[['1.3']] <- marginaleffects::avg_slopes(models1[['1.3']])
models2[['1.4']] <- marginaleffects::avg_slopes(models1[['1.4']])
models2[['1.5']] <- marginaleffects::avg_slopes(models1[['1.5']])
models2[['1.6']] <- marginaleffects::avg_slopes(models1[['1.6']])

modelsummary::modelsummary(models2, stars = T, shape = term : contrast ~ model)

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

gtExtras::gtsave_extra(table1, "table_h1_b_path_ame.png", path = "tables", zoom = 10, expand = 5)

##- Analysis, b path (H2) ------------------------------------------------------

# FE Models
models1 <- list()
models1[['2.1']] <- fixest::feglm(treated_topic ~ treated_media*pol_position_diff_party + treated_topic_lag1| politician_id, data = data_war, family = binomial(link = "logit"))
models1[['2.2']] <- fixest::feglm(treated_topic ~ treated_media*pol_position_diff_party + treated_topic_lag1  + treatment |politician_id,  data = data_war, family = binomial(link = "logit"))
models1[['2.3']] <- fixest::feglm(treated_topic ~ treated_media*pol_position_diff_party + treated_topic_lag1| politician_id, data = data_gau, family = binomial(link = "logit"))
models1[['2.4']] <- fixest::feglm(treated_topic ~ treated_media*pol_position_diff_party + treated_topic_lag1 + treatment | politician_id, data = data_gau, family = binomial(link = "logit"))
models1[['2.5']] <- fixest::feglm(treated_topic ~ treated_media*pol_position_diff_party + treated_topic_lag1| politician_id, data = data_pooled, family = binomial(link = "logit"))
models1[['2.6']] <- fixest::feglm(treated_topic ~ treated_media*pol_position_diff_party + treated_topic_lag1 + treatment | politician_id, data = data_pooled, family = binomial(link = "logit"))

modelsummary::modelsummary(models1)
msum <- modelsummary::modelsummary(models1,
                                   output = "gt",
                                   stars = c("*"=0.05, "**"=0.01, "***"=0.001),
                                   coef_map = cm,
                                   gof_map = gm,
                                   fmt = scaleFUN,
                                   title = "",
                                   notes = list('B Path (H2): Effect of media reporting on speech behavior. Reported are log odds of a logit model with fixed effects.',
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

gtExtras::gtsave_extra(table1, "table_h2_b_path_lo.png", path = "tables", zoom = 10, expand = 5)

## AME -------------------------------------------------------------------------

models2 <- list()
models2[['2.1']] <- marginaleffects::avg_slopes(models1[['2.1']])
models2[['2.2']] <- marginaleffects::avg_slopes(models1[['2.2']])
models2[['2.3']] <- marginaleffects::avg_slopes(models1[['2.3']])
models2[['2.4']] <- marginaleffects::avg_slopes(models1[['2.4']])
models2[['2.5']] <- marginaleffects::avg_slopes(models1[['2.5']])
models2[['2.6']] <- marginaleffects::avg_slopes(models1[['2.6']])

modelsummary::modelsummary(models2)


msum <- modelsummary::modelsummary(models2,
                                   output = "gt",
                                   stars = c("*"=0.05, "**"=0.01, "***"=0.001),
                                   coef_map = cm,
                                   gof_map = gm,
                                   fmt = scaleFUN,
                                   title = "",
                                   notes = list('B Path (H2): Effect of media reporting on speech behavior. Reported are Average Marginal Effects (AME) of a logit model with fixed effects.',
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

gtExtras::gtsave_extra(table1, "table_h2_b_path_ame.png", path = "tables", zoom = 10, expand = 5)

##- H2 in Categories -----------------------------------------------------------

sub_war <- data_war |>  mutate(pol_position_diff_party = as.factor(pol_position_diff_party))
sub_gau <- data_gau |>  mutate(pol_position_diff_party = as.factor(pol_position_diff_party))
sub_pooled <- data_pooled |>  mutate(pol_position_diff_party = as.factor(pol_position_diff_party))

# FE Models
models1 <- list()
models1[['2.1']] <- fixest::feglm(treated_topic ~ treated_media*pol_position_diff_party + treated_topic_lag1| politician_id, data = sub_war, family = binomial(link = "logit"))
models1[['2.2']] <- fixest::feglm(treated_topic ~ treated_media*pol_position_diff_party + treated_topic_lag1  + treatment |politician_id,  data = sub_war, family = binomial(link = "logit"))
models1[['2.3']] <- fixest::feglm(treated_topic ~ treated_media*pol_position_diff_party + treated_topic_lag1| politician_id, data = sub_gau, family = binomial(link = "logit"))
models1[['2.4']] <- fixest::feglm(treated_topic ~ treated_media*pol_position_diff_party + treated_topic_lag1 + treatment | politician_id, data = sub_gau, family = binomial(link = "logit"))
models1[['2.5']] <- fixest::feglm(treated_topic ~ treated_media*pol_position_diff_party + treated_topic_lag1| politician_id, data = sub_pooled, family = binomial(link = "logit"))
models1[['2.6']] <- fixest::feglm(treated_topic ~ treated_media*pol_position_diff_party + treated_topic_lag1 + treatment | politician_id, data = sub_pooled, family = binomial(link = "logit"))


modelsummary::modelsummary(models1)

msum <- modelsummary::modelsummary(models1,
                                   output = "gt",
                                   stars = c("*"=0.05, "**"=0.01, "***"=0.001),
                                   coef_map = cm,
                                   gof_map = gm,
                                   fmt = scaleFUN,
                                   title = "",
                                   notes = list('B Path (H2): Effect of media reporting on speech behavior. Reported are log odds of a logit model with fixed effects.',
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

gtExtras::gtsave_extra(table1, "table_h2_b_path_cat_lo.png", path = "tables", zoom = 10, expand = 5)

###- Visualization (Predicted Probs) -------------------------------------------

models_pred_prob <- fixest::feglm(treated_topic ~ treated_media*pol_position_diff_party + treated_topic_lag1 + treatment | politician_id, data = sub_pooled, family = binomial(link = "logit"))

boot_data <- sub_pooled |> 
  filter(politician_id %in% names(fixef(models_pred_prob)[[1]])) |> 
  filter(!is.na(pol_position_diff_party))


boot_results <- boot(data = boot_data, statistic = boot_function_ideological_distance, R = 1000, strata = boot_data$pol_position_diff_party) 

pred_data <- summary(boot_results) |> 
  bind_cols(compute_all_cis(boot_results)) |> 
  mutate(pol_position_diff_party = (row_number() - 1) %/% 50) |>
  group_by(pol_position_diff_party) |> 
  mutate(treated_media = seq(0.06, 0.25, length.out = 50)) |> 
  ungroup()

# Plot
pred_data |> 
  mutate(group_plot = case_when(
    pol_position_diff_party %in% c(0,1,2) ~ "Low Deviation",
    pol_position_diff_party %in% c(3,4,5) ~ "Medium Deviation",
    pol_position_diff_party %in% c(6,7,8) ~ "High Deviation",
    .default = NA
  )) |> 
  mutate(group_plot = factor(group_plot, levels = c("Low Deviation", "Medium Deviation", "High Deviation"))) |> 
  ggplot() +
  geom_line(aes(x = treated_media, y = original, color = as.factor(pol_position_diff_party))) +
  geom_errorbar(aes(x = treated_media, y = original,
                    ymin = ci_lower, ymax = ci_higher, color = as.factor(pol_position_diff_party)), alpha = .3) +
  labs(
    x = "Media Salience (H2)",
    y = "Predicted Probability and 95% Confidence Intervals",
    color = "Ideological Distance\nfrom Party Leadership",
    fill = "Ideological Distance\nfrom Party Leadership"
  ) +
  theme_minimal() +
  theme(
    legend.background = element_blank(),
    legend.position = "bottom"
  ) +
  facet_wrap(~group_plot)

ggsave(file = "graph/Pred_Probs_h2.png",
       width = 10, height = 5, dpi = 900)

##- Analysis, c path H1 --------------------------------------------------------

models1 <- list()
models1[['1.1']]  <- fixest::feglm(treated_topic ~ treatment*party_leader_2 | politician_id + session_id, data = data_war, family = binomial(link = "logit"))
models1[['1.2']]  <- fixest::feglm(treated_topic ~ treatment*party_leader_2 + treated_topic_lag1 | politician_id + session_id, data = data_war, family = binomial(link = "logit"))

models1[['1.3']]  <- fixest::feglm(treated_topic ~ treatment*party_leader_2 | politician_id + session_id, data = data_gau, family = binomial(link = "logit"))
models1[['1.4']]  <- fixest::feglm(treated_topic ~ treatment*party_leader_2 + treated_topic_lag1 | politician_id + session_id, data = data_gau, family = binomial(link = "logit"))

models1[['1.5']]  <- fixest::feglm(treated_topic ~ treatment*party_leader_2 | politician_id + session_id, data = data_gau, family = binomial(link = "logit"))
models1[['1.6']]  <- fixest::feglm(treated_topic ~ treatment*party_leader_2 + treated_topic_lag1 | politician_id + session_id, data = data_gau, family = binomial(link = "logit"))

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

gtExtras::gtsave_extra(table1, "table_h1_c_path_lo.png", path = "tables", zoom = 10, expand = 5)

### AME ------------------------------------------------------------------------

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

gtExtras::gtsave_extra(table1, "table_h1_c_path_ame.png", path = "tables", zoom = 10, expand = 5)

##- Analysis, c path H2 --------------------------------------------------------

models1 <- list()
models1[['1.1']]  <- fixest::feglm(treated_topic ~ treatment*pol_position_diff_party | politician_id + session_id, data = data_war, family = binomial(link = "logit"))
models1[['1.2']]  <- fixest::feglm(treated_topic ~ treatment*pol_position_diff_party + treated_topic_lag1 | politician_id + session_id, data = data_war, family = binomial(link = "logit"))
models1[['1.3']]  <- fixest::feglm(treated_topic ~ treatment*pol_position_diff_party | politician_id + session_id, data = data_gau, family = binomial(link = "logit"))
models1[['1.4']]  <- fixest::feglm(treated_topic ~ treatment*pol_position_diff_party + treated_topic_lag1 | politician_id + session_id, data = data_gau, family = binomial(link = "logit"))
models1[['1.5']]  <- fixest::feglm(treated_topic ~ treatment*pol_position_diff_party | politician_id + session_id, data = data_pooled, family = binomial(link = "logit"))
models1[['1.6']]  <- fixest::feglm(treated_topic ~ treatment*pol_position_diff_party + treated_topic_lag1 | politician_id + session_id, data = data_pooled, family = binomial(link = "logit"))

modelsummary::modelsummary(models1)
#
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

gtExtras::gtsave_extra(table1, "table_h2_c_path_lo.png", path = "tables", zoom = 10, expand = 5)

### AME ------------------------------------------------------------------------

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

gtExtras::gtsave_extra(table1, "table_h2_c_path_ame.png", path = "tables", zoom = 10, expand = 5)