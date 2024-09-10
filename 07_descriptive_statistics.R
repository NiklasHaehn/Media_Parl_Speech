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

#- Event Study War -------------------------------------------------------------

event1 <- estimatr::lm_robust(z_score ~ relevel(time_diff_war, ref = "-1")*as.factor(classification),
                              data = media_war,
                              se_type = "stata")

out <- data.frame(
  var = names(event1$coefficients[str_detect(names(event1$coefficients),":")]),
  estimate = event1$coefficients[str_detect(names(event1$coefficients),":")],
  conf.low = event1$conf.low[str_detect(names(event1$coefficients),":")],
  conf.high = event1$conf.high[str_detect(names(event1$coefficients),":")]
) |> 
  mutate(topic = str_remove(var, ".*\\)")) |> 
  mutate(week = str_remove_all(var, '.*\\"\\)') |> 
           str_remove("\\:.*")) |> 
  mutate(topic = trimws(topic)) |> 
  mutate(week = trimws(week)) |> 
  mutate(week = as.numeric(week))

ggplot(out) +
  geom_vline(xintercept = -1, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(x = week, y = estimate), position = position_dodge(0.5)) +
  geom_errorbar(aes(x=week, ymin=conf.low, ymax=conf.high),
                width=0,size=1, position=position_dodge(0.5)) +
  geom_smooth(data = out[out$week >= 0,], aes(x = week, y = estimate), position = position_dodge(0.5), loose = .5)+
  geom_smooth(data = out[out$week < 0,], aes(x = week, y = estimate), position = position_dodge(0.5), loose = .5)+
  labs(x = "Time Difference to Treatment in Weeks",
       y = "Estimate") +
  facet_wrap(~ topic) +
  theme_bw()
ggsave(filename="Eventstudy_topics_war.png", path = "graph", 
       width = 10, height = 6, dpi = 900)


event1 <- estimatr::lm_robust(z_score ~ relevel(time_diff_gau, ref = "-1")*as.factor(classification),
                              data = media_gau,
                              se_type = "stata")
summary(event1)

out <- data.frame(
  var = names(event1$coefficients[str_detect(names(event1$coefficients),":")]),
  estimate = event1$coefficients[str_detect(names(event1$coefficients),":")],
  conf.low = event1$conf.low[str_detect(names(event1$coefficients),":")],
  conf.high = event1$conf.high[str_detect(names(event1$coefficients),":")]
) |> 
  mutate(topic = str_remove(var, ".*\\)")) |> 
  mutate(week = str_remove_all(var, '.*\\"\\)') |> 
           str_remove("\\:.*")) |> 
  mutate(topic = trimws(topic)) |> 
  mutate(week = trimws(week)) |> 
  mutate(week = as.numeric(week))

ggplot(out) +
  geom_vline(xintercept = -1, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(x = week, y = estimate), position = position_dodge(0.5)) +
  geom_errorbar(aes(x=week, ymin=conf.low, ymax=conf.high),
                width=0,size=1, position=position_dodge(0.5)) +
  geom_smooth(data = out[out$week >= 0,], aes(x = week, y = estimate), position = position_dodge(0.5), loose = .5)+
  geom_smooth(data = out[out$week < 0,], aes(x = week, y = estimate), position = position_dodge(0.5), loose = .5)+
  labs(x = "Time Difference to Treatment in Weeks",
       y = "Estimate") +
  facet_wrap(~ topic) +
  theme_bw()
ggsave(filename="Eventstudy_topics_gau.png", path = "graph", 
       width = 10, height = 6, dpi = 900)



##- Event Study Media ----------------------------------------------------------

media_war_agg <- media_war |> 
  mutate(treated_topic_war = ifelse(classification %in% c("Defense", "International", "Foreign", "Immigration"),1,0))


event1 <- estimatr::lm_robust(z_score ~ relevel(time_diff_war, ref = "-1")*as.factor(treated_topic_war),
                              data = media_war_agg,
                              se_type = "stata")

out <- data.frame(
  var = names(event1$coefficients[str_detect(names(event1$coefficients),":")]),
  estimate = event1$coefficients[str_detect(names(event1$coefficients),":")],
  conf.low = event1$conf.low[str_detect(names(event1$coefficients),":")],
  conf.high = event1$conf.high[str_detect(names(event1$coefficients),":")]
) |> 
  mutate(topic = str_remove(var, ".*\\)")) |> 
  mutate(week = str_remove_all(var, '.*\\"\\)') |> 
           str_remove("\\:.*")) |> 
  mutate(topic = trimws(topic)) |> 
  mutate(week = trimws(week)) |> 
  mutate(week = as.numeric(week))


g.event_1 <- ggplot(out) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(x = week, y = estimate), position = position_dodge(0.5), color = "grey", alpha = .5) +
  geom_errorbar(aes(x=week, ymin=conf.low, ymax=conf.high),
                width=0,size=1, position=position_dodge(0.5), color = "grey", alpha = .5) +
  geom_smooth(data = out[out$week >= 0,], aes(x = week, y = estimate), position = position_dodge(0.5), loose = .5, se =F, color = "black")+
  geom_smooth(data = out[out$week < 0,], aes(x = week, y = estimate), position = position_dodge(0.5), loose = .5, se = F, color = "black")+
  xlab("") +
  ylab("") +
  theme_bw()

media_gau_agg <- media_gau |> 
  mutate(treated_topic_gau = ifelse(classification %in% c("Energy"),1,0)) |> 
  filter(as.numeric(as.character(time_diff_gau)) >= -60 & as.numeric(as.character(time_diff_gau)) <= 23)


event1 <- estimatr::lm_robust(z_score ~ relevel(time_diff_gau, ref = "-1")*as.factor(treated_topic_gau),
                              data = media_gau_agg,
                              se_type = "stata")

out <- data.frame(
  var = names(event1$coefficients[str_detect(names(event1$coefficients),":")]),
  estimate = event1$coefficients[str_detect(names(event1$coefficients),":")],
  conf.low = event1$conf.low[str_detect(names(event1$coefficients),":")],
  conf.high = event1$conf.high[str_detect(names(event1$coefficients),":")]
) |> 
  mutate(topic = str_remove(var, ".*\\)")) |> 
  mutate(week = str_remove_all(var, '.*\\"\\)') |> 
           str_remove("\\:.*")) |> 
  mutate(topic = trimws(topic)) |> 
  mutate(week = trimws(week)) |> 
  mutate(week = as.numeric(week))


g.event_2 <- ggplot(out) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(x = week, y = estimate), position = position_dodge(0.5), color = "grey", alpha = .5) +
  geom_errorbar(aes(x=week, ymin=conf.low, ymax=conf.high),
                width=0,size=1, position=position_dodge(0.5), color = "grey", alpha = .5) +
  geom_smooth(data = out[out$week >= 0,], aes(x = week, y = estimate), position = position_dodge(0.5), loose = .5, se =F, color = "black")+
  geom_smooth(data = out[out$week < 0,], aes(x = week, y = estimate), position = position_dodge(0.5), loose = .5, se = F, color = "black")+
  xlab("") +
  ylab("") +
  theme_bw()

test <- ggpubr::ggarrange(g.event_1, g.event_2, nrow = 2,
                          labels = "AUTO")

fig.1 <- ggpubr::annotate_figure(test, left = textGrob("Estimate", rot = 90, vjust = 1, gp = gpar(cex = 1.0)),
                                 bottom = textGrob("Time Difference to Treatment in Weeks", gp = gpar(cex = 1.0)))

ggsave(fig.1, file = "graph/Eventstudy_combined_media_treated.png",
       height = 6, width = 6, dpi = 900)

#- Correlation Matrix CAP ------------------------------------------------------

# Complete
cor(data_war[,which(str_detect(names(data_pooled), "share"))]) |> 
  reshape2::melt() |> 
  mutate(Var1 = str_remove_all(Var1, "share_"),
         Var2 = str_remove_all(Var2, "share_")) |> 
  ggplot(aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  labs(x = "",
       y = "") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1,1), space = "Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
  coord_fixed()

ggsave(filename = "./graph/correlation_media.png",
       height = 6, width = 6, dpi = 900)


#- Summary Table ---------------------------------------------------------------

# Numeric
summary_numeric_1 <- data_pooled |> 
  select(intersect(names(cm), names(data_pooled))) |> 
  create_summary_table(type = "numeric") 

summary_numeric <- media_data_pooled |> 
  select(intersect(names(cm), names(media_data_pooled))) |> 
  create_summary_table(type = "numeric") |> 
  distinct() |> 
  bind_rows(summary_numeric_1)

rename_df <- data.frame(
  raw = names(cm),
  clean = unname(cm),
  stringsAsFactors = FALSE
) |> 
  filter(raw %in% summary_numeric$skim_variable)
  
table1 <- gt::gt(summary_numeric) |> 
  tab_header(
    title = md("*Variable type: numeric*")
  ) |> 
  cols_label(
    skim_variable = "Variable",
    n_missing = "Missing Values",
    numeric.mean = "Mean",
    numeric.sd = "Standard Deviation",
    numeric.p0 = "0",
    numeric.p25 = "25",
    numeric.p50 = "50",
    numeric.p75 = "75",
    numeric.p100 = "100"
  ) |> 
  tab_spanner(
    label = "Percentile",
    columns = c(numeric.p0, numeric.p25, numeric.p50, numeric.p75, numeric.p100)
  ) |> 
  text_transform(
    locations = cells_body(columns = c(skim_variable)),
    fn = function(x) {
      new_values <- rename_df$clean[match(x, rename_df$raw)]
      ifelse(is.na(new_values), x, new_values)
    }
  ) |> 
  fmt_integer(
    columns = c(n_missing)
  ) |> 
  fmt_number(
    columns = c(numeric.mean, numeric.sd, numeric.p0, numeric.p25, numeric.p50, numeric.p75, numeric.p100),
    decimals = 2
  )

gtExtras::gtsave_extra(table1, "summary_numeric.png", path = "tables", zoom = 10, expand = 5)

# Factor
summary_factor_1 <- data_pooled |> 
  select(intersect(names(cm), names(data_pooled))) |> 
  create_summary_table(type = "factor") 

summary_factor <- media_data_pooled |> 
  select(intersect(names(cm), names(media_data_pooled))) |> 
  create_summary_table(type = "factor") |> 
  bind_rows(summary_factor_1) |> 
  distinct() |> 
  select(-complete_rate, -character.empty, -character.whitespace)

rename_df <- data.frame(
  raw = names(cm),
  clean = unname(cm),
  stringsAsFactors = FALSE
) |> 
  filter(raw %in% summary_factor$skim_variable)

table1 <- gt::gt(summary_factor) |> 
  tab_header(
    title = md("*Variable type: factor*")
  ) |> 
  cols_label(
    skim_variable = "Variable",
    n_missing = "Missing Values",
    character.min = "Min",
    character.max = "Max",
    character.n_unique = "Number Levels"
  ) |> 
  text_transform(
    locations = cells_body(columns = c(skim_variable)),
    fn = function(x) {
      new_values <- rename_df$clean[match(x, rename_df$raw)]
      ifelse(is.na(new_values), x, new_values)
    }
  ) 

gtExtras::gtsave_extra(table1, "summary_factor.png", path = "tables", zoom = 10, expand = 5)

# Data
summary_date_1 <- data_pooled |> 
  select(intersect(names(cm), names(data_pooled))) |> 
  create_summary_table(type = "date") |> 
  mutate(skim_variable = if_else(skim_variable =="date", "Date (Parliamentary Sessions)", skim_variable))

summary_date <- media_data_pooled |> 
  select(intersect(names(cm), names(media_data_pooled))) |> 
  create_summary_table(type = "date") |> 
  mutate(skim_variable = if_else(skim_variable =="date", "Date (Media Articles)", skim_variable)) |> 
  bind_rows(summary_date_1) |> 
  distinct() |> 
  select(- complete_rate)

rename_df <- data.frame(
  raw = names(cm),
  clean = unname(cm),
  stringsAsFactors = FALSE
) |> 
  filter(raw %in% summary_date$skim_variable)

table1 <- gt::gt(summary_date) |> 
  tab_header(
    title = md("*Variable type: date*")
  ) |> 
  cols_label(
    skim_variable = "Variable",
    n_missing = "Missing Values",
    Date.min = "Min",
    Date.max = "Max",
    Date.median = "Median",
    Date.n_unique = "Number Unique Dates"
  ) |> 
  text_transform(
    locations = cells_body(columns = c(skim_variable)),
    fn = function(x) {
      new_values <- rename_df$clean[match(x, rename_df$raw)]
      ifelse(is.na(new_values), x, new_values)
    }
  ) 

gtExtras::gtsave_extra(table1, "summary_date.png", path = "tables", zoom = 10, expand = 5)

# Number of articles over time -------------------------------------------------

plot <- media |> 
  filter(date >= as.Date("2009-01-01")) |> 
  filter(date <= as.Date("2013-01-01") | date >= as.Date("2020-01-01")) |> 
  filter(date <= as.Date("2022-01-01")) |>
  #  mutate(week = floor_date(date, unit = "week")) |> 
  count(newspaper, date)

ggplot() +
  geom_smooth(data = plot[plot$date <= as.Date("2013-01-01"),] ,
              aes(y = n, x = date, color = newspaper), se = F) +
  geom_smooth(data = plot[plot$date >= as.Date("2020-01-01"),] ,
              aes(y = n, x = date, color = newspaper), se = F) +
  labs(color = "Outlet",
       x = "Date",
       y = "Number of Articles per Day") +
  scale_color_discrete(
    labels = c("Spiegel", "SZ", "Taz", "Welt", "Zeit")
  ) +
  theme_minimal()

ggsave(file = "graph/Number_of_articles_over_time.png",
       width = 12, height = 6)