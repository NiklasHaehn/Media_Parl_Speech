#- Setup -----------------------------------------------------------------------
set.seed(1234)
options(scipen = 999)


pacman::p_load(dplyr,
               readr,
               tidyr,
               lexRankr,
               stringr,
               ggplot2,
               lubridate)

#- Load Data -------------------------------------------------------------------


spiegel_09 <- read.csv("_data/_processed/Media/240216_categorized_spiegel_10.csv") |>
  select(date, description, classification, classification_score, paywall) |>
  mutate(newspaper = "spiegel") |>
  mutate(paywall = as.character(paywall)) |>
  mutate(paywall = case_match(paywall,
    "1" ~ "True",
    "0" ~ "False",
    .default = paywall
  ))

spiegel_19 <- read.csv("_data/_processed/Media/240216_categorized_spiegel_20.csv")  |>
  select(date, description, classification, classification_score, paywall) |>
  mutate(newspaper = "spiegel")  |>
  mutate(paywall = as.character(paywall)) |>
  mutate(paywall = case_match(paywall,
                              "1" ~ "True",
                              "0" ~ "False",
                              .default = paywall
  ))

sz_09      <- read.csv("_data/_processed/Media/240216_categorized_sueddeutsche_10.csv")  |>
  select(date, description, classification, classification_score, paywall) |>
  mutate(newspaper = "sz")  |>
  mutate(paywall = as.character(paywall)) |>
  mutate(paywall = case_match(paywall,
                              "1" ~ "True",
                              "0" ~ "False",
                              .default = paywall
  ))

sz_19      <- read.csv("_data/_processed/Media/240216_categorized_sz_20.csv")  |>
  select(date, description, classification, classification_score, paywall) |>
  mutate(newspaper = "sz")  |>
  mutate(paywall = as.character(paywall)) |>
  mutate(paywall = case_match(paywall,
                              "1" ~ "True",
                              "0" ~ "False",
                              .default = paywall
  ))

taz_09     <- read.csv("_data/_processed/Media/240216_categorized_taz_10.csv")  |>
  select(date, description, classification, classification_score) |>
  mutate(newspaper = "taz")

taz_19     <- read.csv("_data/_processed/Media/240216_categorized_taz_20.csv")  |>
  select(date, description, classification, classification_score) |>
  mutate(newspaper = "taz") 

welt_09    <- read.csv("_data/_processed/Media/240216_categorized_welt_10.csv")  |>
  select(date, description, classification, classification_score, paywall) |>
  mutate(newspaper = "welt")  |>
  mutate(paywall = as.character(paywall)) |>
  mutate(paywall = case_match(paywall,
                              "1" ~ "True",
                              "0" ~ "False",
                              .default = paywall
  ))

welt_19    <- read.csv("_data/_processed/Media/240216_categorized_welt_20.csv")  |>
  select(date, description, classification, classification_score, paywall) |>
  mutate(newspaper = "welt")  |>
  mutate(paywall = as.character(paywall)) |>
  mutate(paywall = case_match(paywall,
                              "1" ~ "True",
                              "0" ~ "False",
                              .default = paywall
  ))

zeit_09    <- read.csv("_data/_processed/Media/240216_categorized_zeit_10.csv")  |>
  select(date, description, classification, classification_score, paywall) |>
  mutate(newspaper = "zeit")  |>
  mutate(paywall = as.character(paywall)) |>
  mutate(paywall = case_match(paywall,
                              "1" ~ "True",
                              "0" ~ "False",
                              .default = paywall
  ))

zeit_19    <- read.csv("_data/_processed/Media/240216_categorized_zeit_20.csv")  |>
  select(date, description, classification, classification_score, paywall) |>
  mutate(newspaper = "zeit")  |>
  mutate(paywall = as.character(paywall)) |>
  mutate(paywall = case_match(paywall,
                              "1" ~ "True",
                              "0" ~ "False",
                              .default = paywall
  ))

data <- bind_rows(spiegel_09, spiegel_19, sz_09, sz_19, taz_09, taz_19, welt_09, welt_19, zeit_09, zeit_19) |>
  filter(description != "")


#- Sampling Data for LLM Comparison --------------------------------------------

sampling_media_original <- data |> 
  slice_sample(n = 100) |> 
  mutate(id = row_number()) |> 
  rename(classification_score_original = classification_score,
         classification_original = classification) |> 
  select(-paywall, -date) 

sampling_data_media <- sampling_media_original |> 
  select(-classification_score_original, -classification_original)

write_csv(sampling_media_original, "_data/_processed/sample_classification/sampling_media_original.csv")
write_csv(sampling_data_media, "_data/_processed/sample_classification/sampling_data_media.csv")
writexl::write_xlsx(sampling_data_media, "_data/_processed/sample_classification/sampling_data_media_human.xlsx")

#- Analysis Paywall Bias -------------------------------------------------------

data |> 
  filter(classification != "") |> 
  group_by(newspaper, classification) |> 
  count(paywall, newspaper, classification) |> 
  group_by(classification, newspaper) |> 
  mutate(share = n/sum(n)) |> 
  ungroup() |> 
  filter(paywall == "True") |> 
  ggplot()+
  labs(y = "Share of Scraped Articles with Paywall",
       x = "Topic (CAP)") +
  geom_col(aes(x= classification, y = share), position = "dodge")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ newspaper, labeller = as_labeller(c("spiegel" = "Spiegel", 
                          "sz" = "Süddeutsche Zeitung (SZ)",
                          "welt" = "Welt",
                          "zeit" = "Zeit")))

ggsave(file = "graph/Paywall_Analysis.png",
       width = 10, height = 7)


#- Data Wrangling --------------------------------------------------------------

# Control for key words

ukraine_keyword <- c("ukraine", "russland", "russisch", "ukrainisch", "ukrainer", "russen")
japan_keyword <- c("japan", "fukushima", "amtomunfall", "\\sgau\\s", "supergau", "reaktor")

data <- data |>
  select(- paywall) |>
  mutate(description = tolower(description)) |>
  mutate(ukraine_keyword = ifelse(str_detect(description, pattern = paste0(ukraine_keyword, collapse = "|")),1,0)) |>
  mutate(japan_keyword = ifelse(str_detect(description, pattern = paste0(japan_keyword, collapse = "|")),1,0)) |>
  mutate(date = as.Date(date))

#- Save Data -------------------------------------------------------------------

write_csv(data, paste0("_data/_processed/Media", stringr::str_remove_all(Sys.Date(), "-"),"_media_preproc_classified.csv"))

