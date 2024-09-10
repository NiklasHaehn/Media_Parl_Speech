#- Setup -----------------------------------------------------------------------
options(scipen = 999)
set.seed(1234)

pacman::p_load(dplyr,
               readr,
               tidyr,
               lexRankr,
               stringr,
               lubridate) 

#- Load Data -------------------------------------------------------------------

data <- read.csv("240319_categorized_speeches.csv")

#- Sampling Data for LLM Analysis ----------------------------------------------

sampling_speeches_original <- data |> 
  filter(!str_detect(id, "_")) |> 
  filter(text != "") |> 
  filter(word_count >= 200) |> 
  slice_sample(n = 100) |> 
  mutate(id = row_number()) |> 
  rename(classification_score_original = classification_score,
         classification_original = classification) |> 
  select(id, text, classification_score_original, classification_original) 

sampling_speeches_media <- sampling_speeches_original |> 
  select(-classification_score_original, -classification_original)

write_csv(sampling_speeches_original, "_data/_processed/sample_classification/sampling_speeches_original.csv")
write_csv(sampling_speeches_media, "_data/_processed/sample_classification/sampling_speeches_media.csv")
writexl::write_xlsx(sampling_speeches_media, "_data/_processed/sample_classification/sampling_speeches_media_human.xlsx")

#- Data Wrangling --------------------------------------------------------------

# Control for key words

ukraine_keyword <- c("ukraine", "russland", "russisch", "ukrainisch", "ukrainer", "russen")
japan_keyword <- c("japan", "fukushima", "amtomunfall", "\\sgau\\s", "supergau", "reaktor")

data <- data |>
  mutate(ukraine_keyword = ifelse(str_detect(contend_cleaned, pattern = paste0(ukraine_keyword, collapse = "|")),1,0)) |>
  mutate(japan_keyword = ifelse(str_detect(contend_cleaned, pattern = paste0(japan_keyword, collapse = "|")),1,0)) 

# Group by speech id 
data_sub <- data |>
  filter(T == (str_detect(id, "_"))) |>
  mutate(id = as.numeric(str_remove_all(id, "_.*"))) |>
  group_by(id, classification) |>
  mutate(classification_score = sum(classification_score)) |>
  ungroup() |>
  group_by(id) |>
  mutate(classification = ifelse(classification_score == max(classification_score), classification, classification[which.max(classification_score)])) |>
  mutate(ukraine_keyword = ifelse(any(ukraine_keyword == 1), 1,0)) |>
  mutate(japan_keyword = ifelse(any(japan_keyword == 1), 1,0)) |>
  ungroup() |>
  select(-text, -contend_cleaned, - X) |>
  distinct()

data <- data |>
  select(-text, -contend_cleaned, - X) |>
  mutate(id = as.numeric(str_remove_all(id, "_.*"))) |>
  filter(!id %in% data_sub) |>
  rbind(data_sub) |>
  mutate(date = as.Date(date)) |>
  group_by(session, electoral_term) |>
  mutate(date = first(date)) |>
  ungroup()
  

rm(data_sub, ukraine_keyword, japan_keyword)

#- Save Data -------------------------------------------------------------------

write_csv(data, paste0("_data/_processed/", stringr::str_remove_all(Sys.Date(), "-"),"_speeches_preproc_classified.csv"))

