#- Setup -----------------------------------------------------------------------
options(scipen = 999)

pacman::p_load(dplyr,
               readr,
               tidyr,
               lexRankr,
               stringr,
               lubridate) 

#- Data Wrangling --------------------------------------------------------------

# Stopwords German and other words
stop_ger <- tidytext::get_stopwords(language = "de", source = "snowball") |>
  add_row(word = c("sehr", "geehrte", "damen", "herren", "geehrter",
                  "herr", "kollege", "kollegen", "kollegin,", "kolleginnen", "frau", "präsident", "präsidentin", "vielen", "dank"),
          lexicon = "snowball")


data <- read_csv("_data/_raw/Reden_BT/speeches_final.csv") |>
        filter(electoral_term >= 16,
               nchar(speech_content) >= 10) |>
        mutate(word_count = nchar(speech_content)) |>
        mutate(speech_content = str_replace_all(speech_content, "\\\n[^[:alpha:]]*", " ")) |>
        rename(text = speech_content) |>
        mutate(text = tolower(text)) |>  
        mutate(text = str_replace_all(text, "[:punct:]", "")) |>
        mutate(text = str_replace_all(text, "[:symbol:]", "")) |>
        mutate(text = str_replace_all(text, "[:number:]", "")) |>
        mutate(text = str_replace_all(text, paste0("\\b", paste0(stop_ger$word,"\\b", collapse = "|\\b"), "\\b"), "")) |>
        mutate(text = str_replace_all(text, "\\s+", " "))

data_sub <- data |>
         mutate(n = str_count(text, "\\w+")) |>
         filter(n >= 512) |>
         mutate(text = str_split(text, " ")) |>
         unnest(text) |>
         filter(text != " "| text != "") |>
         group_by(id) |>
         mutate(row = as.character(ceiling(row_number()/512))) |>
         ungroup() |>
         mutate(id = paste0(as.character(id), "_", row)) |>
         group_by(id) |>
         mutate(text = paste0(text, collapse = " ")) |>
         distinct() |>
         select(-n, - row)

data <- data |>
  filter(!id %in% data_sub) |>
  rbind(data_sub)

#- Save Data -------------------------------------------------------------------

write_csv(data, paste0("_data/_processed/", stringr::str_remove_all(Sys.Date(), "-"),"_speeches_preproc.csv"))

