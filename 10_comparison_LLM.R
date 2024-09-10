#- Setup -----------------------------------------------------------------------
options(scipen = 999)
set.seed(1234)

pacman::p_load(dplyr,
               readr,
               tidyr,
               lexRankr,
               stringr,
               ggplot2) 

#- Load Data -------------------------------------------------------------------

old_wd <- getwd()
setwd("_data/_processed/sample_classification")

poltex_media <- read_csv("bert_media.csv")
bert_media <- read_table("bert_media.txt")
human_media <- readxl::read_xlsx("sampling_data_media_human.xlsx") |> 
  rename(classification_human = Classification_human)
original_media <- read_csv("sampling_media_original.csv")

poltex_speeches <- read_csv("bert_speeches.csv")
bert_speeches <- read_table("bert_speeches.txt")
human_speeches <- readxl::read_xlsx("sampling_speeches_human.xlsx") 
original_speeches <- read_csv("sampling_speeches_original.csv")

#- CAP Codebook ----------------------------------------------------------------

cap_codebook <- data.frame(
  prediction = c(1:10, 12:21, 23),
  classification = c("Macroeconomics",
                     "Civil",
                     "Health",
                     "Agriculture",
                     "Labor",
                     "Education",
                     "Environment",
                     "Energy",
                     "Immigration",
                     "Transportation",
                     "Law",
                     "Social",
                     "Housing",
                     "Domestic",
                     "Defense",
                     "Technology",
                     "Foreign",
                     "International",
                     "Government",
                     "Public",
                     "Culture")
)

#- Data Management -------------------------------------------------------------

# Media
bert_media <- bert_media |> 
  mutate(index = index + 1) |> 
  left_join(cap_codebook) |> 
  rename(id = index,
         classification_bert = classification,
         classification_score_bert = probability) |> 
  select(-prediction, -prediction2, -probability_2)

poltex_media <- poltex_media |> 
  left_join(cap_codebook, by = c("predicted" = "prediction")) |> 
  rename(classification_poltex = classification) |> 
  select(-predicted)

media <- original_media |> 
  left_join(human_media) |> 
  left_join(poltex_media) |> 
  left_join(bert_media) |> 
  select(-description, newspaper)

media <- media |> 
  mutate(original_result = if_else(classification_human == classification_original, 1, 0)) |> 
  mutate(bert_result = if_else(classification_human == classification_bert, 1, 0)) |> 
  mutate(poltex_result = if_else(classification_human == classification_poltex, 1, 0))

media_agg <- media |>
  group_by(classification_human) |> 
    summarise(original_result = mean(original_result),
              bert_result = mean(bert_result),
              poltex_result = mean(poltex_result)) |> 
  ungroup()

# Speeches
bert_speeches <- bert_speeches |> 
  mutate(index = index + 1) |> 
  left_join(cap_codebook) |> 
  rename(id = index,
         classification_bert = classification,
         classification_score_bert = probability) |> 
  select(-prediction, -prediction2, -probability_2)

poltex_speeches <- poltex_speeches |> 
  left_join(cap_codebook, by = c("predicted" = "prediction")) |> 
  rename(classification_poltex = classification) |> 
  select(-predicted)

speeches <- original_speeches |> 
  left_join(human_speeches) |> 
  left_join(poltex_speeches) |> 
  left_join(bert_speeches) 

speeches <- speeches |> 
  mutate(original_result = if_else(classification_human == classification_original, 1, 0)) |> 
  mutate(bert_result = if_else(classification_human == classification_bert, 1, 0)) |> 
  mutate(poltex_result = if_else(classification_human == classification_poltex, 1, 0))

speeches_agg <- speeches |> 
  group_by(classification_human) |> 
  summarise(original_result = mean(original_result),
            bert_result = mean(bert_result),
            poltex_result = mean(poltex_result)) |> 
  ungroup()

#- Visualization ---------------------------------------------------------------
setwd(old_wd)

# Media

  ggplot(media_agg)+
  geom_col(aes(x = classification_human, y = original_result)) +
  labs(x = "Human Annotated Topics (CAP)",
       y = "Share Correct Annotations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file = "graph/LLM_com_original_media.png",
         height = 5, width = 8)
  
  ggplot(media_agg)+
  geom_col(aes(x = classification_human, y = bert_result))  +
    labs(x = "Human Annotated Topics (CAP)",
         y = "Share Correct Annotations") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file = "graph/LLM_com_bert_media.png",
         height = 5, width = 8)
  
  ggplot(media_agg)+
    geom_col(aes(x = classification_human, y = poltex_result))  +
    labs(x = "Human Annotated Topics (CAP)",
         y = "Share Correct Annotations") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(file = "graph/LLM_com_poltex_media.png",
         height = 5, width = 8)

mean(media$original_result) # 51 %
mean(media$bert_result) # 48 %
mean(media$poltex_result) # 2 %

# Speeches

ggplot(speeches_agg)+
  geom_col(aes(x = classification_human, y = original_result)) +
  labs(x = "Human Annotated Topics (CAP)",
       y = "Share Correct Annotations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file = "graph/LLM_com_original_speeches.png",
       height = 5, width = 8)

ggplot(speeches_agg)+
  geom_col(aes(x = classification_human, y = bert_result))  +
  labs(x = "Human Annotated Topics (CAP)",
       y = "Share Correct Annotations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file = "graph/LLM_com_bert_speeches.png",
       height = 5, width = 8)

ggplot(speeches_agg)+
  geom_col(aes(x = classification_human, y = poltex_result))  +
  labs(x = "Human Annotated Topics (CAP)",
       y = "Share Correct Annotations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file = "graph/LLM_com_poltex_speeches.png",
       height = 5, width = 8)

mean(speeches$original_result) # 47 %
mean(speeches$bert_result) # 47 %
mean(speeches$poltex_result) # 3 %


