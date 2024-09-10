#- Setup -----------------------------------------------------------------------
options(scipen = 999)

pacman::p_load(dplyr,
               readr,
               stringr,
               ggplot2,
               lubridate,
               tidytext) 

library(tm)
library(slam)
library(SnowballC)
library(SnowballC)
library(quanteda)

#- Data Wrangling --------------------------------------------------------------

# Read data 2020 following
setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Scraping/data"))

spiegel <- read_csv("spiegel.csv", col_select = c(3:10)) %>%
           mutate(newspaper = "spiegel") %>%
           select(date, keywords, newspaper, description)

sz      <- read_csv("sz.csv", col_select = c(3:10))  %>%
           mutate(newspaper = "sz") %>%
           select(date, keywords, newspaper, description)

taz     <- read_csv("taz.csv", col_select = c(3:10)) %>%
           mutate(newspaper = "taz") %>%
           select(date, keywords, newspaper, description)

welt    <- read_csv("welt.csv", col_select = c(3:10)) %>%
           mutate(newspaper = "welt") %>%
           select(date, keywords, newspaper, description)

zeit    <- read_csv("zeit.csv", col_select = c(3:10)) %>%
           mutate(newspaper = "zeit") %>%
           select(date, keywords, newspaper, description)

media19_desc  <- rbind(spiegel, taz, zeit, welt, sz) %>%
  tidylog::filter(!is.na(description)) %>%
  mutate(ID = row_number())

write_csv(media19_desc, file = "media19.csv")

# Read data 2009 following
setwd(paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/Scraping/german-media-scrape/R"))

spiegel <- read_csv("spiegel.csv") %>%
  mutate(newspaper = "spiegel") %>%
  select(date, keywords, newspaper, description)

sz      <- read_csv("sueddeutsche.csv")  %>%
  mutate(newspaper = "sz") %>%
  select(date, keywords, newspaper, description)

taz     <- read_csv("taz_full.csv") %>%
  mutate(newspaper = "taz") %>%
  select(date, keywords, newspaper, description)

welt    <- read_csv("welt_1.csv") %>%
  mutate(newspaper = "welt") %>%
  select(date, keywords, newspaper, description)

zeit    <- read_csv("zeit_full.csv") %>%
  mutate(newspaper = "zeit") %>%
  select(date, keywords, newspaper, description)

media09_desc  <- rbind(spiegel, taz, zeit, welt, sz) %>%
  tidylog::filter(!is.na(description)) %>%
  mutate(ID = row_number())

write_csv(media09_desc, file = "media09.csv")
