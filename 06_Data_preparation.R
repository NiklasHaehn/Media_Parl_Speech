#- Set up ----------------------------------------------------------------------
set.seed(1234)
rm(list = ls())
options(scipen = 999)

pacman::p_load(dplyr,
               readr,
               tidyr,
               lexRankr,
               stringr,
               readxl,
               fastLink,
               ggplot2,
               lubridate) 

#- Load Data --------------------------------------------------------------------

directory <- "/Users/niklashahn/Library/CloudStorage/Dropbox/Masterarbeit/Daten_Analyse/_data/_processed"

speeches <- latest_csv(directory = directory, name = "speeches_preproc_classified")
media <- latest_csv(directory = directory, name = "media_preproc_classified")

directory <- "/Users/niklashahn/Library/CloudStorage/Dropbox/Masterarbeit/Daten_Analyse/_data/_raw"
party_leader <- latest_xls(directory = directory, name = "leader")

factions <- read.csv("Reden_Analyse/Daten/Open_Discourse/csv/factions.csv")
names(factions)[1] <- "faction_id"

kandidatenbefragung_09 <- haven::read_dta("/Users/niklashahn/Library/CloudStorage/Dropbox/Masterarbeit/Daten_Analyse/_data/_raw/MP_Befragung/cs-transfer/ZA5318_GLES09_Kandidaten/ZA5318_v2-0-0.dta") 
kandidatenbefragung_17 <- haven::read_dta("/Users/niklashahn/Library/CloudStorage/Dropbox/Masterarbeit/Daten_Analyse/_data/_raw/MP_Befragung/cs-transfer/ZA6814_GLES17_Kandidaten_v3-0-0/ZA6814_v3-0-0.dta") 
kandidatenbefragung_21 <- haven::read_dta("/Users/niklashahn/Library/CloudStorage/Dropbox/Masterarbeit/Daten_Analyse/_data/_raw/MP_Befragung/cs-transfer/ZA7704_GLES21_Kandidierende_v2-0-0/ZA7704_v2-0-0.dta") 

#- Combine Data ----------------------------------------------------------------

##- MP Information -------------------------------------------------------------

###- Basic Information MPs -----------------------------------------------------

bt_members <- btmembers::import_members()
bt_members_namen <- as.data.frame((bt_members[[1]]))
bt_members_bio   <- as.data.frame((bt_members[[2]])) 
bt_members_wp    <- as.data.frame((bt_members[[3]]))

df_bio <- bt_members_bio |>
  left_join(bt_members_wp, by = "id") |>
  left_join(bt_members_namen, by = "id") |>
  rename(first_name = vorname,
         last_name = nachname) |>
  mutate(religion = case_when(
    religion == "katholisch" ~ "katholisch",
    religion == "evangelisch" ~ "evangelisch",
    religion == "römisch-katholisch" ~ "katholisch",
    religion == "evangelisch-lutherisch" ~ "evangelisch",
    religion == "neuapostolisch" ~ "andere_christlich",
    religion == "evangelisch-reformiert" ~ "evangelisch",
    religion == "religionslos" ~ "keine",
    religion == "Atheist" ~ "keine",
    religion == "alevitisch" ~ "andere_christlich",
    religion == "Islam" ~ "muslimisch",
    religion == "protestantisch" ~ "evangelisch",
    religion == "muslimisch" ~ "muslimisch",
    religion == "humanistisch" ~ "keine",
    religion == "evangelisch-altreformiert" ~ "evangelisch",
    religion == "griechisch-orthodox" ~ "katholisch",
    religion == "alt-katholisch" ~ "katholisch",
    religion == "russisch-orthodox" ~ "katholisch",
    religion == "christlich-freikirchlich" ~ "evangelisch",
    religion == "evangelisch-protestantisch" ~ "evangelisch",
    .default = NA)) |>
  mutate(familienstand= case_when(
    str_detect(familienstand, "unverheiratet") == T ~ "ledig",
    str_detect(familienstand, "ledig") == T ~ "ledig",
    str_detect(familienstand, "verheiratet") == T ~ "verheiratet",
    str_detect(familienstand, "verwitwet") == T ~ "verwitwet",
    str_detect(familienstand, "geschieden") == T ~ "geschieden",
    str_detect(familienstand, "Lebensge") == T ~ "ledig",
    str_detect(familienstand, "verpartnert") == T ~ "ledig",
    str_detect(familienstand, "patchwork") == T ~ "verheiratet",
    str_detect(familienstand, "alleinerziehend") == T ~ "geschieden",
    str_detect(familienstand, "verlobt") == T ~ "ledig",
    str_detect(familienstand, "getrennt") == T ~ "verheiratet",
    .default = NA)) |>
  mutate(liste = case_when(
    liste == "BW" ~ "BWG",
    liste == "BY" ~ "BAY",
    liste == "BE" ~ "BLN",
    liste ==  "BB" ~ "BRA",
    liste == "HB" ~ "BRE",
    liste == "HH" ~ "HBG",
    liste == "HE" ~ "HES",
    liste == "MV" ~ "MBV",
    liste == "NI" ~ "NDS",
    liste == "NW" ~ "NRW",
    liste == "RP" ~ "RPF",
    liste == "ST" ~ "SAA",
    liste == "SN" ~ "SAC",
    liste == "SL" ~ "SLD",
    liste == "SH" ~ "SWH",
    liste == "TH" ~ "THÜ",
    .default = NA)) |>
  mutate(id_bio = row_number()) |>
  tidylog::filter(wp %in% c(17,19,20)) |>
  mutate(geburtsjahr = lubridate::year(as.Date(geburtsdatum))) |>
  mutate(partei_kurz = case_when(
    partei_kurz == "DIE LINKE." ~ "Linke",
    partei_kurz == "BÜNDNIS 90/DIE GRÜNEN" ~ "GRÜNE",
    .default = partei_kurz)) |>
  mutate(wkr_nummer = as.character(wkr_nummer))


###- Kandidatenstudie 2009 -----------------------------------------------------

# Kandidatenbefragung an Stammdaten anpassen
kandidatenbefragung_09 <- kandidatenbefragung_09 |>
  replace(.,.== -7 | . == 96 | . == 99 | . == 97, NA) |>
  select(a1, gebjahr, kand_typ, wknr, bundesland, gewinner_wk,
         c3, contains("c4_"), d8_2, # 1: sehr, 5: überhaupt nicht
         d11, # 1: eigene Meinung, 2: Parteimeinung
         e1, e16, e15) |>
  rename(partei_kurz = a1,
         geburtsjahr = gebjahr,
         wkr_nummer = wknr,
         pol_position = c3,
         pol_position_cdu = c4_1,
         pol_position_csu = c4_2,
         pol_position_spd = c4_3,
         pol_position_gruene = c4_4,
         pol_position_fdp = c4_5,
         pol_position_linke = c4_6,
         pol_position_npd = c4_7,
         party_leader_powerfull = d8_2,
         behavior_different_from_party_leader = d11,
         geschlecht = e1,
         familienstand = e16,
         religion = e15) |>
  mutate(partei_kurz = case_when(
    partei_kurz == 1 ~ "SPD",
    partei_kurz == 2 ~ "CDU",
    partei_kurz == 3 ~ "CSU",
    partei_kurz == 4 ~ "FDP",
    partei_kurz == 5 ~ "GRÜNE",
    partei_kurz == 6 ~ "Linke",
    .default = NA)) |>
  mutate(geschlecht = case_when(
    geschlecht == 1 ~ "männlich",
    geschlecht == 2 ~ "weiblich",
    .default = NA)) |>
  mutate(liste = case_when(
    bundesland == 1 ~ "BWG",
    bundesland == 2 ~ "BAY",
    bundesland == 3 ~ "BLN",
    bundesland == 4 ~ "BRA",
    bundesland == 5 ~ "BRE",
    bundesland == 6 ~ "HBG",
    bundesland == 7 ~ "HES",
    bundesland == 8 ~ "MBV",
    bundesland == 9 ~ "NDS",
    bundesland == 10 ~ "NRW",
    bundesland == 11 ~ "RPF",
    bundesland == 14 ~ "SAA",
    bundesland == 13 ~ "SAC",
    bundesland == 12 ~ "SLD",
    bundesland == 15 ~ "SWH",
    bundesland == 16 ~ "THÜ",
    .default = NA)) |>
  mutate(bundesland = case_when(
    bundesland == 1 ~ "Baden-Württemberg",
    bundesland == 2 ~ "Bayern",
    bundesland == 3 ~ "Berlin",
    bundesland == 4 ~ "Brandenburg",
    bundesland == 5 ~ "Bremen",
    bundesland == 6 ~ "Hamburg",
    bundesland == 7 ~ "Hessen",
    bundesland == 8 ~ "Mecklenburg-Vorpommern",
    bundesland == 9 ~ "Niedersachsen",
    bundesland == 10 ~ "Nordrhein-Westfalen",
    bundesland == 11 ~ "Rheinland-Pfalz",
    bundesland == 14 ~ "Saarland",
    bundesland == 13 ~ "Sachsen",
    bundesland == 12 ~ "Sachsen-Anhalt",
    bundesland == 15 ~ "Schleswig-Holstein",
    bundesland == 16 ~ "Thüringen",
    .default = NA)) |>
  mutate(religion = case_when(
    religion == 1 ~ "evangelisch",
    religion == 2 ~ "katholisch",
    religion == 3 ~ "andere_christlich",
    religion == 4 ~ "muslimisch",
    religion == 5 ~ "jüdisch",
    religion == 6 ~ "hinduistisch",
    religion == 7 ~ "keiner",
    .default = NA)) |>
  mutate(familienstand = case_when(
    familienstand == 1 ~ "verheiratet",
    familienstand == 2 ~ "verheiratet",
    familienstand == 3 ~ "geschieden",
    familienstand == 4 ~ "verwitwet",
    familienstand == 5 ~ "ledig",
    familienstand == 6 ~ "ledig",
    familienstand == 7 ~ "ledig",
    .default = NA)) |>
  mutate(kand_typ = case_when(
    kand_typ == 1 ~ "Wahlkreis",
    kand_typ == 2 ~ "Liste",
    kand_typ == 3 ~ "beides",
    .default = NA)) |>
  mutate(wp = 17) |>
  filter(!is.na(partei_kurz)) |>
  mutate(partei_kurz = as.character(partei_kurz)) |>
  mutate(pol_position_own_party = case_when(
    partei_kurz == "SPD" ~ pol_position_spd, 
    partei_kurz == "CDU" ~ pol_position_cdu,
    partei_kurz == "CSU" ~ pol_position_csu,
    partei_kurz == "FDP" ~ pol_position_fdp,
    partei_kurz == "GRÜNE" ~ pol_position_gruene,
    partei_kurz == "Linke"~ pol_position_linke,
    .default = NA)) |>
  mutate(pol_position_diff_party = abs(pol_position_own_party - pol_position))

###- Kandidatenstudie 2017 -----------------------------------------------------

kandidatenbefragung_17 <- kandidatenbefragung_17 |>
  replace(.,.== -7 | . == 96 | . == 99 | . == 97 | . == -92  | . == -99 | . == -96, NA) |>
  select(a1, geburtsjahr, kandidaturtyp, wknr, bundesland, wk_gewinner, geschlecht,
         c3, contains("c4", ), # 1: sehr, 5: überhaupt nicht
         d3, # 1: eigene Meinung, 2: Parteimeinung
         e14, e15) |>
  rename(partei_kurz = a1,
         wkr_nummer = wknr,
         kand_typ= kandidaturtyp,
         gewinner_wk = wk_gewinner,
         pol_position = c3,
         pol_position_cdu = c4a,
         pol_position_csu = c4b,
         pol_position_spd = c4c,
         pol_position_gruene = c4d,
         pol_position_fdp = c4e,
         pol_position_linke = c4f,
         pol_position_afd = c4g,
         behavior_different_from_party_leader = d3,
         familienstand = e15,
         religion = e14) |>
  mutate(bundesland = as.character(bundesland)) |>
  mutate(partei_kurz = case_when(
    partei_kurz == 4 ~ "SPD",
    partei_kurz == 2 ~ "CDU",
    partei_kurz == 3 ~ "CSU",
    partei_kurz == 5 ~ "FDP",
    partei_kurz == 6 ~ "GRÜNE",
    partei_kurz == 7 ~ "Linke",
    partei_kurz == 322 ~ "AFD",
    .default = NA)) |>
  mutate(geschlecht = case_when(
    geschlecht == 1 ~ "männlich",
    geschlecht == 2 ~ "weiblich",
    .default = NA)) |>
  mutate(liste = case_when(
    bundesland == 8 ~ "BWG",
    bundesland == 9 ~ "BAY",
    bundesland == 11 ~ "BLN",
    bundesland == 12 ~ "BRA",
    bundesland == 4 ~ "BRE",
    bundesland == 2 ~ "HBG",
    bundesland == 6 ~ "HES",
    bundesland == 13 ~ "MBV",
    bundesland == 3 ~ "NDS",
    bundesland == 5 ~ "NRW",
    bundesland == 7 ~ "RPF",
    bundesland == 15 ~ "SAA",
    bundesland == 14 ~ "SAC",
    bundesland == 10 ~ "SLD",
    bundesland == 1 ~ "SWH",
    bundesland == 16 ~ "THÜ",
    .default = NA)) |>
  mutate(religion = case_when(
    religion == 1 ~ "evangelisch",
    religion == 2 ~ "katholisch",
    religion == 3 ~ "muslimisch",
    religion == 4 ~ "jüdisch",
    religion == 5 ~ "andere",
    religion == 6 ~ "keiner",
    .default = NA)) |>
  mutate(familienstand = case_when(
    familienstand == 1 ~ "verheiratet",
    familienstand == 2 ~ "verwitwet",
    familienstand == 3 ~ "geschieden",
    familienstand == 4 ~ "ledig",
    .default = NA)) |>
  mutate(kand_typ = case_when(
    kand_typ == 1 ~ "Liste",
    kand_typ == 2 ~ "Wahlkreis",
    kand_typ == 3 ~ "beides",
    .default = NA)) |>
  mutate(wp = 19) |>
  filter(!is.na(partei_kurz)) |>
  mutate(partei_kurz = as.character(partei_kurz)) |>
  mutate(pol_position_own_party = case_when(
    partei_kurz == "SPD" ~ pol_position_spd, 
    partei_kurz == "CDU" ~ pol_position_cdu,
    partei_kurz == "CSU" ~ pol_position_csu,
    partei_kurz == "FDP" ~ pol_position_fdp,
    partei_kurz == "GRÜNE" ~ pol_position_gruene,
    partei_kurz == "Linke"~ pol_position_linke,
    partei_kurz == "AfD"~ pol_position_afd,
    .default = NA)) |>
  mutate(pol_position_diff_party = abs(pol_position_own_party - pol_position)) |>
  select(- c1ac4, -c1cc4)

###- Kandidatenstudie 2021 -----------------------------------------------------

kandidatenbefragung_21  <- kandidatenbefragung_21 |>
  replace(.,.== -7 | . == 96 | . == 99 | . == 97 | . == -92  | . == -99 | . == -96, NA) |>
  select(partei, geburtsjahr, kandidaturtyp, wknr, bula, ws1_wk, geschlecht,
         c5, contains("c6"), # 1: sehr, 5: überhaupt nicht
         d3, # 1: eigene Meinung, 2: Parteimeinung
         e13, e15) |>
  rename(partei_kurz = partei,
         wkr_nummer = wknr,
         kand_typ= kandidaturtyp,
         gewinner_wk = ws1_wk,
         pol_position = c5,
         pol_position_cdu = c6b,
         pol_position_csu = c6c,
         pol_position_spd = c6d,
         pol_position_fdp = c6e,
         pol_position_gruene = c6f,
         pol_position_linke = c6g,
         pol_position_afd = c6h,
         behavior_different_from_party_leader = d3,
         familienstand = e15,
         bundesland = bula,
         religion = e13) |>
  mutate(bundesland = as.character(bundesland)) |>
  mutate(partei_kurz = case_when(
    partei_kurz == 4 ~ "SPD",
    partei_kurz == 2 ~ "CDU",
    partei_kurz == 3 ~ "CSU",
    partei_kurz == 5 ~ "FDP",
    partei_kurz == 6 ~ "GRÜNE",
    partei_kurz == 7 ~ "Linke",
    partei_kurz == 322 ~ "AFD",
    .default = NA)) |>
  mutate(geschlecht = case_when(
    geschlecht == 1 ~ "männlich",
    geschlecht == 2 ~ "weiblich",
    .default = NA)) |>
  mutate(liste = case_when(
    bundesland == 8 ~ "BWG",
    bundesland == 9 ~ "BAY",
    bundesland == 11 ~ "BLN",
    bundesland == 12 ~ "BRA",
    bundesland == 4 ~ "BRE",
    bundesland == 2 ~ "HBG",
    bundesland == 6 ~ "HES",
    bundesland == 13 ~ "MBV",
    bundesland == 3 ~ "NDS",
    bundesland == 5 ~ "NRW",
    bundesland == 7 ~ "RPF",
    bundesland == 15 ~ "SAA",
    bundesland == 14 ~ "SAC",
    bundesland == 10 ~ "SLD",
    bundesland == 1 ~ "SWH",
    bundesland == 16 ~ "THÜ",
    .default = NA)) |>
  mutate(religion = case_when(
    religion == 1 ~ "evangelisch",
    religion == 2 ~ "evangelisch",
    religion == 3 ~ "katholisch",
    religion == 4 ~ "christlich",
    religion == 5 ~ "muslimisch",
    religion == 6 ~ "juedisch",
    religion == 7 ~ "keiner",
    religion == 8 ~ "keiner",
    .default = NA)) |>
  mutate(familienstand = case_when(
    familienstand == 1 ~ "verheiratet",
    familienstand == 2 ~ "verheiratet",
    familienstand == 5 ~ "verwitwet",
    familienstand == 4 ~ "geschieden",
    familienstand == 3 ~ "ledig",
    .default = NA)) |>
  mutate(kand_typ = case_when(
    kand_typ == 1 ~ "Liste",
    kand_typ == 2 ~ "Wahlkreis",
    kand_typ == 3 ~ "beides",
    .default = NA)) |>
  mutate(wp = 19) |>
  filter(!is.na(partei_kurz)) |>
  mutate(partei_kurz = as.character(partei_kurz)) |>
  mutate(pol_position_own_party = case_when(
    partei_kurz == "SPD" ~ pol_position_spd, 
    partei_kurz == "CDU" ~ pol_position_cdu,
    partei_kurz == "CSU" ~ pol_position_csu,
    partei_kurz == "FDP" ~ pol_position_fdp,
    partei_kurz == "GRÜNE" ~ pol_position_gruene,
    partei_kurz == "Linke"~ pol_position_linke,
    partei_kurz == "AfD"~ pol_position_afd,
    .default = NA)) |>
  mutate(pol_position_diff_party = abs(pol_position_own_party - pol_position)) 

###- Combine Kandidatensutdie with basic Information ---------------------------

df_survey <- kandidatenbefragung_09 |> 
  bind_rows(kandidatenbefragung_17) |>
  bind_rows(kandidatenbefragung_21) |>
    mutate(id_survey = row_number()) |>
    mutate(wkr_nummer = as.character(wkr_nummer))

# Election District
survey_wk <- df_survey |>
    tidylog::filter(kand_typ == "Wahlkreis"| kand_typ == "beides") |>
    tidylog::filter(gewinner_wk == 1)
  
bio_wk <- df_bio |>
    tidylog::filter(mandatsart == "Direktwahl")
  
merged_wk  <- schrittweiser_merge(survey_wk, bio_wk, c("wp","partei_kurz", "wkr_nummer"))  
  

# List
survey_list <-  df_survey |>
    tidylog::filter(kand_typ == "Liste"| kand_typ == "beides") |>
    tidylog::filter(! id_survey %in% merged_wk$id_survey)
  
bio_list <- df_bio |>
    tidylog::filter(mandatsart == "Landesliste")
  
merged_list <- schrittweiser_merge(survey_list, bio_list, c("wp","partei_kurz", "geburtsjahr", "geschlecht", "liste", "familienstand", "religion"))

mp_data_Kandidatenstudie <- merged_wk |>
  bind_rows(merged_list) |>
  select(- contains(".y"), - contains("_y"), -contains("_x"))

mp_data <- df_bio |>
  anti_join(mp_data_Kandidatenstudie, by = c("id", "wp")) |>
  bind_rows(mp_data_Kandidatenstudie) |>
  mutate(party = partei_kurz) |>
  mutate(party = ifelse(party == "CSU", "CDU", party))
  
##- Party Leadership -----------------------------------------------------------

party_leader <- party_leader |>
  mutate(party_leader_1 = ifelse(position %in% c(
    "Fraktionsvorsitzender",
    "Parlamentarischer Geschäftsführer", "Parteivorsitzender",
    "Generalsekretär", "Bundesgeschäftsführer", "Präsident des Deutschen Bundestags",
    "Vizepräsident des Deutschen Bundestags"),1,0),
    party_leader_2 = ifelse(position %in% c(
      "Fraktionsvorsitzender",
      "Parlamentarischer Geschäftsführer", "Parteivorsitzender", "Stellvertretender Parlamentarischer Geschäftsführer",
      "Generalsekretär", "Bundesgeschäftsführer", "Präsident des Deutschen Bundestags",
      "Stellvertrender Fraktionsvorsitzender", "Präsidiumsmitglied", "Stellvertretender Generalsekretär",
      "Stellvertretender Parteivorsitzender", "Vizepräsident des Deutschen Bundestags"),1,0),
    party_leader_3 = ifelse(! is.na(position),1,0)) |>
  # Every person only one time
  group_by(first_name, last_name) |>
  arrange(party_leader_1) |>
  slice(1) |>
  ungroup() |>
  mutate(party = ifelse(party == "Grünen","GRÜNE", party)) |>
  mutate(wp = term)

###- Merge Party Leadership with MP Information --------------------------------

merge_fastlink <- fastLink(dfA = party_leader, dfB = mp_data,
                    varnames = c("first_name", "last_name", "wp"),
                    stringdist.match = c("first_name", "last_name"),
                    stringdist.method = "jw",
                    partial.match = c("first_name", "last_name"),
                    threshold.match = 0.5)
party_leader <- getMatches(dfA = party_leader, dfB = mp_data, 
                fl.out = merge_fastlink, threshold.match = 0.5)

mp_data <- mp_data |>
  anti_join(party_leader, by = "id") |>
  bind_rows(party_leader) 

#- Speeches --------------------------------------------------------------------

speeches <- speeches |>
  # Add information of MPs
  mutate(politician_id = as.character(politician_id)) |>
  tidylog::left_join(mp_data, by = c("politician_id" = "id")) |>
  filter(!position_short %in% c("Presidium of Parliament", "Guest", "Not found")) |>
  mutate(across(contains("party_leader"), ~ifelse(position_short %in% c("Minister",
                                                                        "Chancellor",
                                                                        "Secretary of State") ,1,.))) |>
  mutate(across(contains("party_leader"), ~ifelse(is.na(.),0,.))) |>
  # government vs opposition
  mutate(government = 0) |>
  mutate(government = ifelse(party %in% c("SPD", "Grüne", "FDP") & wp == 20,1,government)) |>
  mutate(government = ifelse(party %in% c("SPD", "CDU") & wp == 19,1,government)) |>
  mutate(government = ifelse(party %in% c("FDP", "CDU") & wp == 17,1,government)) |>
  filter(!politician_id == -1) |>
  tidylog::filter(date >= as.Date("2009-01-01")) |>
  arrange(electoral_term, session) |>
  group_by(electoral_term, session) |>
  mutate(session_id = cur_group_id()) |>
  ungroup() 

agg_speeches <- expand_grid(
  session_id = unique(speeches$session_id),
  classification = unique(speeches$classification),
  party_leader_2 = unique(speeches$party_leader_2),
  government = unique(speeches$government)
)

agg_speeches <- speeches |>
group_by(date, session_id, classification, party_leader_2, government) |>
  summarise(n_speeches = n()) |>
  ungroup() |>
  right_join(agg_speeches, by =c("session_id", "classification", "government", "party_leader_2")) |>
  mutate(n_speeches = ifelse(is.na(n_speeches), 0, n_speeches)) |>
  group_by(session_id) |>
  arrange(date) |>
  mutate(date = first(date))
  
#- Media  ----------------------------------------------------------------------

media <- media |> filter(!is.na(date)) |>
  filter(!is.na(classification))

data_grid <- expand.grid(date = seq(min(media$date), max(media$date), by = 1),   
                         classification = unique(media$classification))

agg_media <- media |>
  tidylog::filter(date >= as.Date("2009-01-01")) |>
  count(date, classification) |>
  tidylog::right_join(data_grid, by = c("date", "classification")) |>
  replace(is.na(.), 0) |>
  group_by(date) |>
    mutate(share = n/sum(n, na.rm = T)) |>
  ungroup() |>
  arrange(date) |>
  group_by(classification) |>
   mutate(z_score = (share - mean(share, na.rm = T)) / sd(share, na.rm = T)) |>
   mutate(mean_5days = zoo::rollapply(share, width = 5, FUN = mean, align = "right", fill = NA)) |>
   mutate(z_score_5days = zoo::rollapply(z_score, width = 5, FUN = mean, align = "right", fill = NA)) |>
  ungroup() |>
  mutate(date = date - days(1)) 

agg_media_wider <- agg_media |>
  pivot_wider(names_from = classification, values_from = c(share, n, mean_5days, z_score, z_score_5days)) |>
  group_by(date) |>
    summarise(across(everything(), ~if(is.numeric(.)) sum(., na.rm = TRUE) else first(.))) |>
  ungroup() 


#- Combine Speeches and Media --------------------------------------------------
  
data <- speeches |>
  tidylog::left_join(agg_media_wider, by = "date") |>
  mutate(date = as.Date(date))

agg_data <- agg_speeches |>
  tidylog::left_join(agg_media, by = c("date", "classification")) |>
  mutate(date = as.Date(date))

#- Prepare Data for Analyis ----------------------------------------------------

# War analysis

data_war <- data |>
  mutate(date = as.Date(date)) |>
  # filter(date < as.Date("2023-01-01") & date >= as.Date("2021-01-01")) |>
  filter(date < as.Date("2022-08-01") & date >= as.Date("2021-01-01")) |>
  mutate(post_treatment_war = ifelse(date >= as.Date("2022-02-24"),1,0)) |>
  arrange(desc(post_treatment_war), date) |>
  mutate(treatment_war_time_point = first(session_id)) |>
  mutate(time_diff = session_id - treatment_war_time_point) |>
  relocate(post_treatment_war, date, time_diff) |>
  mutate(time_diff = as.factor(time_diff)) |>
  relocate(date, z_score_Defense, share_Defense)  |>
  mutate(treated_topic_war = ifelse(classification %in% c("Defense", "International", "Foreign", "Immigration"),1,0)) |>
  mutate(treatment_war = treated_topic_war * post_treatment_war) |>
  mutate(treated_media_war = mean_5days_Defense + mean_5days_International  + mean_5days_Foreign + mean_5days_Immigration) |>
  mutate(z_score_treated_media_war = (treated_media_war - mean(treated_media_war, na.rm = T)) / sd(treated_media_war, na.rm = T))

agg_data_war <- agg_speeches |>
  mutate(date = as.Date(date)) |>
  filter(date < as.Date("2022-08-01") & date >= as.Date("2021-01-01")) |>
  mutate(post_treatment_war= ifelse(date >= as.Date("2022-02-24"),1,0)) |>
  arrange(desc(post_treatment_war), date) |>
  mutate(treatment_war_time_point = first(session_id)) |>
  mutate(time_diff_war = session_id - treatment_war_time_point) |>
  relocate(post_treatment_war, date, time_diff_war) |>
  mutate(time_diff_war = as.factor(time_diff_war)) 

media_war <- agg_media |>
  mutate(date = as.Date(date)) |>
  filter(date < as.Date("2022-08-01") & date >= as.Date("2021-01-01")) |>
  mutate(treatment_war = ifelse(date == as.Date("2022-02-24"),1,0)) |> 
  mutate(treatment = ifelse((date >= as.Date("2022-02-24") & classification %in% c("Defense", "International", "Foreign", "Immigration")),1,0)) |>
  mutate(week = lubridate::week(date)) |>
  mutate(month = lubridate::month(date)) |>
  mutate(year = lubridate::year(date)) |>
  arrange(year, month) |>
  group_by(year, month) |>
  mutate(year_month = cur_group_id()) |>
  ungroup() |>
  arrange(year, week) |>
  group_by(year, week) |>
  mutate(year_week = cur_group_id()) |>
  mutate(treatment_war_week = ifelse(any(treatment_war == 1),year_week,NA)) |>
  ungroup() |>
  arrange(treatment_war_week) |>
  mutate(treatment_war_week = first(treatment_war_week)) |>
  mutate(time_diff_war = year_week - treatment_war_week) |>
  mutate(time_diff_war = as.factor(time_diff_war))

media_articles_war <- media |>
    mutate(date = as.Date(date)) |>
    filter(date < as.Date("2022-08-01") & date >= as.Date("2021-01-01")) |>
    mutate(treated_topic_war = ifelse(classification %in% c("Defense", "International", "Foreign", "Immigration"),1,0)) |>
    mutate(treatment_war = ifelse((date >= as.Date("2022-02-24") & treated_topic_war == 1),1,0)) 

data_war <- data_war |>
  group_by(politician_id) |>
  mutate(treated_topic_war_lag1 = lag(treated_topic_war),
         treated_topic_war_lag2 = lag(treated_topic_war,2),
         treated_topic_war_lead1 = lead(treated_topic_war),
         treated_topic_war_lead2 = lead(treated_topic_war,2)) |>
  ungroup()


# SUPER GAU analysis (11.03.2011)

data_gau <- data |>
  mutate(date = as.Date(date)) |>
  filter(date < as.Date("2011-09-01") & date >= as.Date("2009-01-01")) |>
  mutate(post_treatment_gau = ifelse(date >= as.Date("2011-03-11"),1,0)) |>
  arrange(desc(post_treatment_gau), date) |>
  mutate(treatment_gau_time_point = first(session_id)) |>
  mutate(time_diff_gau = session_id - treatment_gau_time_point) |>
  relocate(post_treatment_gau, date, time_diff_gau) |>
  mutate(time_diff_gau = as.factor(time_diff_gau)) |>
  mutate(treated_topic_gau = ifelse(classification %in% c("Energy"),1,0)) |>
  mutate(treatment_gau = treated_topic_gau * post_treatment_gau) |>
  mutate(treated_media_gau = mean_5days_Energy) |>
  mutate(z_score_treated_media_gau = (treated_media_gau - mean(treated_media_gau, na.rm = T)) / sd(treated_media_gau, na.rm = T))

agg_gau <- agg_speeches |>
  mutate(date = as.Date(date)) |>
  filter(date < as.Date("2011-09-01") & date >= as.Date("2009-01-01")) |>
  mutate(post_treatment_gau = ifelse(date >= as.Date("2011-03-11"),1,0)) |>
  arrange(desc(post_treatment_gau), date) |>
  mutate(treatment_gau_time_point = first(session_id)) |>
  mutate(time_diff_gau = session_id - treatment_gau_time_point) |>
  relocate(post_treatment_gau, date, time_diff_gau) |>
  mutate(time_diff_gau = as.factor(time_diff_gau))

media_gau <- agg_media |>
  mutate(date = as.Date(date)) |>
  filter(date < as.Date("2011-09-01") & date >= as.Date("2009-01-01")) |>
  mutate(treatment_gau = ifelse(date == as.Date("2011-03-11"),1,0)) |>
  mutate(treatment = ifelse((date >= as.Date("2022-02-24") & classification %in% c("Energy")),1,0)) |>
  mutate(week = lubridate::week(date)) |>
  mutate(month = lubridate::month(date)) |>
  mutate(year = lubridate::year(date)) |>
  arrange(year, month) |>
  group_by(year, month) |>
  mutate(year_month = cur_group_id()) |>
  ungroup() |>
  arrange(year, week) |>
  group_by(year, week) |>
  mutate(year_week = cur_group_id()) |>
  mutate(treatment_gau_week = ifelse(any(treatment_gau == 1),year_week,NA)) |>
  ungroup() |>
  arrange(treatment_gau_week) |>
  mutate(treatment_gau_week = first(treatment_gau_week)) |>
  mutate(time_diff_gau = year_week - treatment_gau_week) |>
  mutate(time_diff_gau = as.factor(time_diff_gau))

media_articles_gau <- media |>
  mutate(date = as.Date(date)) |>
  filter(date < as.Date("2011-09-01") & date >= as.Date("2009-01-01")) |>
  mutate(treated_topic_gau = ifelse(classification %in% c("Energy"),1,0)) |>
  mutate(treatment_gau = ifelse((date >= as.Date("2011-03-11") & treated_topic_gau == 1),1,0)) 

data_gau <- data_gau |>
  group_by(politician_id) |>
  mutate(treated_topic_gau_lag1 = lag(treated_topic_gau),
         treated_topic_gau_lag2 = lag(treated_topic_gau,2),
         treated_topic_gau_lead1 = lead(treated_topic_gau),
         treated_topic_gau_lead2 = lead(treated_topic_gau,2)) |>
  ungroup()

save(data_gau, data_war, media_war, media_gau, media_articles_gau, media_articles_war, file = "data_analysis.RData")
  