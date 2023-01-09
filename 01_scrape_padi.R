
tstamp <- get_formatted_timestamp()

padi_dab_survey_list_raw <- read_html("https://www.diveagainstdebris.org/diver/pharaoh-dive-club") %>%
  html_elements('a') %>%
  html_attr("href")

padi_dab_survey_list <- paste0(
  "https://www.diveagainstdebris.org",
  padi_dab_survey_list_raw[grep("^/debris-", padi_dab_survey_list_raw)])

all_surveys <- map_dfr(padi_dab_survey_list, scrape_dab_single_url)

padi_to_oosc <- readr::read_csv("data/oosc_scraped_col_mapping.csv") %>%
  select(col_oosc, col_scrape) %>%
  filter(!is.na(col_oosc)) %>%
  mutate(col_oosc = make.unique(.data[["col_oosc"]], sep = "_")) %>%
  deframe()

scraped_padi_data <- all_surveys %>%
  mutate(survey_id = row_number()) %>%
  rename(all_of(padi_to_oosc))

grouping_duplicated_columns_in_padi <- scraped_padi_data %>%
  select(survey_id, matches("_\\d$")) %>%
  pivot_longer(-survey_id, names_to = c("col", "group"), names_pattern = "(.*?)_(\\d)$") %>%
  group_by(survey_id, col) %>%
  summarise(value = sum(value, na.rm = TRUE),  .groups = "drop") %>%
  pivot_wider(survey_id, names_from = "col")

scraped_padi_data_dupl_on_join <- scraped_padi_data %>%
  select(!matches("_\\d$")) %>%
  left_join(grouping_duplicated_columns_in_padi, by = "survey_id") %>%
  select(survey_id, matches("\\.[x|y]$")) %>%
  pivot_longer(-survey_id, names_to = c("col", "group"), names_pattern = "(.*?).(x|y)$") %>%
  group_by(survey_id, col) %>%
  summarise(value = sum(value, na.rm = TRUE),  .groups = "drop") %>%
  pivot_wider(survey_id, names_from = "col")

survey_site <- readr::read_csv("data/pdc_dive_sites_gps.csv") %>%
  select(name, name_key, town_cities, GPS_coordinates)

oosc_kobo_form <- koboAPI::get_form(oosc_debris_kobo_asset, api = "kobotoolbox")

oosc_kobo_choices <- oosc_kobo_form[["choices"]]

substrate_list <- oosc_kobo_choices %>%
  filter(list_name == "substrate_list") %>%
  pull(name) %>%
  unique()

scraped_data_final <- scraped_padi_data %>%
  select(survey_id, leader_name:wave_conditions) %>%
  left_join(scraped_padi_data_dupl_on_join, by = "survey_id") %>%
  mutate(total_weight = as.numeric(str_extract(total_weight, "\\d+\\.*\\d*")),
         survey_site = case_when(
           survey_site == "ADS168 - Abu Sauatir" ~  "Abu Sauatir",
           TRUE ~ survey_site
         ),
         other_organisation = case_when(
           oosc %in% c("Pharaoh Dive Club", "Pharaoh Dice Club") ~ "Pharaoh Dive Club",
           TRUE ~ NA_character_
         ),
         oosc = case_when(
           oosc %in% c("Pharaoh Dive Club", "Pharaoh Dice Club") ~ "no",
           TRUE ~ oosc
         ),
         survey_site = case_when(
           survey_site == "Abu Hamra" ~ "Abu Hamara",
           survey_site == "Abu sauatir" ~ "Abu Sauatir",
           TRUE ~ survey_site
         ),
         today = dmy(start),
         survey_time = as.integer(stringr::str_extract(survey_time, "\\d+")),
         dominant_substrate = case_when(
           dominant_substrate %in% !!substrate_list ~ dominant_substrate,
           TRUE ~ "other"
         ),
         weather_conditions_sunny = case_when(
           str_detect(weather_conditions, "[Ss]unny") ~ 1,
           TRUE ~ 0
         ),
         weather_conditions_windy = case_when(
           str_detect(weather_conditions, "[Ww]indy") ~ 1,
           TRUE ~ 0
         ),
         weather_conditions_warm = case_when(
           str_detect(weather_conditions, "[Ww]arm|[Hh]ot|°C|degrees") ~ 1,
           TRUE ~ 0
         ),
         weather_conditions_breeze = case_when(
           str_detect(weather_conditions, "[Bb]reeze") ~ 1,
           TRUE ~ 0
         ),
         weather_conditions_cold = case_when(
           str_detect(weather_conditions, "[Cc]old") ~ 1,
           TRUE ~ 0
         ),
         weather_conditions_rain = case_when(
           str_detect(weather_conditions, "[Rr]ain") ~ 1,
           TRUE ~ 0
         ),
         weather_conditions_weather_dontknow = case_when(
           is.na(weather_conditions) ~ 1,
           TRUE ~ 0
         ),
         dive_depth = as.numeric(str_remove(str_match(dive_depth, "[^–]+$"), " meters$")),
         beach_dive_line = dplyr::case_when(
           dive_depth > 0 ~ "dive",
           TRUE ~ "beach"
         ),
         beach_ecosystem = case_when(
           beach_ecosystem == "coral reef" ~ "coral_reef",
           beach_ecosystem == "Desert" ~ "sand",
           beach_ecosystem == "coral/rocky reef" ~ "coral_reef",
           TRUE ~ beach_ecosystem
         ),
         wave_conditions = case_when(
           wave_conditions == "Calm (glassy to rippled) for waves 0 – 0.1 meter high" ~ "calm",
           wave_conditions == "Smooth (wavelets) for waves 0.1 - 0.5 meter high" ~ "calm",
           wave_conditions == "Slight for waves 0.5 -1.25 meter high" ~ "moderate",
          TRUE ~ wave_conditions
         ),
         survey_area = as.numeric(str_remove(survey_area, " m2")),
         participants = as.integer(participants)
  ) %>%
  add_select_multiple_start_col("weather_conditions") %>%
  left_join(survey_site, by = c("survey_site" = "name")) %>%
  mutate(
    name_key = case_when(
      is.na(name_key) & !is.na(survey_site) ~ survey_site,
      TRUE ~ name_key
    )
  ) %>%
  select(-survey_site, -town_village, -Country, -start, -pre_loaded_GPS_coordinates) %>%
  rename(survey_site = name_key, town_village = town_cities, survey_area_m2 = survey_area)


data_files_padi_surveys <- sort(list.files("data/padi_surveys/"))

lastest_padi_survey <- data_files_padi_surveys[length(data_files_padi_surveys)]

old_scraped_padi_data <- readr::read_csv(paste0("data/padi_surveys/", lastest_padi_survey))

diff_new_old_scrapped_data <- dplyr::setdiff(scraped_data_final, old_scraped_padi_data)

if(nrow(diff_new_old_scrapped_data) > 0){
  write_csv(scraped_data_final, paste0("data/padi_surveys/padi_dab_pdc_survey_data_", tstamp, ".csv"))
}

