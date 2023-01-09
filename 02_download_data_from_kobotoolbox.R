devtools::load_all(".")
i
tstamp <- get_formatted_timestamp()

oosc_asset_lists <- kobo_asset_list()

oosc_debris_kobo_asset <- oosc_asset_lists %>%
  filter(name == "Open Ocean Science Centre Debris Collection Tool") %>%
  filter(date_created == max(date_created)) %>%
  pull(uid)

oosc_kobo_data <- robotoolbox::kobo_data(oosc_debris_kobo_asset) %>%
  mutate(survey_area_length = as.numeric(str_extract(survey_area, "^[\\d]+")),
         survey_area_width = as.numeric(str_extract(survey_area, "(\\d+)(?!.*\\d)")),
         survey_area_m2 = survey_area_length * survey_area_width,
         start = lubridate::as_datetime(start),
         end = lubridate::as_datetime(end),
         `_submission_time` = lubridate::as_datetime(`_submission_time`),
         today = as.Date(today),
         across(where(is.character), sanitize_string))

write_csv_if_change(oosc_kobo_data, "data/oosc_surveys", "oosc_kobo_survey_data")
