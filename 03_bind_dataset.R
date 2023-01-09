tstamp <- get_formatted_timestamp()

oosc_kobo_data <- read_latest_csv("data/oosc_surveys")

padi_scraped_data <- read_latest_csv("data/padi_surveys")

final_data <- bind_rows(oosc_kobo_data, padi_scraped_data) %>%
  arrange(today) %>%
  mutate(gps_coordinates = case_when(
    is.na(pre_loaded_GPS_coordinates) & is.na(GPS_coordinates)  ~ alternative_GPS_coordinates,
    is.na(pre_loaded_GPS_coordinates) & is.na(alternative_GPS_coordinates) ~ GPS_coordinates,
    TRUE ~ pre_loaded_GPS_coordinates
    ),
    start = lubridate::as_datetime(start),
    end = lubridate::as_datetime(end),
    `_submission_time` = lubridate::as_datetime(`_submission_time`)
  ) %>%
  separate(gps_coordinates,
           into = c("gps_coordinates_latitude", "gps_coordinates_longitude", "gps_coordinates_altitude", "gps_coordinates_precision"),
           sep = " ") %>%
  mutate(across(starts_with("gps_coordinates_"), as.double))


write_csv_if_change(final_data, "data/final_data", "oosc_debris_data")
