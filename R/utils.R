add_select_multiple_start_col <- function(df, col){
  select_multiple_cols_df <- df %>%
    dplyr::select(dplyr::contains(paste0(col, "_"))) %>%
    dplyr::rename_with(~str_replace(.x, paste0(col, "_"), "")) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~{
      dplyr::case_when(.x == 1 ~ dplyr::cur_column(),
                       TRUE ~ NA_character_)
    })) %>%
    tidyr::unite(!!col, dplyr::everything(), sep = " ", na.rm = TRUE) %>%
    dplyr::mutate(!!col := dplyr::if_else(.data[[col]] == "", NA_character_, .data[[col]])) %>%
    dplyr::pull(.data[[col]])

  first_choice_col <- names(df)[grep(paste0(col, "_"), names(df))[1]]

  df %>%
    dplyr::mutate(!!col := select_multiple_cols_df, .before = first_choice_col)
}

scrape_dab_single_url <- function(url){
  message(url)
  success <- FALSE
  while(!success){
    if(exists("survey_html")){
      rm(list = "survey_html")
    }
    tryCatch({
      survey_html <- read_html(url)
      if(exists("survey_html")){
        success <- TRUE
      }},
      error = function(e){
        message(sprintf("Execution stopped due to the following error:\n\n%s", e))
      },
      finally = {
      }
    )
  }

  survey_header_df <- css_class_to_df(survey_html, ".DebrisStats__stat", "Label", "Text")

  survey_info_df <- css_class_to_df(survey_html, ".DebrisData__", "label", "text")

  survey_collected_materials <- survey_html %>%
    html_elements("table") %>%
    html_table() %>%
    map_dfc(function(x){

      material_type <- unique(str_remove(names(x), " materials collected$"))

      tibble(cols = x[[1]], data = x[[2]]) %>%
        pivot_wider(names_from = cols, values_from = data) %>%
        rename_with(~paste0(material_type, "_", .x))
    })

  bind_cols(survey_header_df, survey_info_df, survey_collected_materials)
}

sanitize_string <- function(string){
  string %>%
    str_remove_all(pattern = "\\n") %>%
    stringr::str_trim()
}

css_class_to_df <- function(html_obj, css_class, label, text ){

  col <- html_obj %>%
    html_elements(paste0(css_class, label)) %>%
    html_text() %>%
    sanitize_string()

  data <- html_obj %>%
    html_elements(paste0(css_class, text)) %>%
    html_text() %>%
    sanitize_string()

  tibble(cols = col, data = data) %>%
    pivot_wider(names_from = cols, values_from = data)
}

get_formatted_timestamp <- function(tzone = "CET") {
  current_time <- `attr<-`(Sys.time(), "tzone", tzone)
  gsub(":", "-", sub(" ", "T", current_time))
}

get_latest_file_in_directory <- function(directory){
  data_files_directory <- sort(list.files(directory))

  latest_file_in_directory <- paste0(directory, "/", data_files_directory[length(data_files_directory)])

}

read_latest_csv <- function(directory){
  readr::read_csv(get_latest_file_in_directory(directory))
}


write_csv_if_change <- function(new_df, directory, file_name){
  old_data <- readr::read_csv(get_latest_file_in_directory(directory))

  temp_dir <- tempdir()

  readr::write_csv(new_df, paste(temp_dir, "temp.csv", sep = "/"))

  new_new_df <- readr::read_csv(paste(temp_dir, "temp.csv", sep = "/"))

  diff_new_old_data <- dplyr::setdiff(new_new_df, old_data)

  if(nrow(diff_new_old_data) > 0){
    write_csv(new_df, paste0(directory, "/", file_name,"_", get_formatted_timestamp(), ".csv"))
  }
}
