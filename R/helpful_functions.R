library(dplyr)
library(stringr)


# Data loading and cleaning -----------------------------------------------

# Paths to all responses data
path_responses <- file.path(
  general_params$responses_dir,
  paste0(
    general_params$responses_prefix,
    general_params$date_main,
    general_params$main_suffix
  )
)

path_responses_hard_copies <- file.path(
  general_params$responses_dir,
  paste0(
    general_params$responses_prefix,
    general_params$date_main,
    general_params$hard_copies_flag,
    general_params$main_suffix
  )
)

path_responses_easy <- file.path(
  general_params$responses_dir,
  paste0(
    general_params$responses_prefix,
    general_params$date_easy,
    general_params$easy_suffix
  )
)

path_responses_easy_hard_copies <- file.path(
  general_params$responses_dir,
  paste0(
    general_params$responses_prefix,
    general_params$date_easy,
    general_params$hard_copies_flag,
    general_params$easy_suffix
  )
)


# Function to create a replacement mapping to final
create_replacement_map <- function(manifest, from, to) {
  setNames(manifest[[to]], manifest[[from]])
}

# Function to standardise responses based on a replacement map
standardise_responses <- function(data, cols, replacement_map) {
  data %>%
    mutate(across(all_of(cols), ~ map_chr(.x, function(x) {
      if_else(x %in% names(replacement_map), replacement_map[x], x)
    })))
}

# Function to dynamically filter and pull columns based on a condition
get_columns <- function(manifests_all, filter_expr, col_to_pull) {
  manifests_all %>%
    filter(!!filter_expr) %>%
    pull(!!rlang::sym(col_to_pull))
}

# Loop over each manifest mapping and standardise mappings in main and easyread
standardise_mappings <- function(responses, responses_easy, manifests) {
  for (df_name in names(manifests)) {
    if (df_name != "all") {
      if (df_name == "map_likert") {
        # standardise responses for likert questions (agree, disagree, etc)
        likert_cols_easy <- get_columns(manifests$all, manifests$all$likert, "question_easy")
        likert_cols_main <- get_columns(manifests$all, manifests$all$likert, "question")
        
        replacement_map_easy <- create_replacement_map(manifests[[df_name]], "easy", "final")
        replacement_map_main <- create_replacement_map(manifests[[df_name]], "main", "final")
        
        responses_easy <- standardise_responses(responses_easy, likert_cols_easy, replacement_map_easy)
        responses <- standardise_responses(responses, likert_cols_main, replacement_map_main)
        
      } else {
        # standardise responses for other questions with given mappings
        col_easy <- as.integer(gsub("map", "", df_name))
        col_main <- get_columns(manifests$all, manifests$all$col_id_easy == col_easy, "col_id")
        
        specific_col_easy <- names(responses_easy)[col_easy]
        specific_col_main <- names(responses)[col_main]
        
        replacement_map_easy <- create_replacement_map(manifests[[df_name]], "easy", "final")
        replacement_map_main <- create_replacement_map(manifests[[df_name]], "main", "final")
        
        responses_easy <- standardise_responses(responses_easy, specific_col_easy, replacement_map_easy)
        responses <- standardise_responses(responses, specific_col_main, replacement_map_main)
      }
    }
  }
  return(list(responses = responses, responses_easy = responses_easy))
}

# Convert free text age into ONS age categories
text_to_age <- function(age_text) {
  age_text <- tolower(str_replace_all(age_text, "\\+|-|\\.|~", " "))
  categorised_ages <- case_when(
    str_detect(age_text, "\\b7[5-9]\\b|\\b8[0-9]\\b|\\b9[0-9]\\b|\\b10[0-9]\\b|\\b11[0-9]\\b|\\b12[0-9]\\b|seventy five|seventy six|seventy seven|seventy eight|seventy nine|eighty|ninety|hundred") ~ "75 or above",
    str_detect(age_text, "\\b6[5-9]\\b|\\b7[0-4]\\b|sixty five|sixty six|sixty seven|sixty eight|sixty nine|seventy|seventy one|seventy two|seventy three|seventy four") ~ "65 to 74",
    str_detect(age_text, "\\b5[5-9]\\b|\\b6[0-4]\\b|fifty five|fifty six|fifty seven|fifty eight|fifty nine|sixty|sixty one|sixty two|sixty three|sixty four") ~ "55 to 64",
    str_detect(age_text, "\\b4[5-9]\\b|\\b5[0-4]\\b|forty five|forty six|forty seven|forty eight|forty nine|fifty|fifty one|fifty two|fifty three|fifty four") ~ "45 to 54",
    str_detect(age_text, "\\b3[5-9]\\b|\\b4[0-4]\\b|thirty five|thirty six|thirty seven|thirty eight|thirty nine|forty|forty one|forty two|forty three|forty four") ~ "35 to 44",
    str_detect(age_text, "\\b2[5-9]\\b|\\b3[0-4]\\b|twenty five|twenty six|twenty seven|twenty eight|twenty nine|thirty|thirty one|thirty two|thirty three|thirty four") ~ "25 to 34",
    str_detect(age_text, "\\b1[6-9]\\b|\\b2[0-4]\\b|sixteen|seventeen|eighteen|nineteen|twenty|twenty one|twenty two|twenty three|twenty four") ~ "16 to 24",
    TRUE ~ NA_character_
  )
  return(categorised_ages)
}

