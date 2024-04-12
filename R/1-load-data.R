library(DHSClogger)
library(dplyr)
library(readxl)
library(readr)
library(writexl)
library(magrittr)
library(purrr)
library(tidyr)
library(stringr)
library(english)

#' run_load_data - this function is responsible for loading and combining the 
#' main and easyread manifest and response files.
#' It gets the names for the input files from config.yml
#'
#' @return a list containing merged responses, the merged manifest and the config parameters
run_load_data <- function() {
  
  logger$info("In run_load_data")
  
  # Read the config file
  config <- yaml::read_yaml(
    file.path("input", "config.yml")
  )
  
  general_params <- config$general_params
  
  # Read in main and bind with main hard copy responses
  responses <- read_xlsx(
    file.path(
      general_params$responses_dir,
      paste0(
        general_params$responses_prefix,
        general_params$date_main,
        general_params$main_suffix
      )
    ), 
    .name_repair = "unique_quiet"
  )
  if (general_params$include_hard_copies) {
    responses <- responses %>% 
      bind_rows(
        read_xlsx(
          file.path(
            general_params$responses_dir,
            paste0(
              general_params$responses_prefix,
              general_params$date_main,
              general_params$hard_copies_flag,
              general_params$main_suffix
            )
          ), 
          .name_repair = "unique_quiet"
        )
      )
  }
  
  # Read in easyread and bind with easyread hard copy responses
  if (general_params$include_easyread) {
    responses_easy <- read_xlsx(
      file.path(
        general_params$responses_dir,
        paste0(
          general_params$responses_prefix,
          general_params$date_easy,
          general_params$easy_suffix
        )
      ), 
      .name_repair = "unique_quiet"
    )
    if (general_params$include_hard_copies) {
      responses_easy <- responses_easy %>% 
        bind_rows(
          read_xlsx(
            file.path(
              general_params$responses_dir,
              paste0(
                general_params$responses_prefix,
                general_params$date_easy,
                general_params$hard_copies_flag,
                general_params$easy_suffix
              )
            ), 
            .name_repair = "unique_quiet"
          )
        )
    }
    
    # Read in data manifest sheets
    manifest_path <- file.path(
      general_params$manifest_dir,
      general_params$manifest_file
    )
    
    manifest_sheets <- excel_sheets(manifest_path)
    
    manifests <- setNames(lapply(manifest_sheets, function(sheet_name) {
      read_xlsx(manifest_path, sheet = sheet_name, na = c("", "NA"))
    }), manifest_sheets)
    
    # # Clean up the easyread responses to fit format of the main responses
    for (df_name in names(manifests)) {
      
      # Skip 'all' as it's for general mappings
      if (df_name != "all") {
        
        # Swap out easyread responses in all likert questions
        if (df_name == "easy_likert") {
          likert_cols <- manifests$all %>% 
            filter(likert_easy) %>% 
            pull(question_easy)
          
          replacement_map <- setNames(
            manifests[[df_name]]$main,
            manifests[[df_name]]$easy
          )
          
          responses_easy <- responses_easy %>%
            mutate(across(all_of(likert_cols), function(x) {
              map_chr(x, ~ if_else(
                .x %in% names(replacement_map),
                replacement_map[.x],
                .x
              ))
            }))
          
        } else {
          # Swap out easyread responses in all other questions with given mappings
          col_id_easy <- as.integer(gsub("easy", "", df_name))
          specific_col <- names(responses_easy)[col_id_easy]
          
          replacement_map <- setNames(
            manifests[[df_name]]$main,
            manifests[[df_name]]$easy
          )
          
          responses_easy <- responses_easy %>%
            mutate(across(all_of(specific_col), function(x) {
              map_chr(x, ~ if_else(
                .x %in% names(replacement_map),
                replacement_map[.x],
                .x
              ))
            }))
        }
      }
    }
    
    # Change header names to fit main
    responses_easy <- responses_easy %>%
      setNames(
        setNames(
          as.character(
            manifests$all$question[!is.na(manifests$all$col_id_easy)]
          ),
          names(.)[manifests$all$col_id_easy[!is.na(manifests$all$col_id_easy)]]
        )
      )
    
    # Add assumed fields (capacity is individual for easyreads)
    responses_easy <- responses_easy %>% 
      mutate(
        `In what capacity are you responding to this survey?` = 
          "An individual sharing my personal views and experiences"
      )
    
    # Fix ages in free text easyread
    responses_easy <- responses_easy %>%
      mutate(
        `What is your age?` = tolower(
          str_replace_all(`What is your age?`, "\\+|-|\\.|~", " ")
        ),
        `What is your age?` = case_when(
          str_detect(
            str_detect(
              `What is your age?`, 
              "\\b7[5-9]\\b|\\b8[0-9]\\b|\\b9[0-9]\\b|\\b10[0-9]\\b|\\b11[0-9]\\b|\\b12[0-9]\\b|seventy five|seventy six|seventy seven|seventy eight|seventy nine|eighty|ninety|hundred"
            ) ~ "75 or above",
            str_detect(
              `What is your age?`,
              "\\b6[5-9]\\b|\\b7[0-4]\\b|sixty five|sixty six|sixty seven|sixty eight|sixty nine|seventy|seventy one|seventy two|seventy three|seventy four"
            ) ~ "65 to 74",
            str_detect(
              `What is your age?`,
              "\\b5[5-9]\\b|\\b6[0-4]\\b|fifty five|fifty six|fifty seven|fifty eight|fifty nine|sixty|sixty one|sixty two|sixty three|sixty four"
            ) ~ "55 to 64",
            str_detect(
              `What is your age?`,
              "\\b4[5-9]\\b|\\b5[0-4]\\b|forty five|forty six|forty seven|forty eight|forty nine|fifty|fifty one|fifty two|fifty three|fifty four"
            ) ~ "45 to 54",
            str_detect(
              `What is your age?`,
              "\\b3[5-9]\\b|\\b4[0-4]\\b|thirty five|thirty six|thirty seven|thirty eight|thirty nine|forty|forty one|forty two|forty three|forty four"
            ) ~ "35 to 44",
            str_detect(
              `What is your age?`,
              "\\b2[5-9]\\b|\\b3[0-4]\\b|twenty five|twenty six|twenty seven|twenty eight|twenty nine|thirty|thirty one|thirty two|thirty three|thirty four"
            ) ~ "25 to 34",
            `What is your age?`,
            "\\b1[6-9]\\b|\\b2[0-4]\\b|sixteen|seventeen|eighteen|nineteen|twenty|twenty one|twenty two|twenty three|twenty four"
          ) ~ "16 to 24",
          TRUE ~ NA
        )
      )
    
    
    # Join into one big table
    responses_all <- bind_rows(responses, responses_easy) %>% 
      select(all_of(names(responses)))
  } else { # If there's no easyread data to deal with
    responses_all <- responses
  }
  
  
  
  
  # need code to squish mappings when there are one-many, many-one, or many-many
  # I.e. the weird North easyread could be several so must be squished to only north
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Remove all organisational responses (they will be manually encoded)
  individual_options <- c(
    "An individual sharing my personal views and experiences including carers and family members",
    "On behalf of someone else"
  )
  
  # add section to extract org responses + devolved
  org_responses <- responses_all %>%
    filter(
      !(`In what capacity are you responding to this survey?` %in% individual_options),
      !is.na(Completed)
    )
  
  responses_all <- responses_all %>%
    filter(`In what capacity are you responding to this survey?` %in% individual_options)
  # Remove any responses that haven't been completed
  responses_all <- responses_all %>%
    filter(!is.na(Completed))
  
  # Drop all-empty columns - but first remove columns that should only be in organisational responses
  # but for some reason these values have been populated for a single row in the individual responses
  responses_all <- responses_all %>%
    select(-c(
      `Which of the following best describes your area of work?`,
      `Where does your organisation provide services?`,
      `Where do you work as a professional?`,
    ))
  
  responses_all <- responses_all[, colSums(is.na(responses_all)) != nrow(responses_all)]
  
  # Output frequency table pre-location filtering
  responses_all %>%
    group_by(
      `Where do you live in the UK?`,
      `Which area of England do you live in?`
    ) %>%
    dplyr::summarise(Frequency = n(), .groups = "drop") %>%
    write.csv(., file = paste0(general_params$output_frequency, Sys.Date(), "-orig-location-freq.csv"))
  
  # previously filtered here for only England
  # add filter exlcuding non UK responses
  responses_ord <- responses_all %>%
    dplyr::filter(`Where do you live in the UK?` != "I live outside the UK")
  
}










merge_topic_columns <- function(df) {
  
  df <- df %>%
    mutate(research_further_comments = if_else(is.na(`What do you think could make this part of the plan better - research?`), research_further_comments,
                                               paste0(research_further_comments, " ", `What do you think could make this part of the plan better - research?`)),
           attitudes_further_comments = if_else(is.na(`What do you think could make this part of the plan better - attitudes?`), attitudes_further_comments,
                                                paste0(attitudes_further_comments, " ", `What do you think could make this part of the plan better - attitudes?`)),
           life_quality_further_comments = if_else(is.na(`What do you think could make this part of the plan better - life quality?`), life_quality_further_comments,
                                                   paste0(life_quality_further_comments, " ", `What do you think could make this part of the plan better - life quality?`)),
           support_young_further_comments = if_else(is.na(`What do you think could make this part of the plan better - support young?`), support_young_further_comments,
                                                    paste0(support_young_further_comments, " ", `What do you think could make this part of the plan better - support young?`)),
           health_services_further_comments = if_else(is.na(`What do you think could make this part of the plan better - health services?`), health_services_further_comments,
                                                      paste0(health_services_further_comments, " ", `What do you think could make this part of the plan better - health services?`)),
           adult_care_further_comments = if_else(is.na(`What do you think could make this part of the plan better - adult care?`), adult_care_further_comments,
                                                 paste0(adult_care_further_comments, " ", `What do you think could make this part of the plan better - adult care?`)),
           welfare_further_comments = if_else(is.na(`What do you think could make this part of the plan better - welfare?`), welfare_further_comments,
                                              paste0(welfare_further_comments, " ", `What do you think could make this part of the plan better - welfare?`)),
           employment_further_comments = if_else(is.na(`What do you think could make this part of the plan better - employment?`), employment_further_comments,
                                                 paste0(employment_further_comments, " ", `What do you think could make this part of the plan better - employment?`)),
           comments_improving_research = if_else(is.na(`What do you think could make this part of the plan better - improving research?`), comments_improving_research,
                                                 paste0(comments_improving_research, " ", `What do you think could make this part of the plan better - improving research?`))
    ) %>%
    mutate(research_further_comments = str_replace(research_further_comments, "^NA", ""), # deals with scenario when research_further_comments = NA
           attitudes_further_comments = str_replace(attitudes_further_comments, "^NA", ""),
           life_quality_further_comments = str_replace(life_quality_further_comments, "^NA", ""),
           support_young_further_comments = str_replace(support_young_further_comments, "^NA", ""),
           health_services_further_comments = str_replace(health_services_further_comments, "^NA", ""),
           adult_care_further_comments = str_replace(adult_care_further_comments, "^NA", ""),
           welfare_further_comments = str_replace(welfare_further_comments, "^NA", ""),
           employment_further_comments = str_replace(employment_further_comments, "^NA", ""),
           comments_improving_research = str_replace(comments_improving_research, "^NA", "")
    ) %>%
    select (-c(`What do you think could make this part of the plan better - research?`,
               `What do you think could make this part of the plan better - attitudes?`,
               `What do you think could make this part of the plan better - life quality?`,
               `What do you think could make this part of the plan better - support young?`,
               `What do you think could make this part of the plan better - health services?`,
               `What do you think could make this part of the plan better - adult care?`,
               `What do you think could make this part of the plan better - welfare?`,
               `What do you think could make this part of the plan better - employment?`,
               `What do you think could make this part of the plan better - improving research?`))
  
  return(df)
  
}



transform_easy_hard_copy <-function(easy_hard_copy_responses){
  
  # function to pull in easy hard copy responses
  # drop two columns that refer to email address (not collected in digital responses)
  # drop an agree/disagree column that was duplicated in the hard copy template
  # rename 8 cols to match the main dataset we wish to add it to
  
  easy_hard_copy_responses <- easy_hard_copy_responses %>%
    select(-contains(c("email"))) %>%
    select(-`How much do you agree or disagree with how we think research into ME/CFS could be improved?`) %>%
    rename(
      `To what extent do you agree or disagree with the actions for improving attitudes and education?` = `How much do you agree or disagree with what we think needs to happen about changing the attitudes and education of health and other professionals, to ME/CFS?`,
      `Which of the following best describes you?` = `Which of these describes you best?`,
      `How would you describe the severity of your symptoms, on average over the past 3 months?` = `How would you describe the effects of ME/CFS on you or the person you know with ME/CFS in the past 3 months?`,
      `How long have you had ME/CFS?` = `How long have you or the person you support had ME/CFS?`,
      `Where do you live in the UK?` = `Where do you use services?`,
      `Is the gender you identify with the same as your sex registered at birth?` = `Are you the same gender as when you were born?`,
      `What is your ethnic group?` = `What is your ethnic background?`,
      `What is your age?` = `How old are you?`,
      `To what extent do you agree or disagree with the actions for improving statutory support?` = `How much do you agree or disagree with what we think needs to happen to improve the quality of life for people with ME/CFS?`,
    )
  
  easy_hard_copy_responses$`What is your age?`<- as.character(easy_hard_copy_responses$`What is your age?`)
  
  # Now set values for key demog fields as these were not present in the manual file
  easy_hard_copy_responses <- tibble::rowid_to_column(easy_hard_copy_responses, "temp_id")
  
  # convert all columns to character
  easy_hard_copy_responses <- easy_hard_copy_responses %>%
    select(all_of(colnames(easy_hard_copy_responses))) %>%
    mutate(across(everything(), as.character))
  
  easy_hard_copy_responses <- easy_hard_copy_responses %>%
    mutate(`Response ID` = paste0('hc_easy_', temp_id),
           `Completed` = "2023-10-31 22:59:45",
           `In what capacity are you responding to this survey?` = "An individual sharing my personal views and experiences including carers and family members") %>%
    select(-temp_id)
  
  return(easy_hard_copy_responses)
  
}

transform_main_hard_copy <- function(main_hard_copy_responses) {
  
  # Now set values for key demog fields as these were not present in the manual file
  main_hard_copy_responses <- tibble::rowid_to_column(main_hard_copy_responses, "temp_id")
  
  # convert all columns to character
  main_hard_copy_responses <- main_hard_copy_responses %>%
    select(all_of(colnames(main_hard_copy_responses))) %>%
    mutate(across(everything(), as.character))
  
  main_hard_copy_responses <- main_hard_copy_responses %>%
    mutate(`Response ID` = paste0('hc_main_', temp_id),
           `Completed` = "2023-10-31 22:59:45",
           `Where do you live in the UK?` = if_else(is.na(`Where do you live in the UK?`), "England", `Where do you live in the UK?`),
           `In what capacity are you responding to this survey?` = gsub("As an individual sharing my personal views and experiences",
                                                                        "An individual sharing my personal views and experiences including carers and family members", `In what capacity are you responding to this survey?`)) %>%
    select(-temp_id)
  
  return(main_hard_copy_responses)
  
}



