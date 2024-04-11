library(DHSClogger)
library(dplyr)
library(readxl)
library(readr)
library(writexl)
library(magrittr)
library(purrr)
library(tidyr)

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
  ) %>% 
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
  
  # Read in easyread and bind with easyread hard copy responses
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
  ) %>% 
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
            map_chr(x, ~ if_else(.x %in% names(replacement_map), replacement_map[.x], .x))
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
            map_chr(x, ~ if_else(.x %in% names(replacement_map), replacement_map[.x], .x))
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
  
  # Fix ages in free text easyread
  
  
  
  # Join into one big table
  responses_all <- bind_rows(responses, responses_easy) %>% 
    select(all_of(names(responses)))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  colnames_main <- manifest[["Question"]]
  colnames_easyread <- easy_manifest[["Question"]]
  
  responses <- setNames(responses, colnames_main)
  easy_responses <- setNames(easy_responses, colnames_easyread)
  
  # I convert both dataframes to all character but in responses I also convert
  # the value nan to NA
  responses <- responses %>%
    select(all_of(colnames_main)) %>%
    mutate(across(everything(), as.character))
  
  easy_responses <- easy_responses %>%
    select(all_of(colnames_easyread)) %>%
    mutate(across(everything(), as.character))
  
  # Now clean up the main manifest - used for both easy and main responses
  # because easy choices mapped to their main equivalent in code below
  
  full_manifest <- manifest %>%
    dplyr::filter(`Analysis group` != "None") %>%
    dplyr::select(Question, Choices) %>%
    dplyr::mutate(Choices = gsub("NA", "", Choices),
                  Choices = gsub("^,( )?", "", Choices),
                  Choices = gsub(",( )?$", "", Choices)) %>%
    # Save each choice option separately
    tidyr::separate(Choices, into = c(letters, paste0(letters, letters)),
                    sep = '",\\s"(?=[a-zA-Z0-9])',
                    fill = "right") %>%
    tidyr::pivot_longer(cols = -Question,
                        names_to = "letters",
                        values_to = "Choices") %>%
    dplyr::mutate(Choices = gsub('"', '', Choices)) %>%
    dplyr::select(-letters) %>%
    dplyr::filter(!is.na(Choices) & Choices != "")
  
  full_manifest <- split(full_manifest$Choices, full_manifest$Question)
  full_manifest <- lapply(full_manifest, function(x) x[!is.na(x)])
  
  # Next step is to clean up the easy data and then merge with main responses
  colnames(responses) <- gsub("’", "'", colnames(responses))
  colnames(easy_responses) <- gsub("’", "'", colnames(easy_responses))
  
  # sort out the age field - it has dates, ages, ages in text, years and months ....
  easy_responses_c <- amend_easy_dates(easy_responses)
  
  # this code maps the easy read options to their main response equivalents
  easy_responses_c <- amend_easy_options(easy_responses_c)
  
  # add capacity as new field to easy reads as they are all easy responses
  easy_responses_c <- easy_responses_c %>%
    mutate(`In what capacity are you responding to this survey?` = "An individual sharing my personal views and experiences including carers and family members")
  
  # The easyread mistakenly includes columns that should have been multi choice but
  # were instead setup to be free format text. It was agreed that these text fields
  # will be merged into existing topic columns fields
  easy_responses_c <- merge_topic_columns(easy_responses_c)
  
  #Join digital main and easy responses
  responses_all <- dplyr::bind_rows(responses, easy_responses_c)
  
  if (general_params$include_hard_copies) {
    #Join combined and hard main copies
    responses_all <- dplyr::bind_rows(responses_all, main_hard_copy_responses)
    
    #Join combined and hard easy read copies (mapped)
    responses_all <- dplyr::bind_rows(responses_all, easy_hard_copy_responses_mapped)
  }
  
  # I need to mutate 3 values in the following field because they have dodgy values
  responses_all <- responses_all %>%
    dplyr::mutate(`How long have you had ME/CFS?` = if_else(`How long have you had ME/CFS?` == "Less than a year",
                                                            "Less than a year?",
                                                            `How long have you had ME/CFS?`))
  
  
  # save this version in case I want to create a file for manual checking
  responses_for_manual_checking <- responses_all
  
  # Remove unnecessary rows ----
  # Remove all organisational responses (they will be manually encoded)
  individual_options <- c("An individual sharing my personal views and experiences including carers and family members",
                          "On behalf of someone else")
  
  # add section to extract org responses + devolved
  org_responses <- responses_all %>%
    dplyr::filter(!(`In what capacity are you responding to this survey?` %in% individual_options)) %>%
    dplyr::filter(!is.na(Completed))
  
  wales_responses <- responses_all %>%
    dplyr::filter(`Where do you live in the UK?` == "Wales") %>%
    dplyr::filter(!is.na(Completed))
  
  scotland_responses <- responses_all %>%
    dplyr::filter(`Where do you live in the UK?` == "Scotland") %>%
    dplyr::filter(!is.na(Completed))
  
  ni_responses <- responses_all %>%
    dplyr::filter(`Where do you live in the UK?` == "Northern Ireland") %>%
    dplyr::filter(!is.na(Completed))
  
  #write labelling summary to excel
  write_xlsx(x = wales_responses, path = "output/responses_wales.xlsx")
  write_xlsx(x = scotland_responses, path = "output/responses_scotland.xlsx")
  write_xlsx(x = ni_responses, path = "output/responses_ni.xlsx")
  write_xlsx(x = org_responses, path = "output/responses_org.xlsx")
  
  responses_all <- responses_all %>%
    dplyr::filter(`In what capacity are you responding to this survey?` %in% individual_options)
  # Remove any responses that haven't been completed
  responses_all <- responses_all %>%
    dplyr::filter(!is.na(Completed))
  
  # Drop all-empty columns - but first remove columns that should only be in organisational responses
  # but for some reason these values have been populated for a single row in the individual responses
  responses_all <- responses_all %>%
    select(-c(`Which of the following best describes your area of work?`,
              `Where does your organisation provide services?`,
              `Where do you work as a professional?`,
    ))
  
  responses_all <- responses_all[, colSums(is.na(responses_all)) != nrow(responses_all)]
  
  # Output frequency table pre-location filtering
  responses_all %>%
    dplyr::group_by(`Where do you live in the UK?`,
                    `Which area of England do you live in?`) %>%
    dplyr::summarise(Frequency = n(), .groups = "drop") %>%
    write.csv(., file = paste0(general_params$output_frequency, Sys.Date(), "-orig-location-freq.csv"))
  
  # previously filtered here for only England
  # add filter exlcuding non UK responses
  responses_ord <- responses_all %>%
    dplyr::filter(`Where do you live in the UK?` != "I live outside the UK")
  
  rm(list = ls()[!(ls() %in% c("return_list", "responses_ord", "full_manifest",
                               "responses_regex", "responses_regex_mult", "responses_for_manual_checking",
                               "general_params"))])
  gc()
  
  return_list$responses_ord <- responses_ord
  return_list$responses_for_manual_checking <- responses_for_manual_checking
  return_list$full_manifest <- full_manifest
  return_list$general_params <- general_params
  
  return(return_list)
}

#' amend_easy_dates - ages in the easyread file are in a free text field and so need to be converted
#' to the same age range standard as using in the main responses
#'
#' @param df - the easyread dataframe
#'
#' @return - the easyread dataframe with ages converted/standardized
amend_easy_dates <- function(df) {
  
  df <- df %>%
    dplyr::mutate(`What is your age?` = gsub("years old", "", `What is your age?`),
                  `What is your age?` = gsub("year old", "", `What is your age?`),
                  `What is your age?` = gsub("years", "", `What is your age?`),
                  `What is your age?` = gsub("yrs", "", `What is your age?`),
                  `What is your age?` = gsub("Yrs", "", `What is your age?`),
                  `What is your age?` = gsub("\\+","", `What is your age?`),
                  `What is your age?` = gsub("\\.","", `What is your age?`),
                  `What is your age?` = gsub("54 years old ","54", `What is your age?`),
                  `What is your age?` = gsub("81 years","81", `What is your age?`),
                  `What is your age?` = gsub("Old enough","", `What is your age?`),
                  `What is your age?` = gsub("50-55","53", `What is your age?`),
                  `What is your age?` = gsub(" ","", `What is your age?`)) %>%
    dplyr::mutate(`What is your age?` = dplyr::case_when(
      grepl('(16)|(17)|(18)|(19)|(20)|(21)|(22)|(23)|(24)', `What is your age?`) ~ "16 to 24",
      grepl('(25)|(26)|(27)|(28)|(29)|(30)|(31)|(32)|(33)|(34)', `What is your age?`) ~ "25 to 34",
      grepl('(35)|(36)|(37)|(38)|(39)|(40)|(41)|(42)|(43)|(44)', `What is your age?`) ~ "35 to 44",
      grepl('(45)|(46)|(47)|(48)|(49)|(50)|(51)|(52)|(53)|(54)', `What is your age?`) ~ "45 to 54",
      grepl('(55)|(56)|(57)|(58)|(59)|(60)|(61)|(62)|(63)|(64)', `What is your age?`) ~ "55 to 64",
      grepl('(65)|(66)|(67)|(68)|(69)|(70)|(71)|(72)|(73)|(74)', `What is your age?`) ~ "65 to 74",
      grepl('(75)|(76)|(77)|(78)|(79)|(80)|(81)|(82)|(83)|(84)', `What is your age?`) ~ "75 or above",
      grepl('(85)|(86)|(87)|(88)|(89)|(90)|(91)|(92)|(93)|(94)', `What is your age?`) ~ "75 or above",
      grepl('(95)|(96)|(97)|(98)|(99)|(100)|(101)|(102)|(103)|(104)', `What is your age?`) ~ "75 or above",
      TRUE ~ ""
    ))
  
  return(df)
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

#' amend_easy_options - the names/labels given choices available in the easyread survey are (inexplicably) different
#' to those in the main survey and so we need to map the easy choices to their main equivalent so they are consistent.
#'
#' @param df - the easyread dataframe
#'
#' @return - an amended easyread dataframe
amend_easy_options <- function(df) {
  
  df <- df %>%
    dplyr::mutate(`research_agree_disagree` = gsub("Definitely agree", "Strongly agree", `research_agree_disagree`),
                  `research_agree_disagree` = gsub("Definitely disagree", "Strongly disagree", `research_agree_disagree`),
                  
                  `attitudes_agree_disagree` = gsub("Definitely agree", "Strongly agree", `attitudes_agree_disagree`),
                  `attitudes_agree_disagree` = gsub("Definitely disagree", "Strongly disagree", `attitudes_agree_disagree`),
                  
                  `life_quality_agree_disagree` = gsub("Definitely agree", "Strongly agree", `life_quality_agree_disagree`),
                  `life_quality_agree_disagree` = gsub("Definitely disagree", "Strongly disagree", `life_quality_agree_disagree`),
                  
                  `support_young_agree_disagree` = gsub("Definitely agree", "Strongly agree", `support_young_agree_disagree`),
                  `support_young_agree_disagree` = gsub("Definitely disagree", "Strongly disagree", `support_young_agree_disagree`),
                  
                  `health_services_agree_disagree` = gsub("Definitely agree", "Strongly agree", `health_services_agree_disagree`),
                  `health_services_agree_disagree` = gsub("Definitely disagree", "Strongly disagree", `health_services_agree_disagree`),
                  
                  `adult_care_agree_disagree` = gsub("Definitely agree", "Strongly agree", `adult_care_agree_disagree`),
                  `adult_care_agree_disagree` = gsub("Definitely disagree", "Strongly disagree", `adult_care_agree_disagree`),
                  
                  `employment_agree_disagree` = gsub("Definitely agree", "Strongly agree", `employment_agree_disagree`),
                  `employment_agree_disagree` = gsub("Definitely disagree", "Strongly disagree", `employment_agree_disagree`),
                  
                  `welfare_agree_disagree` = gsub("Definitely agree", "Strongly agree", `welfare_agree_disagree`),
                  `welfare_agree_disagree` = gsub("Definitely disagree", "Strongly disagree", `welfare_agree_disagree`),
                  
                  `To what extent do you agree or disagree with the actions for improving research?` = gsub("Definitely agree", "Strongly agree", `To what extent do you agree or disagree with the actions for improving research?`),
                  `To what extent do you agree or disagree with the actions for improving research?` = gsub("Definitely disagree", "Strongly disagree", `To what extent do you agree or disagree with the actions for improving research?`),
                  
                  `To what extent do you agree or disagree with the actions for improving attitudes and education?` = gsub("Definitely agree", "Strongly agree", `To what extent do you agree or disagree with the actions for improving attitudes and education?`),
                  `To what extent do you agree or disagree with the actions for improving attitudes and education?` = gsub("Definitely disagree", "Strongly disagree", `To what extent do you agree or disagree with the actions for improving attitudes and education?`),
                  
                  `To what extent do you agree or disagree with the actions for improving statutory support?` = gsub("Definitely agree", "Strongly agree", `To what extent do you agree or disagree with the actions for improving statutory support?`),
                  `To what extent do you agree or disagree with the actions for improving statutory support?` = gsub("Definitely disagree", "Strongly disagree", `To what extent do you agree or disagree with the actions for improving statutory support?`),
                  
                  # NB: "prefer not to say" option for the main survey, which doesn't have a corresponding answer in the easy read
                  `Which of the following best describes you?` = gsub("I am a family member, friend or supporter of someone with ME/CFS but I do not provide care for them", "I am a family member, friend or other supporter (including advocate) of someone with ME/CFS, without regular caring responsibilities", `Which of the following best describes you?`),
                  `Which of the following best describes you?` = gsub("I am or have been an unpaid carer for someone with ME/CFS", "I am an unpaid carer for someone who has ME/CFS or has had ME/CFS in the past", `Which of the following best describes you?`),
                  `Which of the following best describes you?` = gsub("I have had ME/CFS in the past", "I had ME/CFS in the past but am now in remission or recovered", `Which of the following best describes you?`),
                  `Which of the following best describes you?` = gsub("I have or I think I have ME/CFS", "I have suspected or diagnosed ME/CFS", `Which of the following best describes you?`),
                  
                  `How would you describe the severity of your symptoms, on average over the past 3 months?` = gsub("Not very bad", "Mild", `How would you describe the severity of your symptoms, on average over the past 3 months?`),
                  `How would you describe the severity of your symptoms, on average over the past 3 months?` = gsub("A little bad", "Moderate", `How would you describe the severity of your symptoms, on average over the past 3 months?`),
                  `How would you describe the severity of your symptoms, on average over the past 3 months?` = gsub("I am/ they are getting better from the effects of ME/CFS", "In remission", `How would you describe the severity of your symptoms, on average over the past 3 months?`),
                  `How would you describe the severity of your symptoms, on average over the past 3 months?` = gsub("Bad", "Severe", `How would you describe the severity of your symptoms, on average over the past 3 months?`),
                  `How would you describe the severity of your symptoms, on average over the past 3 months?` = gsub("Very bad", "Very severe", `How would you describe the severity of your symptoms, on average over the past 3 months?`),
                  `How would you describe the severity of your symptoms, on average over the past 3 months?` = gsub("I do not want to say", "Prefer not to say", `How would you describe the severity of your symptoms, on average over the past 3 months?`),
                  `How would you describe the severity of your symptoms, on average over the past 3 months?` = gsub("I do not know", "Don't know", `How would you describe the severity of your symptoms, on average over the past 3 months?`),
                  `How would you describe the severity of your symptoms, on average over the past 3 months?` = gsub("I am better now", "Recovered", `How would you describe the severity of your symptoms, on average over the past 3 months?`),
                  # NB: This easy response doesn't have a natural main equivalent example so is assigned to "Don't know"
                  `How would you describe the severity of your symptoms, on average over the past 3 months?` = gsub("I do not have and have never had ME/CFS", "Don't know", `How would you describe the severity of your symptoms, on average over the past 3 months?`),
                  
                  `How long have you had ME/CFS?` = gsub("I do not want to say", "Prefer not to say", `How long have you had ME/CFS?`),
                  
                  `Where do you live in the UK?` = gsub("Outside of the UK", "I live outside the UK", `Where do you live in the UK?`),
                  
                  `How long have you been an unpaid carer or supporter of the person with ME/CFS?` = gsub("1 to 5 years", "1-5 years", `How long have you been an unpaid carer or supporter of the person with ME/CFS?`),
                  `How long have you been an unpaid carer or supporter of the person with ME/CFS?` = gsub("6 to 10 years", "6-10 years", `How long have you been an unpaid carer or supporter of the person with ME/CFS?`),
                  `How long have you been an unpaid carer or supporter of the person with ME/CFS?` = gsub("11 to 20 years", "11-20 years", `How long have you been an unpaid carer or supporter of the person with ME/CFS?`),
                  `How long have you been an unpaid carer or supporter of the person with ME/CFS?` = gsub("I do not want to say", "Prefer not to say", `How long have you been an unpaid carer or supporter of the person with ME/CFS?`),
                  # NB: This easy response doesn't have a natural main equivalent example so is assigned to "Prefer not to say"
                  `How long have you been an unpaid carer or supporter of the person with ME/CFS?` = gsub("I am not a carer or supporter of someone with ME/CFS", "Prefer not to say", `How long have you been an unpaid carer or supporter of the person with ME/CFS?`),
                  
                  `What is your sex?` = gsub("I do not want to say", "Prefer not to say", `What is your sex?`),
                  
                  `Is the gender you identify with the same as your sex registered at birth?` = gsub("I do not want to say", "Prefer not to say", `Is the gender you identify with the same as your sex registered at birth?`),
                  
                  `What is your ethnic group?` = gsub("Asian", "Asian or British Asian - includes Indian, Pakistani, Bangladeshi, Chinese or any other Asian background", `What is your ethnic group?`),
                  `What is your ethnic group?` = gsub("Black", "Black, black British, Caribbean, African or any other black background", `What is your ethnic group?`),
                  `What is your ethnic group?` = gsub("From more than 1 ethnic background", "Mixed or multiple ethnic groups - includes white and black Caribbean, white and black African, white and Asian or any other mixed or multiple background", `What is your ethnic group?`),
                  `What is your ethnic group?` = gsub("Other", "Other, includes Arab or any other ethnic group", `What is your ethnic group?`),
                  `What is your ethnic group?` = gsub("I do not want to say", "Prefer not to say", `What is your ethnic group?`),
                  `What is your ethnic group?` = gsub("White", "White - includes British, Northern Irish, Irish, Gypsy, Irish Traveller, Roma or any other white background", `What is your ethnic group?`),
                  
    )
  
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



