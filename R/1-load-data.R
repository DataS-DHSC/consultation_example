library(DHSClogger)
library(writexl)

# Read the config file
config <- yaml::read_yaml(
  file.path("input", "config.yml"),
  readLines.warn = FALSE
)
general_params <- config$general_params

source("R/helpful_functions.R")

#' run_load_data - this function loads, standardises, and combines the main, 
#' easyread, and respective hard copy data into one clean table.
#' 
#' It gets the names for the input files from config.yml
#' It uses the manifest excel file to understand the data.
#'
run_load_data <- function() {
  
  logger$info("In run_load_data")
  logger$info("Reading in data...")
  # Read in main and bind with main hard copy responses
  responses <- read_xlsx(
    path_responses, 
    .name_repair = "unique_quiet",
    col_types = "text"
  )
  if (general_params$include_hard_copies) {
    responses <- responses %>% 
      bind_rows(
        read_xlsx(
          path_responses_hard_copies, 
          .name_repair = "unique_quiet",
          col_types = "text"
        ) %>% 
          mutate(
            `Response ID` = paste0("main_hard_copy_", row_number()),
            Completed = as.character(as.Date(general_params$date_main))
          )
      )
  }
  
  # Read in easyread and bind with easyread hard copy responses
  if (general_params$include_easyread) {
    responses_easy <- read_xlsx(
      path_responses_easy, 
      .name_repair = "unique_quiet",
      col_types = "text"
    )
    if (general_params$include_hard_copies) {
      responses_easy <- responses_easy %>% 
        bind_rows(
          read_xlsx(
            path_responses_easy_hard_copies, 
            .name_repair = "unique_quiet",
            col_types = "text"
          ) %>% 
            mutate(
              `Response ID` = paste0("easyread_hard_copy_", row_number()),
              Completed = as.character(as.Date(general_params$date_easy))
            )
        )
    }
    logger$info("Data read sucessfully.")
    
    # Read in data manifest sheets
    logger$info("Standardising data using manifest...")
    
    manifest_path <- file.path(
      general_params$manifest_dir,
      general_params$manifest_file
    )
    
    manifest_sheets <- excel_sheets(manifest_path)
    
    manifests <- setNames(lapply(manifest_sheets, function(sheet_name) {
      read_xlsx(manifest_path, sheet = sheet_name, na = c("", "NA"))
    }), manifest_sheets)
    
    # Standardise possible responses (using manifest)
    updated_data <- standardise_mappings(responses, responses_easy, manifests)
    responses <- updated_data$responses
    responses_easy <- updated_data$responses_easy
    
    # Change column headers to fit those in main (using manifest)
    responses_easy <- responses_easy %>%
      setNames(
        setNames(
          as.character(
            manifests$all$question[!is.na(manifests$all$col_id_easy)]
          ),
          names(.)[manifests$all$col_id_easy[!is.na(manifests$all$col_id_easy)]]
        )
      )
    
    # Add assumed fields (using manifest)
    assumed_responses <- manifests$all %>% 
      filter(!is.na(assumed_answer)) %>% 
      select(question, assumed_answer)
    
    for (i in 1:nrow(assumed_responses)) {
      current_question <- assumed_responses$question[i]
      current_choice <- assumed_responses$assumed_answer[i]
      responses_easy[[current_question]] <- current_choice
    }
    
    logger$info("Standardising data further...")
    
    # Fix ages in free text easyread
    responses_easy <- responses_easy %>% 
      mutate(`What is your age?` = text_to_age(`What is your age?`))
    
    # Join into one big table
    responses_all <- bind_rows(responses, responses_easy) %>% 
      select(all_of(names(responses)))
    
    logger$info("Data standardised sucessfully.")
  } else {
    # If there's no easyread data to deal with, just skip the above
    responses_all <- responses
  }
  
  logger$info("Further data cleaning...")
  
  # Remove any responses that haven't been completed and remove redundant cols
  responses_all <- responses_all %>% 
    filter(!is.na(Completed)) %>% 
    select(
      -c(manifests$all %>% filter(omit) %>% pull(question))
    )
  
  logger$info("Data cleaned successfully.")
  
  # Output frequency table pre-location filtering
  logger$info("Saving to file...")
  
  responses_all %>%
    write.csv(
      paste0(
        general_params$output_dir,
        "responses_all_",
        str_replace_all(Sys.Date(), "-", "_"),
        ".csv"
      ),
      row.names = FALSE,
      na = ""
    )
  
  logger$info("Data saved.")
  
  
  # Any further filtering, such as keeping only responses from individuals, 
  # tends to vary considerably from consultation to consultation.
  # Such filtering and writing to other tables can be added below or done 
  # manually in excel.
  # For example...
  
  # # Output a table for just individuals
  # responses_all %>%
  #   filter(
  #     `In what capacity are you responding to this survey?` %in% c(
  #       "An individual sharing my personal views and experiences",
  #       "On behalf of someone else"
  #     )
  #   ) %>% 
  #   write.csv(
  #     paste0(
  #       general_params$output_dir,
  #       "responses_indiv_",
  #       str_replace_all(Sys.Date(), "-", "_"),
  #       ".csv"
  #     )
  #   )
  # 
  # # Output a frequency table of counts by location
  # responses_all %>%
  #   group_by(
  #     `Where do you live in the UK?`,
  #     `Which area of England do you live in?`
  #   ) %>%
  #   summarise(Frequency = n()) %>%
  #   ungroup() %>% 
  #   write.csv(
  #     paste0(
  #       general_params$output_frequency,
  #       Sys.Date(),
  #       "_location_freq.csv"
  #     )
  #   )
}
