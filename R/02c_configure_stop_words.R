
# glossary words from previous step
glossary_words <- file_tools$read_glossary_words()

# save raw word counts
raw_word_summary <- 
  DHSCconsultations::raw_word_summary(
    responses,
    col_id = "response_id"
  ) %>%
  DHSCconsultations::view_raw_word_summary() %>%
  DHSCconsultations::write_raw_word_summary(
    file.path(
      config$general_params$output_dir,
      "raw_words"
    )
  )


# Rerun the below until you are happy that all stop words have been added
stop_words <- file_tools$read_stop_words()

word_summary <- 
  DHSCconsultations::word_summary(
    responses, 
    clean_text,
    glossary_words = glossary_words,
    stop_words = stop_words,
    col_id = "response_id"
  ) %>%
  DHSCconsultations::view_word_summary()  

# if happy save stop_words
file_tools$save_stop_words()

