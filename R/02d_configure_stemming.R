
# glossary and stop words from previous steps
glossary_words <- file_tools$read_glossary_words()
stop_words <- file_tools$read_stop_words()

# Rerun the below until you are happy that all stemming exceptions have been added
stem_word_exceptions <- file_tools$read_stem_word_exceptions()

stemming_summary <- 
  DHSCconsultations::stemming_summary(
    responses, 
    clean_text,
    stem_word_exceptions = stem_word_exceptions,
    glossary_words = glossary_words,
    stop_words = stop_words,
    col_id = "response_id"
  ) %>%
  view("stemmed words")

# if happy save stemming exceptions
file_tools$save_stem_word_exceptions()

stemming_summary %>%
  write_xlsx(
    file.path(
      config$general_params$output_dir,
      sprintf("stemmed_words.xlsx")
    )
  )



