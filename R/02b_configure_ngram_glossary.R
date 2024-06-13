
# save raw ngrams
raw_ngram_summary <- 
  DHSCconsultations::ngram_summary(
    responses, 
    clean_text,
    col_id = "response_id"
  ) %>%
  DHSCconsultations::view_ngram_summary() %>%
  DHSCconsultations::write_ngram_summary(
    file.path(
      config$general_params$output_dir,
      "raw_ngrams"
    )
  )


# Check happy that all glossary terms have been accounted for
glossary_words <- file_tools$read_glossary_words()

ngram_summary <- 
  DHSCconsultations::ngram_summary(
    responses, 
    clean_text,
    glossary_words = glossary_words,
    col_id = "response_id"
  ) %>%
  DHSCconsultations::view_ngram_summary()

# if happy save glossary words
file_tools$save_glossary_words()
