
# Calculate LDA for a range of topic model number
lda_out <- 
  DHSCconsultations::lda_fitter(
    responses,
    "response_id",
    list(
      "Sense & Sensibility" = c(6, 10, 15),
      "Pride & Prejudice" = c(6, 10, 15),
      "Mansfield Park" = c(6, 10, 15),
      "Emma" = c(6, 10, 15),
      "Northanger Abbey" = c(6, 10, 15),
      "Persuasion" = c(6, 10, 15)
    ),
    1984,
    clean_text,
    file_tools$read_glossary_words(),
    file_tools$read_stop_words(),
    stem_word_exceptions = file_tools$read_stem_word_exceptions()
  ) %>%
  DHSCconsultations::view_lda_fitter()


# save output of graphs
lda_out %>%
  DHSCconsultations::write_lda_fitter(
    file.path(
      config$general_params$output_dir, 
      "lda_fit"
    )
  )
