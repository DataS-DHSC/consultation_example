
# re-run the below code while changing the `clean_text` function
# to account for special characters and acronyms
source(file.path("R", "clean_text.R"))

text_clean_summary <- 
  DHSCconsultations::text_cleaner_summary(
    responses, 
    \(x) clean_text(x, squish = FALSE),
    col_id = "response_id"
  ) %>%
  view(title = "character replacements")


# once happy that all special characters and acronyms are cleaned save
# summary
text_clean_summary %>%
  write_xlsx(
    file.path(
      config$general_params$output_dir,
      sprintf("character_replacements.xlsx")
    )
  )