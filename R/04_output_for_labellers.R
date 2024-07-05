library(markdown)
library(tidyverse)

# Excel file with each document-topic assignment, including gamma scores and 
# the original text

# read in responses_all_ with date based on the one in config
# use jane austin data for now...
responses_all <- janeaustenr::austen_books() %>%
  filter(text != "") %>%
  group_by(book) %>%
  mutate(
    response_id = ntile(n = 100)
  ) %>%
  group_by(response_id, book) %>%
  summarise(text = str_c(text, collapse = " "), .groups = "drop") %>%
  pivot_wider(names_from = book, values_from = text)


lda_object <- readRDS("output/lda_object.rds")
lda_question_names <- lda_object %>% names()

for (question in lda_question_names) {
  
  # Get the gamma table for the current question
  table_gamma <- lda_object[[question]]$gamma
  
  # Join responses_all with the current table_gamma
  responses_and_gamma <- responses_all %>%
    select(response_id, question) %>% 
    mutate(response_id = as.character(response_id)) %>%
    left_join(table_gamma, by = c("response_id" = "document")) %>%
    mutate(question = question) %>% 
    rename(response_text = 2) %>% 
    select(
      response_id,
      question,
      response_text,
      topic,
      gamma
    )
  
  # Define the filename
  filename <- paste0("output/output_for_labellers/topic_gamma_q", i, ".csv")
  
  # Write the result to a CSV file
  write.csv(responses_and_gamma, file = filename, row.names = FALSE)
  
  # Optionally, print a message to indicate progress
  cat("Saved file:", filename, "\n")
}


# Loop to create PDFs

question_text_list <- names(lda_object)

for (i in seq_along(question_text_list)) {
  rmarkdown::render(
    "R/output_for_labellers.Rmd",
    output_file = paste0("../output/output_for_labellers/question_", i ,".pdf")
  )
}
