
library(tidyverse)
#library(DHSCconsultations)
devtools::load_all("../DHSCconsultations")

# Read the config file
config <- yaml::read_yaml(
  file.path("input", "config.yml")
)

if (!exists("file_tools")) {
  file_tools <- new.env()
  source(file.path("R", "file_tools.R"), local = file_tools)
}


get_example_responses <- function(n_responses = 20) {
  return(
    janeaustenr::austen_books() |>
      filter(text != "") |>
      group_by(book) |>
      mutate(
        response_id = ntile(n = n_responses)
      ) |>
      group_by(response_id, book) |>
      summarise(text = str_c(text, collapse = " "), .groups = "drop") |>
      pivot_wider(names_from = book, values_from = text)
  )
}


responses <- text_tools$get_example_responses(n_responses = 100)
