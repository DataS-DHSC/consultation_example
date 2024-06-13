
library(readr)
library(tidytext)

# load in default glossary terms - expensive so only do once
glossary_cache <- DHSCconsultations::get_tlap_glossary()

# helper functions to read and write glossary
read_glossary_words <- function(include_defaults = TRUE) {
  glossary <- .read_consultation_file(config$glossary_words)
  
  if (include_defaults) {
    glossary <- c(
      glossary,
      glossary_cache
    )
  }
  
  return(glossary)
}

copy_glossary_words <- function() {
  .copy_consultation_file(config$glossary_words)
}


# helper functions to read and write stop words
read_stop_words <- function(include_defaults = TRUE) {
  stop_words <- .read_consultation_file(config$stop_words)

  if (include_defaults) {
    stop_words <- c(
      stop_words,
      tidytext::stop_words$word
    )
  }
  
  return(stop_words)
}

copy_stop_words <- function() {
  .copy_consultation_file(config$stop_words)
}

# helper functions to read and write stemming exceptions
read_stem_word_exceptions <- function() {
  .read_consultation_file(config$stem_word_exceptions)
}

copy_stem_word_exceptions <- function() {
  .copy_consultation_file(config$stem_word_exceptions)
}


# generic helpers
.read_consultation_file <- function(...) {
  file_path <- file.path(config$general_params$input_dir, ...)
  
  if (!file.exists(file_path)) return(NULL)
    
  df <- readr::read_lines(
    file_path,
    skip_empty_rows = TRUE
  )
  
  return(df)
}

.copy_consultation_file <- function(...) {
  file.copy(
    file.path(config$general_params$input_dir, ...),
    file.path(config$general_params$output_dir, ...),
    overwrite = TRUE
  )
}