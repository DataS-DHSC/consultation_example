
library(tidyverse)

#' Clean text strings for analysis with LDA
#' 
#' This function should be modified based on need. Note
#' that further transforms are performed prior to applying
#' LDA and these will need to be considered in parallel with
#' any cleaning performed i.e. are numbers removed or is the
#' text converted to lower case.
#'
#' @param txt text to clean 
#'
#' @return cleaned text
#'
#' @examples
#' clean_text("Department  of Health & Social  Care")
clean_text <- function(txt, squish = TRUE) {
  txt <- txt |>
    # deal with special unicode characters
    str_replace_all("\U0001f1ec", "G") |>
    str_replace_all("\U0001f1e7", "B") |>
    str_replace_all("\U0001f1fa", "U") |>
    str_replace_all("\U0001f1f8", "S") |>
    str_replace_all("\U0001f1f5", "P") |>
    
    # hyphens, en, em dashes to spaces
    str_replace_all("\u2014|\u2013", "-") |>
    str_replace_all("-", " ") |>
    
    # single quotes
    str_replace_all("\u2018|`|\u2032|\u00b4", "'") |>
    
    # remove possessives (will be stemmed anyway) 
    str_replace_all("'s|s'", "s") |>
    
    # convert often used symbols to word equivalents
    str_replace_all("\\+", "plus") |>
    str_replace_all("&", "and") |>
    
    # keep only numbers, letters, apostrophes, and spaces
    str_replace_all("[^0-9a-zA-Z'\\s]", "") |>
    
    # replacements that depend on capitalisation
    str_replace_all("\\bUS\\b", "United States") |>
    
    # convert to lower case
    str_to_lower() |>
    
    # number replacements (these are removed)
    str_replace_all("(?<=\\b|[^0-9])16(?=\\b|[^0-9])", "sixteen") |>
    str_replace_all("(?<=\\b|[^0-9])17(?=\\b|[^0-9])", "seventeen") |>
    str_replace_all("(?<=\\b|[^0-9])18(?=\\b|[^0-9])", "eighteen") |>
    str_replace_all("(?<=\\b|[^0-9])19(?=\\b|[^0-9])", "nineteen") |>
    str_replace_all("(?<=\\b|[^0-9])20(?=\\b|[^0-9])", "twenty") |>
    str_replace_all("(?<=\\b|[^0-9])21(?=\\b|[^0-9])", "twentyone") |>
    
    # remove numbers
    str_replace_all("[^a-zA-Z'\\s]", "") 
  
  # clean up spaces  
  if (squish) {
    txt <- txt |> str_squish()
  }
  
  return(txt)
}
