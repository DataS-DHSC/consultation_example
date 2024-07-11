
# install packages if needed
if (!requireNamespace("librarian")) install.packages("librarian", quiet = TRUE)

# need to sort out the below
suppressWarnings(
  librarian::stock(
    DataS-DHSC/DHSClogger,
    DataS-DHSC/DHSCcolours,
    tidytext,
    quiet = TRUE
  )
)

# Setup logging -----------------------------------------------------------
logger <- DHSClogger::get_dhsc_logger()
# set threshold of console log to information and above
logger$set_threshold("log.console", "INFO")


# Read the config file
config <- yaml::read_yaml(
  file.path("input", "config.yml")
)

# call the pipeline
