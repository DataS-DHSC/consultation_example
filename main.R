# Project launch file, either run everything by sourcing this script, or run 
# line by line with ctrl enter

# Load global packages ----------------------------------------------------

# Check librarian package management installed
if (!requireNamespace("librarian")) install.packages("librarian", quiet = TRUE)

# Use suppress to prevent build warnings
# Note can install from github
suppressWarnings(librarian::shelf(
  DataS-DHSC/DHSClogger,
  DataS-DHSC/consultations,
  tidyverse,
  readxl,
  yaml
))

# Setup logging -----------------------------------------------------------

logger <- DHSClogger::get_dhsc_logger()

# set threshold of console log to information and above
logger$set_threshold("log.console", "INFO")/
  
  
# Call main code ----------------------------------------------------------

logger$info("[Begin]")

source("./R/1-load-data.R", local = TRUE)
# source other scripts here...


logger$info("---- Loading and standardising all response data -----")
run_load_data()
