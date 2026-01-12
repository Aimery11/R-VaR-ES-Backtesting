
# 0) Force working directory to the folder containing THIS file (run.R)
.get_this_file <- function() {
  # Case 1: sourced via source() / RStudio "Source"
  p <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
  if (!is.na(p) && nzchar(p)) return(p)
  
  # Case 2: executed via Rscript --file=run.R
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    p <- sub("^--file=", "", file_arg[1])
    p <- tryCatch(normalizePath(p), error = function(e) NA_character_)
    if (!is.na(p) && nzchar(p)) return(p)
  }
  
  # Case 3 (optional): RStudio API fallback
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) NA_character_)
    if (!is.na(p) && nzchar(p)) return(normalizePath(p))
  }
  
  NA_character_
}

.this_file <- .get_this_file()
if (!is.na(.this_file)) {
  setwd(dirname(.this_file))
  message("Working directory set to: ", getwd())
} else {
  message("Could not detect run.R path. Make sure you opened the project or setwd() manually.")
}

# 1) Minimal package check / install (interactive)

required <- c("tidyverse","tidyquant","lubridate","conflicted","memoise")

missing <- setdiff(required, rownames(installed.packages()))
if (length(missing) > 0) {
  if (interactive()) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, quiet = TRUE)
  } else {
    stop("Missing packages: ", paste(missing, collapse = ", "),
         "\nRun in R: install.packages(c(", paste0('"', missing, '"', collapse = ", "), "))")
  }
}

# 2) If renv.lock exists: restore (optional but recommended)

if (file.exists("renv.lock")) {
  if (!requireNamespace("renv", quietly = TRUE)) {
    if (interactive()) install.packages("renv", quiet = TRUE)
    else stop("renv.lock exists but renv is not installed. Run: install.packages('renv')")
  }
  message("renv.lock detected → restoring environment…")
  renv::restore(prompt = FALSE)
}

# 3) Run the app

source("main.R")
run_cli()