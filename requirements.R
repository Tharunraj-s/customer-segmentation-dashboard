# requirements.R

required_packages <- c(
  "shiny",
  "bslib",
  "thematic",
  "dplyr",
  "ggplot2",
  "readxl",
  "rlang",
  "readr",
  "lubridate",
  "scales",
  "cluster",
  "factoextra",
  "plotly",
  "DT",
  "tidyr",
  "stringr"
)

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)

if (length(missing) > 0) {
  install.packages(missing)
} else {
  message("All required packages are already installed.")
}

invisible(lapply(required_packages, library, character.only = TRUE))
message("Packages loaded successfully.")

