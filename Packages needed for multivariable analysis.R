#Here are the packages needed for the analysis RScript
#First, restart R
#Second, run the following code

req <- c(
  "dplyr",
  "tidyr",
  "forcats",
  "purrr",
  "stringr",
  "tibble",
  "readr",
  "gtsummary",
  "gt",
  "broom",
  "broom.helpers",
  "car",
  "pROC",
  "xfun",
  "knitr",
  "rmarkdown"
)

remove.packages(intersect(req, rownames(installed.packages())))
install.packages(req, repos = "https://cloud.r-project.org/")
invisible(lapply(req, library, character.only = TRUE))