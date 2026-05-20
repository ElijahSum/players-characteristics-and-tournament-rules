# Install with:
# Rscript -e "source('requirements.R')"

packages <- c(
  "broom",
  "data.table",
  "dplyr",
  "fixest",
  "ggplot2",
  "ggrepel",
  "lfe",
  "lubridate",
  "modelsummary",
  "patchwork",
  "plotly",
  "purrr",
  "readr",
  "stargazer",
  "stringr",
  "tibble",
  "tidyr"
)

missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

