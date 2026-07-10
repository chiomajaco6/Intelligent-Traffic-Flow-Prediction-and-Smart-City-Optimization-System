# =============================================================
# FILE:    scripts/00_install_packages.R
# PURPOSE: Install all project packages (run once only)
# =============================================================

packages <- c(
  'tidyverse',
  'lubridate',
  'plotly',
  'caret',
  'randomForest',
  'xgboost',
  'Metrics',
  'anomalize',
  'tibbletime',
  'corrplot',
  'shiny',
  'shinydashboard',
  'DT',
  'scales',
  'knitr',
  'rmarkdown'
)

new_pkgs <- packages[!(packages %in% installed.packages()[,'Package'])]
if (length(new_pkgs) > 0) {
  install.packages(new_pkgs, dependencies = TRUE)
} else {
  message('All packages already installed!')
}

sink('session_info.txt')
sessionInfo()
sink()
message('Done! session_info.txt saved.')