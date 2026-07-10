# =============================================================
# FILE:    scripts/01_preprocessing.R
# PURPOSE: Load, clean, and engineer features from traffic data
# INPUT:   data/traffic.csv
# OUTPUT:  data/traffic_clean.csv
# PRD:     OBJ-01 | FR-01 to FR-08
# =============================================================

library(tidyverse)
library(lubridate)

# Load raw dataset ----------------------------------------------
message('Loading dataset...')
df <- read_csv('data/traffic.csv', show_col_types = FALSE)
message('Loaded: ', nrow(df), ' rows | ', ncol(df), ' columns')
glimpse(df)

# Check for missing values --------------------------------------
message('\n--- Missing Values ---')
print(colSums(is.na(df)))

# Parse DateTime to POSIXct ------------------------------------
message('Parsing DateTime...')
df <- df %>%
  mutate(DateTime = parse_date_time(DateTime, 
                                    orders = c('ymd HMS', 'ymd')))
stopifnot('DateTime parse failed' = !any(is.na(df$DateTime)))
message('DateTime parsed OK')

# Engineer time-based features ---------------------------------
message('Engineering features...')
df <- df %>%
  mutate(
    hour         = hour(DateTime),
    day_of_week  = wday(DateTime, label = TRUE, abbr = FALSE),
    day_num      = wday(DateTime),
    month        = month(DateTime, label = TRUE, abbr = TRUE),
    month_num    = month(DateTime),
    year         = year(DateTime),
    is_weekend   = if_else(wday(DateTime) %in% c(1, 7), 1L, 0L),
    is_peak_hour = if_else(hour %in% c(7,8,9,17,18,19), 1L, 0L)
  )

# Remove duplicates --------------------------------------------
n_before <- nrow(df)
df       <- df %>% distinct()
message('Duplicates removed: ', n_before - nrow(df))

# Convert Junction to factor -----------------------------------
df <- df %>% mutate(Junction = as.factor(Junction))
message('Junction levels: ', paste(levels(df$Junction), collapse = ', '))

# Flag outliers ------------------------------------------------
df <- df %>%
  group_by(Junction) %>%
  mutate(
    is_outlier = if_else(
      abs(Vehicles - mean(Vehicles)) > 3 * sd(Vehicles), 1L, 0L
    )
  ) %>%
  ungroup()
message('Outliers flagged: ', sum(df$is_outlier, na.rm = TRUE))

# Drop ID column -----------------------------------------------
df_clean <- df %>% select(-ID)

# Final summary ------------------------------------------------
message('\n--- Clean Dataset Summary ---')
message('Rows    : ', nrow(df_clean))
message('Columns : ', ncol(df_clean))
message('Dates   : ', min(df_clean$DateTime), ' to ', max(df_clean$DateTime))
message('Vehicles: ', min(df_clean$Vehicles), ' to ', max(df_clean$Vehicles))

# Save clean dataset -------------------------------------------
write_csv(df_clean, 'data/traffic_clean.csv')
message('Saved -> data/traffic_clean.csv')
message('Preprocessing COMPLETE!')