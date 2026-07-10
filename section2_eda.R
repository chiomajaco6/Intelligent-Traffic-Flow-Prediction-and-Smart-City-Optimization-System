# ============================================================
# GROUP 3: Intelligent Traffic Flow Prediction System
# SECTION 2: Exploratory Data Analysis (EDA)
# ============================================================

# ── 0. LIBRARIES ─────────────────────────────────────────────
library(tidyverse)
library(lubridate)
library(scales)
library(gridExtra)
library(corrplot)

cat("==========================================================\n")
cat("  GROUP 3 | SECTION 2: EXPLORATORY DATA ANALYSIS\n")
cat("==========================================================\n\n")

# ── 1. LOAD CLEANED DATA FROM SECTION 1 ──────────────────────
cat("[ STEP 1 ] Loading cleaned dataset from Section 1...\n")

# Try to load the cleaned file produced in Section 1;
# fall back to raw CSV so the script is self-contained.
cleaned_path <- "traffic_cleaned.csv"
raw_path     <- "traffic.csv"

if (file.exists(cleaned_path)) {
  df <- read_csv(cleaned_path, show_col_types = FALSE)
  cat("  ✔ Loaded cleaned dataset:", nrow(df), "rows x", ncol(df), "cols\n\n")
} else if (file.exists(raw_path)) {
  cat("  ⚠ Cleaned file not found – loading raw CSV and re-parsing...\n")
  df <- read_csv(raw_path, show_col_types = FALSE) %>%
    rename_with(tolower) %>%
    mutate(
      datetime  = parse_date_time(datetime, orders = c("ymd HMS", "mdy HMS", "dmy HMS")),
      hour      = hour(datetime),
      day_of_week = wday(datetime, label = TRUE, abbr = TRUE),
      month     = month(datetime, label = TRUE, abbr = TRUE),
      week_num  = isoweek(datetime),
      is_weekend = day_of_week %in% c("Sat", "Sun"),
      junction  = as.factor(junction)
    )
  cat("  ✔ Raw dataset loaded and parsed:", nrow(df), "rows\n\n")
} else {
  stop("No data file found. Place 'traffic_cleaned.csv' or 'traffic.csv' in the working directory.")
}

# Ensure required columns exist
required_cols <- c("datetime", "junction", "vehicles", "hour",
                   "day_of_week", "month", "is_weekend")
missing_cols  <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) stop("Missing columns: ", paste(missing_cols, collapse = ", "))

# ── 2. DATASET OVERVIEW ──────────────────────────────────────
cat("[ STEP 2 ] Dataset Overview\n")
cat("  ─────────────────────────────────────────\n")
cat("  Rows          :", nrow(df), "\n")
cat("  Columns       :", ncol(df), "\n")
cat("  Date range    :", format(min(df$datetime)), "→", format(max(df$datetime)), "\n")
cat("  Junctions     :", n_distinct(df$junction), "–",
    paste(sort(unique(df$junction)), collapse = ", "), "\n")
cat("  Missing values:", sum(is.na(df)), "\n")
cat("  Duplicates    :", sum(duplicated(df)), "\n\n")

# ── 3. UNIVARIATE STATISTICS ─────────────────────────────────
cat("[ STEP 3 ] Univariate Statistics for 'vehicles'\n")

stats_overall <- df %>%
  summarise(
    n        = n(),
    mean     = round(mean(vehicles, na.rm = TRUE), 2),
    median   = median(vehicles, na.rm = TRUE),
    sd       = round(sd(vehicles, na.rm = TRUE), 2),
    min      = min(vehicles, na.rm = TRUE),
    max      = max(vehicles, na.rm = TRUE),
    q25      = quantile(vehicles, .25, na.rm = TRUE),
    q75      = quantile(vehicles, .75, na.rm = TRUE),
    skewness = round(
      (mean(vehicles, na.rm = TRUE) - median(vehicles, na.rm = TRUE)) /
        sd(vehicles, na.rm = TRUE), 3)
  )

cat("  Overall vehicle count statistics:\n")
print(as.data.frame(stats_overall), row.names = FALSE)

stats_by_junction <- df %>%
  group_by(junction) %>%
  summarise(
    n      = n(),
    mean   = round(mean(vehicles), 2),
    median = median(vehicles),
    sd     = round(sd(vehicles), 2),
    min    = min(vehicles),
    max    = max(vehicles),
    .groups = "drop"
  )

cat("\n  Per-junction statistics:\n")
print(as.data.frame(stats_by_junction), row.names = FALSE)

# ── 4. TEMPORAL PATTERNS ─────────────────────────────────────
cat("\n[ STEP 4 ] Temporal Traffic Patterns\n")

# 4a – Hourly
hourly_avg <- df %>%
  group_by(hour) %>%
  summarise(avg_vehicles = mean(vehicles), .groups = "drop")

cat("  Peak hour (all junctions):",
    hourly_avg$hour[which.max(hourly_avg$avg_vehicles)], ":00 →",
    round(max(hourly_avg$avg_vehicles), 1), "avg vehicles\n")

cat("  Quiet hour              :",
    hourly_avg$hour[which.min(hourly_avg$avg_vehicles)], ":00 →",
    round(min(hourly_avg$avg_vehicles), 1), "avg vehicles\n")

# 4b – Day of week
dow_avg <- df %>%
  group_by(day_of_week) %>%
  summarise(avg_vehicles = mean(vehicles), .groups = "drop") %>%
  arrange(desc(avg_vehicles))

cat("  Busiest day  :", as.character(dow_avg$day_of_week[1]),
    "–", round(dow_avg$avg_vehicles[1], 1), "avg vehicles\n")
cat("  Quietest day :", as.character(dow_avg$day_of_week[nrow(dow_avg)]),
    "–", round(dow_avg$avg_vehicles[nrow(dow_avg)], 1), "avg vehicles\n")

# 4c – Weekend vs weekday
wk_compare <- df %>%
  group_by(is_weekend) %>%
  summarise(avg = round(mean(vehicles), 2), .groups = "drop") %>%
  mutate(label = ifelse(is_weekend, "Weekend", "Weekday"))

cat("  Weekday avg  :", wk_compare$avg[!wk_compare$is_weekend], "\n")
cat("  Weekend avg  :", wk_compare$avg[wk_compare$is_weekend], "\n")

# 4d – Monthly trend
monthly_avg <- df %>%
  group_by(month) %>%
  summarise(avg_vehicles = mean(vehicles), .groups = "drop")

cat("  Busiest month :", as.character(monthly_avg$month[which.max(monthly_avg$avg_vehicles)]),
    "–", round(max(monthly_avg$avg_vehicles), 1), "avg vehicles\n")

# ── 5. JUNCTION-LEVEL ANALYSIS ───────────────────────────────
cat("\n[ STEP 5 ] Junction-Level Analysis\n")

junction_hourly <- df %>%
  group_by(junction, hour) %>%
  summarise(avg_vehicles = mean(vehicles), .groups = "drop")

peak_by_junction <- junction_hourly %>%
  group_by(junction) %>%
  slice_max(avg_vehicles, n = 1) %>%
  rename(peak_hour = hour, peak_avg = avg_vehicles)

cat("  Peak hour per junction:\n")
print(as.data.frame(peak_by_junction), row.names = FALSE)

# ── 6. CORRELATION ANALYSIS ──────────────────────────────────
cat("\n[ STEP 6 ] Correlation Analysis\n")

# Build a numeric correlation matrix from available numeric columns
num_df <- df %>%
  select(where(is.numeric)) %>%
  select(-any_of(c("id", "ID"))) %>%   # drop ID columns
  na.omit()

cor_mat <- cor(num_df)
cat("  Correlation with 'vehicles':\n")
cor_with_vehicles <- sort(cor_mat["vehicles", ], decreasing = TRUE)
print(round(cor_with_vehicles, 3))

# ── 7. TRAFFIC DENSITY CLASSIFICATION ───────────────────────
cat("\n[ STEP 7 ] Traffic Density Classification\n")

q33 <- quantile(df$vehicles, 0.33)
q66 <- quantile(df$vehicles, 0.66)

df <- df %>%
  mutate(traffic_level = case_when(
    vehicles <= q33 ~ "Low",
    vehicles <= q66 ~ "Medium",
    TRUE            ~ "High"
  ) %>% factor(levels = c("Low", "Medium", "High")))

level_dist <- df %>%
  count(traffic_level) %>%
  mutate(pct = round(100 * n / sum(n), 1))

cat("  Thresholds → Low: ≤", q33, "| Medium: ≤", q66, "| High: >", q66, "\n")
cat("  Distribution:\n")
print(as.data.frame(level_dist), row.names = FALSE)

# ── 8. ANOMALY FLAGGING ──────────────────────────────────────
cat("\n[ STEP 8 ] Anomaly Detection (IQR method)\n")

iqr_val <- IQR(df$vehicles)
lower   <- quantile(df$vehicles, 0.25) - 1.5 * iqr_val
upper   <- quantile(df$vehicles, 0.75) + 1.5 * iqr_val

df <- df %>%
  mutate(is_anomaly = vehicles < lower | vehicles > upper)

n_anomalies <- sum(df$is_anomaly)
cat("  IQR bounds: [", round(lower, 1), ",", round(upper, 1), "]\n")
cat("  Anomalies flagged:", n_anomalies,
    paste0("(", round(100 * n_anomalies / nrow(df), 2), "% of records)\n"))

# ── 9. VISUALIZATIONS ────────────────────────────────────────
cat("\n[ STEP 9 ] Generating EDA Plots → 'eda_plots.png'\n")

theme_traffic <- theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 11, hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey40"),
    axis.title    = element_text(size = 9),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# P1 – Histogram of vehicle counts
p1 <- ggplot(df, aes(x = vehicles)) +
  geom_histogram(bins = 40, fill = "#2196F3", color = "white", alpha = 0.85) +
  geom_vline(xintercept = mean(df$vehicles), color = "#E53935", linetype = "dashed", linewidth = 0.8) +
  annotate("text", x = mean(df$vehicles) + 2, y = Inf, vjust = 1.5,
           label = paste0("Mean = ", round(mean(df$vehicles), 1)),
           color = "#E53935", size = 3) +
  labs(title = "Distribution of Vehicle Counts",
       subtitle = "Dashed line = mean",
       x = "Vehicles", y = "Frequency") +
  theme_traffic

# P2 – Average vehicles by hour
p2 <- ggplot(hourly_avg, aes(x = hour, y = avg_vehicles)) +
  geom_area(fill = "#42A5F5", alpha = 0.3) +
  geom_line(color = "#1565C0", linewidth = 1) +
  geom_point(color = "#1565C0", size = 2) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(title = "Average Traffic by Hour of Day",
       subtitle = "All junctions combined",
       x = "Hour (0–23)", y = "Avg Vehicles") +
  theme_traffic

# P3 – Box plot by junction
p3 <- ggplot(df, aes(x = junction, y = vehicles, fill = junction)) +
  geom_boxplot(outlier.size = 0.8, outlier.alpha = 0.4, alpha = 0.8) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  labs(title = "Vehicle Count Distribution by Junction",
       subtitle = "Boxes = IQR; whiskers = 1.5×IQR",
       x = "Junction", y = "Vehicles") +
  theme_traffic

# P4 – Heatmap: hour × day-of-week
heatmap_data <- df %>%
  group_by(day_of_week, hour) %>%
  summarise(avg_vehicles = mean(vehicles), .groups = "drop")

p4 <- ggplot(heatmap_data, aes(x = hour, y = day_of_week, fill = avg_vehicles)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(low = "#E3F2FD", high = "#0D47A1",
                      name = "Avg\nVehicles") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(title = "Traffic Heatmap: Hour × Day of Week",
       subtitle = "Darker = heavier traffic",
       x = "Hour", y = "Day of Week") +
  theme_traffic +
  theme(legend.position = "right")

# P5 – Day-of-week bar chart
p5 <- df %>%
  group_by(day_of_week, is_weekend) %>%
  summarise(avg_vehicles = mean(vehicles), .groups = "drop") %>%
  ggplot(aes(x = day_of_week, y = avg_vehicles, fill = is_weekend)) +
  geom_col(alpha = 0.85) +
  scale_fill_manual(values = c("FALSE" = "#29B6F6", "TRUE" = "#FF7043"),
                    labels = c("Weekday", "Weekend"), name = "") +
  labs(title = "Average Traffic by Day of Week",
       subtitle = "Orange = weekend",
       x = "Day", y = "Avg Vehicles") +
  theme_traffic

# P6 – Monthly trend
p6 <- ggplot(monthly_avg, aes(x = month, y = avg_vehicles, group = 1)) +
  geom_line(color = "#43A047", linewidth = 1.1) +
  geom_point(color = "#1B5E20", size = 2.5) +
  labs(title = "Monthly Average Traffic Volume",
       subtitle = "Trend across calendar months",
       x = "Month", y = "Avg Vehicles") +
  theme_traffic

# P7 – Junction hourly heatmap
p7 <- ggplot(junction_hourly, aes(x = hour, y = junction, fill = avg_vehicles)) +
  geom_tile(color = "white", linewidth = 0.4) +
  scale_fill_gradient(low = "#FFF9C4", high = "#E65100",
                      name = "Avg\nVehicles") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(title = "Traffic by Hour × Junction",
       subtitle = "Orange = peak traffic load",
       x = "Hour", y = "Junction") +
  theme_traffic +
  theme(legend.position = "right")

# P8 – Traffic level distribution
p8 <- ggplot(df, aes(x = traffic_level, fill = traffic_level)) +
  geom_bar(alpha = 0.85) +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.4, size = 3.2) +
  scale_fill_manual(values = c("Low" = "#66BB6A",
                               "Medium" = "#FFA726",
                               "High" = "#EF5350"),
                    guide = "none") +
  labs(title = "Traffic Density Level Distribution",
       subtitle = "Classified by tertile thresholds",
       x = "Traffic Level", y = "Count") +
  theme_traffic

# ── Assemble & save ──────────────────────────────────────────
png("eda_plots.png", width = 1600, height = 1800, res = 130)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8,
             ncol = 2,
             top = grid::textGrob(
               "Group 3 | Traffic EDA — Section 2",
               gp = grid::gpar(fontsize = 14, fontface = "bold")))
dev.off()
cat("  ✔ Saved: eda_plots.png\n")

# ── 10. SAVE ENRICHED DATASET ────────────────────────────────
cat("\n[ STEP 10 ] Saving enriched dataset → 'traffic_eda.csv'\n")
write_csv(df, "traffic_eda.csv")
cat("  ✔ Saved:", nrow(df), "rows,", ncol(df), "columns\n")

# ── SUMMARY REPORT ───────────────────────────────────────────
cat("\n")
cat("==========================================================\n")
cat("  SECTION 2 EDA — COMPLETE SUMMARY\n")
cat("==========================================================\n")
cat("  Dataset    :", nrow(df), "records across", n_distinct(df$junction), "junctions\n")
cat("  Date range :", format(min(df$datetime)), "→", format(max(df$datetime)), "\n")
cat("  Vehicles   : mean =", stats_overall$mean,
    "| sd =", stats_overall$sd,
    "| range [", stats_overall$min, "–", stats_overall$max, "]\n")
cat("  Peak hour  :", hourly_avg$hour[which.max(hourly_avg$avg_vehicles)],
    ":00 (avg", round(max(hourly_avg$avg_vehicles), 1), "vehicles)\n")
cat("  Busiest day:", as.character(dow_avg$day_of_week[1]), "\n")
cat("  Anomalies  :", n_anomalies,
    paste0("(", round(100 * n_anomalies / nrow(df), 2), "%)\n"))
cat("  Output files: eda_plots.png | traffic_eda.csv\n")
cat("==========================================================\n")
cat("  ► Ready for Section 3: Peak Period & Factor Analysis\n")
cat("==========================================================\n")
