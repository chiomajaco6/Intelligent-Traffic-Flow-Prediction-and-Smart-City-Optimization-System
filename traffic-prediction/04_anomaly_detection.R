# =============================================================
# FILE:    scripts/04_anomaly_detection.R
# PURPOSE: Detect and visualise anomalous traffic patterns
# INPUT:   data/traffic_clean.csv
# OUTPUT:  output/plot_10_stat_anomalies.png
#          output/plot_11_all_junctions_anomalies.png
#          output/anomaly_summary.csv
# PRD:     OBJ-07 | FR-24 to FR-27
# =============================================================

library(tidyverse)

df <- read_csv('data/traffic_clean.csv', show_col_types = FALSE) %>%
  mutate(
    DateTime = as.POSIXct(DateTime),
    Junction = as.factor(Junction)
  )

message('Anomaly detection started on ', nrow(df), ' rows')

# FR-24: Flag statistical anomalies (z-score > 3) -------------
df_flagged <- df %>%
  group_by(Junction) %>%
  mutate(
    z_score      = (Vehicles - mean(Vehicles)) / sd(Vehicles),
    stat_anomaly = if_else(abs(z_score) > 3, 'Anomaly', 'Normal')
  ) %>%
  ungroup()

n_anom <- sum(df_flagged$stat_anomaly == 'Anomaly')
message('Statistical anomalies detected (|z| > 3): ', n_anom)

# Anomaly breakdown per junction ------------------------------
breakdown <- df_flagged %>%
  group_by(Junction, stat_anomaly) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(stat_anomaly == 'Anomaly')
print(breakdown)

# PLOT 10: Anomalies on Junction 1 time series ----------------
p10 <- df_flagged %>%
  filter(Junction == '1') %>%
  ggplot(aes(x=DateTime, y=Vehicles, color=stat_anomaly)) +
  geom_point(size=0.6, alpha=0.6) +
  scale_color_manual(
    values = c('Normal'='#ADB9CA', 'Anomaly'='#C00000')
  ) +
  labs(
    title    = 'Statistical Anomaly Detection — Junction 1',
    subtitle = 'Red points = vehicle counts beyond 3 standard deviations',
    x = 'Date', y = 'Vehicle Count', color = NULL
  ) +
  theme_minimal(base_size=13) +
  theme(
    plot.title    = element_text(face='bold', color='#1F3864'),
    plot.subtitle = element_text(color='#595959'),
    legend.position = 'bottom'
  )
ggsave('output/plot_10_stat_anomalies.png', p10, width=12, height=5, dpi=150)
message('Saved: plot_10_stat_anomalies.png')

# PLOT 11: Anomalies across all junctions ---------------------
p11 <- df_flagged %>%
  ggplot(aes(x=DateTime, y=Vehicles, color=stat_anomaly)) +
  geom_point(size=0.4, alpha=0.5) +
  scale_color_manual(
    values = c('Normal'='#ADB9CA', 'Anomaly'='#C00000')
  ) +
  facet_wrap(~Junction, nrow=2, labeller=label_both) +
  labs(
    title    = 'Anomaly Detection Across All Junctions',
    subtitle = 'Red = vehicle counts beyond 3 standard deviations from junction mean',
    x = 'Date', y = 'Vehicle Count', color = NULL
  ) +
  theme_minimal(base_size=12) +
  theme(
    plot.title      = element_text(face='bold', color='#1F3864'),
    plot.subtitle   = element_text(color='#595959'),
    legend.position = 'bottom',
    strip.text      = element_text(face='bold')
  )
ggsave('output/plot_11_all_junctions_anomalies.png', p11, width=14, height=8, dpi=150)
message('Saved: plot_11_all_junctions_anomalies.png')

# FR-27: Anomaly summary table --------------------------------
anomaly_tbl <- df_flagged %>%
  filter(stat_anomaly == 'Anomaly') %>%
  select(DateTime, Junction, Vehicles, z_score) %>%
  mutate(z_score = round(z_score, 2)) %>%
  arrange(desc(abs(z_score)))

message('Top 10 most extreme anomalies:')
print(head(anomaly_tbl, 10))

write_csv(anomaly_tbl, 'output/anomaly_summary.csv')
message('Saved -> output/anomaly_summary.csv')
message('\nAnomaly Detection COMPLETE!')