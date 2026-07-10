# =============================================================
# FILE:    scripts/02_eda.R
# PURPOSE: EDA — traffic patterns, peaks, correlations
# INPUT:   data/traffic_clean.csv
# OUTPUT:  output/plot_01 to plot_07 (PNG) + summary_statistics.csv
# PRD:     OBJ-02, OBJ-03, OBJ-04 | FR-09 to FR-15
# =============================================================

library(tidyverse)
library(scales)

df <- read_csv('data/traffic_clean.csv', show_col_types = FALSE) %>%
  mutate(
    Junction    = as.factor(Junction),
    day_of_week = factor(day_of_week,
                         levels = c('Sunday','Monday','Tuesday','Wednesday',
                                    'Thursday','Friday','Saturday'))
  )

message('EDA started on ', nrow(df), ' rows')

# Shared theme -------------------------------------------------
tt <- theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face='bold', size=14, color='#1F3864'),
    plot.subtitle = element_text(size=11, color='#595959'),
    axis.title    = element_text(face='bold'),
    legend.position = 'bottom'
  )

# Summary statistics -------------------------------------------
stats <- df %>%
  group_by(Junction) %>%
  summarise(
    n      = n(),
    mean   = round(mean(Vehicles), 1),
    median = median(Vehicles),
    sd     = round(sd(Vehicles), 1),
    min    = min(Vehicles),
    max    = max(Vehicles),
    .groups = 'drop'
  )
print(stats)
write_csv(stats, 'output/summary_statistics.csv')
message('Saved: summary_statistics.csv')

# PLOT 1: Average vehicles by hour ----------------------------
p1 <- df %>%
  group_by(hour, Junction) %>%
  summarise(avg = mean(Vehicles), .groups='drop') %>%
  ggplot(aes(x=hour, y=avg, fill=Junction)) +
  geom_col(position='dodge') +
  scale_x_continuous(breaks=0:23) +
  scale_fill_brewer(palette='Blues') +
  labs(title='Average Vehicle Count by Hour of Day',
       subtitle='Morning (7-9am) and evening (5-7pm) peaks visible',
       x='Hour of Day', y='Average Vehicles', fill='Junction') + tt
ggsave('output/plot_01_hourly_traffic.png', p1, width=12, height=6, dpi=150)
message('Saved: plot_01_hourly_traffic.png')

# PLOT 2: Vehicle distribution by junction --------------------
p2 <- df %>%
  ggplot(aes(x=Junction, y=Vehicles, fill=Junction)) +
  geom_boxplot(outlier.alpha=0.3, outlier.size=0.8) +
  scale_fill_brewer(palette='Blues') +
  labs(title='Vehicle Count Distribution by Junction',
       subtitle='Median, spread and outliers per junction',
       x='Junction', y='Vehicle Count') +
  tt + theme(legend.position='none')
ggsave('output/plot_02_junction_boxplot.png', p2, width=8, height=6, dpi=150)
message('Saved: plot_02_junction_boxplot.png')

# PLOT 3: Heatmap — hour vs day of week ----------------------
p3 <- df %>%
  group_by(hour, day_of_week) %>%
  summarise(avg = mean(Vehicles), .groups='drop') %>%
  ggplot(aes(x=hour, y=day_of_week, fill=avg)) +
  geom_tile(color='white', linewidth=0.3) +
  scale_fill_gradient(low='#EBF3FB', high='#1F3864', name='Avg Vehicles') +
  scale_x_continuous(breaks=seq(0,23,2)) +
  labs(title='Traffic Heatmap: Hour of Day vs Day of Week',
       subtitle='Darker colour = higher congestion',
       x='Hour of Day', y=NULL) + tt
ggsave('output/plot_03_heatmap.png', p3, width=12, height=5, dpi=150)
message('Saved: plot_03_heatmap.png')

# PLOT 4: Time series per junction ----------------------------
p4 <- df %>%
  group_by(date = floor_date(DateTime, 'day'), Junction) %>%
  summarise(daily_total = sum(Vehicles), .groups='drop') %>%
  ggplot(aes(x=date, y=daily_total, color=Junction)) +
  geom_line(alpha=0.7, linewidth=0.6) +
  scale_color_brewer(palette='Set1') +
  scale_y_continuous(labels=comma) +
  labs(title='Daily Total Vehicles Over Time by Junction',
       x='Date', y='Daily Total Vehicles', color='Junction') + tt
ggsave('output/plot_04_timeseries.png', p4, width=14, height=6, dpi=150)
message('Saved: plot_04_timeseries.png')

# PLOT 5: Weekday vs Weekend ----------------------------------
p5 <- df %>%
  mutate(day_type = if_else(is_weekend==1, 'Weekend', 'Weekday')) %>%
  group_by(hour, day_type) %>%
  summarise(avg = mean(Vehicles), .groups='drop') %>%
  ggplot(aes(x=hour, y=avg, color=day_type, group=day_type)) +
  geom_line(linewidth=1.2) + geom_point(size=2) +
  scale_color_manual(values=c('Weekday'='#2E75B6','Weekend'='#C55A11')) +
  scale_x_continuous(breaks=0:23) +
  labs(title='Weekday vs Weekend Traffic by Hour',
       subtitle='Commuter pattern disappears on weekends',
       x='Hour of Day', y='Average Vehicles', color=NULL) + tt
ggsave('output/plot_05_weekend_weekday.png', p5, width=11, height=6, dpi=150)
message('Saved: plot_05_weekend_weekday.png')

# PLOT 6: Monthly trend ---------------------------------------
p6 <- df %>%
  group_by(month, Junction) %>%
  summarise(avg = mean(Vehicles), .groups='drop') %>%
  ggplot(aes(x=month, y=avg, color=Junction, group=Junction)) +
  geom_line(linewidth=1) + geom_point(size=2.5) +
  scale_color_brewer(palette='Set1') +
  labs(title='Average Vehicle Count by Month',
       subtitle='Seasonal variation by junction',
       x='Month', y='Average Vehicles', color='Junction') + tt
ggsave('output/plot_06_monthly_trend.png', p6, width=10, height=6, dpi=150)
message('Saved: plot_06_monthly_trend.png')

# Peak vs Off-peak statistical test ---------------------------
peak_v    <- df %>% filter(is_peak_hour==1) %>% pull(Vehicles)
offpeak_v <- df %>% filter(is_peak_hour==0) %>% pull(Vehicles)
wt        <- wilcox.test(peak_v, offpeak_v, alternative='greater')
message('\n--- Peak vs Off-Peak Test ---')
message('Peak median    : ', median(peak_v))
message('Off-peak median: ', median(offpeak_v))
message('Wilcoxon p-value: ', format(wt$p.value, scientific=TRUE))
message('Significant: ', wt$p.value < 0.05)

# PLOT 7: Peak vs off-peak density ----------------------------
p7 <- df %>%
  mutate(period = if_else(is_peak_hour==1,
                          'Peak (7-9am & 5-7pm)', 'Off-Peak')) %>%
  ggplot(aes(x=Vehicles, fill=period)) +
  geom_density(alpha=0.5) +
  scale_fill_manual(values=c('Peak (7-9am & 5-7pm)'='#1F3864',
                             'Off-Peak'='#ADB9CA')) +
  labs(title='Vehicle Density: Peak vs Off-Peak Hours',
       subtitle=paste0('Wilcoxon p-value: ', format(wt$p.value, digits=3)),
       x='Vehicle Count', y='Density', fill=NULL) + tt
ggsave('output/plot_07_peak_density.png', p7, width=10, height=6, dpi=150)
message('Saved: plot_07_peak_density.png')

message('\nEDA COMPLETE! All 7 plots saved to output/')