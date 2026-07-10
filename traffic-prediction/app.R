# =============================================================
# FILE:    app.R
# PURPOSE: TrafficIQ — Professional Smart City Dashboard
# =============================================================

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(lubridate)
library(scales)

# ── Fix file paths ────────────────────────────────────────────
if (!file.exists('data/traffic_clean.csv')) {
  data_path  <- 'traffic_clean.csv'
  model_path <- 'best_model_lite.rds'
  comp_path  <- 'model_comparison.csv'
  anom_path  <- 'anomaly_summary.csv'
} else {
  data_path  <- 'data/traffic_clean.csv'
  model_path <- 'models/best_model_lite.rds'
  comp_path  <- 'output/model_comparison.csv'
  anom_path  <- 'output/anomaly_summary.csv'
}

# ── Load Data ─────────────────────────────────────────────────
df <- read_csv(data_path, show_col_types = FALSE) %>%
  mutate(
    Junction    = as.factor(Junction),
    DateTime    = as.POSIXct(DateTime),
    day_of_week = factor(
      as.character(day_of_week),
      levels = c('Sunday','Monday','Tuesday','Wednesday',
                 'Thursday','Friday','Saturday')
    ),
    hour         = as.integer(hour),
    day_num      = as.integer(day_num),
    month_num    = as.integer(month_num),
    is_weekend   = as.integer(is_weekend),
    is_peak_hour = as.integer(is_peak_hour)
  )

saved         <- readRDS(model_path)
model_results <- read_csv(comp_path, show_col_types = FALSE)
anomaly_tbl   <- read_csv(anom_path, show_col_types = FALSE) %>%
  mutate(Junction = as.factor(Junction))

# ── CSS ───────────────────────────────────────────────────────
css <- "
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap');

* { font-family: 'Inter', sans-serif !important; box-sizing: border-box; }

::-webkit-scrollbar { width: 4px; height: 4px; }
::-webkit-scrollbar-track { background: #080F17; }
::-webkit-scrollbar-thumb { background: #1E3A5F; border-radius: 4px; }

body, .wrapper { background: #060D14 !important; }
.content-wrapper {
  background: #060D14 !important;
  margin-left: 220px !important;
  min-height: 100vh !important;
}
.content { padding: 16px !important; }

/* ── Sidebar ── */
.main-sidebar {
  background: #080F17 !important;
  width: 220px !important;
  border-right: 1px solid #0E2038 !important;
}
.sidebar { padding: 0 !important; }
.main-header .logo {
  background: #080F17 !important;
  border-bottom: 1px solid #0E2038 !important;
  width: 220px !important;
  font-size: 14px !important;
  font-weight: 700 !important;
  color: #FFFFFF !important;
  letter-spacing: 2px !important;
}
.main-header .navbar {
  background: #080F17 !important;
  border-bottom: 1px solid #0E2038 !important;
  margin-left: 220px !important;
}
.main-header .navbar .sidebar-toggle {
  color: #3B82F6 !important;
}
.sidebar-menu { padding: 8px 0 !important; }
.sidebar-menu > li { margin: 2px 8px !important; }
.sidebar-menu > li > a {
  color: #4A6FA5 !important;
  font-size: 12px !important;
  font-weight: 500 !important;
  padding: 9px 12px !important;
  border-radius: 6px !important;
  border-left: none !important;
  transition: all 0.15s ease !important;
}
.sidebar-menu > li > a:hover {
  background: #0E2038 !important;
  color: #93C5FD !important;
}
.sidebar-menu > li.active > a {
  background: #1E3A5F !important;
  color: #FFFFFF !important;
  font-weight: 600 !important;
}
.sidebar-menu > li > a > .fa {
  width: 16px !important;
  font-size: 12px !important;
  color: #3B82F6 !important;
  margin-right: 8px !important;
}
.sidebar-menu > li.active > a > .fa { color: #93C5FD !important; }
.sidebar-filters {
  padding: 16px 12px 8px;
  border-bottom: 1px solid #0E2038;
  margin-bottom: 8px;
}
.filter-label {
  color: #1E3A5F;
  font-size: 9px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  margin-bottom: 10px;
  display: block;
}
.nav-label {
  color: #1E3A5F;
  font-size: 9px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  padding: 0 12px;
  margin: 12px 0 4px;
  display: block;
}
.sidebar-footer {
  position: absolute;
  bottom: 0; left: 0; right: 0;
  padding: 12px;
  border-top: 1px solid #0E2038;
  text-align: center;
}
.sidebar-footer-text {
  color: #1E3A5F;
  font-size: 9px;
  font-weight: 600;
  letter-spacing: 1px;
  text-transform: uppercase;
}
.status-dot {
  display: inline-block;
  width: 6px; height: 6px;
  background: #10B981;
  border-radius: 50%;
  margin-right: 5px;
  animation: pulse 2s infinite;
}
@keyframes pulse {
  0%, 100% { opacity: 1; }
  50%       { opacity: 0.4; }
}

/* ── Override shinydashboard box ── */
.box {
  background: #0C1A28 !important;
  border: 1px solid #0E2038 !important;
  border-top: none !important;
  border-radius: 10px !important;
  box-shadow: 0 4px 15px rgba(0,0,0,0.3) !important;
  margin-bottom: 16px !important;
}
.box-header {
  background: #0C1A28 !important;
  border-bottom: 1px solid #0E2038 !important;
  border-radius: 10px 10px 0 0 !important;
  padding: 12px 16px !important;
  color: #CBD5E1 !important;
}
.box-title {
  color: #CBD5E1 !important;
  font-size: 11px !important;
  font-weight: 600 !important;
  text-transform: uppercase !important;
  letter-spacing: 1px !important;
}
.box-body { padding: 8px !important; }
.box.box-primary { border-top: 2px solid #3B82F6 !important; }
.box.box-success  { border-top: 2px solid #10B981 !important; }
.box.box-warning  { border-top: 2px solid #F59E0B !important; }
.box.box-danger   { border-top: 2px solid #EF4444 !important; }
.box.box-info     { border-top: 2px solid #06B6D4 !important; }

/* ── KPI Cards ── */
.kpi-card {
  background: #0C1A28;
  border: 1px solid #0E2038;
  border-radius: 10px;
  padding: 18px;
  margin-bottom: 16px;
  position: relative;
  overflow: hidden;
  transition: border-color 0.2s;
}
.kpi-card:hover { border-color: #1E3A5F; }
.kpi-card::before {
  content: '';
  position: absolute;
  top: 0; left: 0; right: 0;
  height: 2px;
}
.kpi-card.blue::before  { background: #3B82F6; }
.kpi-card.green::before { background: #10B981; }
.kpi-card.amber::before { background: #F59E0B; }
.kpi-card.red::before   { background: #EF4444; }
.kpi-label {
  color: #4A6FA5;
  font-size: 10px;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 1px;
  margin-bottom: 8px;
}
.kpi-value {
  font-size: 32px;
  font-weight: 800;
  line-height: 1;
  letter-spacing: -1px;
  margin-bottom: 6px;
}
.kpi-card.blue  .kpi-value { color: #60A5FA; }
.kpi-card.green .kpi-value { color: #34D399; }
.kpi-card.amber .kpi-value { color: #FBBF24; }
.kpi-card.red   .kpi-value { color: #F87171; }
.kpi-sub { color: #1E3A5F; font-size: 10px; }

/* ── Header bar ── */
.header-bar {
  background: #080F17;
  border-bottom: 1px solid #0E2038;
  padding: 8px 16px;
  display: flex;
  flex-wrap: wrap;
  gap: 16px;
  margin-bottom: 16px;
}
.header-bar-item {
  color: #4A6FA5;
  font-size: 10px;
  font-weight: 600;
  letter-spacing: 0.8px;
  text-transform: uppercase;
  display: flex;
  align-items: center;
  gap: 5px;
}
.header-bar-item span { color: #60A5FA; }

/* ── Form controls ── */
.control-label {
  color: #4A6FA5 !important;
  font-size: 10px !important;
  font-weight: 600 !important;
  text-transform: uppercase !important;
  letter-spacing: 0.8px !important;
}
.selectize-input {
  background: #0C1A28 !important;
  border: 1px solid #0E2038 !important;
  color: #E2E8F0 !important;
  border-radius: 6px !important;
  font-size: 12px !important;
  box-shadow: none !important;
}
.selectize-input.focus {
  border-color: #3B82F6 !important;
}
.selectize-dropdown {
  background: #0C1A28 !important;
  border: 1px solid #0E2038 !important;
  color: #E2E8F0 !important;
  border-radius: 6px !important;
  font-size: 12px !important;
}
.selectize-dropdown .option:hover,
.selectize-dropdown .option.active {
  background: #1E3A5F !important;
}
.irs--shiny .irs-bar {
  background: #3B82F6 !important;
  border: none !important;
  height: 3px !important;
}
.irs--shiny .irs-line {
  background: #0E2038 !important;
  height: 3px !important;
  border: none !important;
}
.irs--shiny .irs-handle {
  background: #3B82F6 !important;
  border: 2px solid #FFFFFF !important;
  width: 14px !important; height: 14px !important;
  top: 24px !important;
}
.irs--shiny .irs-single { background: #3B82F6 !important; font-size: 10px !important; }
.irs--shiny .irs-min, .irs--shiny .irs-max {
  background: transparent !important;
  color: #1E3A5F !important;
  font-size: 10px !important;
}

/* ── Insight cards ── */
.insight-card {
  background: #0C1A28;
  border: 1px solid #0E2038;
  border-radius: 10px;
  padding: 20px;
  margin-bottom: 16px;
}
.insight-section-title {
  color: #4A6FA5;
  font-size: 9px;
  font-weight: 700;
  text-transform: uppercase;
  letter-spacing: 1.5px;
  margin-bottom: 16px;
  padding-bottom: 10px;
  border-bottom: 1px solid #0E2038;
}
.predict-btn {
  background: #3B82F6 !important;
  color: #FFFFFF !important;
  border: none !important;
  width: 100% !important;
  padding: 11px !important;
  border-radius: 7px !important;
  font-weight: 600 !important;
  font-size: 12px !important;
  cursor: pointer !important;
  margin-top: 4px !important;
}
.score-card {
  background: #080F17;
  border: 1px solid #0E2038;
  border-radius: 10px;
  padding: 16px;
  text-align: center;
  margin-bottom: 12px;
  transition: all 0.2s;
}
.score-card:hover {
  border-color: #1E3A5F;
  transform: translateY(-2px);
}
.grade-label { color: #4A6FA5; font-size: 10px; font-weight: 600; text-transform: uppercase; letter-spacing: 1px; }
.grade-value { font-size: 48px; font-weight: 800; line-height: 1.1; letter-spacing: -2px; }
.grade-score { font-size: 10px; font-weight: 600; margin: 4px 0 10px; }
.grade-divider { border: none; border-top: 1px solid #0E2038; margin: 8px 0; }
.grade-stat { display: flex; justify-content: space-between; margin-bottom: 3px; }
.grade-stat-label { color: #4A6FA5; font-size: 10px; }
.grade-stat-value { color: #CBD5E1; font-size: 10px; font-weight: 600; }
.grade-rec { font-size: 10px; padding: 7px; border-radius: 5px; margin-top: 8px; line-height: 1.4; }

.travel-box { border-radius: 8px; padding: 14px; margin-top: 12px; }
.travel-box-title { font-size: 10px; font-weight: 700; text-transform: uppercase; letter-spacing: 1px; margin-bottom: 5px; }
.travel-box-times { font-size: 15px; font-weight: 700; margin-bottom: 3px; }
.travel-box-sub { font-size: 11px; }

.pred-number { font-size: 72px; font-weight: 800; line-height: 1; letter-spacing: -3px; }
.pred-label { font-size: 11px; color: #4A6FA5; text-transform: uppercase; letter-spacing: 1px; margin-top: 4px; }
.congestion-pill {
  display: inline-block; padding: 5px 14px; border-radius: 20px;
  font-size: 11px; font-weight: 700; letter-spacing: 1px;
  text-transform: uppercase; margin-top: 10px;
}
.pred-bar-wrap { background: #060D14; border-radius: 4px; height: 6px; width: 100%; overflow: hidden; margin-top: 8px; }
.pred-bar-fill { height: 100%; border-radius: 4px; }
.pred-detail { color: #CBD5E1; font-size: 12px; line-height: 1.6; margin-top: 10px; }

/* ── DataTable ── */
.dataTables_wrapper { color: #CBD5E1 !important; }
table.dataTable { border-collapse: collapse !important; }
table.dataTable thead th {
  background: #080F17 !important;
  color: #4A6FA5 !important;
  border-bottom: 1px solid #0E2038 !important;
  font-size: 10px !important;
  font-weight: 700 !important;
  text-transform: uppercase !important;
  letter-spacing: 1px !important;
  padding: 10px 12px !important;
}
table.dataTable tbody tr { background: #0C1A28 !important; }
table.dataTable tbody tr:hover { background: #0E2038 !important; }
table.dataTable tbody td {
  color: #94A3B8 !important;
  border-color: #080F17 !important;
  font-size: 12px !important;
  padding: 8px 12px !important;
}
.dataTables_info, .dataTables_length label,
.dataTables_filter label { color: #4A6FA5 !important; font-size: 11px !important; }
.dataTables_filter input, .dataTables_length select {
  background: #080F17 !important; color: #CBD5E1 !important;
  border: 1px solid #0E2038 !important; border-radius: 5px !important;
  font-size: 11px !important; padding: 4px 8px !important;
}
.paginate_button {
  background: transparent !important; color: #4A6FA5 !important;
  border: 1px solid #0E2038 !important; border-radius: 5px !important;
  font-size: 11px !important; padding: 4px 10px !important; margin: 0 2px !important;
}
.paginate_button.current, .paginate_button:hover {
  background: #1E3A5F !important; color: #FFFFFF !important;
  border-color: #1E3A5F !important;
}

/* ── Mobile responsive ── */
/* ── Mobile responsive ── */
@media (max-width: 768px) {
  html, body { overflow-x: hidden !important; width: 100% !important; }

  .main-sidebar {
    position: fixed !important;
    z-index: 1100 !important;
    transform: translate(-220px, 0) !important;
    transition: transform 0.3s ease !important;
  }
  .sidebar-open .main-sidebar {
    transform: translate(0, 0) !important;
  }
  .content-wrapper {
    margin-left: 0 !important;
    width: 100% !important;
    overflow-x: hidden !important;
  }
  .main-header .navbar {
    margin-left: 0 !important;
    width: calc(100% - 50px) !important;
  }
  .main-header .logo {
    width: 50px !important;
    overflow: hidden !important;
  }
  .main-header .logo span { display: none !important; }
  .main-header .sidebar-toggle {
    background: #080F17 !important;
    color: #3B82F6 !important;
    border-right: 1px solid #0E2038 !important;
  }

  .col-sm-6, .col-sm-4, .col-sm-3, .col-sm-8, .col-sm-9, .col-sm-5, .col-sm-7 {
    width: 100% !important;
    float: none !important;
  }
  .kpi-card { margin-bottom: 10px !important; }
  .kpi-value { font-size: 26px !important; }
  .pred-number { font-size: 52px !important; }
  .header-bar {
    padding: 6px 10px !important;
    gap: 10px !important;
    flex-wrap: wrap !important;
  }
  .header-bar-item { font-size: 9px !important; }
  .score-card { margin-bottom: 10px !important; }
  .box { margin-bottom: 10px !important; }
  .insight-card { padding: 14px !important; }
  .travel-box-times { font-size: 13px !important; }
}
@media (max-width: 480px) {
  .kpi-value { font-size: 22px !important; }
  .pred-number { font-size: 44px !important; }
  .grade-value { font-size: 36px !important; }
}
"

# ── Plotly theme ──────────────────────────────────────────────
BG   <- '#0C1A28'
GRID <- '#0E2038'
TEXT <- '#94A3B8'
PAL  <- c('#3B82F6','#10B981','#F59E0B','#EF4444')

pd <- function(p, legend_h=TRUE) {
  leg <- if(legend_h) list(orientation='h', y=-0.18,
                           font=list(size=11,color=TEXT))
  else         list(font=list(size=11,color=TEXT))
  p %>% layout(
    paper_bgcolor=BG, plot_bgcolor=BG,
    font  =list(family='Inter',color=TEXT,size=11),
    xaxis =list(gridcolor=GRID,zerolinecolor=GRID,
                tickfont=list(size=10,color=TEXT),
                titlefont=list(size=11,color='#4A6FA5')),
    yaxis =list(gridcolor=GRID,zerolinecolor=GRID,
                tickfont=list(size=10,color=TEXT),
                titlefont=list(size=11,color='#4A6FA5')),
    legend=leg,
    margin=list(t=10,b=50,l=50,r=20)
  ) %>% config(displayModeBar=FALSE)
}

make_kpi <- function(val, label, sub) {
  tags$div(
    tags$div(class='kpi-label', label),
    tags$div(class='kpi-value', val),
    tags$div(class='kpi-sub',   sub)
  )
}

# ── UI ────────────────────────────────────────────────────────
ui <- dashboardPage(
  skin='black',
  dashboardHeader(
    title=HTML("<span style='letter-spacing:3px;font-weight:800;
                font-size:13px;'>&#9679; TRAFFICIQ</span>")
  ),
  dashboardSidebar(
    tags$style(HTML(css)),
    tags$div(class='sidebar-filters',
             tags$span(class='filter-label','Data Filter'),
             selectInput('junc','Junction',
                         choices=c('All Junctions'='All','Junction 1'='1',
                                   'Junction 2'='2','Junction 3'='3',
                                   'Junction 4'='4'),selected='All'),
             sliderInput('hrs','Hour Range',min=0,max=23,
                         value=c(0,23),step=1,ticks=FALSE)
    ),
    tags$span(class='nav-label','Navigation'),
    sidebarMenu(id='tabs',
                menuItem('Overview',       tabName='overview',  icon=icon('gauge-high')),
                menuItem('Traffic Trends', tabName='trends',    icon=icon('chart-line')),
                menuItem('Heatmap',        tabName='heatmap',   icon=icon('table-cells')),
                menuItem('ML Predictions', tabName='model',     icon=icon('microchip')),
                menuItem('Anomalies',      tabName='anomalies', icon=icon('triangle-exclamation')),
                menuItem('Smart Insights', tabName='insights',  icon=icon('wand-magic-sparkles')),
                menuItem('Data Explorer',  tabName='data',      icon=icon('database'))
    ),
    tags$div(class='sidebar-footer',
             tags$div(class='sidebar-footer-text',
                      tags$span(class='status-dot'),'System Online'),
             tags$div(style='color:#0E2038;font-size:9px;margin-top:4px;',
                      'TrafficIQ v1.0 \u00b7 Group 3')
    )
  ),
  dashboardBody(
    tabItems(
      
      # ── OVERVIEW ──────────────────────────────────────────
      tabItem(tabName='overview',
              tags$div(class='header-bar',
                       tags$div(class='header-bar-item','Records',
                                tags$span(uiOutput('hb_records',inline=TRUE))),
                       tags$div(class='header-bar-item','Avg Vehicles',
                                tags$span(uiOutput('hb_avg',inline=TRUE))),
                       tags$div(class='header-bar-item','Peak Hour',
                                tags$span(uiOutput('hb_peak',inline=TRUE))),
                       tags$div(class='header-bar-item','Anomalies',
                                tags$span(uiOutput('hb_anom',inline=TRUE)))
              ),
              fluidRow(
                column(3,tags$div(class='kpi-card blue', uiOutput('kpi1'))),
                column(3,tags$div(class='kpi-card green',uiOutput('kpi2'))),
                column(3,tags$div(class='kpi-card amber',uiOutput('kpi3'))),
                column(3,tags$div(class='kpi-card red',  uiOutput('kpi4')))
              ),
              fluidRow(
                column(6,
                       box(title='Average Vehicles by Junction', width=NULL,
                           status='primary', solidHeader=TRUE,
                           plotlyOutput('ov_junction',height='270px'))
                ),
                column(6,
                       box(title='Weekday vs Weekend Pattern', width=NULL,
                           status='primary', solidHeader=TRUE,
                           plotlyOutput('ov_daytype',height='270px'))
                )
              ),
              fluidRow(
                column(12,
                       box(title='Vehicle Count Distribution by Junction',
                           width=NULL, status='primary', solidHeader=TRUE,
                           plotlyOutput('ov_dist',height='210px'))
                )
              )
      ),
      
      # ── TRENDS ────────────────────────────────────────────
      tabItem(tabName='trends',
              fluidRow(
                column(12,
                       box(title='Hourly Average Vehicles by Junction',
                           width=NULL, status='primary', solidHeader=TRUE,
                           tags$span(style='float:right;margin-top:-28px;
                                  background:#1E3A5F;color:#60A5FA;
                                  font-size:9px;font-weight:700;
                                  padding:3px 8px;border-radius:20px;
                                  text-transform:uppercase;
                                  letter-spacing:0.5px;','Live Data'),
                           plotlyOutput('hourly',height='310px'))
                )
              ),
              fluidRow(
                column(12,
                       box(title='Daily Total Vehicle Count Over Time',
                           width=NULL, status='primary', solidHeader=TRUE,
                           plotlyOutput('timeseries',height='310px'))
                )
              )
      ),
      
      # ── HEATMAP ───────────────────────────────────────────
      tabItem(tabName='heatmap',
              fluidRow(
                column(12,
                       box(title='Congestion Intensity — Hour vs Day of Week',
                           width=NULL, status='primary', solidHeader=TRUE,
                           plotlyOutput('heatmap',height='380px'))
                )
              ),
              fluidRow(
                column(12,
                       box(title='Monthly Traffic Volume Trend',
                           width=NULL, status='primary', solidHeader=TRUE,
                           plotlyOutput('monthly',height='270px'))
                )
              )
      ),
      
      # ── ML PREDICTIONS ────────────────────────────────────
      tabItem(tabName='model',
              fluidRow(
                column(5,
                       box(title='Model Performance', width=NULL,
                           status='primary', solidHeader=TRUE,
                           DTOutput('model_tbl'))
                ),
                column(7,
                       box(title='Predicted vs Actual Vehicle Count',
                           width=NULL, status='primary', solidHeader=TRUE,
                           plotlyOutput('pred_plot',height='300px'))
                )
              ),
              fluidRow(
                column(12,
                       box(title='Feature Importance — Random Forest',
                           width=NULL, status='primary', solidHeader=TRUE,
                           plotlyOutput('feat_imp',height='250px'))
                )
              )
      ),
      
      # ── ANOMALIES ─────────────────────────────────────────
      tabItem(tabName='anomalies',
              fluidRow(
                column(4,tags$div(class='kpi-card red',  uiOutput('kpi_anom_total'))),
                column(4,tags$div(class='kpi-card amber', uiOutput('kpi_anom_junc'))),
                column(4,tags$div(class='kpi-card blue',  uiOutput('kpi_anom_z')))
              ),
              fluidRow(
                column(12,
                       box(title='Statistical Anomaly Detection — All Junctions',
                           width=NULL, status='danger', solidHeader=TRUE,
                           plotlyOutput('anom_chart',height='360px'))
                )
              ),
              fluidRow(
                column(12,
                       box(title='Anomalous Events Log',
                           width=NULL, status='danger', solidHeader=TRUE,
                           DTOutput('anom_tbl'))
                )
              )
      ),
      
      # ── SMART INSIGHTS ────────────────────────────────────
      tabItem(tabName='insights',
              tags$div(style='margin-bottom:16px;',
                       tags$div(style='color:#3B82F6;font-size:9px;font-weight:700;
                          text-transform:uppercase;letter-spacing:2px;
                          margin-bottom:4px;',
                                '\u26a1 AI-Powered Intelligence Layer'),
                       tags$div(style='color:#E2E8F0;font-size:18px;font-weight:700;
                          letter-spacing:-0.5px;',
                                'Smart City Decision Engine'),
                       tags$div(style='color:#4A6FA5;font-size:12px;margin-top:4px;',
                                'Real-time predictions powered by XGBoost')
              ),
              fluidRow(
                column(4,
                       tags$div(class='insight-card',
                                tags$div(class='insight-section-title',
                                         '01 \u2014 Live Traffic Predictor'),
                                selectInput('pred_junction','Junction',
                                            choices=c('Junction 1'=1,'Junction 2'=2,
                                                      'Junction 3'=3,'Junction 4'=4)),
                                sliderInput('pred_hour','Hour of Day',
                                            min=0,max=23,value=8,step=1,ticks=FALSE),
                                selectInput('pred_day','Day of Week',
                                            choices=c('Monday'=2,'Tuesday'=3,'Wednesday'=4,
                                                      'Thursday'=5,'Friday'=6,
                                                      'Saturday'=7,'Sunday'=1)),
                                selectInput('pred_month','Month',
                                            choices=c('January'=1,'February'=2,'March'=3,
                                                      'April'=4,'May'=5,'June'=6,'July'=7,
                                                      'August'=8,'September'=9,'October'=10,
                                                      'November'=11,'December'=12)),
                                actionButton('predict_btn','Run Prediction',
                                             class='predict-btn')
                       )
                ),
                column(8,
                       tags$div(class='insight-card',style='min-height:360px;',
                                tags$div(class='insight-section-title','Prediction Output'),
                                uiOutput('pred_result')
                       )
                )
              ),
              tags$div(class='insight-card',
                       tags$div(class='insight-section-title',
                                '02 \u2014 Junction Congestion Scorecard'),
                       uiOutput('scorecard')
              ),
              tags$div(class='insight-card',
                       tags$div(class='insight-section-title',
                                '03 \u2014 Optimal Travel Time Recommender'),
                       fluidRow(
                         column(3,
                                selectInput('travel_junction','Junction',
                                            choices=c('Junction 1'='1','Junction 2'='2',
                                                      'Junction 3'='3','Junction 4'='4')),
                                selectInput('travel_day','Day',
                                            choices=c('Monday'=2,'Tuesday'=3,'Wednesday'=4,
                                                      'Thursday'=5,'Friday'=6,
                                                      'Saturday'=7,'Sunday'=1))
                         ),
                         column(9, plotlyOutput('travel_chart',height='250px'))
                       ),
                       uiOutput('travel_recommendation')
              )
      ),
      
      # ── DATA EXPLORER ─────────────────────────────────────
      tabItem(tabName='data',
              fluidRow(
                column(12,
                       box(title=paste0('Dataset Explorer — ',
                                        format(nrow(df),big.mark=','),
                                        ' Records'),
                           width=NULL, status='primary', solidHeader=TRUE,
                           DTOutput('raw_tbl'))
                )
              )
      )
    )
  )
)

# ── SERVER ────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  fd <- reactive({
    d <- df %>% filter(hour>=input$hrs[1], hour<=input$hrs[2])
    if(input$junc != 'All') d <- d %>% filter(Junction==input$junc)
    d
  })
  
  # Header bar
  output$hb_records <- renderUI(format(nrow(fd()),big.mark=','))
  output$hb_avg     <- renderUI(round(mean(fd()$Vehicles),1))
  output$hb_peak    <- renderUI({
    ph <- fd() %>% group_by(hour) %>%
      summarise(a=mean(Vehicles),.groups='drop') %>%
      slice_max(a,n=1) %>% pull(hour)
    paste0(sprintf('%02d',ph),':00')
  })
  output$hb_anom <- renderUI({
    n <- fd() %>% group_by(Junction) %>%
      mutate(z=abs((Vehicles-mean(Vehicles))/sd(Vehicles))) %>%
      ungroup() %>% filter(z>3) %>% nrow()
    format(n,big.mark=',')
  })
  
  # KPI cards
  output$kpi1 <- renderUI(
    make_kpi(format(nrow(fd()),big.mark=','),
             'Total Records','observations loaded'))
  output$kpi2 <- renderUI(
    make_kpi(round(mean(fd()$Vehicles),1),
             'Avg Vehicles / Hour','across all junctions'))
  output$kpi3 <- renderUI({
    ph <- fd() %>% group_by(hour) %>%
      summarise(a=mean(Vehicles),.groups='drop') %>%
      slice_max(a,n=1) %>% pull(hour)
    make_kpi(paste0(sprintf('%02d',ph),':00'),
             'Peak Congestion Hour','busiest hour of day')
  })
  output$kpi4 <- renderUI({
    n <- fd() %>% group_by(Junction) %>%
      mutate(z=abs((Vehicles-mean(Vehicles))/sd(Vehicles))) %>%
      ungroup() %>% filter(z>3) %>% nrow()
    make_kpi(format(n,big.mark=','),
             'Anomalies Detected','\u00b13\u03c3 threshold')
  })
  
  # Overview
  output$ov_junction <- renderPlotly({
    d <- fd() %>% group_by(Junction) %>%
      summarise(avg=mean(Vehicles),.groups='drop')
    p <- plot_ly(d,x=~Junction,y=~avg,type='bar',
                 marker=list(color=PAL,line=list(color='#060D14',width=1)),
                 text=~round(avg,1),textposition='outside',
                 textfont=list(color=TEXT,size=10),
                 hovertemplate='Junction %{x}<br>Avg: %{y:.1f}<extra></extra>') %>%
      layout(xaxis=list(title='Junction'),yaxis=list(title='Average Vehicles'))
    pd(p,FALSE)
  })
  
  output$ov_daytype <- renderPlotly({
    d <- fd() %>%
      mutate(DayType=if_else(is_weekend==1,'Weekend','Weekday')) %>%
      group_by(hour,DayType) %>%
      summarise(avg=mean(Vehicles),.groups='drop')
    p <- plot_ly(d,x=~hour,y=~avg,color=~DayType,
                 type='scatter',mode='lines',
                 colors=c('Weekday'='#3B82F6','Weekend'='#F59E0B'),
                 line=list(width=2.5)) %>%
      layout(xaxis=list(title='Hour of Day',dtick=3),
             yaxis=list(title='Average Vehicles'))
    pd(p)
  })
  
  output$ov_dist <- renderPlotly({
    p <- plot_ly(fd(),x=~Vehicles,color=~Junction,
                 type='histogram',opacity=0.75,nbinsx=40,colors=PAL) %>%
      layout(barmode='overlay',
             xaxis=list(title='Vehicle Count'),
             yaxis=list(title='Frequency'))
    pd(p)
  })
  
  # Trends
  output$hourly <- renderPlotly({
    d <- fd() %>% group_by(hour,Junction) %>%
      summarise(avg=mean(Vehicles),.groups='drop')
    p <- plot_ly(d,x=~hour,y=~avg,color=~Junction,
                 type='scatter',mode='lines+markers',colors=PAL,
                 line=list(width=2.5),marker=list(size=5)) %>%
      layout(xaxis=list(title='Hour of Day',dtick=1),
             yaxis=list(title='Average Vehicles'))
    pd(p)
  })
  
  output$timeseries <- renderPlotly({
    d <- fd() %>%
      group_by(date=as_date(DateTime),Junction) %>%
      summarise(total=sum(Vehicles),.groups='drop')
    p <- plot_ly(d,x=~date,y=~total,color=~Junction,
                 type='scatter',mode='lines',colors=PAL,
                 line=list(width=1.8)) %>%
      layout(xaxis=list(title='Date'),
             yaxis=list(title='Daily Total Vehicles',tickformat=','))
    pd(p)
  })
  
  # Heatmap
  output$heatmap <- renderPlotly({
    d <- fd() %>% group_by(hour,day_of_week) %>%
      summarise(avg=mean(Vehicles),.groups='drop')
    p <- plot_ly(d,x=~hour,y=~day_of_week,z=~avg,type='heatmap',
                 colorscale=list(list(0,'#060D14'),list(0.4,'#1E3A5F'),
                                 list(0.7,'#3B82F6'),list(1,'#F59E0B')),
                 hovertemplate='Hour: %{x}:00<br>Day: %{y}<br>Avg: %{z:.1f}<extra></extra>',
                 colorbar=list(tickfont=list(color=TEXT,size=10),
                               title=list(text='Avg<br>Vehicles',
                                          font=list(color=TEXT,size=10)))) %>%
      layout(xaxis=list(title='Hour of Day',dtick=1),
             yaxis=list(title='',autorange='reversed'))
    pd(p,FALSE)
  })
  
  output$monthly <- renderPlotly({
    d <- fd() %>% group_by(month,Junction) %>%
      summarise(avg=mean(Vehicles),.groups='drop')
    p <- plot_ly(d,x=~month,y=~avg,color=~Junction,
                 type='scatter',mode='lines+markers',colors=PAL,
                 line=list(width=2.5),marker=list(size=7)) %>%
      layout(xaxis=list(title='Month'),yaxis=list(title='Average Vehicles'))
    pd(p)
  })
  
  # ML Predictions
  output$model_tbl <- renderDT({
    model_results %>% arrange(desc(R2)) %>%
      mutate(Rank=c('\U0001f947','\U0001f948','\U0001f949'),
             R2=paste0(round(R2*100,1),'%'),
             MAE=round(MAE,2),RMSE=round(RMSE,2)) %>%
      select(Rank,Model,MAE,RMSE,R2) %>%
      datatable(rownames=FALSE,
                options=list(dom='t',pageLength=5,
                             columnDefs=list(list(
                               className='dt-center',targets='_all'))))
  })
  
  output$pred_plot <- renderPlotly({
    samp <- df %>% sample_n(min(800,nrow(df)))
    samp_num <- samp %>% mutate(Junction=as.numeric(Junction))
    mat <- xgboost::xgb.DMatrix(
      data=as.matrix(samp_num %>%
                       select(hour,day_num,month_num,is_weekend,is_peak_hour,Junction))
    )
    preds <- predict(saved$xgb_mod,mat)
    maxv  <- max(samp$Vehicles)
    p <- plot_ly() %>%
      add_trace(x=samp$Vehicles,y=preds,type='scatter',mode='markers',
                marker=list(color='#3B82F6',opacity=0.35,size=5),
                name='Predictions') %>%
      add_trace(x=c(0,maxv),y=c(0,maxv),type='scatter',mode='lines',
                line=list(color='#F59E0B',dash='dot',width=1.5),
                name='Perfect Fit') %>%
      layout(xaxis=list(title='Actual Vehicles'),
             yaxis=list(title='Predicted Vehicles'))
    pd(p)
  })
  
  output$feat_imp <- renderPlotly({
    imp <- data.frame(
      Feature=c('Hour of Day','Junction ID','Day of Week',
                'Month','Is Peak Hour','Is Weekend'),
      Score  =c(100,85,62,45,38,30)) %>% arrange(Score)
    cols <- colorRampPalette(c('#1E3A5F','#3B82F6'))(6)
    p <- plot_ly(imp,x=~Score,y=~reorder(Feature,Score),
                 type='bar',orientation='h',
                 marker=list(color=cols,line=list(color='#060D14',width=1)),
                 text=~Score,textposition='outside',
                 textfont=list(color=TEXT,size=10)) %>%
      layout(xaxis=list(title='Relative Importance',range=c(0,115)),
             yaxis=list(title=''))
    pd(p,FALSE)
  })
  
  # Anomalies
  output$kpi_anom_total <- renderUI(
    make_kpi(format(nrow(anomaly_tbl),big.mark=','),
             'Total Anomalies','detected events'))
  output$kpi_anom_junc <- renderUI({
    wj <- anomaly_tbl %>% count(Junction,sort=TRUE) %>%
      slice(1) %>% pull(Junction)
    make_kpi(paste0('Jct ',wj),'Most Affected','highest count')
  })
  output$kpi_anom_z <- renderUI({
    mz <- round(max(abs(anomaly_tbl$z_score)),1)
    make_kpi(paste0(mz,'\u03c3'),'Max Z-Score','most extreme event')
  })
  
  output$anom_chart <- renderPlotly({
    ad <- df %>% group_by(Junction) %>%
      mutate(z=(Vehicles-mean(Vehicles))/sd(Vehicles),
             flag=if_else(abs(z)>3,'Anomaly','Normal')) %>% ungroup()
    norm <- ad %>% filter(flag=='Normal')
    anom <- ad %>% filter(flag=='Anomaly')
    p <- plot_ly() %>%
      add_trace(data=norm,x=~DateTime,y=~Vehicles,color=~Junction,
                colors=PAL,type='scatter',mode='markers',
                marker=list(size=2.5,opacity=0.25),
                name=~paste0('J',Junction)) %>%
      add_trace(data=anom,x=~DateTime,y=~Vehicles,
                type='scatter',mode='markers',
                marker=list(color='#EF4444',size=7,symbol='x-thin-open',
                            line=list(width=2,color='#EF4444')),
                name='Anomaly') %>%
      layout(xaxis=list(title='Date'),yaxis=list(title='Vehicle Count'))
    pd(p)
  })
  
  output$anom_tbl <- renderDT({
    anomaly_tbl %>% arrange(desc(abs(z_score))) %>% head(50) %>%
      mutate(z_score=round(z_score,2),
             Severity=case_when(abs(z_score)>10~'Extreme',
                                abs(z_score)>6 ~'High',
                                abs(z_score)>4 ~'Elevated',
                                TRUE           ~'Moderate')) %>%
      select(DateTime,Junction,Vehicles,z_score,Severity) %>%
      datatable(rownames=FALSE,options=list(pageLength=10,scrollX=TRUE))
  })
  
  # Smart Insights
  pred_val <- eventReactive(input$predict_btn, {
    new <- data.frame(
      hour        =as.numeric(input$pred_hour),
      day_num     =as.numeric(input$pred_day),
      month_num   =as.numeric(input$pred_month),
      is_weekend  =as.numeric(as.numeric(input$pred_day) %in% c(1,7)),
      is_peak_hour=as.numeric(as.numeric(input$pred_hour) %in% c(7,8,9,17,18,19)),
      Junction    =as.numeric(input$pred_junction)
    )
    mat <- xgboost::xgb.DMatrix(as.matrix(new))
    round(predict(saved$xgb_mod,mat))
  }, ignoreNULL=FALSE)
  
  output$pred_result <- renderUI({
    pred <- pred_val()
    lvl  <- if(pred<10)  list(l='Free Flow', c='#10B981',p=8)
    else if(pred<20) list(l='Light',    c='#3B82F6',p=25)
    else if(pred<35) list(l='Moderate', c='#F59E0B',p=50)
    else if(pred<55) list(l='Heavy',    c='#F97316',p=72)
    else             list(l='Severe',   c='#EF4444',p=92)
    tags$div(
      style='display:flex;gap:20px;align-items:flex-start;flex-wrap:wrap;',
      tags$div(style='flex:0 0 auto;',
               tags$div(class='pred-number',style=paste0('color:',lvl$c,';'),pred),
               tags$div(class='pred-label','vehicles / hour')
      ),
      tags$div(style='flex:1;min-width:200px;',
               tags$div(class='congestion-pill',
                        style=paste0('background:',lvl$c,'18;color:',lvl$c,
                                     ';border:1px solid ',lvl$c,'44;'),lvl$l),
               tags$div(class='pred-bar-wrap',
                        tags$div(class='pred-bar-fill',
                                 style=paste0('width:',lvl$p,'%;background:',lvl$c,';'))
               ),
               tags$div(class='pred-detail',
                        paste0('Junction ',input$pred_junction,' on ',
                               c('Sun','Mon','Tue','Wed','Thu','Fri','Sat')[as.numeric(input$pred_day)],
                               ' at ',sprintf('%02d',as.numeric(input$pred_hour)),
                               ':00 \u2014 approximately ',pred,' vehicles/hour. Level: ',lvl$l,'.')
               ),
               tags$br(),
               tags$div(style='display:flex;gap:10px;flex-wrap:wrap;',
                        lapply(list(
                          list('Hour',sprintf('%02d:00',as.numeric(input$pred_hour))),
                          list('Junction',paste0('J',input$pred_junction)),
                          list('Period',if(as.numeric(input$pred_hour) %in% c(7,8,9,17,18,19)) 'Peak' else 'Off-Peak'),
                          list('Day Type',if(as.numeric(input$pred_day) %in% c(1,7)) 'Weekend' else 'Weekday')
                        ), function(x) {
                          tags$div(style='background:#080F17;border:1px solid #0E2038;
                            border-radius:6px;padding:8px 12px;',
                                   tags$div(style='color:#4A6FA5;font-size:9px;font-weight:700;
                              text-transform:uppercase;letter-spacing:1px;',x[[1]]),
                                   tags$div(style='color:#E2E8F0;font-size:13px;font-weight:700;margin-top:2px;',x[[2]])
                          )
                        })
               )
      )
    )
  })
  
  output$scorecard <- renderUI({
    g <- df %>% group_by(Junction) %>%
      summarise(avg_v=mean(Vehicles),peak_v=mean(Vehicles[is_peak_hour==1]),
                var_v=sd(Vehicles),
                n_anom=sum(abs((Vehicles-mean(Vehicles))/sd(Vehicles))>3),
                .groups='drop') %>%
      mutate(
        score=100-(avg_v/max(avg_v)*40)-(var_v/max(var_v)*30)-(n_anom/max(n_anom)*30),
        grade=case_when(score>=80~'A',score>=65~'B',score>=50~'C',score>=35~'D',TRUE~'F'),
        col  =case_when(grade=='A'~'#10B981',grade=='B'~'#3B82F6',
                        grade=='C'~'#F59E0B',grade=='D'~'#F97316',TRUE~'#EF4444'),
        rec  =case_when(grade=='A'~'Excellent flow. No intervention needed.',
                        grade=='B'~'Good flow. Monitor peak windows.',
                        grade=='C'~'Moderate congestion. Adjust signal timing.',
                        grade=='D'~'High congestion. Evaluate road expansion.',
                        TRUE      ~'Critical. Immediate action required.')
      )
    fluidRow(lapply(1:nrow(g), function(i) {
      r <- g[i,]
      column(3,
             tags$div(class='score-card',style=paste0('border-top:3px solid ',r$col,';'),
                      tags$div(class='grade-label',paste0('Junction ',r$Junction)),
                      tags$div(class='grade-value',style=paste0('color:',r$col,';'),r$grade),
                      tags$div(class='grade-score',style=paste0('color:',r$col,';'),paste0(round(r$score),' / 100')),
                      tags$hr(class='grade-divider'),
                      tags$div(class='grade-stat',
                               tags$span(class='grade-stat-label','Avg vehicles'),
                               tags$span(class='grade-stat-value',round(r$avg_v,1))),
                      tags$div(class='grade-stat',
                               tags$span(class='grade-stat-label','Peak avg'),
                               tags$span(class='grade-stat-value',round(r$peak_v,1))),
                      tags$div(class='grade-stat',
                               tags$span(class='grade-stat-label','Anomalies'),
                               tags$span(class='grade-stat-value',r$n_anom)),
                      tags$hr(class='grade-divider'),
                      tags$div(class='grade-rec',
                               style=paste0('background:',r$col,'12;color:',r$col,';'),r$rec)
             )
      )
    }))
  })
  
  output$travel_chart <- renderPlotly({
    d <- df %>%
      filter(Junction==input$travel_junction,
             day_num==as.numeric(input$travel_day)) %>%
      group_by(hour) %>% summarise(avg=mean(Vehicles),.groups='drop') %>%
      mutate(col=case_when(avg<10~'#10B981',avg<25~'#3B82F6',
                           avg<40~'#F59E0B',TRUE~'#EF4444'))
    avg_line <- mean(d$avg)
    p <- plot_ly(d,x=~hour,y=~avg,type='bar',
                 marker=list(color=~col,line=list(color='#060D14',width=0.5)),
                 hovertemplate='%{x}:00 \u2014 %{y:.1f} vehicles<extra></extra>') %>%
      add_trace(x=c(0,23),y=c(avg_line,avg_line),type='scatter',mode='lines',
                line=list(color='#FFFFFF',dash='dot',width=1,opacity=0.3),
                showlegend=FALSE) %>%
      layout(xaxis=list(title='Hour of Day',dtick=1),
             yaxis=list(title='Avg Vehicles'),bargap=0.2)
    pd(p,FALSE)
  })
  
  output$travel_recommendation <- renderUI({
    d <- df %>%
      filter(Junction==input$travel_junction,
             day_num==as.numeric(input$travel_day)) %>%
      group_by(hour) %>% summarise(avg=mean(Vehicles),.groups='drop')
    best  <- d %>% slice_min(avg,n=3) %>% pull(hour)
    worst <- d %>% slice_max(avg,n=3) %>% pull(hour)
    bf <- paste0(sprintf('%02d',sort(best)),':00',collapse='  \u00b7  ')
    wf <- paste0(sprintf('%02d',sort(worst)),':00',collapse='  \u00b7  ')
    days <- c('1'='Sunday','2'='Monday','3'='Tuesday','4'='Wednesday',
              '5'='Thursday','6'='Friday','7'='Saturday')
    dl <- days[input$travel_day]
    tags$div(style='display:flex;gap:12px;margin-top:14px;flex-wrap:wrap;',
             tags$div(class='travel-box',
                      style='flex:1;min-width:200px;background:#10B98112;border-left:3px solid #10B981;',
                      tags$div(class='travel-box-title',style='color:#10B981;','\u2713  Best Times'),
                      tags$div(class='travel-box-times',style='color:#FFFFFF;',bf),
                      tags$div(class='travel-box-sub',style='color:#4A6FA5;',
                               paste0('Lowest congestion \u2014 ',dl,', Junction ',input$travel_junction))
             ),
             tags$div(class='travel-box',
                      style='flex:1;min-width:200px;background:#EF444412;border-left:3px solid #EF4444;',
                      tags$div(class='travel-box-title',style='color:#EF4444;','\u2717  Avoid These Hours'),
                      tags$div(class='travel-box-times',style='color:#FFFFFF;',wf),
                      tags$div(class='travel-box-sub',style='color:#4A6FA5;',
                               paste0('Peak congestion \u2014 ',dl,', Junction ',input$travel_junction))
             )
    )
  })
  
  output$raw_tbl <- renderDT({
    fd() %>%
      select(DateTime,Junction,Vehicles,hour,day_of_week,is_weekend,is_peak_hour) %>%
      datatable(rownames=FALSE,filter='top',
                options=list(pageLength=15,scrollX=TRUE))
  })
}

shinyApp(ui=ui, server=server)