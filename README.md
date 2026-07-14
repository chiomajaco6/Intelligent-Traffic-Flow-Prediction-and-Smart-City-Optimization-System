# Intelligent Traffic Flow Prediction and Smart City Optimization System

<img width="1919" height="960" alt="image" src="https://github.com/user-attachments/assets/b8a88ad8-d726-4793-9124-7d772e6dadeb" />

## Overview

This repository implements an R-based workflow for traffic flow prediction and smart-city optimization. It includes data preprocessing, exploratory data analysis (EDA), model training and evaluation, anomaly detection, and a Shiny application for interactive visualization and demonstration.

## Features

- Data cleaning and preprocessing pipelines
- Exploratory data analysis and summary statistics
- Model training and selection with saved model artifacts
- Anomaly detection on traffic streams
- Shiny app for visualization and interactive exploration

## Repository Structure

- [traffic-prediction/](traffic-prediction/) — primary R project folder
  - [traffic-prediction/01_preprocessing.R](traffic-prediction/01_preprocessing.R)
  - [traffic-prediction/02_eda.R](traffic-prediction/02_eda.R)
  - [traffic-prediction/03_modeling.R](traffic-prediction/03_modeling.R)
  - [traffic-prediction/04_anomaly_detection.R](traffic-prediction/04_anomaly_detection.R)
  - [traffic-prediction/app.R](traffic-prediction/app.R)
  - [traffic-prediction/scripts/00_install_packages.R](traffic-prediction/scripts/00_install_packages.R)
- [traffic-prediction/data/](traffic-prediction/data/) — datasets (raw and cleaned)
- [models/](models/) — serialized model objects (e.g., `best_model.rds`)
- [traffic-prediction/output/](traffic-prediction/output/) — analysis outputs and reports
- [report/](report/) — project reporting materials

## Data

- Primary datasets are located in [traffic-prediction/data/](traffic-prediction/data/):
  - `traffic.csv` — raw sample data
  - `traffic_clean.csv` — cleaned dataset used for modeling
  - `traffic_small.csv` — small sample for fast testing

Data columns and types may vary; inspect the CSV header or open the preprocessing script to see expected fields.

## Prerequisites

- R (version 4.0 or later recommended)
- The project uses common data science packages (e.g., `tidyverse`, `data.table`, `caret`, `shiny`).
- Install dependencies by running the included installer script:

```r
source("traffic-prediction/scripts/00_install_packages.R")
```

## Quick Start

1. Install packages (see above).
2. Preprocess the data:

```bash
Rscript traffic-prediction/01_preprocessing.R
```

3. Run EDA and generate summary statistics:

```bash
Rscript traffic-prediction/02_eda.R
```

4. Train and evaluate models:

```bash
Rscript traffic-prediction/03_modeling.R
```

5. Run anomaly detection:

```bash
Rscript traffic-prediction/04_anomaly_detection.R
```

6. Launch the Shiny app for interactive exploration:

```r
shiny::runApp("traffic-prediction")
```

## Using Trained Models

Saved models are R serialized objects (`.rds`) in the `models/` directory. To load and use a model:

```r
model <- readRDS("models/best_model.rds")
# Example prediction (adjust to your data and model API):
preds <- predict(model, newdata = your_new_data)
```

## Outputs

- Summary statistics and model comparison CSVs are written to [traffic-prediction/output/](traffic-prediction/output/).
- Anomaly detection results are available as `anomaly_summary.csv` in the output folder.

## Reproducibility

- The scripts are designed to be run in sequence. For reproducible runs, use the provided CSV files or fix seeds inside the modeling scripts if needed.

## Development & Contribution

- To contribute, fork the repository and open a pull request with a clear description of changes and tests/examples.
- For issues and feature requests, open an issue in the repository.

## Deployment

- The Shiny app can be deployed to `shinyapps.io` or another Shiny hosting service. Deployment configuration (if used) is under `rsconnect/`.

## License

- No license file is included. Add a `LICENSE` file to specify reuse terms (e.g., MIT, Apache 2.0).

## Contact

- For questions, open an issue or contact the repository maintainer.


This is the link to the web application 
https://trafficiq.shinyapps.io/trafficiq/