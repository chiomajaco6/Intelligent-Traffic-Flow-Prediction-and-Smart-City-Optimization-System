# =============================================================
# FILE:    scripts/03_modeling.R
# PURPOSE: Train, evaluate, and compare ML models
# INPUT:   data/traffic_clean.csv
# OUTPUT:  models/best_model.rds | output/model_comparison.csv
# PRD:     OBJ-05, OBJ-06 | FR-16 to FR-23
# =============================================================

library(tidyverse)
library(caret)
library(randomForest)
library(xgboost)
library(Metrics)

set.seed(42)

# Prepare modeling dataset -------------------------------------
df <- read_csv('data/traffic_clean.csv', show_col_types = FALSE) %>%
  mutate(Junction = as.factor(Junction)) %>%
  select(Vehicles, hour, day_num, month_num,
         is_weekend, is_peak_hour, Junction) %>%
  na.omit()

message('Modeling data: ', nrow(df), ' rows, ', ncol(df), ' columns')

# 80/20 Train/test split ---------------------------------------
idx        <- createDataPartition(df$Vehicles, p = 0.80, list = FALSE)
train_data <- df[ idx, ]
test_data  <- df[-idx, ]
message('Train: ', nrow(train_data), ' | Test: ', nrow(test_data))

# 5-fold CV control -------------------------------------------
cv_ctrl <- trainControl(method='cv', number=5, verboseIter=TRUE)

# Helper: evaluate any model ----------------------------------
eval_model <- function(model, test, name) {
  p <- predict(model, test)
  data.frame(
    Model = name,
    MAE   = round(mae(test$Vehicles, p), 3),
    RMSE  = round(rmse(test$Vehicles, p), 3),
    R2    = round(cor(test$Vehicles, p)^2, 4)
  )
}

# MODEL 1: Linear Regression ----------------------------------
message('\nTraining Linear Regression...')
lm_mod     <- train(Vehicles ~ ., data=train_data, method='lm')
lm_results <- eval_model(lm_mod, test_data, 'Linear Regression')
message('LR done. R2 = ', lm_results$R2)

# MODEL 2: Random Forest --------------------------------------
message('\nTraining Random Forest (please wait)...')
rf_mod <- train(
  Vehicles ~ ., data = train_data, method = 'rf',
  trControl = cv_ctrl,
  tuneGrid  = expand.grid(mtry = c(2, 3, 4)),
  ntree     = 200
)
rf_results <- eval_model(rf_mod, test_data, 'Random Forest')
message('RF done. R2 = ', rf_results$R2)

# MODEL 3: XGBoost (direct — bypasses caret compatibility issue)
message('\nTraining XGBoost (please wait)...')

# Convert to numeric matrix (xgboost requires this)
train_num <- train_data %>%
  mutate(Junction = as.numeric(Junction))
test_num  <- test_data %>%
  mutate(Junction = as.numeric(Junction))

train_matrix <- xgb.DMatrix(
  data  = as.matrix(train_num %>% select(-Vehicles)),
  label = train_num$Vehicles
)
test_matrix <- xgb.DMatrix(
  data  = as.matrix(test_num %>% select(-Vehicles)),
  label = test_num$Vehicles
)

xgb_params <- list(
  objective        = 'reg:squarederror',
  eta              = 0.1,
  max_depth        = 6,
  subsample        = 0.8,
  colsample_bytree = 0.8
)

xgb_mod <- xgb.train(
  params  = xgb_params,
  data    = train_matrix,
  nrounds = 200,
  verbose = 0
)

xgb_preds  <- predict(xgb_mod, test_matrix)
xgb_results <- data.frame(
  Model = 'XGBoost',
  MAE   = round(mae(test_num$Vehicles, xgb_preds), 3),
  RMSE  = round(rmse(test_num$Vehicles, xgb_preds), 3),
  R2    = round(cor(test_num$Vehicles, xgb_preds)^2, 4)
)
message('XGBoost done. R2 = ', xgb_results$R2)

# Compile results ---------------------------------------------
comparison <- bind_rows(lm_results, rf_results, xgb_results) %>%
  arrange(desc(R2))

message('\n=== MODEL COMPARISON ===')
print(comparison)
write_csv(comparison, 'output/model_comparison.csv')
message('Saved -> output/model_comparison.csv')

# Feature importance plot -------------------------------------
rf_imp <- varImp(rf_mod)$importance %>%
  rownames_to_column('Feature') %>%
  arrange(desc(Overall))

p_imp <- ggplot(rf_imp, aes(x=reorder(Feature, Overall), y=Overall)) +
  geom_col(fill='#2E75B6') + coord_flip() +
  labs(title='Random Forest — Feature Importance',
       x=NULL, y='Importance Score') +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face='bold', color='#1F3864'))
ggsave('output/plot_08_feature_importance.png', p_imp, width=8, height=5, dpi=150)
message('Saved: plot_08_feature_importance.png')

# Predicted vs Actual -----------------------------------------
best_name <- comparison$Model[1]
message('Best model: ', best_name)

pred_vals <- if (best_name == 'XGBoost') {
  xgb_preds
} else if (best_name == 'Random Forest') {
  predict(rf_mod, test_data)
} else {
  predict(lm_mod, test_data)
}

actual_vals <- if (best_name == 'XGBoost') {
  test_num$Vehicles
} else {
  test_data$Vehicles
}

pred_df <- data.frame(Actual = actual_vals, Predicted = pred_vals)

p_pred <- ggplot(pred_df, aes(x=Actual, y=Predicted)) +
  geom_point(alpha=0.3, color='#2E75B6', size=0.8) +
  geom_abline(slope=1, intercept=0, color='red', linetype='dashed', size=1) +
  labs(title=paste0('Predicted vs Actual — ', best_name),
       subtitle=paste0('R² = ', comparison$R2[1],
                       '  RMSE = ', comparison$RMSE[1]),
       x='Actual Vehicles', y='Predicted Vehicles') +
  theme_minimal(base_size=13) +
  theme(plot.title=element_text(face='bold', color='#1F3864'))
ggsave('output/plot_09_predicted_vs_actual.png', p_pred, width=8, height=7, dpi=150)
message('Saved: plot_09_predicted_vs_actual.png')

# Save best model ---------------------------------------------
saveRDS(
  list(
    rf_mod  = rf_mod,
    lm_mod  = lm_mod,
    xgb_mod = xgb_mod,
    best    = best_name,
    comparison = comparison
  ),
  'models/best_model.rds'
)
message('All models saved -> models/best_model.rds')
message('\nModeling COMPLETE!')