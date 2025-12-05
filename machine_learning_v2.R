pacman::p_load(tidyverse, leaps, glmnet, pls)

# ============================================================
# LOAD DATA
# ============================================================
data_clean <- readRDS("data/joined_data_clean.rds")
view(data_clean)
# ============================================================
# YOUR FOUR DATASETS
# ============================================================
vff_variabler <- data_clean %>%
  select(tilskuere, runde, år, ugedag, tidsgruppe,
         seneste_kamp, vff_vundet_2, regn_gruppe,
         temperatur, vind, akk_indbyggertal, kamp_gruppe, ferie_flag) %>%  na.omit()

vff_10 <- data_clean %>%
  select(d10_tilskuere, d10, runde, år, ugedag, tidsgruppe,
         seneste_kamp, vff_vundet_2, akk_indbyggertal, kamp_gruppe, ferie_flag) %>%  na.omit()

vff_7 <- data_clean %>%
  select(d7_tilskuere, d7, d10_tilskuere, d10, runde, år, ugedag, tidsgruppe,
         seneste_kamp, vff_vundet_2, akk_indbyggertal, kamp_gruppe, ferie_flag) %>%  na.omit()

vff_3 <- data_clean %>%
  select(d3_tilskuere, d3, d7_tilskuere, d7, d10_tilskuere, d10, runde, år, ugedag, tidsgruppe,
         seneste_kamp, vff_vundet_2, akk_indbyggertal, kamp_gruppe, ferie_flag) %>%  na.omit() %>%  view()


# ============================================================
# SAFE REGSUBSETS PREDICT FUNCTION
# ============================================================
predict.regsubset.safe <- function(object, newdata, id, formula) {
  mat <- model.matrix(formula, newdata)
  coefi <- coef(object, id = id)
  vars <- names(coefi)
  mat <- mat[, vars, drop = FALSE]
  drop(mat %*% coefi)
}

# ============================================================
# FUNCTION: RUN ALL MODELS ON ANY DATASET
# ============================================================
run_all_models <- function(data, response) {
  
  formula <- as.formula(paste(response, "~ ."))
  
  set.seed(8)
  train_idx <- sample(1:nrow(data), nrow(data) / 2)
  train <- data[train_idx, ]
  test  <- data[-train_idx, ]
  
  y_train <- train[[response]]
  y_test  <- test[[response]]
  
  # ============================================================
  # BEST SUBSET CV
  # ============================================================
  k <- 10
  folds <- sample(rep(1:k, length.out = nrow(train)))
  cv.errors <- matrix(NA, k, ncol(train) - 1)
  
  for (j in 1:k) {
    best.fit.cv <- regsubsets(
      formula,
      data = train[folds != j, ],
      nvmax = ncol(train) - 1
    )
    
    for (i in 1:(ncol(train) - 1)) {
      
      pred <- predict.regsubset.safe(
        best.fit.cv,
        newdata = train[folds == j, ],
        id = i,
        formula = formula
      )
      
      cv.errors[j, i] <- mean((train[[response]][folds == j] - pred)^2)
    }
  }
  
  best.size <- which.min(colMeans(cv.errors))
  
  best.fit <- regsubsets(
    formula,
    data = train,
    nvmax = best.size
  )
  
  best.pred <- predict.regsubset.safe(
    best.fit,
    newdata = test,
    id = best.size,
    formula = formula
  )
  
  best_subset_rmse <- sqrt(mean((best.pred - y_test)^2))
  
  
  # ============================================================
  # RIDGE / LASSO
  # ============================================================
  x_train <- model.matrix(formula, train)[, -1]
  x_test  <- model.matrix(formula, test)[, -1]
  
  # Ridge
  ridge.cv <- cv.glmnet(x_train, y_train, alpha = 0)
  ridge.pred <- predict(ridge.cv, newx = x_test, s = ridge.cv$lambda.min)
  ridge_rmse <- sqrt(mean((ridge.pred - y_test)^2))
  
  # Lasso
  lasso.cv <- cv.glmnet(x_train, y_train, alpha = 1)
  lasso.pred <- predict(lasso.cv, newx = x_test, s = lasso.cv$lambda.min)
  lasso_rmse <- sqrt(mean((lasso.pred - y_test)^2))
  
  
  # ============================================================
  # PCR
  # ============================================================
  pcr.fit <- pcr(
    formula,
    data = train,
    scale = TRUE,
    validation = "CV"
  )
  
  pcr.pred <- predict(pcr.fit, newdata = test, ncomp = 10)
  pcr_rmse <- sqrt(mean((pcr.pred - y_test)^2))
  
  
  # ============================================================
  # PLS
  # ============================================================
  pls.fit <- plsr(
    formula,
    data = train,
    scale = TRUE,
    validation = "CV"
  )
  
  pls.pred <- predict(pls.fit, newdata = test, ncomp = 10)
  pls_rmse <- sqrt(mean((pls.pred - y_test)^2))
  
  
  # ============================================================
  # RETURN RESULTS
  # ============================================================
  tibble(
    Model = c("Best subset", "Ridge", "Lasso", "PCR", "PLS"),
    RMSE = c(best_subset_rmse, ridge_rmse, lasso_rmse, pcr_rmse, pls_rmse)
  )
}

# ============================================================
# RUN MODELS ON ALL FOUR OUTCOMES
# ============================================================

results_tilskuere <- run_all_models(vff_variabler, "tilskuere")
results_10 <- run_all_models(vff_10, "d10_tilskuere")
results_7  <- run_all_models(vff_7, "d7_tilskuere")
results_3  <- run_all_models(vff_3, "d3_tilskuere")

results_tilskuere
results_10
results_7
results_3






