pacman::p_load(tidyverse, leaps, glmnet, pls)

# Indlæs data fra data_cleaning.R script
data_clean <- readRDS("data/joined_data_clean.rds")
view(data_clean)

# Variabelvalg til machine learning modeller

# Før 10 dage før kampen (ingen billetsalg endnu)
vff_variabler <- data_clean %>%
  select(tilskuere, runde, år, ugedag, tidsgruppe,
         seneste_kamp, vff_vundet_2, regn_gruppe,
         temperatur, vind, akk_indbyggertal, kamp_gruppe, ferie_flag) %>% na.omit()

# 10 dage før kampen (salg og fraktion tilgængelig)
vff_10 <- data_clean %>%
  select(tilskuere, salg_10, frak_10, runde, år, ugedag, tidsgruppe,
         seneste_kamp, vff_vundet_2, akk_indbyggertal, kamp_gruppe, ferie_flag) %>%  na.omit()

# 7 dage før kampen (akkumuleret salg og fraktion)
vff_7 <- data_clean %>%
  mutate(
    salg_7_akk = salg_10 + salg_7,
    frak_7_akk = frak_10 + frak_7
  ) %>%
  select(tilskuere, salg_7_akk, frak_7_akk, runde, år, ugedag, tidsgruppe,
         seneste_kamp, vff_vundet_2, akk_indbyggertal, kamp_gruppe, ferie_flag) %>% na.omit()

# 3 dage før kampen (akkumuleret salg og fraktion)
vff_3 <- data_clean %>%
  mutate(
    salg_3_akk = salg_10 + salg_7 + salg_3,
    frak_3_akk = frak_10 + frak_7 + frak_3
  ) %>%
  select(tilskuere, salg_3_akk, frak_3_akk, runde, år, ugedag, tidsgruppe,
         seneste_kamp, vff_vundet_2, akk_indbyggertal, kamp_gruppe, ferie_flag) %>% na.omit()

### note: Jeg har også prøvet at dividere billetsalg med fraktion, og dette giver et "perfekt" prediktion.
#         men jeg tror at vi bare skal have akkumuleret tal for at få det mest forståelige RMSE resultat
# 
# mutate(
# salg_3_akkumuleret = salg_10 + salg_7 + salg_3,
# frak_3_akkumuleret = frak_10 + frak_7 + frak_3,
# tilskuere_est = salg_3_akkumuleret / frak_3_akkumuleret'
# )
# 
# mutate(
# salg_7_akkumuleret = salg_10 + salg_7,
# frak_7_akkumuleret = frak_10 + frak_7,
# tilskuere_est = salg_7_akkumuleret / frak_7_akkumuleret
# )
# 
# mutate(
# tilskuere_est = salg_10 / frak_10
# )


# Funktion til predict i best subset selection
predict.regsubsets <- function(object, newdata, id, formula) {
  mat <- model.matrix(formula, newdata)
  coefi <- coef(object, id = id)
  vars <- names(coefi)
  mat[, vars, drop = FALSE] %*% coefi
}

# Funktion til at køre alle machine learning modeller
run_all_models <- function(data, response, train_frac = 2/3, seed = 1) {
  
  set.seed(seed)
  n <- nrow(data)
  train_idx <- sample(1:n, floor(train_frac * n))
  test_idx <- setdiff(1:n, train_idx)
  train <- data[train_idx, ]
  test <- data[test_idx, ]
  
  y_train <- train[[response]]
  y_test  <- test[[response]]
  
  formula <- as.formula(paste(response, "~ ."))
  
  # ----------------- Best Subset Selection -----------------
  k <- 10
  folds <- sample(rep(1:k, length.out = nrow(train)))
  cv.errors <- matrix(NA, k, ncol(train) - 1)
  
  for (j in 1:k) {
    best.fit.cv <- regsubsets(formula, data = train[folds != j, ], nvmax = ncol(train) - 1)
    for (i in 1:(ncol(train) - 1)) {
      pred <- predict.regsubsets(best.fit.cv, train[folds == j, ], id = i, formula = formula)
      cv.errors[j, i] <- mean((train[[response]][folds == j] - pred)^2)
    }
  }
  
  best.size <- which.min(colMeans(cv.errors))
  best.fit <- regsubsets(formula, data = train, nvmax = best.size)
  best.pred <- predict.regsubsets(best.fit, test, id = best.size, formula = formula)
  best_subset_rmse <- sqrt(mean((best.pred - y_test)^2))
  best_subset_coef <- coef(best.fit, best.size)
  
  # ----------------- Ridge Regression -----------------
  x_train <- model.matrix(formula, train)[, -1]
  x_test  <- model.matrix(formula, test)[, -1]
  
  ridge.cv <- cv.glmnet(x_train, y_train, alpha = 0)
  ridge.pred <- predict(ridge.cv, newx = x_test, s = ridge.cv$lambda.min)
  ridge_rmse <- sqrt(mean((ridge.pred - y_test)^2))
  ridge_coef <- as.matrix(coef(ridge.cv, s = "lambda.min"))
  
  # ----------------- Lasso Regression -----------------
  lasso.cv <- cv.glmnet(x_train, y_train, alpha = 1)
  lasso.pred <- predict(lasso.cv, newx = x_test, s = lasso.cv$lambda.min)
  lasso_rmse <- sqrt(mean((lasso.pred - y_test)^2))
  lasso_coef <- as.matrix(coef(lasso.cv, s = "lambda.min"))
  
  # ----------------- PCR -----------------
  pcr.fit <- pcr(formula, data = train, scale = TRUE, validation = "CV")
  max_comp <- min(nrow(train) - 1, ncol(train) - 1)
  cv_msep <- MSEP(pcr.fit)$val[1,1,]
  best_pcr_ncomp <- min(which.min(cv_msep), max_comp)
  pcr.pred <- predict(pcr.fit, newdata = test, ncomp = best_pcr_ncomp)
  pcr_rmse <- sqrt(mean((pcr.pred - y_test)^2))
  
  # ----------------- PLS -----------------
  pls.fit <- plsr(formula, data = train, scale = TRUE, validation = "CV")
  cv_msep_pls <- MSEP(pls.fit)$val[1,1,]
  best_pls_ncomp <- min(which.min(cv_msep_pls), max_comp)
  pls.pred <- predict(pls.fit, newdata = test, ncomp = best_pls_ncomp)
  pls_rmse <- sqrt(mean((pls.pred - y_test)^2))
  
  # ----------------- Returner resultater -----------------
  list(
    RMSE = tibble(
      Model = c("Best subset", "Ridge", "Lasso", "PCR", "PLS"),
      RMSE = c(best_subset_rmse, ridge_rmse, lasso_rmse, pcr_rmse, pls_rmse)
    ),
    BestSubset = list(coef = best_subset_coef, size = best.size),
    Ridge = list(coef = ridge_coef, lambda_min = ridge.cv$lambda.min),
    Lasso = list(coef = lasso_coef, lambda_min = lasso.cv$lambda.min),
    PCR = list(ncomp = best_pcr_ncomp, model = pcr.fit),
    PLS = list(ncomp = best_pls_ncomp, model = pls.fit)
  )
}

# Kør modeller på de udvalgte variabler
results_tilskuere <- run_all_models(vff_variabler, "tilskuere")
results_10 <- run_all_models(vff_10, "tilskuere")
results_7  <- run_all_models(vff_7, "tilskuere")
results_3  <- run_all_models(vff_3, "tilskuere")

results_tilskuere
results_10
results_7
results_3

