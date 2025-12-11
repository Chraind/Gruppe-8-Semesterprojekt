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
  mat <- model.matrix(formula, newdata) # Predictors bliver lavet om til tal som er klar til matrix multiplikation
                                        # Orthogonal factor som ugedag laves om til ugedagL, ugedagQ, ugedagC osv.
                                        # Kategoriske variabler som kamp_gruppe laves om til kamp_gruppemiddel, kamp_gruppestor 
  coefi <- coef(object, id = id) # Fordi regsubsets ikke har en predict() funktion, 
                                 # kan vi udregne de bedste koefficienter med id predictors
  vars <- names(coefi) # Vi får navnene af predictors (kolonnerne) 
  mat[, vars, drop = FALSE] %*% coefi # Her laves matrix multiplikation. Vi ganger predictors i tal-format med koefficienterne
}

# Funktion til at køre alle machine learning modeller
run_all_models <- function(data, response, train_frac = 2/3, seed = 8) { 
  # Vi definerer en funktion, som skal køre ML modellerne.
  # data = vores udvalgte variabler. response = variablen vi ønsker at predict (tilskuere), 
  # train_frac = 2/3, vi bruger 2/3 af vores rows til predictions.
  # seed = 8, seed inkluderes i funktionen så hvis man ønsker at køre funktionen med en anden seed, 
  # kan man tilføje det når man vil se resultaterne. For eksempel results_10 <- run_all_models(vff_10, "tilskuere", seed = 69420)
  
  set.seed(seed) 
  n <- nrow(data)
  train_idx <- sample(1:n, floor(train_frac * n)) # Her splittes tilfældige rows til training. Den ganger 2/3 med totale rows. 
  test_idx <- setdiff(1:n, train_idx) # Her er den sidste 1/3 som bliver brugt til tests, fundet med setdiff() funktion
  train <- data[train_idx, ]
  test <- data[test_idx, ] # Test og training subsets vælges 
  
  y_train <- train[[response]]
  y_test  <- test[[response]] # Vi ekstraherer det tal vi ønsker at finde, i vores tilfælde tilskuere
  
  formula <- as.formula(paste(response, "~ .")) # Laver en ligning der sætter tilskuere mod alle andre predictors
 
  # ----------------- Lineær Regression (alle variabler) -----------------
  lm.fit <- lm(formula, data = train)
  lm.pred <- predict(lm.fit, newdata = test)
  lm_rmse <- sqrt(mean((lm.pred - y_test)^2))
  
  # ----------------- Best Subset Selection -----------------
  k <- 10
  folds <- sample(rep(1:k, length.out = nrow(train))) # Tildeler tilfældigt hver training row til et af de 10 folds. 
  cv.errors <- matrix(NA, k, ncol(train) - 1) # En tom matrix til at gemme MSE for hver fold og subset størrelse, bliver brugt i for loop 
                                              # subset størrelse kan f.eks. være subset 4 = model med 4 kolonner/predictors.
  
  for (j in 1:k) { # Loop over hver fold. regsubsets() finder de bedste modeller med 1 til p predictors.
                   # Vi vælger alle rækker der ikke tilhører fold j, så der trænes på 9 folds eller 9/10 af data
    best.fit.cv <- regsubsets(formula, data = train[folds != j, ], nvmax = ncol(train) - 1)
    
    for (i in 1:(ncol(train) - 1)) { # Loop over hver subset størrelse (med 1 predictor, 2 predictor, 3 predictor osv.) inde i hver fold.
                                     # predict.regsubsets(...) laver prediktioner på fold "j" (test fold) med subset størrelsen "i"
                                     # mean(...) beregner MSE for hver fold "j" og subset størrelse "i". cv.errors får tildelt MSE
                                     # Dette gøres for at finde den subset model der har lavest fejl. F.eks. subset 3 med 3 predictors
      pred <- predict.regsubsets(best.fit.cv, train[folds == j, ], id = i, formula = formula)
      cv.errors[j, i] <- mean((train[[response]][folds == j] - pred)^2)
    }
  }
  
  best.size <- which.min(colMeans(cv.errors)) # colMeans() giver gennemsnittet af hver kolonne MSE på tværs af alle 10 folds. 
                                              # which.min() giver kolonnen (model) med mindst gennemsnitlige MSE.
                                              # vi får altså den model der giver den bedste balance mellem bias og variance
  best_subset_cv_mse <- min(colMeans(cv.errors))
  best.fit <- regsubsets(formula, data = train, nvmax = best.size) # regsubsets(...) laver best subset selection på alt træningsdata 
                                                                   # og bruger best.size antal kolonner
  best.pred <- predict.regsubsets(best.fit, test, id = best.size, formula = formula) # Funktion vi lavede tidligere
  best_subset_rmse <- sqrt(mean((best.pred - y_test)^2)) # Vi udregner først residualerne, forskellen mellem de predictede og faktiske værdier
                                                         # Derefter kvadreret fejl ^2 for at finde MSE, findes gennemsnittet med mean()
                                                         # til sidst kvadratrod for "root mean squared error"
                                                         # Vi bruger RMSE fordi tallet er i samme enhed som tilskuere og er let at forstå
  best_subset_coef <- coef(best.fit, best.size) # Trækker de valgte predictors ud fra modellen for at vise dem i resultatet
  
  # ----------------- Ridge Regression -----------------
  x_train <- model.matrix(formula, train)[, -1] # model.matrix() beskrevet overfor. Vi bruger -1 subset for at fjerne Intercept rækken
  x_test  <- model.matrix(formula, test)[, -1]
  
  ridge.cv <- cv.glmnet(x_train, y_train, alpha = 0) # cv.glmnet(...) deler træningsdata i 10 folds, alpha = 0 vælger Ridge
                                                     # den laver ridge regression for mange værdier af regulariseringsstyrken λ lambda
                                                     # og tester hver model ved cross validation. 
                                                     # Så finder den værdien af λ lambda der har lavest MSE
  ridge_cv_mse <- min(ridge.cv$cvm)
  ridge.pred <- predict(ridge.cv, newx = x_test, s = ridge.cv$lambda.min)
  ridge_rmse <- sqrt(mean((ridge.pred - y_test)^2))
  ridge_coef <- as.matrix(coef(ridge.cv, s = "lambda.min")) # coef() trækker koefficienterne ud så de vises i resultaterne
  
  # ----------------- Lasso Regression -----------------
  lasso.cv <- cv.glmnet(x_train, y_train, alpha = 1)
  lasso_cv_mse <- min(lasso.cv$cvm)
  lasso.pred <- predict(lasso.cv, newx = x_test, s = lasso.cv$lambda.min)
  lasso_rmse <- sqrt(mean((lasso.pred - y_test)^2))
  lasso_coef <- as.matrix(coef(lasso.cv, s = "lambda.min"))
  
  # ----------------- PCR -----------------
  # Fit PCR-modellen på træningsdata, scale = TRUE  → skalerer alle variabler (PCR/PLS kræver standardisering)
  # validation = "CV" → udfører cross-validation for at vurdere hvor mange Principal Components der skal bruges
  # PCR vælger Principal komponenter der forklarer X bedst
  pcr.fit <- pcr(formula, data = train, scale = TRUE, validation = "CV")
  max_comp <- min(nrow(train) - 1, ncol(train) - 1) # Maksimalt tilladte komponenter (kan ikke overstige antal rækker eller antal prædiktorer)
  cv_msep <- MSEP(pcr.fit)$val[1,1,] # Hent CV-fejl (MSEP) for hvert komponent-antal MSEP(...) viser [statistic, response, component]
  best_pcr_ncomp <- min(which.min(cv_msep), max_comp) # which.min(cv_msep) finder antal komponenter med lavest MSEP
                                                      # min() sikrer at vi ikke går over max_comp
  pcr_cv_mse <- min(cv_msep, na.rm = TRUE)
  pcr.pred <- predict(pcr.fit, newdata = test, ncomp = best_pcr_ncomp) # Forudsig testdata med det optimale antal komponenter
  pcr_rmse <- sqrt(mean((pcr.pred - y_test)^2)) # Beregn RMSE på testdata
  
  # ----------------- PLS -----------------
  # PLS vælger Latente komponenter der forklarer Y bedst
  pls.fit <- plsr(formula, data = train, scale = TRUE, validation = "CV") # plsr(...) fungerer på samme måde som pcr(...) men 
                                                                          # finder optimalt antal latent components i stedet.
  cv_msep_pls <- MSEP(pls.fit)$val[1,1,]
  best_pls_ncomp <- min(which.min(cv_msep_pls), max_comp)
  pls_cv_mse <- min(cv_msep_pls, na.rm = TRUE)
  pls.pred <- predict(pls.fit, newdata = test, ncomp = best_pls_ncomp)
  pls_rmse <- sqrt(mean((pls.pred - y_test)^2))
  
  # ----------------- Returner resultater -----------------
  list(
    RMSE = tibble(
      Model = c("Lineær (alle variabler)", "Best subset", "Ridge", "Lasso", "PCR", "PLS"),
      RMSE = c(lm_rmse, best_subset_rmse, ridge_rmse, lasso_rmse, pcr_rmse, pls_rmse)
    ),
    CV_RMSE = tibble(
      Model = c("Best subset", "Ridge", "Lasso", "PCR", "PLS"),
      CV_RMSE = sqrt(c(best_subset_cv_mse, ridge_cv_mse, lasso_cv_mse, pcr_cv_mse, pls_cv_mse))
    ),
    Linear = list(model = lm.fit),
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

### Prædiktion på nye data (fx best-case/worst-case scenario for de første kampe i foråret)
# lineær model
lm.fit <- results_tilskuere$Linear$model

# --- BEST CASE scenario ---
best_case <- tibble(
  runde = 20,
  år = 2026,
  ugedag = factor("Søn", levels = levels(vff_variabler$ugedag)),
  tidsgruppe = factor("midt", levels = levels(vff_variabler$tidsgruppe)),
  seneste_kamp = factor("vundet", levels = levels(vff_variabler$seneste_kamp)),
  vff_vundet_2 = 1,
  regn_gruppe = factor("ingen regn", levels = levels(vff_variabler$regn_gruppe)),
  temperatur = 8,                    # forårsvejr
  vind = 3,                          # mild vind
  akk_indbyggertal = 10246,
  kamp_gruppe = factor("stor", levels = levels(vff_variabler$kamp_gruppe)),
  ferie_flag = factor("nej", levels = levels(vff_variabler$ferie_flag))
)

# --- WORST CASE scenario ---
worst_case <- tibble(
  runde = 20,
  år = 2026,
  ugedag = factor("Søn", levels = levels(vff_variabler$ugedag)),
  tidsgruppe = factor("sent", levels = levels(vff_variabler$tidsgruppe)),
  seneste_kamp = factor("tabt", levels = levels(vff_variabler$seneste_kamp)),
  vff_vundet_2 = 0,
  regn_gruppe = factor("meget regn", levels = levels(vff_variabler$regn_gruppe)),
  temperatur = -5,                    # koldt
  vind = 20,                         # kraftig vind
  akk_indbyggertal = 10246,
  kamp_gruppe = factor("stor", levels = levels(vff_variabler$kamp_gruppe)),
  ferie_flag = factor("nej", levels = levels(vff_variabler$ferie_flag))
)

# --- 3) Predict med 95% prediktionsinterval (point estimate + interval for nye observationer) ---
pred_best <- predict(lm.fit, newdata = best_case, interval = "prediction", level = 0.95)
pred_worst <- predict(lm.fit, newdata = worst_case, interval = "prediction", level = 0.95)

# Saml resultater
results <- tibble(
  Scenario = c("Best case", "Worst case"),
  Fit = c(pred_best[ , "fit"], pred_worst[ , "fit"]),
  PI_lower = c(pred_best[ , "lwr"], pred_worst[ , "lwr"]),
  PI_upper = c(pred_best[ , "upr"], pred_worst[ , "upr"])
)

print(results)

# --- 4) Ekstra: Simpel følsomhedsanalyse på temperatur og regn (valgfrit) ---
temps <- seq(-2, 12, by = 2)
preds_temp <- sapply(temps, function(t) {
  nd <- best_case
  nd$temperatur <- t
  predict(lm.fit, newdata = nd)
})
sensitivity <- data.frame(temperatur = temps, tilskuere_est = as.numeric(preds_temp))
print(sensitivity)

# Define a range for akk_indbyggertal (e.g., ±20% around median)
pop_range <- seq(
  0.8 * 10246,
  1.2 * 10246,
  length.out = 10
)

# Predict tilskuere for each population value
preds_pop <- sapply(pop_range, function(pop) {
  nd <- best_case
  nd$akk_indbyggertal <- pop
  predict(lm.fit, newdata = nd)
})

# Put results in a data.frame
sensitivity_pop <- data.frame(
  akk_indbyggertal = pop_range,
  tilskuere_est = as.numeric(preds_pop)
)

print(sensitivity_pop)
coef(lm.fit)["akk_indbyggertal"]

plot(vff_variabler$akk_indbyggertal, vff_variabler$tilskuere,
     xlab = "Akkumuleret indbyggertal",
     ylab = "Tilskuere",
     main = "Tilskuere vs Akk. indbyggertal")
abline(lm(tilskuere ~ akk_indbyggertal, data = vff_variabler), col="red")

# Sammenlign resultatet med formålet med undersøgelsen.
# Vi kan se i best case at når vejret er godt og når VFF har vundet de sidste to kampe osv. så kommer der flere tilskuere til kampen.
# Det stemmer overens med formålet for at lave machine learning modellen. 
# Den giver et godt billede af hvor mange der kommer til kampen.

# Hvis du vil gemme resultater:
# write.csv(results, "predictions_VFF_vs_BFF_2026-02-15.csv", row.names = FALSE)

# Sammenligning af test_RMSE
# results_tilskuere:
# Uden billetsalg klarer den lineære funktion og PLS bedst. RMSE på den lineære ligger på 1125 og PLS 1129. Lasso ligger også tæt på 1151
# med en anden seed kan de godt bytte plads.
# Lasso og Ridge skrumper nogle af koefficienterne, men det ligner at der ikke er nogen stærk multikollinearitet eller unødvendige variabler
# PCR klarer sig ikke så godt - det kan tyde på overfitting
# 
# results_10, results_7, results_3
# Lasso, PLS, best subset og den lineære regression klarer sig bedst
# PCR og Ridge bliver ustabile, når vi tilføjer den stærke predictor (billetsalg)

#
# Evaluering 
# 

# Hvordan er Test-MSE sammenlignet med CV-MSE
# results_tilskuere:
# Vi har brugt RMSE til at vurdere modellerne. Når de omregnes til CV_RMSE ligger de tæt på test_RMSE.
# Dette indikerer at cross validation processen giver en pålidelig vurdering af fejl uden for datasættet
# Lasso og PLS opnår den laveste prediction error, som kan tyde på at regularisering og reduktion af predictors 
# hjælper med at forbedre generaliseringsevnen. 
# Best subset selection har en lidt højere test_RMSE på 1313, Det kan muligvis tyde på overfitting da vi ikke har tilstrækkeligt nok data.
# 
# results_10, results_7, results_3
# Når vi inkluderer billetsalg fra op til 10 dage før kampen, falder fejlene mere og mere. 
# Best subset selection, Lasso og PLS klarer sig bedst og man kan vurdere billetsalg er en stærk predictor
# Ridge klarer sig ringere og ringere i test_RMSE. Dette sker fordi Ridge laver alle predictors tættere på 0, også den stærke billetsalg
# PCR bliver ustabil i test_RMSE - det kan tyde på overfitting



















# ===============================
# 1) Hent lineær model fra run_all_models
# ===============================
lm.fit <- results_tilskuere$Linear$model

# ===============================
# 2) Opret best-case og worst-case newdata
# ===============================
best_case <- tibble(
  runde = median(vff_variabler$runde),
  år = 2026,
  ugedag = factor("Søn", levels = levels(vff_variabler$ugedag)),
  tidsgruppe = factor("midt", levels = levels(vff_variabler$tidsgruppe)),
  seneste_kamp = factor("vundet", levels = levels(vff_variabler$seneste_kamp)),
  vff_vundet_2 = 1,
  regn_gruppe = factor("ingen regn", levels = levels(vff_variabler$regn_gruppe)),
  temperatur = 8,
  vind = 3,
  akk_indbyggertal = 10500,
  kamp_gruppe = factor("stor", levels = levels(vff_variabler$kamp_gruppe)),
  ferie_flag = factor("nej", levels = levels(vff_variabler$ferie_flag))
)

worst_case <- tibble(
  runde = median(vff_variabler$runde),
  år = 2026,
  ugedag = factor("Søn", levels = levels(vff_variabler$ugedag)),
  tidsgruppe = factor("sent", levels = levels(vff_variabler$tidsgruppe)),
  seneste_kamp = factor("tabt", levels = levels(vff_variabler$seneste_kamp)),
  vff_vundet_2 = 0,
  regn_gruppe = factor("meget regn", levels = levels(vff_variabler$regn_gruppe)),
  temperatur = 1,
  vind = 10,
  akk_indbyggertal = 10000,
  kamp_gruppe = factor("stor", levels = levels(vff_variabler$kamp_gruppe)),
  ferie_flag = factor("nej", levels = levels(vff_variabler$ferie_flag))
)

# ===============================
# 3) Funktion til sikker newdata
# ===============================
safe_newdata <- function(model, newdata) {
  vars <- all.vars(formula(model))[-1]   # alle predictors, ikke respons
  newdata <- newdata[, vars, drop = FALSE]
  
  # match factor-levels med model
  for (v in names(newdata)) {
    if (is.factor(model$model[[v]])) {
      newdata[[v]] <- factor(newdata[[v]], levels = levels(model$model[[v]]))
    }
  }
  newdata
}

best_case  <- safe_newdata(lm.fit, best_case)
worst_case <- safe_newdata(lm.fit, worst_case)

# ===============================
# 4) Predict med 95% prediktionsintervaller
# ===============================
pred_best  <- predict(lm.fit, newdata = best_case, interval = "prediction", level = 0.95)
pred_worst <- predict(lm.fit, newdata = worst_case, interval = "prediction", level = 0.95)

# ===============================
# 5) Saml resultater i en tabel
# ===============================
results <- tibble(
  Scenario = c("Best case", "Worst case"),
  Fit      = c(pred_best[,"fit"], pred_worst[,"fit"]),
  PI_lower = c(pred_best[,"lwr"], pred_worst[,"lwr"]),
  PI_upper = c(pred_best[,"upr"], pred_worst[,"upr"])
)

results


