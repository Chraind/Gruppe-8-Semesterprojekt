pacman::p_load(tidyverse, vroom, janitor, polite, rjstat, rvest, lubridate,
               stringi, httr, jsonlite, purrr, utils, RSQLite, DBI, leaps, 
               glmnet)

pacman::p_load("tidyverse", "magrittr", "nycflights13", "gapminder",
               "Lahman", "maps", "lubridate", "pryr", "hms", "hexbin",
               "feather", "htmlwidgets", "broom", "pander", "modelr",
               "XML", "httr", "jsonlite", "lubridate", "microbenchmark",
               "splines", "ISLR2", "MASS", "testthat", "leaps", "caret",
               "RSQLite", "class", "viridis", "boot", "glmnet", "pls")

joined_data <- readRDS("data/joined_data.rds")

# Remove rows with NAs (simplest approach)
data_clean <- na.omit(joined_data)
glimpse(vff_variabler)

vff_variabler <- data_clean %>% 
  dplyr::select(tilskuere, runde, år, ugedag, tidsgruppe, seneste_kamp, vff_vundet_2, regn_gruppe,
                temperatur, vind, helligdag, akk_indbyggertal)

# Best subset selection (og dermed forward og backward selection)
# method = "forward" eller "backward" i regsubsets() funktionen.

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

set.seed(8)
train <- sample(1:nrow(vff_variabler), nrow(vff_variabler)*2/3)
test <- (-train)

vff_train <- vff_variabler[train,]
vff_test <- vff_variabler[test,]

glimpse(vff_variabler)


k <- 10 # Vi danner 10 folds
n <- nrow(vff_train) # registrerer hvor mange observationer, vi har.
set.seed(1) 
folds <- sample(rep(1:k, length = n)) #Vi tildeler en værdi mellem 1 og
dim(vff_train)[2]  # Der er p variabler og dermed p-1 prædiktorer

cv.errors <- matrix(NA, k, dim(vff_train)[2]-1,
                    dimnames = list(NULL, paste(1:(dim(vff_train)[2]-1))))
cv.errors

for (j in 1:k) { # her gennemløbes alle folds
  best.fit <- regsubsets(tilskuere ~ .,
                         data = vff_train[folds != j, ],
                         nvmax = dim(vff_train)[2]-1)
  for (i in 1:(dim(vff_train)[2]-1)) { # her gennemløbes alle kandidatmodeller
    pred <- predict(best.fit, vff_train[folds == j, ], id = i)
    # predict-funktionen ovenfor kalder den funktion, vi har lavet tidligere. 
    cv.errors[j, i] <-
      mean((vff_train$tilskuere[folds == j] - pred)^2) # Her udregnes MSE for hver 
    # fold for hver kandidatmodel 
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean) # apply er en smart funktion, der 
# gennemløber alle rækker og tager gennemsnittet henover hver søjle, som svarer 
# til hver kandidatmodel.
mean.cv.errors # Vi får altså en gennemsnitlig MSE for hver kandidatmodel.
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b") # Her plottes disse gennemsnit for hver størrelse,
which.min(mean.cv.errors)


# Her fittes modellen til ALLE træningsdata
reg.best <- regsubsets(tilskuere ~ ., data = vff_train,
                       nvmax = dim(vff_train)[2]-1)
coef(reg.best, which.min(mean.cv.errors))

pred_best_subset <- predict(reg.best, vff_test, id = which.min(mean.cv.errors))


mse_best_subset <- mean((vff_test$tilskuere - pred_best_subset)^2)
mse_best_subset
sqrt(mse_best_subset)



# Ridge regression

vff_variabler


x <- model.matrix(tilskuere ~ ., vff_variabler)[, -1]
y <- vff_variabler$tilskuere
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
coef(ridge.mod)
dim(coef(ridge.mod))
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]
ridge.mod <- glmnet(x[train, ], y[train], alpha = 0,
                    lambda = grid, thresh = 1e-12)
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0) 
bestlam <- cv.out$lambda.min
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test, ]) 
mean((ridge.pred - y.test)^2)
sqrt(mean((ridge.pred - y.test)^2))



# Lasso regression

x <- model.matrix(tilskuere ~ ., vff_variabler)[, -1]
y <- vff_variabler$tilskuere
grid <- 10^seq(10, -2, length = 100)
lasso.mod <- glmnet(x, y, alpha = 1, lambda = grid)
xxx <- coef(lasso.mod)
dim(coef(lasso.mod))
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1,
                    lambda = grid, thresh = 1e-12)
plot(lasso.mod)
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1) 
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test, ]) 
mean((lasso.pred - y.test)^2)
sqrt(mean((lasso.pred - y.test)^2))


# PCR


set.seed(1)
pcr.fit <- pcr(tilskuere ~ ., 
               data = vff_variabler, 
               subset = train,
               scale = TRUE, 
               validation = "CV", 
               segments = 10)

summary(pcr.fit) 
validationplot(pcr.fit, val.type = "MSEP")

pcr.pred <- predict(pcr.fit, x[test, ], ncomp = 25)
mean((pcr.pred - y.test)^2) 
sqrt(mean((pcr.pred - y.test)^2)) 

# PLS

set.seed(1)
pls.fit <- plsr(tilskuere ~ ., 
                data = vff_variabler, 
                subset = train,
                scale = TRUE, 
                validation = "CV", 
                segments = 10)

summary(pls.fit) 

validationplot(pls.fit, val.type = "MSEP")

pls.pred <- predict(pls.fit, x[test, ], ncomp = 1) # Vi laver prædiktionen.
mean((pls.pred - y.test)^2) # MSE på test data
sqrt(mean((pls.pred - y.test)^2)) # RMSE på test data
