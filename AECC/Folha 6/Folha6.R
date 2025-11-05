library(modeldata)
library(tidyverse)
library(glmnet)

set.seed(123)

#Ex1
ames <- ames %>%
  mutate(Sale_Price = log10(Sale_Price))
num_vars <- names(ames)[sapply(ames, is.numeric)]
ames <- ames[,num_vars]

idx <- sample(seq_len(nrow(ames)), size = round(0.8 * nrow(ames)))
ames_tr <- ames[idx, ]
ames_te <- ames[-idx, ]

rmse <- function(obs, pred) sqrt(mean((obs - pred)^2))
r2 <- function(obs, pred) 11 - sum((obs - pred)^2) / sum((obs - mean(obs))^2)

mod_ols <- lm(Sale_Price ~ ., data = ames_tr)
pred_ols <- predict(mod_ols, new_data = ames_te)

rmse_ols <- rmse(ames_te$Sale_Price, pred_ols)
r2_ols <- r2(ames_te$Sale_Price, pred_ols)

tibble(modelo = "OLS", RMSE = rmse_ols, R2 = r2_ols)

X_tr <- model.matrix(Sale_Price ~ ., data = ames_tr)[, -1]
y_tr <- ames_tr$Sale_Price
X_te <- model.matrix(Sale_Price ~ ., data = ames_te)[, -1]

set.seed(123)
cv_ridge <- cv.glmnet(X_tr, y_tr, alpha = 0, nfolds = 10, standardize = TRUE)

# previsão em teste usando lambda.min
pred_ridge <- as.numeric(predict(cv_ridge, newx = X_te, s = "lambda.min"))

rmse_ridge <- rmse(ames_te$Sale_Price, pred_ridge)
r2_ridge   <- r2(ames_te$Sale_Price, pred_ridge)

tibble(Modelo = c("OLS", "Ridge (lambda.min)"),
       RMSE   = c(rmse_ols, rmse_ridge),
       R2     = c(r2_ols,   r2_ridge))

#Ex2
cv_lasso <- cv.glmnet(X_tr, y_tr, alpha = 1, nfolds = 10, standardize = TRUE)

lambda_min <- cv_lasso$lambda.min
lambda_1se <- cv_lasso$lambda.1se

tibble(lambda_min = lambda_min, lambda_1se = lambda_1se)

coef_l1se <- coef(cv_lasso, s = "lambda.1se")
nz <- rownames(coef_l1se)[as.numeric(coef_l1se) != 0]
nz <- setdiff(nz, "(Intercept)")
tibble(variavel = sort(nz))

pred_lasso_min <- as.numeric(predict(cv_lasso, newx = X_te, s = "lambda.min"))
pred_lasso_1se <- as.numeric(predict(cv_lasso, newx = X_te, s = "lambda.1se"))

rmse_min <- rmse(ames_te$Sale_Price, pred_lasso_min)
r2_min <- r2(ames_te$Sale_Price,   pred_lasso_min)

rmse_1se <- rmse(ames_te$Sale_Price, pred_lasso_1se)
r2_1se <- r2(ames_te$Sale_Price,   pred_lasso_1se)

tibble(Modelo = c("Lasso (lambda.min)", "Lasso (lambda.1se)"),
       RMSE = c(rmse_min, rmse_1se),
       R2 = c(r2_min,   r2_1se))