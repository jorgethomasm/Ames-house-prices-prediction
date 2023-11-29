## Base Worflows Set

# My final objective is to stack (combine / ensemble) several models of different nature. However, for a first model I'd like to keep interpretability.

# ### Linear Models

# **Where to start?** In order to have a set of reference models, I will proceed as follows:

# -   Cherry-picking predictors with linear influence from correlograms @fig-num-corr and @fig-cat-corr.
# -   Establish a ***workflow set*** of linear models:
#     -   Fit the simplest OLS univariate lm `GrLivArea` vs. `SalePrice`.
#     -   Cherry-pick predictors and fit a linear hyper-plane: multivariate lm.
#     -   Add some flexibility on top fitting a Spline GAM.


# ---------- Preprocessing Pipelines ----------

# Uni-lm (SIMPLEST reasonable model)
simplest_rec <-
  recipe(SalePrice_bc ~ GrLivArea, data = ames_train) |>  
  step_YeoJohnson(GrLivArea) |>
  step_normalize(all_numeric_predictors())

# simplest_rec |> prep() |>  bake(new_data = NULL) # Sanity check for warnings

# Multi-lm 
multi_lm_rec <-
  recipe(SalePrice_bc ~ GrLivArea + TotBaths + GarageArea + TotalBsmtSF + YearBuilt + OverallQual + KitchenQual + Foundation + ExistFireplace + HeatingQC, data = ames_train) |>  
  step_YeoJohnson(all_numeric_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors())

# multi_lm_rec |> prep() |> bake(new_data = NULL) # Sanity check for warnings

# GAM: Splines 
multi_gam_rec <-
  multi_lm_rec |>
  step_bs(all_numeric_predictors())  # GAM basis fun expansion.


# ---------- Model specification ----------
lm_spec <- 
  linear_reg() |>
  set_mode("regression") |>
  set_engine("lm")


# ---------- Set of Workflows ----------  
my_lm_recipes <- list(simplest_rec, multi_lm_rec, multi_gam_rec)
my_lms_specs <- list(lm_spec)

# Combine a list of pre-processors with a list of models:
ames_wfset <- workflow_set(my_lm_recipes, my_lms_specs, 
                           cross = TRUE, 
                           case_weights = NULL)
ames_wfset


# ---------- Fit Bootstraps ----------
doParallel::registerDoParallel()
set.seed(1982)

ames_lms_res <- workflow_map(ames_wfset, "fit_resamples", resamples = ames_boots)

save(ames_lms_res, file = "./models/trained_resamples/ames_lms_res.Rdata")

# Load fitted Bootstraps

# ames_lms_res <- load(file = "./models/trained_resamples/ames_lms_res.Rdata")

ames_lms_res |>  
  collect_metrics(summarize = TRUE) |>
  select(-c(.estimator, .config)) |>
  filter(.metric == "rmse") |>
  knitr::kable(caption = "Three Base Linear Models")


# EXTRACT WORKFLOWS

ames_lms_res |>
  extract_workflow("recipe_2_linear_reg") |>
  fit(ames_train) |>  
  tidy() |> 
  select(-statistic) |>
  arrange(-abs(estimate)) |>
  slice_head(n = 15) |> 
  knitr::kable(caption = 'Multi-lm')

 
# Get best model and fit with whole training set

# TRAIN ALL!

fit_lm_bsplines <- 
  extract_workflow(ames_lms_res, "recipe_3_linear_reg") |>
  fit(ames_train)

# Predicting on the test set
ames_test <- 
  ames_all |>
  select(-SalePrice) |>
  filter(dataset == "test")

res_1_bsplines <- 
  predict(fit_lm_bsplines, ames_test) |>
  mutate(SalePrice_pred = inv_boxcox(.pred, boxcox_lambda))  # orignal values in USD

# .csv for submission
sub_1 <- data.frame(Id = ames_test$Id, SalePrice = res_1_bsplines$SalePrice_pred)
# write.csv(sub_1, "./data/submissions/sub_1_splines.csv", quote = FALSE, row.names = FALSE)


# PCA RECIPES

# lm_pca_rec <- 
#   glmnet_base_rec |>
#   # step_pca(all_numeric_predictors(), num_comp = tune()) |>
#   step_pca(all_numeric_predictors(), threshold = 0.90) |>
#   step_normalize(all_numeric_predictors())

# lm_pca_rec |> prep(verbose = TRUE) |> bake(new_data = NULL) 


# pca_wf <- 
#   workflow() |>
#   add_model(lm_spec)|>
#   add_recipe(lm_pca_rec)

# PCA SPECS

# lm_pca_res <- pca_wf |>
#   fit_resamples(ames_boots)

# lm_pca_res |>
#   collect_metrics(summarize = TRUE)

# RANDOM FOREST CODE



tree_rec <- 
  recipe(SalePrice_bc ~ ., data = ames_train) |>
  update_role(Id, new_role = "Id variabe") |>
  update_role(SalePrice, new_role = "original outcome") |>
  update_role(dataset, new_role = "splitting variable") |>
  step_dummy(all_nominal_predictors()) 


rf_spec <-
  rand_forest(mtry = tune(), trees = 1000, min_n = tune()) |>
  set_engine("ranger") |>
  set_mode("regression")

rf_wf <- workflow() |>
  add_recipe(tree_rec) |>
  add_model(rf_spec)

# Hyperparameters tuning ---
set.seed(1982)
doParallel::registerDoParallel()

rf_tune <- tune_grid(rf_wf,                     
                    resamples = ames_folds, 
                    grid = 10)

# Explore Results
show_best(rf_tune, metric = "rmse")

#  26 minutes Aprox. ---

# Set best parameters
final_rf <- rf_wf |>
  finalize_workflow(select_best(rf_tune, metric = "rmse"))

final_rf

# save(final_rf, file = "./models/final_models/final_rf.Rdata")
# final_rf <- load("./models/final_models/final_rf.Rdata")


# final_rf <- load("./models/final_models/final_rf.Rdata")
# final_rf

# Test error estimation using the bootstraps

# doParallel::registerDoParallel()
# set.seed(1982)

# final_rf |> 
#   fit_resamples(resamples = ames_boots) |>
#   collect_metrics(summarize = TRUE)

# For curiosity, I fitted the 1000 bootstraps to the **best rf candidate** (code above) obtaining a mean RMSE = 0.0772



#| label: lumping

# ames_all <- 
#   ames_all |>
#   mutate(OverallQual = fct_lump(OverallQual, n = 8)) |>
#   mutate(OverallQual = fct_relevel(OverallQual, "Other"))  