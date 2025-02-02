#### SET UP

# Import libraries
library(haven)
library(glmnet)
library(purrr)
library(margins)
library(skimr)
library(kableExtra)
library(Hmisc)
library(cowplot)
library(gmodels) 
library(lspline)
library(sandwich)
library(modelsummary)

library(rattle)
library(caret)
library(pROC)
library(ranger)
library(rpart)
library(partykit)
library(rpart.plot)

#location of folders
data_in  <- "data/"
data_out <- "clean_data/"

# set data dir, load theme and functions
# setwd("/Users/talgatilimbekuulu/Documents/ceu/prediction_gabor/DA3-phdma/assignment_3")

source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# define output folder
use_case_dir <- "/Users/talgatilimbekuulu/Documents/ceu/prediction_gabor/DA3-phdma/assignment_3/"
output <- paste0(use_case_dir,"output/")


# Import the data
# Use the data after running the sample_preparation.R script, should contain 16820 obs 114 variables

##########################################################################################################
##########################################################################################################

# Define variable sets ----------------------------------------------

rawvars <-  c("curr_assets", "curr_liab", "extra_exp", "extra_inc", "extra_profit_loss", "fixed_assets",
              "inc_bef_tax", "intang_assets", "inventories", "liq_assets", "material_exp", "personnel_exp",
              "profit_loss_year", "sales", "share_eq", "subscribed_cap")
qualityvars <- c("balsheet_flag", "balsheet_length", "balsheet_notfullyear")
engvar <- c("total_assets_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs",
            "share_eq_bs", "subscribed_cap_bs", "intang_assets_bs", "extra_exp_pl",
            "extra_inc_pl", "extra_profit_loss_pl", "inc_bef_tax_pl", "inventories_pl",
            "material_exp_pl", "profit_loss_year_pl", "personnel_exp_pl")
engvar2 <- c("extra_profit_loss_pl_quad", "inc_bef_tax_pl_quad",
             "profit_loss_year_pl_quad", "share_eq_bs_quad")
engvar3 <- c(grep("*flag_low$", names(data), value = TRUE),
             grep("*flag_high$", names(data), value = TRUE),
             grep("*flag_error$", names(data), value = TRUE),
             grep("*flag_zero$", names(data), value = TRUE))
hr <- c("female", "ceo_age", "flag_high_ceo_age", "flag_low_ceo_age",
        "flag_miss_ceo_age", "ceo_count", "labor_avg_mod",
        "flag_miss_labor_avg", "foreign_management")
firm <- c("age", "age2", "new", "ind2_cat", "m_region_loc", "urban_m")

# interactions for logit, LASSO
interactions1 <- c("ind2_cat*age", "ind2_cat*age2",
                    "ind2_cat*sales_mil_log",
                   "ind2_cat*ceo_age", "ind2_cat*foreign_management",
                   "ind2_cat*female",   "ind2_cat*urban_m", "ind2_cat*labor_avg_mod")
interactions2 <- c("sales_mil_log*age", "sales_mil_log*female",
                   "sales_mil_log*profit_loss_year_pl", "sales_mil_log*foreign_management")

X1 <- c("sales_mil_log", "sales_mil_log_sq", "profit_loss_year_pl", "ind2_cat")
X2 <- c("sales_mil_log", "sales_mil_log_sq", "profit_loss_year_pl", "fixed_assets_bs","share_eq_bs","curr_liab_bs ",  
        "curr_liab_bs_flag_high ", "curr_liab_bs_flag_error",  "age","foreign_management" , "ind2_cat")
X3 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar)
X4 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar, engvar2, engvar3,  hr, qualityvars)
X5 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar, engvar2, engvar3,  hr, qualityvars, interactions1, interactions2)

# for LASSO
logitvars <- c("sales_mil_log", "sales_mil_log_sq", engvar, engvar2, engvar3, hr, firm, qualityvars, interactions1, interactions2)

# for RF (no interactions, no modified features)
rfvars  <-  c("sales_mil", rawvars, hr, firm, qualityvars)


# Check simplest model X1
ols_modelx1 <- lm(formula(paste0("target ~", paste0(X1, collapse = " + "))),
                  data = data)
summary(ols_modelx1)

glm_modelx1 <- glm(formula(paste0("target ~", paste0(X1, collapse = " + "))),
                   data = data, family = "binomial")
summary(glm_modelx1)

#calculate average marginal effects (dy/dx) for logit
mx1 <- margins(glm_modelx1)

sum_table <- summary(glm_modelx1) %>%
  coef() %>%
  as.data.frame() %>%
  select(Estimate) %>%
  mutate(factor = row.names(.)) %>%
  merge(summary(mx1)[,c("factor","AME")])

kable(x = sum_table, format = "latex", digits = 3,
      col.names = c("Variable", "Coefficient", "dx/dy"),
      caption = "Average Marginal Effects (dy/dx) for Logit Model") 

# Check  model X2
ols_modelx2 <- lm(formula(paste0("target ~", paste0(X2, collapse = " + "))),
                  data = data)
summary(ols_modelx2)

glm_modelx2 <- glm(formula(paste0("target ~", paste0(X2, collapse = " + "))),
                   data = data, family = "binomial")
summary(glm_modelx2)

#calculate average marginal effects (dy/dx) for logit
mx2 <- margins(glm_modelx2)

sum_table <- summary(glm_modelx2) %>%
  coef() %>%
  as.data.frame() %>%
  select(Estimate) %>%
  mutate(factor = row.names(.)) %>%
  merge(summary(mx2)[,c("factor","AME")])

kable(x = sum_table, format = "latex", digits = 3,
      col.names = c("Variable", "Coefficient", "dx/dy"),
      caption = "Average Marginal Effects (dy/dx) for Logit Model") 


# separate datasets -------------------------------------------------------

set.seed(121223)

train_indices <- as.integer(createDataPartition(data$target, p = 0.8, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

dim(data_train)
dim(data_holdout)

Hmisc::describe(data$target_f)
Hmisc::describe(data_train$target_f)
Hmisc::describe(data_holdout
                $target_f)

# PART I PREDICT PROBABILITIES
# Predict logit models ----------------------------------------------
#######################################################x

# 5 fold cross-validation
train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)

# Train Logit Models ----------------------------------------------

logit_model_vars <- list("X1" = X1, "X2" = X2, "X3" = X3, "X4" = X4, "X5" = X5)

CV_RMSE_folds <- list()
logit_models <- list()

for (model_name in names(logit_model_vars)) {
  
  features <- logit_model_vars[[model_name]]
  
  set.seed(121223)
  glm_model <- train(
    formula(paste0("target_f ~", paste0(features, collapse = " + "))),
    method = "glm",
    data = data_train,
    family = binomial,
    trControl = train_control
  )
  
  logit_models[[model_name]] <- glm_model
  # Calculate RMSE on test for each fold
  CV_RMSE_folds[[model_name]] <- glm_model$resample[,c("Resample", "RMSE")]
  
}

# Logit lasso -----------------------------------------------------------

lambda <- 10^seq(-1, -4, length = 10)
grid <- expand.grid("alpha" = 1, lambda = lambda)

set.seed(121223)
system.time({
  logit_lasso_model <- train(
    formula(paste0("target_f ~", paste0(logitvars, collapse = " + "))),
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    family = "binomial",
    trControl = train_control,
    tuneGrid = grid,
    na.action=na.exclude
  )
})

tuned_logit_lasso_model <- logit_lasso_model$finalModel
best_lambda <- logit_lasso_model$bestTune$lambda
logit_models[["LASSO"]] <- logit_lasso_model
lasso_coeffs <- as.matrix(coef(tuned_logit_lasso_model, best_lambda))
CV_RMSE_folds[["LASSO"]] <- logit_lasso_model$resample[,c("Resample", "RMSE")]

########################################

# Draw ROC Curve and calculate AUC for each folds --------------------------------
CV_AUC_folds <- list()

for (model_name in names(logit_models)) {
  
  auc <- list()
  model <- logit_models[[model_name]]
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <-
      model$pred %>%
      filter(Resample == fold)
    
    roc_obj <- roc(cv_fold$obs, cv_fold$growth)
    auc[[fold]] <- as.numeric(roc_obj$auc)
  }
  
  CV_AUC_folds[[model_name]] <- data.frame("Resample" = names(auc),
                                           "AUC" = unlist(auc))
}

# For each model: average RMSE and average AUC for models ----------------------------------

CV_RMSE <- list()
CV_AUC <- list()

for (model_name in names(logit_models)) {
  CV_RMSE[[model_name]] <- mean(CV_RMSE_folds[[model_name]]$RMSE)
  CV_AUC[[model_name]] <- mean(CV_AUC_folds[[model_name]]$AUC)
}

#  6 models, (5 logit and the logit lasso). 

nvars <- lapply(logit_models, FUN = function(x) length(x$coefnames))
nvars[["LASSO"]] <- sum(lasso_coeffs != 0)

logit_summary1 <- data.frame("Number of predictors" = unlist(nvars),
                             "CV RMSE" = unlist(CV_RMSE),
                             "CV AUC" = unlist(CV_AUC))

kable(x = logit_summary1, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("Number of predictors","CV RMSE","CV AUC")) 

# Take best model  

best_logit_no_loss <- logit_models[["X4"]]

logit_predicted_probabilities_holdout <- predict(best_logit_no_loss, newdata = data_holdout, type = "prob")
data_holdout[,"best_logit_no_loss_pred"] <- logit_predicted_probabilities_holdout[,"growth"]
RMSE(data_holdout[, "best_logit_no_loss_pred", drop=TRUE], data_holdout$target)

# discrete ROC (with thresholds in steps) on holdout -------------------------------------------------
thresholds <- seq(0.05, 0.75, by = 0.05)

cm <- list()
true_positive_rates <- c()
false_positive_rates <- c()
for (thr in thresholds) {
  holdout_prediction <- ifelse(data_holdout[,"best_logit_no_loss_pred"] < thr, "nogrowth", "growth") %>%
    factor(levels = c("nogrowth", "growth"))
  cm_thr <- confusionMatrix(holdout_prediction,data_holdout$target_f)$table
  cm[[as.character(thr)]] <- cm_thr
  true_positive_rates <- c(true_positive_rates, cm_thr["growth", "growth"] /
                             (cm_thr["growth", "growth"] + cm_thr["nogrowth", "growth"]))
  false_positive_rates <- c(false_positive_rates, cm_thr["growth", "nogrowth"] /
                              (cm_thr["growth", "nogrowth"] + cm_thr["nogrowth", "nogrowth"]))
}

tpr_fpr_for_thresholds <- tibble(
  "threshold" = thresholds,
  "true_positive_rate" = true_positive_rates,
  "false_positive_rate" = false_positive_rates
)

discrete_roc_plot <- ggplot(
  data = tpr_fpr_for_thresholds,
  aes(x = false_positive_rate, y = true_positive_rate, color = threshold)) +
  labs(x = "False positive rate (1 - Specificity)", y = "True positive rate (Sensitivity)") +
  geom_point(size=2, alpha=0.8) +
  scale_color_viridis(option = "G", direction = -1, discrete = FALSE) +
  scale_x_continuous(expand = c(0.01,0.01), limit=c(0,1), breaks = seq(0,1,0.1)) +
  scale_y_continuous(expand = c(0.01,0.01), limit=c(0,1), breaks = seq(0,1,0.1)) +
  theme_bg() +
  theme(legend.position ="right") +
  theme(legend.title = element_text(size = 4), 
        legend.text = element_text(size = 4),
        legend.key.size = unit(.4, "cm")) 
discrete_roc_plot



# continuous ROC on holdout with best model (Logit 4) -------------------------------------------

roc_obj_holdout <- roc(data_holdout$target, data_holdout$best_logit_no_loss_pred)

# using roc_obj_holdout create continuous ROC plot

roc2 <- createRocPlot(roc_obj_holdout, "best_logit_no_loss_roc_plot_holdout")


# put together plots
roc_plot <- grid.arrange(discrete_roc_plot, roc2, ncol = 2)
# save plot
ggsave("output/roc_plot.png",plot =  roc_plot, width = 8, height = 4, units = "in", dpi = 300)


# Confusion table with different tresholds ----------------------------------------------------------

# default: the threshold 0.5 is used to convert probabilities to binary classes
logit_class_prediction <- predict(best_logit_no_loss, newdata = data_holdout)
summary(logit_class_prediction)

# confusion matrix: summarize different type of errors and successfully predicted cases
cm_object1 <- confusionMatrix(logit_class_prediction, data_holdout$target_f, positive = "growth")
cm1 <- cm_object1$table
cm1

# 0.36 threshold
holdout_prediction <-
  ifelse(data_holdout$best_logit_no_loss_pred < 0.05, "nogrowth", "growth") %>%
  factor(levels = c("nogrowth", "growth"))
cm_object1b <- confusionMatrix(holdout_prediction,data_holdout$target_f)
cm1b <- cm_object1b$table
cm1b

# a sensible choice: mean of predicted probabilities
mean_predicted_default_prob <- mean(data_holdout$best_logit_no_loss_pred)
mean_predicted_default_prob

holdout_prediction <-
  ifelse(data_holdout$best_logit_no_loss_pred < mean_predicted_default_prob, "nogrowth", "growth") %>%
  factor(levels = c("nogrowth", "growth"))
cm_object2 <- confusionMatrix(holdout_prediction,data_holdout$target_f)
cm2 <- cm_object2$table
cm2

# Calibration plot

create_calibration_plot(data_holdout, 
                        file_name = "ch17-figure-1-logit-m4-calibration", 
                        prob_var = "best_logit_no_loss_pred", 
                        actual_var = "target",
                        n_bins = 10)


# Introduce loss function
# relative cost of of a false negative classification (as compared with a false positive classification)
FP=1
FN=2
cost = FN/FP
# the prevalence, or the proportion of cases in the population (n.cases/(n.controls+n.cases))
prevelance = sum(data_train$target)/length(data_train$target)

# Draw ROC Curve and find optimal threshold with loss function --------------------------

best_tresholds <- list()
expected_loss <- list()
logit_cv_rocs <- list()
logit_cv_threshold <- list()
logit_cv_expected_loss <- list()

for (model_name in names(logit_models)) {
  
  model <- logit_models[[model_name]]
  colname <- paste0(model_name,"_prediction")
  
  best_tresholds_cv <- list()
  expected_loss_cv <- list()
  
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <-
      model$pred %>%
      filter(Resample == fold)
    
    roc_obj <- roc(cv_fold$obs, cv_fold$growth)
    best_treshold <- coords(roc_obj, "best", ret="all", transpose = FALSE,
                            best.method="youden", best.weights=c(cost, prevelance))
    best_tresholds_cv[[fold]] <- best_treshold$threshold
    expected_loss_cv[[fold]] <- (best_treshold$fp*FP + best_treshold$fn*FN)/length(cv_fold$growth)
  }
  
  # average
  best_tresholds[[model_name]] <- mean(unlist(best_tresholds_cv))
  expected_loss[[model_name]] <- mean(unlist(expected_loss_cv))
  
  # for fold #5
  logit_cv_rocs[[model_name]] <- roc_obj
  logit_cv_threshold[[model_name]] <- best_treshold
  logit_cv_expected_loss[[model_name]] <- expected_loss_cv[[fold]]
  
}

logit_summary2 <- data.frame("Avg of optimal thresholds" = unlist(best_tresholds),
                             "Threshold for Fold5" = sapply(logit_cv_threshold, function(x) {x$threshold}),
                             "Avg expected loss" = unlist(expected_loss),
                             "Expected loss for Fold5" = unlist(logit_cv_expected_loss))

kable(x = logit_summary2, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("Avg of optimal thresholds","Threshold for fold #5",
                                  "Avg expected loss","Expected loss for fold #5")) 



# Pick best model based on average expected loss ----------------------------------

best_logit_with_loss <- logit_models[["X4"]]
best_logit_optimal_treshold <- best_tresholds[["X4"]]

logit_predicted_probabilities_holdout <- predict(best_logit_with_loss, newdata = data_holdout, type = "prob")
data_holdout[,"best_logit_with_loss_pred"] <- logit_predicted_probabilities_holdout[,"growth"]

# ROC curve on holdout
roc_obj_holdout <- roc(data_holdout$target, data_holdout[, "best_logit_with_loss_pred", drop=TRUE])
roc_obj_holdout

# Get expected loss on holdout
holdout_treshold <- coords(roc_obj_holdout, x = best_logit_optimal_treshold, input= "threshold",
                           ret="all", transpose = FALSE)
expected_loss_holdout <- (holdout_treshold$fp*FP + holdout_treshold$fn*FN)/length(data_holdout$target)
expected_loss_holdout

# Confusion table on holdout with optimal threshold
holdout_prediction <-
  ifelse(data_holdout$best_logit_with_loss_pred < best_logit_optimal_treshold, "nogrowth", "growth") %>%
  factor(levels = c("nogrowth", "growth"))

cm_object3 <- confusionMatrix(holdout_prediction,data_holdout$target_f)
cm3 <- cm_object3$table
cm3

# PREDICTION WITH RANDOM FOREST
#################################################

# -----------------------------------------------
# RANDOM FOREST GRAPH EXAMPLE
# -----------------------------------------------

data_for_graph <- data_train
levels(data_for_graph$target_f) <- list("stay" = "nogrowth", "exit" = "growth")

set.seed(131323)
rf_for_graph <-
  rpart(
    formula = target_f ~ sales_mil + profit_loss_year+ foreign_management,
    data = data_for_graph,
    control = rpart.control(cp = 0.0028, minbucket = 100)
  )

rpart.plot(rf_for_graph, tweak=1, digits=2, extra=107, under = TRUE)

#################################################

#################################################
# Probability forest
# Split by gini, ratio of 1's in each tree, average over trees
#################################################

# 5 fold cross-validation

train_control <- trainControl(
  method = "cv",
  n = 5,
  classProbs = TRUE, # same as probability = TRUE in ranger
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)
train_control$verboseIter <- TRUE

tune_grid <- expand.grid(
  .mtry = c(5, 6, 7),
  .splitrule = "gini",
  .min.node.size = c(10, 15)
)

# getModelInfo("ranger")
set.seed(131323)
rf_model_p <- train(
  formula(paste0("target_f ~ ", paste0(rfvars , collapse = " + "))),
  method = "ranger",
  data = data_train,
  tuneGrid = tune_grid,
  trControl = train_control
)

rf_model_p$results

best_mtry <- rf_model_p$bestTune$mtry
best_min_node_size <- rf_model_p$bestTune$min.node.size

# Get average (ie over the folds) RMSE and AUC ------------------------------------
CV_RMSE_folds[["rf_p"]] <- rf_model_p$resample[,c("Resample", "RMSE")]

auc <- list()
for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
  cv_fold <-
    rf_model_p$pred %>%
    filter(Resample == fold)
  
  roc_obj <- roc(cv_fold$obs, cv_fold$growth)
  auc[[fold]] <- as.numeric(roc_obj$auc)
}
CV_AUC_folds[["rf_p"]] <- data.frame("Resample" = names(auc),
                                     "AUC" = unlist(auc))

CV_RMSE[["rf_p"]] <- mean(CV_RMSE_folds[["rf_p"]]$RMSE)
CV_AUC[["rf_p"]] <- mean(CV_AUC_folds[["rf_p"]]$AUC)

# Now use loss function and search for best thresholds and expected loss over folds -----
best_tresholds_cv <- list()
expected_loss_cv <- list()

for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
  cv_fold <-
    rf_model_p$pred %>%
    filter(mtry == best_mtry,
           min.node.size == best_min_node_size,
           Resample == fold)
  
  roc_obj <- roc(cv_fold$obs, cv_fold$growth)
  best_treshold <- coords(roc_obj, "best", ret="all", transpose = FALSE,
                          best.method="youden", best.weights=c(cost, prevelance))
  best_tresholds_cv[[fold]] <- best_treshold$threshold
  expected_loss_cv[[fold]] <- (best_treshold$fp*FP + best_treshold$fn*FN)/length(cv_fold$growth)
}

# average
best_tresholds[["rf_p"]] <- mean(unlist(best_tresholds_cv))
expected_loss[["rf_p"]] <- mean(unlist(expected_loss_cv))


rf_summary <- data.frame("CV RMSE" = CV_RMSE[["rf_p"]],
                         "CV AUC" = CV_AUC[["rf_p"]],
                         "Avg of optimal thresholds" = best_tresholds[["rf_p"]],
                         "Threshold for Fold5" = best_treshold$threshold,
                         "Avg expected loss" = expected_loss[["rf_p"]],
                         "Expected loss for Fold5" = expected_loss_cv[[fold]])

kable(x = rf_summary, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("CV RMSE", "CV AUC",
                                  "Avg of optimal thresholds","Threshold for fold #5",
                                  "Avg expected loss","Expected loss for fold #5")) 

# Create plots - this is for Fold5

createLossPlot(roc_obj, best_treshold, "rf_p_loss_plot")
createRocPlotWithOptimal(roc_obj, best_treshold, "rf_p_roc_plot")

# Take model to holdout and estimate RMSE, AUC and expected loss ------------------------------------

rf_predicted_probabilities_holdout <- predict(rf_model_p, newdata = data_holdout, type = "prob")
data_holdout$rf_p_prediction <- rf_predicted_probabilities_holdout[,"growth"]
RMSE(data_holdout$rf_p_prediction, data_holdout$target)

# ROC curve on holdout
roc_obj_holdout <- roc(data_holdout$target, data_holdout[, "rf_p_prediction", drop=TRUE])

# AUC
as.numeric(roc_obj_holdout$auc)

# Get expected loss on holdout with optimal threshold
holdout_treshold <- coords(roc_obj_holdout, x = best_tresholds[["rf_p"]] , input= "threshold",
                           ret="all", transpose = FALSE)
expected_loss_holdout <- (holdout_treshold$fp*FP + holdout_treshold$fn*FN)/length(data_holdout$target)
expected_loss_holdout

#################################################
# Classification forest
# Split by Gini, majority vote in each tree, majority vote over trees
#################################################
# Show expected loss with classification RF and default majority voting to compare

train_control <- trainControl(
  method = "cv",
  n = 5
)
train_control$verboseIter <- TRUE

set.seed(13505)
rf_model_f <- train(
  formula(paste0("target_f ~ ", paste0(rfvars , collapse = " + "))),
  method = "ranger",
  data = data_train,
  tuneGrid = tune_grid,
  trControl = train_control
)

data_train$rf_f_prediction_class <-  predict(rf_model_f,type = "raw")
data_holdout$rf_f_prediction_class <- predict(rf_model_f, newdata = data_holdout, type = "raw")

#We use predicted classes to calculate expected loss based on our loss fn
fp <- sum(data_holdout$rf_f_prediction_class == "growth" & data_holdout$target_f == "nogrowth")
fn <- sum(data_holdout$rf_f_prediction_class == "nogrowth" & data_holdout$target_f == "growth")
(fp*FP + fn*FN)/length(data_holdout$target)


# Summary results ---------------------------------------------------

nvars[["rf_p"]] <- length(rfvars)

summary_results <- data.frame("Number of predictors" = unlist(nvars),
                              "CV RMSE" = unlist(CV_RMSE),
                              "CV AUC" = unlist(CV_AUC),
                              "CV threshold" = unlist(best_tresholds),
                              "CV expected Loss" = unlist(expected_loss))

model_names <- c("Logit X1", "Logit X4",
                 "Logit LASSO","RF probability")
summary_results <- summary_results %>%
  filter(rownames(.) %in% c("X1", "X4", "LASSO", "rf_p"))
rownames(summary_results) <- model_names

kable(x = summary_results, format = "latex", booktabs=TRUE,  digits = 3, row.names = TRUE,
      linesep = "", col.names = c("Number of predictors", "CV RMSE", "CV AUC",
                                  "CV threshold", "CV expected Loss")) 


#############################################################
# Different industries
data_holdout1 <- data_holdout %>% filter(ind == 3)
# Take model to holdout and estimate AUC and expected loss ------------------------------------

best_logit_with_loss <- logit_models[["X4"]]
best_logit_optimal_treshold <- best_tresholds[["X4"]]

logit_predicted_probabilities_holdout <- predict(best_logit_with_loss, newdata = data_holdout1, type = "prob")
data_holdout1[,"best_logit_with_loss_pred"] <- logit_predicted_probabilities_holdout[,"growth"]

# ROC curve on holdout
roc_obj_holdout <- roc(data_holdout1$target, data_holdout1[, "best_logit_with_loss_pred", drop=TRUE])
roc_obj_holdout

# Get expected loss on holdout
holdout_treshold <- coords(roc_obj_holdout, x = best_logit_optimal_treshold, input= "threshold",
                           ret="all", transpose = FALSE)
expected_loss_holdout <- (holdout_treshold$fp*FP + holdout_treshold$fn*FN)/length(data_holdout1$target)
expected_loss_holdout








