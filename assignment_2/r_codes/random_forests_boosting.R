# CLEAR MEMORY
rm(list=ls())


library(rattle)
library(tidyverse)
library(caret)
library(ranger)
library(Hmisc)
library(knitr)
library(kableExtra)
library(xtable)


#location of folders
data_in  <- "raw_data/"
data_out <- "clean_data/"

# set data dir, load theme and functions
source("ch00-tech-prep/theme_bg.R")
source("ch00-tech-prep/da_helper_functions.R")

# Import the cleaned data
data <- read_csv(paste(data_out,"airbnb_sydney_cleaned.csv", sep = ""))
data2 <- read_csv(paste(data_out,"airbnb_sydney_cleaned_june_holdout.csv", sep = ""))

####################################################################################

# create train and holdout samples -------------------------------------------
# train is where we do it all, incl CV

set.seed(02122023)


train_indices <- as.integer(createDataPartition(data$price, p = 0.7, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

data_holdout2 <- data2[-train_indices, ]



dim(data_train)
dim(data_holdout)

# Define models: 

# Basic Variables inc neighnourhood
basic_vars <- c("n_accommodates","n_beds","n_bedroom_count", "f_room_type", 
                "f_bathroom","f_minimum_nights","f_neighbourhood_cleansed")

# reviews
reviews <- reviews <- c("f_number_of_reviews","d_host_is_superhost",
                        "d_host_has_profile_pic","d_host_identity_verified","d_instant_bookable")
# amenities
amenities <-  grep("^d_.*", names(data), value = TRUE)
# delete from amenities vector first 3 columns
amenities <- amenities[-c(1:4)]

X1  <- c("f_room_type*d_private_balcony", "f_room_type*d_view","f_room_type*f_neighbourhood_cleansed",
         "f_property_type*f_neighbourhood_cleansed")

# Additional interactions of factors and dummies
X2  <- c("d_netflix*f_room_type","d_bathtub*f_room_type", "d_outdoor_furniture*f_room_type")


predictors_1 <- c(basic_vars)
predictors_2 <- c(basic_vars, reviews, amenities)
predictors_E <- c(basic_vars, reviews, amenities, X1,X2)

#########################################################################################

# RANDOM FORESTS -------------------------------------------------------
#
#########################################################################################
# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)

# set tuning for benchamrk model (2)
tune_grid <- expand.grid(
  .mtry = c(8, 10, 12),
  .splitrule = "variance",
  .min.node.size = c(5, 10, 15)
)

set.seed(1234)
system.time({
  rf_model_2 <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

rf_model_2

# Latex Tables
# Show Model with all the combinations
rf_tuning_modelB <- rf_model_2$results %>%
  dplyr::select(mtry, min.node.size, RMSE) %>%
  dplyr::rename(nodes = min.node.size) %>%
  spread(key = mtry, value = RMSE)

#########################################################################################
# Variable Importance Plots -------------------------------------------------------
#########################################################################################
# first need a function to calculate grouped varimp
group.importance <- function(rf.obj, groups) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(importance(rf.obj)[g], na.rm = TRUE)
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}

rf_model_2_var_imp <- importance(rf_model_2$finalModel)/1000
rf_model_2_var_imp_df <-
  data.frame(varname = names(rf_model_2_var_imp),imp = rf_model_2_var_imp) %>%
  mutate(varname = gsub("f_neighbourhood_cleansed", "Borough:", varname) ) %>%
  mutate(varname = gsub("f_room_type", "Room type:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))


##############################
# 1) full varimp plot, above a cutoff
##############################

# to have a quick look
plot(varImp(rf_model_2))
cutoff = 1500
rf_model_2_var_imp_plot <- ggplot(rf_model_2_var_imp_df[rf_model_2_var_imp_df$imp>cutoff,],
                                  aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=1.5) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=1) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bg() +
  theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6),
        axis.title.x = element_text(size=6), axis.title.y = element_text(size=6))
rf_model_2_var_imp_plot

# save plot in the graphs folder
ggsave("graphs/feature_import_plot.png", width = 6, height = 4, units = "in", dpi = 300)


pdp_n_acc <- pdp::partial(rf_model_2, pred.var = "n_accommodates", pred.grid = distinct_(data_holdout, "n_accommodates"), train = data_train)
pdp_n_acc_plot <- pdp_n_acc %>%
  autoplot( ) +
  geom_point(color=color[1], size=2) +
  geom_line(color=color[1], size=1) +
  ylab("Predicted price") +
  xlab("Accommodates (persons)") +
  scale_x_continuous(limit=c(2,6), breaks=seq(1,6,1))+
  theme_bg()
pdp_n_acc_plot

# save plot in the graphs folder
ggsave("graphs/pdp_n_acc_plot.png", width = 6, height = 4, units = "in", dpi = 300)

pdp_n_roomtype <- pdp::partial(rf_model_2, pred.var = "f_room_type", pred.grid = distinct_(data_holdout, "f_room_type"), train = data_train)
pdp_n_roomtype_plot <- pdp_n_roomtype %>%
  autoplot( ) +
  geom_point(color=color[1], size=2) +
  ylab("Predicted price") +
  xlab("Room type") +
  theme_bg()
pdp_n_roomtype_plot


#### GBM Model

# GBM  -------------------------------------------------------

gbm_grid <-  expand.grid(interaction.depth = c(1, 5, 10), # complexity of the tree
                         n.trees = (4:10)*50, # number of iterations, i.e. trees
                         shrinkage = 0.1, # learning rate: how quickly the algorithm adapts
                         n.minobsinnode = 20 # the minimum number of training set samples in a node to commence splitting
)

set.seed(1234)
system.time({
  gbm_model <- train(formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
                     data = data_train,
                     method = "gbm",
                     trControl = train_control,
                     verbose = FALSE,
                     tuneGrid = gbm_grid)
})
gbm_model


### All models
# OLS
basic_lev_ols  <- c("n_accommodates","n_beds","n_bedroom_count", "f_room_type")
basic_add_ols <- c("f_bathroom","f_minimum_nights","f_neighbourhood_cleansed")
reviews_ols <- c("f_number_of_reviews","d_host_is_superhost",
             "d_host_has_profile_pic","d_host_identity_verified","d_instant_bookable")
poly_lev_ols <- c("n_accommodates2", "n_amenities_count2", "n_beds2")

set.seed(1234)
system.time({
  ols_model <- train(
    formula(paste0(" price ~",paste(c(basic_lev_ols,basic_add_ols,reviews_ols,poly_lev_ols),collapse = " + "))),
    data = data_train,
    method = "lm",
    trControl = train_control
  )
})

ols_model_coeffs <-  ols_model$finalModel$coefficients
ols_model_coeffs_df <- data.frame(
  "variable" = names(ols_model_coeffs),
  "ols_coefficient" = ols_model_coeffs
) %>%
  mutate(variable = gsub("`","",variable))

# All three models
final_models <-
  list("OLS" = ols_model,
       "Random forest" = rf_model_2,
       "GBM (basic tuning)"  = gbm_model)

results <- resamples(final_models) %>% summary()

result_4 <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")

kable(x = result_4, format = "latex", digits = 3, booktabs=TRUE, linesep = "")

result_5 <- map(final_models, ~{
  RMSE(predict(.x, newdata = data_holdout2), data_holdout2[["price"]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("Holdout RMSE" = ".")
kable(x = result_5, format = "latex", digits = 3, booktabs=TRUE, linesep = "") 





