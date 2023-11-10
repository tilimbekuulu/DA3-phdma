library("lmtest")
library("sandwich")

# graph1 

p1 <- ggplot(data = sample, aes(x = educ, y = log_wage)) +
  geom_jitter(width = 0.08,color = "blue", size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  geom_smooth(method="loess", color="darkorange1", se=F, size=0.8, na.rm=T)+
  labs(x = "Education (years)", y = "ln(Hourly wage, US dollars)") + theme_linedraw() 


p2 <- ggplot(data = sample, aes(x = exper, y = log_wage)) +
  geom_jitter(width = 0.08,color = "blue", size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  geom_smooth(method="loess", color="darkorange1", se=F, size=0.8, na.rm=T)+
  labs(x = "Experience (years)", y = "ln(Hourly wage, US dollars)") + theme_linedraw() 

 graph1 <- grid.arrange(p1, p2, ncol=2)

ggsave("graphs/graph1.png",plot = graph1, width = 10, height = 5, units = "in", dpi = 300)


# Table 1 regression of log_wage on educ, exper and summarize results
regression_1 <- lm(log_wage ~ educ + exper + expersq, data = sample)
summary(regression_1)  
coeftest(regression_1, vcov = sandwich)

# Linear regressions in logs now

# Model 1: Linear regression on age
model1log <- as.formula(log_wage ~ educ + exper + expersq)
# Models 2-4: 
model2log <- as.formula(log_wage ~ educ + exper + expersq + male + married + black + black*male)
model3log <- as.formula(log_wage ~ educ + exper + expersq + male + married + black + black*male + top_states + top_industry + low_states)
model4log <- as.formula(log_wage ~ educ + exper + expersq + male + married + black + black*male + 
                          top_states + top_industry + low_states  + top_states*top_industry+ low_states*top_industry)
reg1log <- lm(model1log, data=sample)
reg2log <- lm(model2log, data=sample)
reg3log <- lm(model3log, data=sample)
reg4log <- lm(model4log, data=sample)


# evaluation of the models

models <- c("reg1log", "reg2log","reg3log", "reg4log")
AIC <- c()
BIC <- c()
RMSE <- c()
RSquared <- c()
regr <- c()
k <- c()

for ( i in 1:length(models)){
  AIC[i] <- AIC(get(models[i]))
  BIC[i] <- BIC(get(models[i]))
  RMSE[i] <- RMSE(predict(get(models[i])), get(models[i])$model$log_wage)
  RSquared[i] <-summary(get(models[i]))$r.squared
  regr[[i]] <- coeftest(get(models[i]), vcov = sandwich)
  k[i] <- get(models[i])$rank -1
}

############################################################
# Linear regression evaluation


# All models
eval <- data.frame(models, k, RSquared, RMSE, BIC)
eval <- eval %>%
  mutate(models = paste0("(",gsub("reg","",models),")")) %>%
  rename(Model = models, "R-squared" = RSquared, "Training RMSE" = RMSE, "N predictors" = k)
#stargazer(eval, summary = F, digits=2, float = F, no.space = T)

# Cross-validation

# set number of folds (4 because of small sample)
k <- 4

# need to set the same seed again and again
set.seed(081123)
cv1log <- train(model1log, sample, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(081123)
cv2log <- train(model2log, sample, method = "lm", trControl = trainControl(method = "cv", number = k))
set.seed(081123)
cv3log <- train(model3log, sample, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")
set.seed(081123)
cv4log <- train(model4log, sample, method = "lm", trControl = trainControl(method = "cv", number = k), na.action = "na.omit")

# calculate average rmse
cv <- c("cv1log", "cv2log", "cv3log", "cv4log")
rmse_cv <- c()

for(i in 1:length(cv)){
  rmse_cv[i] <- sqrt((get(cv[i])$resample[[1]][1]^2 +
                        get(cv[i])$resample[[1]][2]^2 +
                        get(cv[i])$resample[[1]][3]^2 +
                        get(cv[i])$resample[[1]][4]^2)/4)
}


# summarize results
cv_matlog <- data.frame(rbind(cv1log$resample[4], "Average"),
                        rbind(cv1log$resample[1], rmse_cv[1]),
                        rbind(cv2log$resample[1], rmse_cv[2]),
                        rbind(cv3log$resample[1], rmse_cv[3]),
                        rbind(cv4log$resample[1], rmse_cv[4])
)

colnames(cv_matlog)<-c("Resample","Model1log", "Model2log", "Model3log", "Model4log")
cv_matlog

#stargazer(cv_matlog, summary = F, digits=3, float=F, out=paste(output,"Ch14_cvmatlog_R.tex",sep=""))
#stargazer(cv_matlog, summary = F, digits=3, float=F, type="text",  out=paste(output,"Ch14_cvmatlog_R.txt",sep=""))













