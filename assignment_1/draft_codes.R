p1 <- ggplot(data = sample, aes(x = educ, y = log_wage)) +
  geom_jitter(width = 0.08,color = "azure4", size = 1.5,  shape = 16, alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  geom_smooth(method="loess", color="blueviolet", se=F, size=0.8, na.rm=T)+
  labs(x = "Education (years)", y = "ln(Hourly wage, US dollars)") + theme_linedraw() 

p2 <- ggplot(data = sample, aes(x = exper, y = log_wage)) +
  geom_jitter(width = 0.08,color = "azure4", size = 1.5,  shape = 16, alpha = 0.5, show.legend=FALSE, na.rm=TRUE) +
  geom_smooth(method="loess", color="blueviolet", se=F, size=0.8, na.rm=T)+
  labs(x = "Experience (years)", y = "ln(Hourly wage, US dollars)") + theme_linedraw() 

graph1 <- grid.arrange(p1, p2, ncol=2)
ggsave("graphs/graph1.png",plot = graph1, width = 10, height = 5, units = "in", dpi = 300)

# graph2 boxplot
p3 <- ggplot(data = sample, aes(x = as.factor(male), y = hourly_wage)) +
  geom_boxplot(color = "blueviolet",  alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  labs( y = "ln(Hourly wage, US dollars)", x = " ") + 
  scale_x_discrete(labels = c("Female", "Male"))+
  theme_linedraw()

p4 <- ggplot(data = sample, aes(x = as.factor(black), y = hourly_wage)) +
  geom_boxplot(color = "blueviolet",  alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  labs( y = "ln(Hourly wage, US dollars)", x = " ") + 
  scale_x_discrete(labels = c("Not Black", "Black"))+
  theme_linedraw()

p5 <- ggplot(data = sample, aes(x = as.factor(married), y = hourly_wage)) +
  geom_boxplot(color = "blueviolet",  alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  labs( y = "ln(Hourly wage, US dollars)", x = " ") + 
  scale_x_discrete(labels = c("Not Married", "Married"))+
  theme_linedraw()

graph2 <- grid.arrange(p3, p4, p5, ncol=3)
ggsave("graphs/graph2.png",plot = graph2, width = 10, height = 5, units = "in", dpi = 300)

stargazer(reg1log, reg2log, reg3log)


# Prediction
data <- sample %>% dplyr::select(educ,exper,expersq,male,married,black, no_child,native_born_us,
                                 top_states,low_states,hard_workers,top_industry,log_wage)
# Add new observation
new <- list(educ = 18, exper = 11, expersq = 121,male = 1,married = 0,black = 1, no_child = 1,native_born_us = 1,
            top_states = 1,low_states = 0,hard_workers = 1,top_industry = 1, log_wage=NA)
reg3 <- lm(log_wage ~ educ + exper + expersq + male + married + black + no_child + native_born_us + 
             top_states+low_states+hard_workers+top_industry, data=data)
summary(reg3)
# prediction
data$lnp2 <- predict(reg3, data)
rmse3 <- RMSE(data$lnp2,data$log_wage)

# prediction for new observation
predln_new <- predict(reg3, newdata = new,se.fit = TRUE, interval = "prediction")
predln_new80 <- predict(reg3, newdata = new,se.fit = TRUE, interval = "prediction", level=0.90)
predln_new80
lnp2_new <- predln_new$fit[[1]]

# predictions in levels
data$lnplev <- exp(data$lnp2)*exp((rmse3^2)/2)
lnp2_new_lev <- exp(lnp2_new)*exp((rmse3^2)/2)

# prediction intervals (log and level)
lnp2_PIlow <- predln_new$fit[2]
lnp2_PIhigh <- predln_new$fit[3]
lnplev_PIlow <- exp(lnp2_PIlow)*exp(rmse3^2/2)
lnplev_PIhigh <- exp(lnp2_PIhigh)*exp(rmse3^2/2)

#80%
lnp2_PIlow80 <- predln_new80$fit[2]
lnp2_PIhigh80 <- predln_new80$fit[3]
lnplev_PIlow80 <- exp(lnp2_PIlow80)*exp(rmse3^2/2)
lnplev_PIhigh80 <- exp(lnp2_PIhigh80)*exp(rmse3^2/2)

# summary of predictions and PI
sum <- matrix( c( lnp2_new, lnp2_PIlow ,lnp2_PIhigh,
                  lnp2_new_lev, lnplev_PIlow, lnplev_PIhigh) , nrow = 3 ,ncol = 2)

colnames(sum) <- c('Model in logs', 'Recalculated to level')
rownames(sum) <- c('Predicted', 'PI_low', 'PI_high')
sum

sum <- matrix( c( lnp2_new, lnp2_PIlow80 ,lnp2_PIhigh80,
                  lnp2_new_lev, lnplev_PIlow80, lnplev_PIhigh80) , nrow = 3 ,ncol = 2)

colnames(sum) <- c('Model in logs', 'Recalculated to level')
rownames(sum) <- c('Predicted', 'PI_low 80%', 'PI_high 80%')

stargazer(sum, type = "latex", float=F, digits=2)



