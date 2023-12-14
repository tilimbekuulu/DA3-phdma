#### SET UP
# CLEAR MEMORY
rm(list=ls())

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

library(grid)

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
data <- read_csv(paste(data_in,"cs_bisnode_panel.csv", sep = "/"))

# drop variables with many NAs
data <- data %>%
  select(-c(COGS, finished_prod, net_dom_sales, net_exp_sales, wages)) %>%
  filter(year !=2016)

###########################################################
# label engineering
###########################################################

# generate status_alive; if sales larger than zero and not-NA, then firm is alive
data  <- data %>%
  mutate(status_alive = sales > 0 & !is.na(sales) %>%
           as.numeric(.))

# Leaving only the years 2012 and 2013 and only the alive firms ( to calculate the growth rate)
data <- data %>%
  filter(year >= 2012 & year <= 2013) %>% 
  filter(status_alive == 1)

# drop comp_id with less than 2 observations
data <- data %>%
  group_by(comp_id) %>%
  mutate(n = n()) %>%
  filter(n >= 2) %>%
  select(-n)

# calculate the growth rate from 2012 to 2013 and dropping year 2013
data <- data %>%
  group_by(comp_id) %>%
  mutate(growth = sales[year == 2013] / sales[year == 2012] - 1) %>% 
  mutate(growth = round(growth, 2)) %>% 
  filter(year == 2012)

table(data$exit_year, exclude = NULL)

# dropping firms that exited in 1998 or earlier (these are clearly mistakes, since they are active at 2012) 2 # observations
data <- data %>% 
  filter(exit_year > 1998|is.na(exit_year) == TRUE)

# new firms
table(data$founded_year, exclude = NULL) # 1806 NAs
# extract year from founded_date
data <- data %>%
  mutate(founded_year_extracted = year(founded_date),
         difference_founded = founded_year - founded_year_extracted) 
table(data$difference_founded, exclude = NULL) # they are similar 

table(data$founded_year_extracted, exclude = NULL)# 2 NAs

data <- data %>%
  mutate(age = (year - founded_year_extracted)) %>% 
  filter(age >= 0) # it cannot be negative (it would imply that the firm was founded after 2012)

table(data$age, exclude = NULL)

# Medium enterprises
data <- data %>%
  mutate(sales_mil = sales / 1000000) %>%
  # look at firms below 10m euro revenues and above 1000 euros
  filter(!(sales_mil > 10)) %>%
  filter(!(sales_mil < 0.001)) %>% 
  mutate(sales_mil_log = log(sales_mil),sales_mil_log_sq=sales_mil_log^2)

# new firms
data <- data %>%
  mutate(new = as.numeric(age <= 1) %>% #  (age could be 0,1 )
           ifelse(balsheet_notfullyear == 1, 1, .))

# change some industry category codes
data <- data %>%
  mutate(ind2_cat = ind2 %>%
           ifelse(. > 56, 60, .)  %>%
           ifelse(. < 26, 20, .) %>%
           ifelse(. < 55 & . > 35, 40, .) %>%
           ifelse(. == 31, 30, .) %>%
           ifelse(is.na(.), 99, .)
  )

table(data$ind2_cat, exclude = NULL)

# Firm characteristics
data <- data %>%
  mutate(age2 = age^2,
         foreign_management = as.numeric(foreign >= 0.5),
         gender_m = factor(gender, levels = c("female", "male", "mix")),
         m_region_loc = factor(region_m, levels = c("Central", "East", "West")))

#financial variables, create ratios
###########################################################
# assets can't be negative. Change them to 0 and add a flag.
data <-data  %>%
  mutate(flag_asset_problem=ifelse(intang_assets<0 | curr_assets<0 | fixed_assets<0,1,0  ))
table(data$flag_asset_problem)

data <- data %>%
  mutate(intang_assets = ifelse(intang_assets < 0, 0, intang_assets),
         curr_assets = ifelse(curr_assets < 0, 0, curr_assets),
         fixed_assets = ifelse(fixed_assets < 0, 0, fixed_assets))

# generate total assets
data <- data %>%
  mutate(total_assets_bs = intang_assets + curr_assets + fixed_assets)
summary(data$total_assets_bs)

pl_names <- c("extra_exp","extra_inc",  "extra_profit_loss", "inc_bef_tax" ,"inventories",
              "material_exp", "profit_loss_year", "personnel_exp")
bs_names <- c("intang_assets", "curr_liab", "fixed_assets", "liq_assets", "curr_assets",
              "share_eq", "subscribed_cap", "tang_assets" )

# divide all pl_names elements by sales and create new column for it
data <- data %>%
  mutate_at(vars(pl_names), list("pl" = ~ ./sales))

# divide all bs_names elements by total_assets_bs and create new column for it
data <- data %>%
  mutate_at(vars(bs_names), list("bs"= ~ ifelse(total_assets_bs == 0, 0, ./total_assets_bs)))

# creating flags, and winsorizing tails
########################################################################

# Variables that represent accounting items that cannot be negative (e.g. materials)
zero <-  c("extra_exp_pl", "extra_inc_pl", "inventories_pl", "material_exp_pl", "personnel_exp_pl",
           "curr_liab_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs", "subscribed_cap_bs",
           "intang_assets_bs")

data <- data %>%
  mutate_at(vars(zero), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(zero), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(zero), funs("flag_error"= as.numeric(.< 0))) %>%
  mutate_at(vars(zero), funs(ifelse(.< 0, 0, .)))


# for vars that could be any, but are mostly between -1 and 1
any <-  c("extra_profit_loss_pl", "inc_bef_tax_pl", "profit_loss_year_pl", "share_eq_bs")

data <- data %>%
  mutate_at(vars(any), funs("flag_low"= as.numeric(.< -1))) %>%
  mutate_at(vars(any), funs(ifelse(.< -1, -1, .))) %>%
  mutate_at(vars(any), funs("flag_high"= as.numeric(.> 1))) %>%
  mutate_at(vars(any), funs(ifelse(.> 1, 1, .))) %>%
  mutate_at(vars(any), funs("flag_zero"= as.numeric(.== 0))) %>%
  mutate_at(vars(any), funs("quad"= .^2))

# dropping flags with no variation
variances<- data %>%
  select(contains("flag")) %>%
  apply(2, var, na.rm = TRUE) == 0

data <- data %>%
  select(-one_of(names(variances)[variances]))

########################################################################
# additional
# including some imputation
########################################################################

# CEO age
data <- data %>%
  mutate(ceo_age = year-birth_year,
         flag_low_ceo_age = as.numeric(ceo_age < 25 & !is.na(ceo_age)),
         flag_high_ceo_age = as.numeric(ceo_age > 75 & !is.na(ceo_age)),
         flag_miss_ceo_age = as.numeric(is.na(ceo_age)))

table(data$ceo_age, exclude = NULL)
mean(data$ceo_age, na.rm = TRUE)

data <- data %>%
  mutate(ceo_age = ifelse(ceo_age < 25, 25, ceo_age) %>%
           ifelse(. > 75, 75, .))
# replace missing values in ceo_age with mean

data <- data %>%
  mutate(ceo_age = ifelse(is.na(ceo_age), 47, ceo_age),
         ceo_young = as.numeric(ceo_age < 40))

table(data$ceo_young, exclude = NULL)

# number of labor avg count missing values
table(is.na(data$labor_avg))
# mean of labor_avg
mean(data$labor_avg, na.rm = TRUE)

data <- data %>%
  mutate(labor_avg_mod = ifelse(is.na(labor_avg), 0.66, labor_avg),
         flag_miss_labor_avg = as.numeric(is.na(labor_avg)))

summary(data$labor_avg)
summary(data$labor_avg_mod)

data <- data %>%
  select(-labor_avg)

# create factors
data <- data %>%
  mutate(urban_m = factor(urban_m, levels = c(1,2,3)),
         ind2_cat = factor(ind2_cat, levels = sort(unique(data$ind2_cat))))


# creating a target variable based on sales growth if sales >= 0.50 then 1 else 0 use ifelse
data <- data %>%
  mutate(target = ifelse(growth >= 0.50, 1, 0))
table(data$target) # 3608 firms with sales growth >= 50%

# check data variables for NA's

# no more imputation, drop obs if key vars missing
data <- data %>%
  filter(!is.na(liq_assets_bs),!is.na(foreign), !is.na(ind))
# drop missing
data <- data %>%
  filter(!is.na(age),!is.na(foreign), !is.na(material_exp_pl), !is.na(m_region_loc))
Hmisc::describe(data$age)

# drop unused factor levels
data <- data %>%
  mutate_at(vars(colnames(data)[sapply(data, is.factor)]), funs(fct_drop))
table(data$ind2_cat)

# make target variable a factor with names of factor
data <- data %>%
  mutate(target_f = factor(target, levels = c(0,1), labels = c("nogrowth", "growth")))
table(data$target_f)
table(data$target)


datasummary_skim(data, type='numeric', histogram = TRUE)
datasummary_skim(data)


## Some graphs""

# Histograms
sales_g <- ggplot(data, aes(sales_mil)) +
  geom_histogram(binwidth = 0.15, fill = color[3], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("Count") +
  xlab("Sales (in million EUR)") +
  theme_bg()

logsales_g2 <- ggplot(data, aes(sales_mil_log)) +
  geom_histogram(binwidth = 0.15, fill = color[3], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("Count") +
  xlab("Log Sales (in million EUR)") +
  theme_bg()

# put sales_g and logsales_g2 in one plot
sales_plot <- grid.arrange(sales_g, logsales_g2, ncol = 2)

# save plot
ggsave("output/sales_plot.png",plot =  sales_plot, width = 8, height = 4, units = "in", dpi = 300)

# Boxplots Financials

g1 <- ggplot(data = data, aes(x = as.factor(target), y = sales_mil_log)) +
  geom_boxplot(color = color[3],  alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  labs( y = "ln(Sales in million EUR)", x = " ") + 
  scale_x_discrete(labels = c("Other", "Fast Growing firm"))+
  theme_linedraw()

 g2 <- ggplot(data = data, aes(x = as.factor(target), y = log(total_assets_bs))) +
  geom_boxplot(color = color[3],  alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  labs( y = "In(Total Assets in million EUR)", x = " ") + 
  scale_x_discrete(labels = c("Other", "Fast Growing firm"))+
  theme_linedraw()

g3 <- ggplot(data = data, aes(x = as.factor(target), y = 100*curr_liab_bs)) +
  geom_boxplot(color = color[3],  alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  labs( y = "Current Liabilities %", x = " ") + 
  scale_x_discrete(labels = c("Other", "Fast Growing firm"))+
  theme_linedraw()

# put together
financials_plot <- grid.arrange(g1, g2,g3, ncol = 3)

# save plot
ggsave("output/financials_plot.png",plot =  financials_plot, width = 8, height = 4, units = "in", dpi = 300)

# Other
g4 <- ggplot(data = data, aes(x = as.factor(target), y = ceo_age)) +
  geom_boxplot(color = color[3],  alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  labs( y = "Age of the CEO", x = " ") + 
  scale_x_discrete(labels = c("Other", "Fast Growing firm"))+
  theme_linedraw()

g5 <- ggplot(data = data, aes(x = as.factor(target), y = age)) +
  geom_boxplot(color = color[3],  alpha = 0.8, show.legend=FALSE, na.rm=TRUE) +
  labs( y = "Age of the firm", x = " ") + 
  scale_x_discrete(labels = c("Other", "Fast Growing firm"))+
  theme_linedraw()

# put together
age_plot <- grid.arrange(g4, g5, ncol = 2)

# save plot
ggsave("output/age_plot.png",plot =  age_plot, width = 8, height = 4, units = "in", dpi = 300)
















