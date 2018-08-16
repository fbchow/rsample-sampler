library(tidyverse)
library(tidymodels)


names_df <- tribble(
  ~name, ~fav_num,
  "Dana", 4,
  "Irene", 8,
  "Mara", 2,
  "Ming", 5,
  "Chaita", 10,
  "Jen", 7,
  "Becky", 4,
  "Jenny", 9,
  "Mine", 2, 
  "Emily", 6
)

boxplot(names_df$fav_num)
View(names_df)
nrow(names_df)
str(names_df)
summary(names_df)


first_split <- initial_split(names_df)
train_names <- analysis(first_split)
test_names <- assessment(first_split)



my_folds <- vfold_cv(train_names, v=3)
my_folds

# The `split` objects contain the information about the sample sizes
my_folds$splits[[1]]


# Use the `analysis` and `assessment` functions to get the data
analysis(my_folds$splits[[1]]) %>% dim()
analysis(my_folds$splits[[1]])


assessment(my_folds$splits[[1]]) %>% dim()
assessment(my_folds$splits[[1]])


# What about the 2nd fold?  
my_folds$splits[[2]]
analysis(my_folds$splits[[2]]) %>% dim()
analysis(my_folds$splits[[2]])

assessment(my_folds$splits[[2]]) %>% dim()
assessment(my_folds$splits[[2]])



# Attrition Example -------------------------------------------------------
data("attrition")
colnames(attrition)
head(attrition)

set.seed(888)
attrition_split <- initial_split(attrition)
attrition_split

attrition_train <- training(attrition_split)
attrit_test <- testing(attrition_split) 

attr_folds <- vfold_cv(attrition_train, v = 10) 
attr_folds



# The `split` objects contain the information about the sample sizes
my_folds$splits[[1]]


# Use the `analysis` and `assessment` functions to get the data
analysis(attr_folds$splits[[1]]) %>% dim()
analysis(attr_folds$splits[[1]])


assessment(attr_folds$splits[[1]]) %>% dim()
assessment(attr_folds$splits[[1]])


# What about the 2nd fold?  
my_folds$splits[[2]]
analysis(attr_folds$splits[[2]]) %>% dim()
analysis(attr_folds$splits[[2]])

assessment(attr_folds$splits[[2]]) %>% dim()
assessment(attr_folds$splits[[2]])



# boostrap quick ----------------------------------------------------------
# example below from Max @topeops
ggplot(attrition, aes(x = Gender, y = MonthlyIncome)) + 
  geom_boxplot() + 
  scale_y_log10()


median_diff <- function(splits) {
  x <- analysis(splits)
  median(x$MonthlyIncome[x$Gender == "Female"]) - 
    median(x$MonthlyIncome[x$Gender == "Male"])     
}


set.seed(808)
bt_resamples <- bootstraps(attrition, times = 500)

bt_resamples$wage_diff <- map_dbl(bt_resamples$splits, median_diff)

ggplot(bt_resamples, aes(x = wage_diff)) + 
  geom_line(stat = "density", adjust = 1.25) + 
  xlab("Difference in Median Monthly Income (Female - Male)")


quantile(bt_resamples$wage_diff, 
         probs = c(0.025, 0.500, 0.975))  


# boostrap fit coefficients on each resample ------------------------------
mod_form <- as.formula(Attrition ~ JobSatisfaction + Gender + MonthlyIncome)

glm_coefs <- function(splits, ...) {
  ## use `analysis` or `as.data.frame` to get the analysis data
  mod <- glm(..., data = analysis(splits), family = binomial)
  as.data.frame(t(coef(mod)))
}

bt_resamples$betas <- map(.x = bt_resamples$splits, 
                          .f = glm_coefs, 
                          mod_form)
bt_resamples
bt_resamples$betas[[1]]

