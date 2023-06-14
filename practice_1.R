# Load data
library(tidyverse)
library(readxl)

employee_attrition_tbl <- read_csv("C:/Users/Sagar/Documents/GitHub/ss23-bdml-SagarWadke/raw_data/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")
definitions_raw_tbl    <- read_excel("C:/Users/Sagar/Documents/GitHub/ss23-bdml-SagarWadke/raw_data/data_definitions.xlsx", sheet = 1, col_names = FALSE)
View(definitions_raw_tbl)

employee_attrition_tbl %>% 
  ggplot(aes(Education)) +
  geom_bar()

# Data preparation ----
# Human readable

definitions_tbl <- definitions_raw_tbl %>% 
  fill(...1, .direction = "down") %>%
  filter(!is.na(...2)) %>%
  separate(...2, into = c("key", "value"), sep = " '", remove = TRUE) %>%
  rename(column_name = ...1) %>%
  mutate(key = as.numeric(key)) %>%
  mutate(value = value %>% str_replace(pattern = "'", replacement = "")) 
definitions_tbl

# DATA PREPARATION ----
# Human readable ----

definitions_list <- definitions_tbl %>% 
  
  # Mapping over lists
  
  # Split into multiple tibbles
  split(.$column_name) %>%
  # Remove column_name
  map(~ select(., -column_name)) %>%
  # Convert to factors because they are ordered an we want to maintain that order
  map(~ mutate(., value = as_factor(value))) 

# definitions_list[[1]]
definitions_list[["Education"]]

for (i in seq_along(definitions_list)) {
  list_name <- names(definitions_list)[i]
  colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value"))
}

definitions_list[["Education"]]

data_merged_tbl <- list(HR_Data = employee_attrition_tbl) %>%
  
  # Join everything
  append(definitions_list, after = 1) %>%
  reduce(left_join) %>%
  
  # Remove unnecessary columns
  select(-one_of(names(definitions_list))) %>%
  
  # Format the "_value"
  set_names(str_replace_all(names(.), pattern = "_value", replacement = "")) %>%
  
  # Resort
  select(sort(names(.))) 

data_merged_tbl %>% 
  distinct(BusinessTravel)

data_merged_tbl %>%
  mutate_if(is.character, as.factor) %>%
  glimpse()

data_merged_tbl %>%
  mutate_if(is.character, as.factor) %>%
  select_if(is.factor) %>%
  glimpse()

data_merged_tbl %>%
  mutate_if(is.character, as.factor) %>%
  select_if(is.factor) %>%
  map(levels)

data_processed_tbl <- data_merged_tbl %>%        
  mutate_if(is.character, as.factor) %>%
  mutate(
    BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", 
                                                    "Travel_Rarely", 
                                                    "Travel_Frequently"),
    MaritalStatus  = MaritalStatus %>% fct_relevel("Single", 
                                                   "Married", 
                                                   "Divorced")
  )

data_processed_tbl %>% 
  select_if(is.factor) %>% 
  map(levels)

process_hr_data_readable <- function(data, definitions_tbl) {
  
  definitions_list <- definitions_tbl %>%
    fill(...1, .direction = "down") %>%
    filter(!is.na(...2)) %>%
    separate(...2, into = c("key", "value"), sep = " '", remove = TRUE) %>%
    rename(column_name = ...1) %>%
    mutate(key = as.numeric(key)) %>%
    mutate(value = value %>% str_replace(pattern = "'", replacement = "")) %>%
    split(.$column_name) %>%
    map(~ select(., -column_name)) %>%
    map(~ mutate(., value = as_factor(value))) 
  
  for (i in seq_along(definitions_list)) {
    list_name <- names(definitions_list)[i]
    colnames(definitions_list[[i]]) <- c(list_name, paste0(list_name, "_value"))
  }
  
  data_merged_tbl <- list(HR_Data = data) %>%
    append(definitions_list, after = 1) %>%
    reduce(left_join) %>%
    select(-one_of(names(definitions_list))) %>%
    set_names(str_replace_all(names(.), pattern = "_value", 
                              replacement = "")) %>%
    select(sort(names(.))) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(
      BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", 
                                                      "Travel_Rarely", 
                                                      "Travel_Frequently"),
      MaritalStatus  = MaritalStatus %>% fct_relevel("Single", 
                                                     "Married", 
                                                     "Divorced")
    )
  
  return(data_merged_tbl)
  
}
process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl) %>% 
  glimpse()

DATA PREPARATION ----
  # Machine readable ----

# libraries
library(rsample)
library(recipes)

# Processing pipeline
# If we had stored our script in an external file
source("C:/Users/Sagar/Documents/GitHub/ss23-bdml-SagarWadke/raw_data/00_scripts/data_processing_pipeline.R")

# If we had our raw data already split into train and test data
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_redable_tbl   <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

employee_attrition_readable_tbl <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)

# Split into test and train
set.seed(seed = 1113)
split_obj <- rsample::initial_split(employee_attrition_readable_tbl, prop = 0.85)

# Assign training and test data
train_readable_tbl <- training(split_obj)
test_readable_tbl  <- testing(split_obj)

#

# Data Preprocessing With Recipes ----

# Plan: Correlation Analysis

# 1. Zero Variance Features ----

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors())

recipe_obj %>% 
  prep()

# 2. Transformations ---- (for skewed features)
library(PerformanceAnalytics)  # for skewness  

skewed_feature_names <- train_readable_tbl %>%
  select(where(is.numeric)) %>%
  map_df(skewness) %>%
  pivot_longer(cols = everything(),
               names_to = "key",
               values_to = "value",
               names_transform = list(key = forcats::fct_inorder)) %>%
  arrange(desc(value)) %>%
  
  # Let's set the cutoff value to 0.7 (beccause TrainingTimesLastYear does not seem to be that skewed)
  filter(value >= 0.7) %>%
  pull(key) %>%
  as.character()

train_readable_tbl %>%
  select(all_of(skewed_feature_names)) %>%
  plot_hist_facet()

!skewed_feature_names %in% c("JobLevel", "StockOptionLevel")

skewed_feature_names <- train_readable_tbl %>%
  select(where(is.numeric)) %>%
  map_df(skewness) %>%
  pivot_longer(cols = everything(),
               names_to = "key",
               values_to = "value",
               names_transform = list(key = forcats::fct_inorder)) %>%
  arrange(desc(value)) %>%
  filter(value >= 0.7) %>%
  filter(!key %in% c("JobLevel", "StockOptionLevel")) %>%
  pull(key) %>%
  as.character()

# We need to convert those columns to factors in th
factor_names <- c("JobLevel", "StockOptionLevel")

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_mutate_at(factor_names, fn = as.factor)

recipe_obj %>% 
  prep() %>% 
  bake(train_readable_tbl) %>% 
  select(skewed_feature_names) %>%
  plot_hist_facet() 

# 3. Center and scale

# Plot numeric data
train_readable_tbl %>% 
  select(where(is.numeric)) %>% 
  plot_hist_facet

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_mutate_at(factor_names, fn = as.factor) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

# You can compare the means attribute before and after prepping the recipe
recipe_obj$steps[[4]] # before prep
prepared_recipe <- recipe_obj %>% prep()
prepared_recipe$steps[[4]]

prepared_recipe %>%
  bake(new_data = train_readable_tbl) %>%
  select(where(is.numeric)) %>% 
  plot_hist_facet()

# 4. Dummy variables ----

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_mutate_at(factor_names, fn = as.factor) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_nominal()) %>% 
  
  # prepare the final recipe
  prep()

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)

train_tbl %>% glimpse()

test_tbl <- bake(recipe_obj, new_data = test_readable_tbl)

?stats::cor

train_tbl %>%
  
  # Convert characters & factors to numeric
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), as.numeric)) %>%
  
  # Correlation
  cor(use = "pairwise.complete.obs") %>% 
  as_tibble() %>%
  mutate(feature = names(.)) %>% 
  select(feature, Attrition_Yes) %>% 
  
  # Filter the target, because we now the correlation is 100%
  filter(!(feature == "Attrition_Yes")) %>% 
  
  # Convert character back to factors
  mutate(across(where(is.character), as_factor))

get_cor <- function(data, target, use = "pairwise.complete.obs",fct_reorder = FALSE, fct_rev = FALSE){
  
  data_cor <- train_tbl %>%

  # Correlation
  get_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T) %>%

  # Create label text
  mutate(feature_name_text = round(Attrition_Yes, digits = 2)) %>%

  # Create flags so that we can change the color for poitive and negative
  mutate(Correlation = case_when(
    (Attrition_Yes) >= 0 ~ "Positive",
    TRUE                   ~ "Negative") %>% as.factor())

  data_cor %>%
    ggplot(aes(x = Attrition_Yes, y = feature, group = feature)) +
    geom_point(aes(color = Correlation), size = 2) +
    geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = 1) +
    geom_vline(xintercept = 0, color = "black", size = 0.5) +
    expand_limits(x = c(-1, 1)) +
    scale_color_manual(values = c("red", "#2dc6d6")) +
    geom_label(aes(label = feature_name_text), hjust = "outward")
}
plot_cor <- function(data, target, fct_reorder = FALSE, fct_rev = FALSE, 
                     include_lbl = TRUE, lbl_precision = 2, 
                     lbl_position = "outward",
                     size = 2, line_size = 1, vert_size = 0.5, 
                     color_pos = "#2dc6d6", color_neg = "red") {
  
  feature_expr <- enquo(target)
  
  # Perform correlation analysis
  data_cor <- data %>%
    get_cor(!! feature_expr, fct_reorder = fct_reorder, fct_rev = fct_rev) %>%
    mutate(feature_name_text = round(!! feature_expr, lbl_precision)) %>%
    mutate(Correlation = case_when(
      (!! feature_expr) >= 0 ~ "Positive",
      TRUE                   ~ "Negative") %>% as.factor())
  
  # Plot analysis
  g <- data_cor %>%
    ggplot(aes(x = !! feature_expr, y = feature, group = feature)) +
    geom_point(aes(color = Correlation), size = size) +
    geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
    geom_vline(xintercept = 0, color = "black", size = vert_size) +
    expand_limits(x = c(-1, 1)) +
    scale_color_manual(values = c(color_neg, color_pos)) +
    theme(legend.position = "bottom")
  
  if (include_lbl) g <- g + geom_label(aes(label = feature_name_text), hjust = lbl_position)
  
  return(g)
  
}
plot_cor(data = train_tbl, target = Attrition_Yes, fct_reorder = T, fct_rev = T)

train_tbl %>% 
  select(Attrition_Yes, contains("JobRole")) %>% 
  plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)