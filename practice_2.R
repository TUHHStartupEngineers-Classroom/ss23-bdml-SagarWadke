library(h2o)

employee_attrition_tbl          <- read_csv("C:/Users/Sagar/Documents/GitHub/ss23-bdml-SagarWadke/raw_data/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")
definitions_raw_tbl             <- read_excel("C:/Users/Sagar/Documents/GitHub/ss23-bdml-SagarWadke/raw_data/data_definitions.xlsx", sheet = 1, col_names = FALSE)
employee_attrition_readable_tbl <- process_hr_data_readable(employee_attrition_tbl, definitions_raw_tbl)
set.seed(seed = 1113)
split_obj                       <- rsample::initial_split(employee_attrition_readable_tbl, prop = 0.85)
train_readable_tbl              <- training(split_obj)
test_readable_tbl               <- testing(split_obj)

recipe_obj <- recipe(Attrition ~., data = train_readable_tbl) %>% 
  step_zv(all_predictors()) %>% 
  step_mutate_at(JobLevel, StockOptionLevel, fn = as.factor) %>% 
  prep()

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

h2o.init()

# Split data into a training and a validation data frame
# Setting the seed is just for reproducability
split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o  <- as.h2o(test_tbl)

# Set the target and predictors
y <- "Attrition"
x <- setdiff(names(train_h2o), y)

?h2o.automl

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame    = train_h2o,
  validation_frame  = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs  = 30,
  nfolds            = 5 
)

typeof(automl_models_h2o)

slotNames(automl_models_h2o)

automl_models_h2o@leaderboard

automl_models_h2o@leader

h2o.getModel("StackedEnsemble_BestOfFamily_4_AutoML_1_20230614_122017") %>% 
  h2o.saveModel(path = "C:/Users/Sagar/Documents/GitHub/ss23-bdml-SagarWadke/h20_models/")