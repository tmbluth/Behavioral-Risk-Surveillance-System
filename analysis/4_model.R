library(tidyverse)
library(doParallel)
library(caret)
library(ranger)
library(klaR)
library(pROC)

# Load in custom function used later (downsample_part)
source('analysis/funcs.r')
# Load in cleaned data
clean_years_final <- read_rds('intermediate_saves/clean_years_final.rds') %>% sample_frac(0.5)

# This custom function partitions and downsamples data. There is also a 'upsample_part' function in funcs.R
set.seed(92)
db_part <- createDataPartition(clean_years_final$Diabetes, 0.7)
db   <- downsample_part(clean_years_final, target = 'Diabetes', train.pct = 0.8, val.pct = 0.15)
strk <- downsample_part(clean_years_final, target = 'Stroke', train.pct = 0.8, val.pct = 0.15)
dep  <- downsample_part(clean_years_final, target = 'Depression', train.pct = 0.8, val.pct = 0.15)

# Top 15 to 16 variables in prediction importance across all 3 models are used
# Diabetes
db <- map(db, function(x)
  dplyr::select(x, Diabetes, 
                General_Health,   Hypertension,   Age_Group,       BMI,             Employment,
                Recent_Checkup,   Need_Equipment, Physical_Health, Heart_Attack,    Race,
                Heart_Disease,  Fruit_Juice,     Sex,             Education,        Arthritis,               
                Limited))
    
# Stroke
strk <- map(strk, function(x) 
  dplyr::select(x, Stroke, 
                Heart_Attack, Employment,     Age_Group, General_Health,  Heart_Disease, 
                Hypertension, Need_Equipment, Limited,   Physical_Health, Arthritis,
                Diabetes,     COPD,           Greens,    Marital,         Education)) 

# Depression
dep <- map(dep, function(x) 
  dplyr::select(x, Depression, 
                Mental_Health,  Limited,   Sex,             Employment,       Age_Group,
                General_Health, Arthritis, Physical_Health, Smoker,           Marital,
                Medical_Cost,   Race,      Need_Equipment,  COPD,             BMI)) 

save(db, strk, dep, file = 'intermediate_saves/modeling_data.RData') 

#==================== Random Forest (Ranger) Models: ======================#

# ALL MODELS TESTED MANY HYPERPARAMETERS. LOWER mtry PERFORMED BEST ON ALL

# 1. Myocardial Infarction (heart attack)
rf_db  <- ranger(Diabetes ~ .,
                   data = db$Train_set,
                   mtry = 5,
                   num.trees = 200,
                   splitrule = 'gini',
                   probability = TRUE)
rf_db$prediction.error # This checks out-of-bag model error. 1 - error = accuracy


# 2. Stroke 
rf_strk  <- ranger(Stroke ~ .,
                     data = strk$Train_set,
                     mtry = 5,
                     num.trees = 200,
                     splitrule = 'gini',
                     probability = TRUE)
rf_strk$prediction.error


# 3. Depression
rf_dep  <- ranger(Depression ~ .,
                    data = dep$Train_set,
                    mtry = 5,
                    num.trees = 200,
                    splitrule = 'gini',
                    probability = TRUE)
rf_dep$prediction.error

# Only the highest performing models will be saved.
save(rf_db, rf_strk, rf_dep, file = 'intermediate_saves/rf_models.Rdata')

# VALIDATION SET NOT NEEDED ON RF MODELS DUE TO OOB ERROR FEEDBACK ON MODEL PERFORMANCE

# Predict RF models
rf_db_p <- predict(rf_db, db$Test_set)$predictions[,1]
auc(db$Test_set$Diabetes, rf_db_p)

rf_strk_p <- predict(rf_strk, strk$Test_set)$predictions[,1]
auc(strk$Test_set$Stroke, rf_strk_p)

rf_dep_p <- predict(rf_dep, dep$Test_set)$predictions[,1]
auc(dep$Test_set$Depression, rf_dep_p)

save(rf_db_p, rf_strk_p, rf_dep_p, file = 'intermediate_saves/rf_p.RData')

rm(rf_db, rf_db_p,
   rf_strk, rf_strk_p,
   rf_dep, rf_dep_p)

#============================ GLMnet Models ================================#

# Use 75% of cores available for faster parallel computing
registerDoParallel(makeCluster(round(detectCores()*0.75,0)))

ctrl <- trainControl(method = 'cv',
                     number = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

glm_tune <- expand.grid(alpha = 0.5, lambda = seq(0, 0.1, length = 10))

glm_db <- train(make.names(Diabetes) ~ .,
                data = db$Train_set,
                method = 'glmnet',
                metric = 'ROC',
                family = 'binomial', 
                tuneGrid = glm_tune,
                trControl = ctrl)
glm_db$results
glm_db_valid <- predict(glm_db, db$Valid_set, type = 'prob')$X1
confusionMatrix(glm_db_valid, db$Valid_set$Diabetes)

glm_strk <- train(make.names(Stroke) ~ .,
                  data = strk$Train_set,
                  method = 'glmnet',
                  metric = 'ROC',
                  family = 'binomial',
                  tuneGrid = glm_tune,
                  trControl = ctrl)
glm_strk$results
glm_strk_valid <- predict(glm_strk, strk$Valid_set, type = 'prob')$X1
confusionMatrix(glm_strk_valid, dep$Valid_set$Stroke)

system.time(
glm_dep <- train(make.names(Depression) ~ .,
                 data = dep$Train_set,
                 method = 'glmnet',
                 metric = 'ROC',
                 family = 'binomial',
                 tuneGrid = glm_tune,
                 trControl = ctrl)
)
glm_dep$results
glm_dep_valid <- predict(glm_dep, dep$Valid_set, type = 'prob')$X1
confusionMatrix(glm_dep_valid, dep$Valid_set$Depression)

save(glm_db, glm_strk, glm_dep, file = 'intermediate_saves/glm_models.RData')

glm_db_p <- predict(glm_db, db$Test_set, type = 'prob')$X1
glm_strk_p <- predict(glm_strk, strk$Test_set, type = 'prob')$X1
glm_dep_p <- predict(glm_dep, dep$Test_set, type = 'prob')$X1

save(glm_db_p, glm_strk_p, glm_dep_p, file = 'intermediate_saves/glm_p.RData')

rm(glm_db, glm_db_valid, glm_db_p, 
   glm_strk, glm_strk_valid, glm_strk_p, 
   glm_dep, glm_dep_valid, glm_dep_p, 
   glm_tune, ctrl)

#=========================== Naive Bayes Models: ===========================#

nb_db <- NaiveBayes(x = dplyr::select(db$Train_set, -Diabetes),
                    grouping = db$Train_set$Diabetes,
                    fL = 0.5,
                    useKernel = TRUE,
                    adjust = 0.5)
nb_db_valid <- predict(nb_db, db$Valid_set, type = 'prob')$posterior[,1]
confusionMatrix(nb_db_valid, db$Valid_set$Diabetes)

system.time(
nb_strk <- NaiveBayes(x = dplyr::select(strk$Train_set, -Stroke),
                      grouping = strk$Train_set$Stroke,
                      fL = 1)
)
nb_strk_valid <- predict(nb_strk, strk$Valid_set, type = 'prob')$posterior[,1]
confusionMatrix(nb_strk_valid, strk$Valid_set$Stroke)

nb_dep <- NaiveBayes(x = dplyr::select(dep$Train_set, -Depression),
                     grouping = dep$Train_set$Depression,
                     fL = 1,
                     useKernel = TRUE)
nb_dep_valid <- predict(nb_dep, dep$Valid_set, type = 'prob')$posterior[,1]
confusionMatrix(nb_dep_valid, dep$Valid_set$Depression)

save(nb_db, nb_strk, nb_dep, file = 'intermediate_saves/nb_models.RData')

# Predictions
nb_db_p <- predict(nb_db, db$Test_set, type = 'prob')$posterior[,1]
auc(db$Test_set$Diabetes, nb_db_p)

nb_strk_p <- predict(nb_strk, strk$Test_set, type = 'prob')$posterior[,1]
auc(strk$Test_set$Stroke, nb_strk_p)

nb_dep_p <- predict(nb_dep, dep$Test_set, type = 'prob')$posterior[,1]
auc(dep$Test_set$Depression, nb_dep_p)

save(nb_db_p, nb_strk_p, nb_dep_p, file = 'intermediate_saves/nb_p.RData')

rm(nb_db, nb_db_valid, nb_db_p,
   nb_strk, nb_strk_valid, nb_strk_p,
   nb_dep, nb_dep_valid, nb_dep_p)

#============================ Ensemble Model ===============================#

library(corrplot)
library(ROCR)
library(caret)
library(dplyr)

# Load in predictions
load('intermediate_saves/modeling_data.RData')
load('intermediate_saves/rf_p.RData')
load('intermediate_saves/glm_p.RData')
load('intermediate_saves/nb_p.RData')

# Combine predictions into one data frame: '1' is Yes, '2' is No
diabetes <- cbind(RF = rf_db_p, GLM = glm_db_p, NB = nb_db_p) %>% as.data.frame()
stroke <- cbind(RF = rf_strk_p, GLM = glm_strk_p, NB = nb_strk_p) %>% as.data.frame()
depression <- cbind(RF = rf_dep_p, GLM = glm_dep_p, NB = nb_dep_p) %>% as.data.frame()
# Lets see how correlated each set of predictions are. Lower is better since it indicates more diversity of model opinion
corrplot(cor(diabetes), method = "color", addCoef.col="grey", title = 'Diabetes Models', mar=c(0,0,2,0))
corrplot(cor(stroke), method = "color", addCoef.col="grey", title = 'Stroke Models', mar=c(0,0,2,0)) 
corrplot(cor(depression), method = "color", addCoef.col="grey", title = 'Depression Models', mar=c(0,0,2,0))

par(mar = c(5,5,5,5))

# Diabetes
diabetes$pred_majority <- rowMeans(diabetes[,c('RF','NB')])
db_perf <- performance(prediction(diabetes, matrix(db$Test_set$Diabetes, nrow = length(db$Test_set$Diabetes), ncol = 4)), 'fpr', 'tpr')
plot(db_perf, col = as.list(1:4), main = 'Diabetes Models', xlab = 'False Positive Rate', ylab = 'True Positive Rate')
legend(x = "bottomright", legend = c("Random Forest", "GLM Net", "Naive Bayes", "Output Ensemble"), fill = 1:4)

# Stroke - Outputs are too highly correlated for ensemble 
strk_perf <- performance(prediction(stroke, matrix(strk$Test_set$Stroke, nrow = length(strk$Test_set$Stroke), ncol = 3)), 'fpr', 'tpr')
plot(strk_perf, col = as.list(1:3), main = "Stroke Models", xlab = 'False Positive Rate', ylab = 'True Positive Rate')
legend(x = "bottomright", legend = c("Random Forest", "GLM Net", "Naive Bayes"), fill = 1:3)

# Depression
depression$pred_majority <- rowMeans(depression)
depression_perf <- performance(prediction(depression, matrix(dep$Test_set$Depression, nrow = length(dep$Test_set$Depression), ncol = 4)), 'fpr', 'tpr')
plot(depression_perf, col = as.list(1:4), main = 'Depression Models', xlab = 'False Positive Rate', ylab = 'True Positive Rate')
legend(x = "bottomright", legend = c("Random Forest", "GLM Net", "Naive Bayes", "Output Ensemble"), fill = 1:4)


# Find models' optimal cutoff
db_cutoff <- performance(prediction(diabetes$RF, labels = db$Test_set$Diabetes), 'fpr', 'tpr')
plot(db_cutoff, colorize = TRUE, main = 'Diabetes RF Cutoffs', xlab = 'False Positive Rate', ylab = 'True Positive Rate')

strk_cutoff <- performance(prediction(stroke$RF, labels = strk$Test_set$Stroke), 'fpr', 'tpr')
plot(strk_cutoff, colorize = TRUE, main = 'Stroke RF Cutoffs', xlab = 'False Positive Rate', ylab = 'True Positive Rate')

dep_cutoff <- performance(prediction(depression$RF, labels = dep$Test_set$Depression), 'fpr', 'tpr')
plot(dep_cutoff, colorize = TRUE, main = 'Depression RF Cutoffs', xlab = 'False Positive Rate', ylab = 'True Positive Rate')

confusionMatrix(ifelse(diabetes$RF > 0.6, 1, 2), db$Test_set$Diabetes)
confusionMatrix(ifelse(stroke$RF > 0.6, 1, 2), strk$Test_set$Stroke)
confusionMatrix(ifelse(depression$RF > 0.6, 1, 2), dep$Test_set$Depression)

