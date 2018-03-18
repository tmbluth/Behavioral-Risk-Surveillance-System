library(caret)
library(pROC)
library(ranger)
library(dplyr)
library(doParallel)
library(klaR)

# Load in custom function used later (downsample_part)
source('analysis/funcs.r')
# Load in cleaned data
load('intermediate_saves/clean_years_final.RData')
# Remove year since it should not de a determinate in future predictions
clean_years_final <- dplyr::select(clean_years_final, -year)

# This custom function partitions and downsamples data. There is also a 'upsample_part' function in funcs.R
db <- downsample_part(df = clean_years_final, target = 'DIABETE3', part.pct = 0.8)
strk <- downsample_part(clean_years_final, target = 'CVDSTRK3', 0.8)
dep <- downsample_part(clean_years_final, target = 'ADDEPEV2', 0.8)

#==================== Variable Importance ======================#

# Use 75% of cores available for faster parallel computing
registerDoParallel(makeCluster(round(detectCores()*0.75,0)))

# Feature/variable Importance must be found to see if any variables can be excluded for a shorter survey in the end
# Models with mtry that is the rounded square root of the number of columns will be assessed with this measure

vi_db  <- ranger(DIABETE3 ~ .,
                   data = db$Train_set,
                   mtry = round(sqrt(ncol(db$Train_set)),0),
                   splitrule = 'gini',
                   importance = 'permutation')$variable.importance

vi_strk <- ranger(CVDSTRK3 ~ .,
                    data = strk$Train_set,
                    mtry = round(sqrt(ncol(strk$Train_set)),0),
                    splitrule = 'gini',
                    importance = 'permutation')$variable.importance

vi_dep <- ranger(ADDEPEV2 ~ .,
                    data = dep$Train_set,
                    mtry = round(sqrt(ncol(dep$Train_set)),0),
                    splitrule = 'gini',
                    importance = 'permutation')$variable.importance

save(vi_db, vi_strk, vi_dep, file = 'intermediate_saves/vi.RData')

par(mar = c(10,0,5,1))
barplot(sort(vi_db$variable.importance, decreasing = T), axes = F, las = 2, main = 'Diabetes Variables of Importance')
barplot(sort(vi_strk$variable.importance, decreasing = T), axes = F, las = 2, main = 'Stroke Variables of Importance')
barplot(sort(vi_dep$variable.importance, decreasing = T), axes = F, las = 2, main = 'Depression Variables of Importance')

# Top ~20 variables in prediction importance across all 3 models are used
# Diabetes
db <- map(db, function(x)
  dplyr::select(x, 
         DIABETE3, GENHLTH, X_RFHYPE5, X_BMI5CAT, X_RFCHOL, EMPLOYMENT, 
         CHECKUP1, USEEQUIP, CVDINFR4, RACE, PHYSHLTH, 
         CVDCRHD4, SEX, EDUCA, PERSDOC2, HAVARTH3,
         INCOME2, X_RFBING5, PA_BENCHMARK, LIMITED, STRFREQ_))
    
# Stroke
strk <- map(strk, function(x)
  dplyr::select(x, 
         CVDSTRK3, EMPLOYMENT, CVDINFR4, X_AGE_G, GENHLTH, CVDCRHD4, 
         X_RFHYPE5, USEEQUIP, LIMITED, PHYSHLTH, HAVARTH3,
         DIABETE3, INCOME2, MARITAL, X_RFCHOL, CHCCOPD,
         EDUCA, PA_BENCHMARK, X_SMOKER3, RENTHOM1, ADDEPEV2))

# Depression
dep <- map(dep, function(x) 
  dplyr::select(x, 
          ADDEPEV2, MENTHLTH, LIMITED, EMPLOYMENT, X_AGE_G, SEX,
          GENHLTH, HAVARTH3, X_SMOKER3, MEDCOST, MARITAL,
          PHYSHLTH, RACE, X_RFCHOL, USEEQUIP, ASTHMA3,
          RENTHOM1, CHCCOPD, INCOME2, X_BMI5CAT, X_RFHYPE5,
          PA_BENCHMARK, PERSDOC2))

save(db, strk, dep, file = 'intermediate_saves/targets.RData')

#==================== Random Forest (Ranger) Models: ======================#

# 1. Myocardial Infarction (heart attack)
rf_db_3  <- ranger(DIABETE3 ~ .,
                   data = db$Train_set,
                   mtry = 3,
                   splitrule = 'gini',
                   probability = TRUE)
rf_db_3$prediction.error # This checks out-of-bag model error. 1 - error = accuracy

rf_db_6  <- ranger(DIABETE3 ~ .,
                   data = db$Train_set,
                   mtry = round(sqrt(ncol(db$Train_set)),0),
                   splitrule = 'gini',
                   probability = TRUE)
rf_db_6$prediction.error

rf_db_15 <- ranger(DIABETE3 ~ .,
                   data = db$Train_set,
                   mtry = 15,
                   splitrule = 'gini',
                   probability = TRUE)
rf_db_15$prediction.error

# 2. Stroke 
rf_strk_3  <- ranger(CVDSTRK3 ~ .,
                     data = strk$Train_set,
                     mtry = 3,
                     splitrule = 'gini',
                     probability = TRUE)
rf_strk_3$prediction.error

system.time(
rf_strk_6 <- ranger(CVDSTRK3 ~ .,
                    data = strk$Train_set,
                    mtry = round(sqrt(ncol(strk$Train_set)),0),
                    splitrule = 'gini',
                    probability = TRUE)
)
rf_strk_6$prediction.error

rf_strk_15 <- ranger(CVDSTRK3 ~ .,
                     data = strk$Train_set,
                     mtry = 15,
                     splitrule = 'gini',
                     probability = TRUE) 
rf_strk_15$prediction.error

# 3. Depression
rf_dep_3  <- ranger(ADDEPEV2 ~ .,
                    data = dep$Train_set,
                    mtry = 3,
                    splitrule = 'gini',
                    probability = TRUE)
rf_dep_3$prediction.error

rf_dep_6  <- ranger(ADDEPEV2 ~ .,
                    data = dep$Train_set,
                    mtry = round(sqrt(ncol(dep$Train_set)),0),
                    splitrule = 'gini',
                    probability = TRUE)
rf_dep_6$prediction.error

rf_dep_15 <- ranger(ADDEPEV2 ~ .,
                    data = dep$Train_set,
                    mtry = 15,
                    splitrule = 'gini',
                    probability = TRUE)
rf_dep_15$prediction.error

# Only the highest performing models will be saved.
save(rf_db_3, rf_strk_3, rf_dep_3, file = 'intermediate_saves/rf_models.Rdata')

# Predict RF models
rf_db_p <- predict(rf_db_3, db$Test_set)$predictions[,1]
auc(db$Test_set$DIABETE3, rf_db_p)

rf_strk_p <- predict(rf_strk_3, strk$Test_set)$predictions[,1]
auc(strk$Test_set$CVDSTRK3, rf_strk_p)

rf_dep_p <- predict(rf_dep_3, dep$Test_set)$predictions[,1]
auc(dep$Test_set$ADDEPEV2, rf_dep_p)

save(rf_db_p, rf_strk_p, rf_dep_p, file = 'intermediate_saves/rf_p.RData')

rm(rf_db_3, rf_db_6, rf_db_15, rf_db_p,
   rf_strk_3, rf_strk_6, rf_strk_15, rf_strk_p,
   rf_dep_3, rf_dep_6, rf_dep_15, rf_dep_p)

#============================ GLMnet Models ================================#

ctrl <- trainControl(method = 'cv',
                     number = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

glm_tune <- expand.grid(alpha = 0.5, lambda = seq(0, 0.1, length = 10))

glm_db <- train(make.names(DIABETE3) ~ .,
                data = db$Train_set,
                method = 'glmnet',
                metric = 'ROC',
                family = 'binomial', 
                tuneGrid = glm_tune,
                trControl = ctrl)
glm_db$results

glm_strk <- train(make.names(CVDSTRK3) ~ .,
                  data = strk$Train_set,
                  method = 'glmnet',
                  metric = 'ROC',
                  family = 'binomial',
                  tuneGrid = glm_tune,
                  trControl = ctrl)
glm_strk$results

system.time(
glm_dep <- train(make.names(ADDEPEV2) ~ .,
                 data = dep$Train_set,
                 method = 'glmnet',
                 metric = 'ROC',
                 family = 'binomial',
                 tuneGrid = glm_tune,
                 trControl = ctrl)
)
glm_dep$results

save(glm_db, glm_strk, glm_dep, file = 'intermediate_saves/glm_models.RData')

glm_db_p <- predict(glm_db, db$Test_set, type = 'prob')$X1
glm_strk_p <- predict(glm_strk, strk$Test_set, type = 'prob')$X1
glm_dep_p <- predict(glm_dep, dep$Test_set, type = 'prob')$X1

save(glm_db_p, glm_strk_p, glm_dep_p, file = 'intermediate_saves/glm_p.RData')

rm(glm_db, glm_db_p, glm_strk, glm_strk_p, glm_dep, glm_dep_p, glm_tune, ctrl)

#=========================== Naive Bayes Models: ===========================#

nb_db <- NaiveBayes(x = dplyr::select(db$Train_set, -DIABETE3),
                    grouping = db$Train_set$DIABETE3,
                    fL = 0.5,
                    useKernel = TRUE,
                    adjust = 0.5)


system.time(
nb_strk <- NaiveBayes(x = dplyr::select(strk$Train_set, -CVDSTRK3),
                      grouping = strk$Train_set$CVDSTRK3,
                      fL = 1)
)

nb_dep <- NaiveBayes(x = dplyr::select(dep$Train_set, -ADDEPEV2),
                     grouping = dep$Train_set$ADDEPEV2,
                     fL = 1,
                     useKernel = TRUE)

save(nb_db, nb_strk, nb_dep, file = 'intermediate_saves/nb_models.RData')

# Predictions
nb_db_p <- predict(nb_db, db$Test_set, type = 'prob')
auc(db$Test_set$DIABETE3, nb_db_p$posterior[,1])

nb_strk_p <- predict(nb_strk, strk$Test_set, type = 'prob')
auc(strk$Test_set$CVDSTRK3, nb_strk_p$posterior[,1])

nb_dep_p <- predict(nb_dep, dep$Test_set, type = 'prob')
auc(dep$Test_set$ADDEPEV2, nb_dep_p$posterior[,1])

save(nb_db_p, nb_strk_p, nb_dep_p, file = 'intermediate_saves/nb_p.RData')

rm(nb_db, nb_db_p, nb_strk, nb_strk_p, nb_dep, nb_dep_p)

#============================ Ensemble Model ===============================#

library(corrplot)
library(ROCR)
library(caret)

# Load in predictions
load('intermediate_saves/targets.RData')
load('intermediate_saves/rf_p.RData')
load('intermediate_saves/glm_p.RData')
load('intermediate_saves/nb_p.RData')

# Combine predictions into one data frame: '1' is Yes, '2' is No
diabetes <- cbind(RF = rf_db_p, GLM = glm_db_p, NB = nb_db_p$posterior[,1]) %>% as.data.frame()
stroke <- cbind(RF = rf_strk_p, GLM = glm_strk_p, NB = nb_strk_p$posterior[,1]) %>% as.data.frame()
depression <- cbind(RF = rf_dep_p, GLM = glm_dep_p, NB = nb_dep_p$posterior[,1]) %>% as.data.frame()
# Lets see how correlated each set of predictions are. Lower is better since it indicates more diversity of model opinion
corrplot(cor(diabetes), method = "color", addCoef.col="grey", title = 'Diabetes Models', mar=c(0,0,2,0))
corrplot(cor(stroke), method = "color", addCoef.col="grey", title = 'Stroke Models', mar=c(0,0,2,0)) 
corrplot(cor(depression), method = "color", addCoef.col="grey", title = 'Depression Models', mar=c(0,0,2,0))

par(mar = c(5,5,5,5))

# Diabetes
diabetes$pred_majority <- rowMeans(diabetes[,c('RF','NB')])
db_perf <- performance(prediction(diabetes, matrix(db$Test_set$DIABETE3, nrow = length(db$Test_set$DIABETE3), ncol = 4)), 'fpr', 'tpr')
plot(db_perf, col = as.list(1:4), main = 'Diabetes Models', xlab = 'False Positive Rate', ylab = 'True Positive Rate')
legend(x = "bottomright", legend = c("Random Forest", "GLM Net", "Naive Bayes", "Output Ensemble"), fill = 1:4)

# Stroke - Outputs are too highly correlated for ensemble 
strk_perf <- performance(prediction(stroke, matrix(strk$Test_set$CVDSTRK3, nrow = length(strk$Test_set$CVDSTRK3), ncol = 3)), 'fpr', 'tpr')
plot(strk_perf, col = as.list(1:3), main = "Stroke Models", xlab = 'False Positive Rate', ylab = 'True Positive Rate')
legend(x = "bottomright", legend = c("Random Forest", "GLM Net", "Naive Bayes"), fill = 1:3)

# Depression
depression$pred_majority <- rowMeans(depression)
depression_perf <- performance(prediction(depression, matrix(dep$Test_set$ADDEPEV2, nrow = length(dep$Test_set$ADDEPEV2), ncol = 4)), 'fpr', 'tpr')
plot(depression_perf, col = as.list(1:4), main = 'Depression Models', xlab = 'False Positive Rate', ylab = 'True Positive Rate')
legend(x = "bottomright", legend = c("Random Forest", "GLM Net", "Naive Bayes", "Output Ensemble"), fill = 1:4)


# Find models' optimal cutoff
db_cutoff <- performance(prediction(diabetes$RF, labels = db$Test_set$DIABETE3), 'fpr', 'tpr')
plot(db_cutoff, colorize = TRUE, main = 'Diabetes RF Cutoffs', xlab = 'False Positive Rate', ylab = 'True Positive Rate')

strk_cutoff <- performance(prediction(stroke$RF, labels = strk$Test_set$CVDSTRK3), 'fpr', 'tpr')
plot(strk_cutoff, colorize = TRUE, main = 'Stroke RF Cutoffs', xlab = 'False Positive Rate', ylab = 'True Positive Rate')

dep_cutoff <- performance(prediction(depression$RF, labels = dep$Test_set$ADDEPEV2), 'fpr', 'tpr')
plot(dep_cutoff, colorize = TRUE, main = 'Depression RF Cutoffs', xlab = 'False Positive Rate', ylab = 'True Positive Rate')

confusionMatrix(ifelse(diabetes$RF > 0.6, 1, 2), db$Test_set$DIABETE3)
confusionMatrix(ifelse(stroke$RF > 0.6, 1, 2), strk$Test_set$DIABETE3)
confusionMatrix(ifelse(depression$RF > 0.6, 1, 2), dep$Test_set$ADDEPEV2)


#========================== USE THE MODELS ==================================#

# For possible responses check out lists below:
load('intermediate_saves/targets.RData')

# Diabetes Model Inputs
sapply(db$Test_set, levels)
# Stroke Model Inputs
sapply(strk$Test_set, levels)
# Depression Model Inputs
sapply(dep$Test_set, levels)

# The RF models take up 6GB of memory so be aware of your capacity
load('intermediate_saves/rf_models.RData')

# Enter your results. Questions (and possible responses) can be found in the 'app' folder in the file 'Q_cat.csv'
# App version will be much more user friendly when completed
predict(rf_db_3, data.frame(GENHLTH = '', X_RFHYPE5 = '', X_BMI5CAT = '', X_RFCHOL = '', EMPLOYMENT = '', 
                            CHECKUP1 = '', USEEQUIP = '', CVDINFR4 = '', RACE = '', PHYSHLTH = '', 
                            CVDCRHD4 = '', SEX = '', EDUCA = '', PERSDOC2 = '', HAVARTH3 = '',
                            INCOME2 = '', X_RFBING5 = '', PA_BENCHMARK = '', LIMITED = '', STRFREQ_ = ''), type = "prob")
predict(rf_strk_3, data.frame(EMPLOYMENT = '', CVDINFR4 = '', X_AGE_G = '', GENHLTH = '', CVDCRHD4 = '', 
                              X_RFHYPE5 = '' = '', USEEQUIP = '', LIMITED = '', PHYSHLTH = '' = '', HAVARTH3 = '',
                              DIABETE3 = '', INCOME2 = '', MARITAL = '', X_RFCHOL = '', CHCCOPD = '',
                              EDUCA = '', PA_BENCHMARK = '', X_SMOKER3 = '', RENTHOM1 = '', ADDEPEV2 = ''), type = "prob")
predict(rf_dep_3, data.frame(MENTHLTH = '', LIMITED = '', EMPLOYMENT = '', X_AGE_G = '', SEX = '',
                             GENHLTH = '', HAVARTH3 = '', X_SMOKER3 = '', MEDCOST = '', MARITAL = '',
                             PHYSHLTH = '', RACE = '', X_RFCHOL = '', USEEQUIP = '', ASTHMA3 = '',
                             RENTHOM1 = '', CHCCOPD = '', INCOME2 = '', X_BMI5CAT = '', X_RFHYPE5,
                             PA_BENCHMARK = '', PERSDOC2 = ''), type = "prob")

#=============================== SVM Models: ===============================#

" Future work using gputools' gpuSvmTrain:
load('intermediate_saves/SVM_data.RData')

svm_train <- filter(clean_years_final, year != '2015') 
svm_Test_set <- filter(clean_years_final, year == '2015') 

prop.table(table(svm_train$DIABETE3)); prop.table(table(svm_Test_set$DIABETE3))
prop.table(table(svm_train$CVDSTRK3)); prop.table(table(svm_Test_set$CVDSTRK3))
prop.table(table(svm_train$ADDEPEV2)); prop.table(table(svm_Test_set$ADDEPEV2))

svm_db <- train(DIABETE3 ~ .,
               data = svm_train,
               method = 'svmLinear',
               tuneGrid = data.frame(C = c(1, 10, 20)),
               trControl = trainControl(method = 'cv',
                                        number = 5,
                                        sampling = 'down'))

svm_roc <- roc(X)
coords(svm_roc, x = 'best', best.method = 'closest.topleft')
"




