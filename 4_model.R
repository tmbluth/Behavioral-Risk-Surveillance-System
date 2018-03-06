library(caret)
library(ranger)
library(dplyr)
library(doParallel)

source('analysis/funcs.r')
load('intermediate_saves/clean_years_final.RData')

Train <- filter(clean_years_final, year != '2015') %>% dplyr::select(-X_STATE, -year)
Test <- filter(clean_years_final, year == '2015') %>% dplyr::select(-X_STATE, -year)

# Test to see if training set has similar distribution to test set
prop.table(table(Train$CVDINFR4)); prop.table(table(Test$CVDINFR4))
prop.table(table(Train$CVDSTRK3)); prop.table(table(Test$CVDSTRK3))
prop.table(table(Train$ADDEPEV2)); prop.table(table(Test$ADDEPEV2))


#==================== Random Forest (Ranger) Models: ======================#

registerDoParallel(makeCluster(detectCores()-4))

# Feature Importance must be found to see if any variables can be excluded across the board.
# Models with mtry that is the rounded square root of the number of columns will be assessed with this measure

# 1. Myocardial Infarction (heart attack)
rf_mi_3  <- ranger(CVDINFR4 ~ .,
                   data = Train,
                   mtry = 3,
                   splitrule = 'gini',
                   probability = TRUE)
confusionMatrix(rf_mi_3$confusion.matrix)

rf_mi_6  <- ranger(CVDINFR4 ~ .,
                   data = Train,
                   mtry = round(sqrt(ncol(Train)),0),
                   splitrule = 'gini',
                   probability = TRUE, 
                   importance = 'permutation')
confusionMatrix(rf_mi_6$confusion.matrix)

rf_mi_10 <- ranger(CVDINFR4 ~ .,
                   data = Train,
                   mtry = 10,
                   splitrule = 'gini',
                   probability = TRUE)
confusionMatrix(rf_mi_10$confusion.matrix) 

# 2. Stroke 
rf_strk_3  <- ranger(CVDSTRK3 ~ .,
                     data = Train,
                     mtry = 3,
                     splitrule = 'gini',
                     probability = TRUE)
confusionMatrix(rf_strk_3$confusion.matrix) 

rf_strk_6 <- ranger(CVDSTRK3 ~ .,
                    data = Train,
                    mtry = round(sqrt(ncol(Train)),0),
                    splitrule = 'gini',
                    probability = TRUE, 
                    importance = 'permutation')
confusionMatrix(rf_strk_6$confusion.matrix)

rf_strk_10 <- ranger(CVDSTRK3 ~ .,
                     data = Train,
                     mtry = 3,
                     splitrule = 'gini',
                     probability = TRUE) 
confusionMatrix(rf_strk_10$confusion.matrix)

# 3. Depression/Anxiety
rf_dep_3  <- ranger(ADDEPEV2 ~ .,
                    data = Train,
                    mtry = 3,
                    splitrule = 'gini',
                    probability = TRUE)
confusionMatrix(rf_dep_3$confusion.matrix)

rf_dep_6  <- ranger(ADDEPEV2 ~ .,
                    data = Train,
                    mtry = round(sqrt(ncol(Train)),0),
                    splitrule = 'gini',
                    probability = TRUE, 
                    importance = 'permutation')
confusionMatrix(rf_dep_6$confusion.matrix)

rf_dep_10 <- ranger(ADDEPEV2 ~ .,
                    data = Train,
                    mtry = 3,
                    splitrule = 'gini',
                    probability = TRUE)
confusionMatrix(rf_dep_10$confusion.matrix)

# Looks like HLTHPLN1, CHCOCNCR, and CHCSCNCR were in the bottom 10 in prediction importance across all 3 models
# Only the highest performing models will be saved.
save(rf_mi_6, rf_strk_3, rf_dep_3, file = 'intermediate_saves/rf_models.Rdata')

# Predict RF models
rf_mi_p <- predict(rf_mi_6, Test) 
confusionMatrix(rf_mi_p$predictions, Test$CVDINFR4) 

rf_strk_p <- predict(rf_strk_3, Test) 
confusionMatrix(rf_strk_p$predictions, Test$CVDSTRK3) 

rf_dep_p <- predict(rf_dep_3, Test) 
confusionMatrix(rf_dep_p$predictions, Test$CVDCRHD4) 

save(rf_mi_p, rf_strk_p, rf_dep_p, file = 'intermediate_saves/rf.Rdata')


#============================ GLMnet Models ================================#

ctrl <- trainControl(method = 'cv',
                     number = 5,
                     sampling = 'down',
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

glm_tune <- expand.grid(alpha = 0.5, lambda = seq(0, 0.1, length = 10))

glm_mi <- train(make.names(CVDINFR4) ~ .,
                data = Train,
                method = 'glmnet',
                metric = 'ROC', family = 'binomial', 
                tuneGrid = glm_tune,
                trControl = ctrl)

glm_strk <- train(make.names(CVDSTRK3) ~ .,
                  data = Train,
                  method = 'glmnet',
                  metric = 'ROC',
                  family = 'binomial',
                  tuneGrid = glm_tune,
                  trControl = ctrl)

system.time(
glm_dep <- train(make.names(ADDEPEV2) ~ .,
                 data = Train,
                 method = 'glmnet',
                 metric = 'ROC',
                 family = 'binomial',
                 tuneGrid = glm_tune,
                 trControl = ctrl)
)

save(glm_mi, glm_strk, glm_dep, file = 'intermediate_saves/glm_models.RData')

glm_mi_p <- predict(glm_mi, Test) 
glm_strk_p <- predict(glm_strk, Test) 
glm_dep_p <- predict(glm_dep, Test) 

prop.table(table(glm_mi_p, Test$CVDINFR4))
prop.table(table(glm_strk_p, Test$CVDSTRK3))
prop.table(table(glm_dep_p, Test$ADDEPEV2))

save(glm_mi_p, glm_strk_p, glm_dep_p, file = 'intermediate_saves/glm.RData')

#=========================== Naive Bayes Models: ===========================#

registerDoSEQ()

nb_tune <- data.frame(fL= 0.1, usekernel = TRUE, adjust = 0.1)

system.time(
nb_mi <- train(make.names(CVDINFR4) ~ .,
               data = Train,
               method = 'nb',
               metric = 'ROC',
               tuneGrid = nb_tune,
               trControl = ctrl)
)

nb_strk <- train(make.names(CVDSTRK3) ~ .,
                 data = Train,
                 method = 'nb',
                 metric = 'ROC',
                 tuneGrid = nb_tune,
                 trControl = ctrl)

nb_dep <- train(make.names(ADDEPEV2) ~ .,
                data = Train,
                method = 'nb',
                metric = 'ROC',
                tuneGrid = nb_tune,
                trControl = ctrl)

save(nb_mi, nb_hd, nb_strk, nb_dep, file = 'intermediate_saves/nb_models.RData')

# Predictions
nb_mi_p <- predict(nb_mi, Test)
prop.table(table(nb_mi_p, Test$CVDINFR4))

nb_strk_p <- predict(nb_strk, Test)
prop.table(table(nb_mi_p, Test$CVDSTRK3))

nb_dep_p <- predict(nb_dep, Test)
prop.table(table(nb_mi_p, Test$ADDEPEV2))

save(nb_mi_p, nb_strk_p, nb_dep_p, file = 'intermediate_saves/nb.RData')


#============================ Ensemble Model ===============================#

load('intermediate_saves/rf.RData')
load('intermediate_saves/glm.RData')
load('intermediate_saves/nb.RData')

myo_inf <- cbind(rf_p = rf_mi_p$predictions, glm_p = glm_mi_p, nb_p = nb_mi_p) %>% as.data.frame()
stroke <- cbind(rf_p = rf_strk_p$predictions, glm_p = glm_strk_p, nb_p = nb_strk_p) %>% as.data.frame()
dep <- cbind(rf_p = rf_dep_p$predictions, glm_p = glm_dep_p, nb_p = nb_dep_p) %>% as.data.frame()

# FIX PRED MAJORITY
myo_inf$pred_majority<-as.factor(ifelse(myo_inf$rf_p==1 & myo_inf$nb_p==1,'1',
                                        ifelse(myo_inf$rf_p==1 & myo_inf$glm_p==1,'1',
                                               ifelse(myo_inf$nb_p==1 & myo_inf$glm_p==1,'1','2'))))
confusionMatrix(myo_inf$pred_majority, Test$CVDINFR4)

stroke$pred_majority<-as.factor(ifelse(stroke$rf_p==1 & stroke$nb_p==1,'1',
                                       ifelse(stroke$rf_p==1 & stroke$glm_p==1,'1',
                                              ifelse(stroke$nb_p==1 & stroke$glm_p==1,'1','2'))))
confusionMatrix(stroke$pred_majority, Test$CVDSTRK3)

dep$pred_majority<-as.factor(ifelse(dep$rf_p==1 & dep$nb_p==1,'1',
                                    ifelse(dep$rf_p==1 & dep$glm_p==1,'1',
                                           ifelse(dep$nb_p==1 & dep$glm_p==1,'1','2'))))
confusionMatrix(dep$pred_majority, Test$ADDEPEV2)


#=============================== SVM Models: ===============================#
" Future work using gputools' gpuSvmTrain:
load('intermediate_saves/SVM_data.RData')

svm_train <- filter(clean_years_final, year != '2015') 
svm_test <- filter(clean_years_final, year == '2015') 

prop.table(table(svm_train$CVDINFR4)); prop.table(table(svm_test$CVDINFR4))
prop.table(table(svm_train$CVDCRHD4)); prop.table(table(svm_test$CVDCRHD4))
prop.table(table(svm_train$CVDSTRK3)); prop.table(table(svm_test$CVDSTRK3))
prop.table(table(svm_train$ADDEPEV2)); prop.table(table(svm_test$ADDEPEV2))

svm_mi <- train(CVDINFR4 ~ .,
               data = svm_train,
               method = 'svmLinear',
               tuneGrid = data.frame(C = c(1, 10, 20)),
               trControl = trainControl(method = 'cv',
                                        number = 5,
                                        sampling = 'down'))
"




