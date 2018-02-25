library(sparklyr)
load('intermediate_saves/clean_years_final.RData')

registerDoParallel(makeCluster(detectCores()-3))
system.time(
  glm.fit <- train(CVDINFR4.2 ~ .,
                   data = clean_years_final,
                   method = 'glm',
                   trControl = trainControl(sampling = 'down'))
)
save(RF, file = 'intermediate_saves/myo_inf.RData')
registerDoSEQ()
