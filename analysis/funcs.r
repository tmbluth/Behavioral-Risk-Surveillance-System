# Custom functions used throughout analysis

make_num_NA <- function(x) {
  x <- ifelse(x %in% c(7, 9, 77, 99, 777, 999, 7777, 9999), NA, 
              ifelse(x %in% c(8, 88, 98, 888), 0, x))
  return(x)  
}


#=====================================================================================================#

diet_questions <- function(x){ 
  require(stringr)
  x <- ifelse(is.na(x), 999, x) 
  x <- ifelse(x < 200, as.numeric(str_trunc(x, width = 2, side = 'left', ellipsis = '')),
         ifelse(x < 301, round(as.numeric(str_trunc(x, width = 2, side = 'left', ellipsis = '')) / (30/7), 2), 
           ifelse(x < 400, round(as.numeric(str_trunc(x, width = 2, side = 'left', ellipsis = '')) / 30, 2),
             ifelse(x == 555, 0, NA))))  

  return(x)
}

#=====================================================================================================#

NA_prop <- function(df) {
  a <- df %>% map_chr(function(x) paste0(round(sum(is.na(x))/nrow(.)*100, 2), '% missing'))
  b <- df %>% map_int(function(x) sum(is.na(x)))
  NA_df <- data.frame(a, b)
  return(NA_df)
}

#=====================================================================================================#

# WORKS WELL BUT CANNOT COMPUTE PROBABILITY IN FUNCTION FORM
rf_tune <- function(.target, .data, .mtry, .num.trees, .importance = 'none'){
  train_downsample <- downSample(x = .data[, names(.data) != .target], 
                                 y = .data[, .target],
                                 yname = .target)
  
  model <- ranger(as.formula(paste(.target, '~ .')),
                  data = droplevels(train_downsample),                     
                  mtry = .mtry,
                  num.trees = .num.trees,
                  importance = .importance,
                  min.node.size = 100,
                  splitrule = 'gini', 
                  probability = TRUE)
  return(model)
}

#=====================================================================================================#

downsample_part <- function(df, target, part.pct) {
require(caret)
  part <- createDataPartition(df[, target], p = part.pct, list = FALSE)
  Train <- df[part, ]
  Train <- downSample(x = Train[, names(Train) != target], 
                         y = Train[, target],
                         yname = target)
  Test <- df[-part, ]
  Test <- downSample(x = Test[, names(Test) != target],
                        y = Test[, target],
                        yname = target)
  return(list(Train_set = Train, Test_set = Test))
}

upsample_part <- function(df, target, part.pct) {
  require(caret)
  part <- createDataPartition(df[, target], p = part.pct, list = FALSE)
  Train <- df[part, ]
  Train <- upSample(x = Train[, names(Train) != target], 
                      y = Train[, target],
                      yname = target)
  Test <- df[-part, ]
  Test <- upSample(x = Test[, names(Test) != target],
                     y = Test[, target],
                     yname = target)
  return(list(Train_set = Train, Test_set = Test))
}
