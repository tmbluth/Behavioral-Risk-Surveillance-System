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

downsample_part <- function(df, target, p) {
  require(caret)
  down_data <- downSample(x = df[, names(df) != target], 
                          y = as.factor(df[, target]),
                          yname = target)
  train_part <- createDataPartition(down_data[, target], p = p, list = FALSE)
  Train <- down_data[train_part, ]
  Test <- down_data[-train_part, ]
  
  return(list(Train = Train, Test = Test))
}

#=====================================================================================================#

plot_lime <- function(Model, Train_set, Sample, title) {
  
  require(ggplot2)
  require(dplyr)
  #require(billboarder)
  
  explainer <- lime::lime(
    x = Train_set,
    model = Model,
    bin_continuous = FALSE
  )
  
  # Run explain() on explainer
  explanation <- lime::explain(
    Sample[1, , drop = FALSE], 
    explainer = explainer, 
    n_labels = 1, 
    n_features = ncol(Sample),
    kernel_width = 0.5
  )
  
  type_pal <- c('Contradicts', 'Supports') 
  explanation$type <- factor(ifelse(sign(explanation$feature_weight) == 
                                      -1, type_pal[1], type_pal[2]), levels = type_pal) 
  explanation_plot_df <- explanation %>%
    mutate(Churn_Predictor = case_when(
      (label == 'Diagnosed' & type == 'Supports') | (label == 'Undiagnosed' & type == 'Contradicts') ~ 'More likely',
      (label == 'Diagnosed' & type == 'Contradicts') | (label == 'Undiagnosed' & type == 'Supports') ~ 'Less likely')
    )  
  
  p1 <- ggplot(explanation_plot_df, aes(x = reorder(feature_desc, abs(feature_weight)), y = feature_weight)) +
    geom_bar(stat = "identity", aes(fill = Churn_Predictor)) + 
    scale_y_continuous('Feature Weight') +
    scale_x_discrete('') +
    ggtitle(title) +
    coord_flip()
  
  p1
  
  # p2 <- billboarder() %>%
  #   bb_barchart(
  #     data = explanation_plot_df,
  #     mapping = bbaes(x = feature_desc, y = feature_weight, group = churn_predictor),
  #     rotated = TRUE,
  #     stacked = TRUE
  #   ) 
  # 
  # p2
} 
