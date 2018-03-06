# Custom functions used throughout analysis

# UNUSED   
# make_NA <- function(x) {  
#   if(is.factor(x)) {
#     levels(x) <- ifelse(levels(x) == 'Don't Know/Refused' | # If the levels of this factor are any of these things..
#                       levels(x) == 'Dont Know/Refused' | 
#                       levels(x) == 'Don't Know' | 
#                       levels(x) == 'Dont Know' |
#                       levels(x) == 'Refused' | 
#                       levels(x) == 'Not Applicable' |
#                       levels(x) == 'Don't Know/Not Applicable' |
#                       levels(x) == 'Dont Know/Not Applicable' |
#                       levels(x) == 'Did Not Have Appt' |
#                       levels(x) == 'No Experience' |
#                       levels(x) == 'Did Not Have Appt/Not Applicable' |
#                       levels(x) == 'Did Not Have a Primary Care Visit' |
#                       levels(x) == 'Did Not See a Specialist', NA, levels(x)) # ..make them NA. Otherwise leave them
#   return(x)
#     } else if(is.character(x)) {
#     x <- ifelse(x == 'Don't Know/Refused' |           # If any of the character strings are any of these things..
#                 x == 'Dont Know/Refused' |
#                 x == 'Don't Know' | 
#                 x == 'Dont Know' |
#                 x == 'Refused' | 
#                 x == 'Not Applicable' |
#                 x == 'Don't Know/Not Applicable' |
#                 x == 'Did Not Have Appt' |
#                 x == 'No Experience' |
#                 x == 'Did Not Have Appt/Not Applicable' |
#                 x == 'Did Not Have a Primary Care Visit' |
#                 x == 'Did Not See a Specialist', NA, x) # ..make them NA. Otherwise leave them
#   return(x)
#   } else { 
#     x <- ifelse(x %in% c(7, 9, 77, 99, 777, 999, 7777, 9999), NA, 
#                 ifelse(x %in% c(8, 88, 98, 888), 0, x))
#     return(x)  
#   } 
# }


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
  print(df %>% map_chr(function(x) paste0(round(sum(is.na(x))/nrow(.)*100, 2), '% missing')))
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
