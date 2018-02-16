load('intermediate_saves/clean_years2.RData')
# Make NA's into a factor level
clean_years_NA <- for(col in seq_along(clean_years2)){
  if(is.factor(clean_years2[,col]) | is.ordered(clean_years2[,col])) {
    map(clean_years2, addNA, ifany = TRUE) %>% as.data.frame()
  }
}