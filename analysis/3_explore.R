library(forcats)
library(purrr)
library(dplyr)
library(ggplot2)
library(pdp)

clean_years_final <- read_rds('intermediate_saves/clean_years_final.rds')

# EDA is a bit less exciting visually when dealing with mainly categorical variables, but the numbers (and some graphs) can tell us a lot quickly

# Check distributions of all variables
map(clean_years_final, function(x) round(prop.table(table(x, clean_years_final$Diabetes, useNA = 'ifany'), 2),3))
map(clean_years_final, function(x) round(prop.table(table(x, clean_years_final$Stroke, useNA = 'ifany'), 2),3))
map(clean_years_final, function(x) round(prop.table(table(x, clean_years_final$Depression, useNA = 'ifany'), 2),3))

#------------------------------------ Hypotheses ------------------------------------#


#1. Having good physical health leads to lower instances of diabetes or stroke
count(clean_years_final, Diabetes, Physical_Health) %>% 
  rename(Count = n) %>% 
  ggplot(aes(x = Diabetes, y = Physical_Health, fill = Count)) + 
  geom_tile() + 
  scale_x_discrete(labels = c('Has Diabetes', 'No Diabetes')) +
  labs(x = '', y = 'Past 30 Days of Poor Physical Health', title = 'Diabetes vs Days of Poor Physical Health')

ggplot(clean_years_final, aes(x = Depression, y = Mental_Health, color = Depression)) + 
  geom_boxplot(show.legend = FALSE) + 
  scale_x_discrete(labels = c('Has Had Stroke', 'Never Had Stroke')) +
  labs(x = '', y = 'Past 30 Days of Poor Physical Health', title = 'Stroke vs Days of Poor Physical Health')

# 2. Number of days per month with mental struggles should be indicitive of having been diagnosed with depression
ggplot(clean_years_final, aes(x = Depression, y = Mental_Health, color = Depression)) + 
  geom_boxplot(show.legend = FALSE) + 
  scale_x_discrete(labels = c('Diagnosed', 'Undiagnosed')) +
  labs(x = '', y = 'Past 30 Days of Poor Mental Health', title = 'Depression Diagnosis via Days of Poor Mental Health')

# 3. Being limited mentally, physically, or in some other manner may lead to depression
count(clean_years_final, Depression, Limited) %>% 
  rename(Count = n) %>% 
  ggplot(aes(x = Depression, y = Limited, fill = Count)) + 
  geom_tile() + 
  scale_x_discrete(labels = c('Diagnosed', 'Undiagnosed')) + 
  scale_y_discrete(labels = c('Yes', 'No', 'Missing/Refused')) +
  labs(x = '', y = 'Degree of Limitation', title = 'Depression Diagnosis due to Limitations')

