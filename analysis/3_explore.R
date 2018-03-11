library(forcats)
library(purrr)
library(dplyr)
library(ggplot2)

load('intermediate_saves/clean_years_final.RData')

# EDA is a bit less exciting visually when dealing with mainly categorical variables, but the numbers (and some graphs) can tell us a lot quickly

# Check distributions of all variables
map(clean_years_final, table, useNA = 'ifany')

#------------------------------------ Hypotheses ------------------------------------#

# 1. High BMI increases likeihood of having had a diabetes or stroke
(db_bmi <- table(clean_years_final$DIABETE3, clean_years_final$BMI, useNA = 'ifany'))
prop.table(db_bmi)
chisq.test(db_bmi) # Significant differences between expected proportion of stoke and actual proportion

(strk_bmi <- table(clean_years_final$CVDSTRK3, clean_years_final$BMI, useNA = 'ifany'))
prop.table(strk_bmi)
chisq.test(strk_bmi) # Significant differences between expected proportion of stoke and actual proportion

#2. Having good physical health leads to lower instances of diabetes or stroke
count(clean_years_final, DIABETE3, PHYSHLTH) %>% 
  rename(Count = n) %>% 
  ggplot(aes(x = DIABETE3, y = PHYSHLTH, fill = Count)) + 
  geom_tile() + 
  scale_x_discrete(labels = c('Has Diabetes', 'No Diabetes')) +
  labs(x = '', y = 'Past 30 Days of Poor Physical Health', title = 'Diabetes vs Days of Poor Physical Health')

ggplot(clean_years_final, aes(x = ADDEPEV2, y = MENTHLTH, color = ADDEPEV2)) + 
  geom_boxplot(show.legend = FALSE) + 
  scale_x_discrete(labels = c('Has Had Stroke', 'Never Had Stroke')) +
  labs(x = '', y = 'Past 30 Days of Poor Physical Health', title = 'Stroke vs Days of Poor Physical Health')

# 3. Number of days per month with mental struggles should be indicitive of having been depressed with mental illness
ggplot(clean_years_final, aes(x = ADDEPEV2, y = MENTHLTH, color = ADDEPEV2)) + 
  geom_boxplot(show.legend = FALSE) + 
  scale_x_discrete(labels = c('Diagnosed', 'Undiagnosed')) +
  labs(x = '', y = 'Past 30 Days of Poor Mental Health', title = 'Depression Diagnosis via Days of Poor Mental Health')

# 3. Being limited mentally, physically, or in some other manner may lead to depression
count(clean_years_final, ADDEPEV2, LIMITED) %>% 
  rename(Count = n) %>% 
  ggplot(aes(x = ADDEPEV2, y = LIMITED, fill = Count)) + 
  geom_tile() + 
  scale_x_discrete(labels = c('Diagnosed', 'Undiagnosed')) + 
  scale_y_discrete(labels = c('Yes', 'No', 'Missing/Refused')) +
  labs(x = '', y = 'Degree of Limitation', title = 'Depression Diagnosis due to Limitations')
