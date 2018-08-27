library(tidyverse)
library(caret)
source('analysis/funcs.r')

all_years <- read_rds('intermediate_saves/all_years.rds')

# View all unique responses:
# for single digit responses 9 = Refused and 7 = Don't Know
# For double digit responses 99 = Refused, 98 or 88 = None/0, 97 or 77 = Don't Know
# For triple digit responses 999 = Refused, 888 = None/0, 777 = Don't Know
lapply(all_years, unique)

# Variables with values that 'make_num_NA' will mistake as NA: CHILDREN (77), EMPLOYMENT (7 = retired), SCNTWRK1 (98, 97)
edge_case_vars <- all_years %>% 
  transmute(EMPLOYMENT= ifelse(EMPLOYMENT == 9, NA, EMPLOYMENT), 
            CHILDREN  = ifelse(CHILDREN == 99, NA, 
                        ifelse(CHILDREN == 88, 0, CHILDREN)),
            STRFREQ_  = ifelse(STRFREQ_ == 99, NA, STRFREQ_))

# Change numeric placeholders into correct meaning (make_num_NA)
clean_years <- all_years %>% 
  dplyr::select(-CHILDREN, -EMPLOYMENT, -STRFREQ_) %>% 
  map(make_num_NA) %>% 
  bind_cols(edge_case_vars);  rm(edge_case_vars, all_years)

NA_prop(clean_years)

# List of integer / ordered vars:
numerics <- c('PHYSHLTH', 'MENTHLTH', 'CHILDREN', 'STRFREQ_', 'FTJUDA1_',
              'FRUTDA1_', 'BEANDAY_', 'GRENDAY_', 'ORNGDAY_', 'VEGEDA1_')

# List of factor vars:
factors <- c('GENHLTH', 'HLTHPLN1', 'PERSDOC2', 'MEDCOST',  'CVDINFR4',  'CVDCRHD4',   
             'CVDSTRK3', 'ASTHMA3',  'CHCSCNCR', 'CHCOCNCR',   'CHCCOPD',
             'HAVARTH3', 'ADDEPEV2', 'CHCKIDNY', 'DIABETE3',   'SEX',
             'MARITAL',  'RENTHOM1', 'VETERAN3', 'EMPLOYMENT', 'LIMITED',
             'USEEQUIP', 'RACE',     '_RFHYPE5', '_RFCHOL',    '_RFBING5', 
             '_SMOKER3', 'PREGNANT', 'PA_BENCHMARK', 'CHECKUP1', 'year',
             '_AGE_G', 'EDUCA', 'INCOME2', '_BMI5CAT')

# Check to see if variable groupings are equal to number of columns
setdiff(c(numerics, factors), names(clean_years)); setdiff(names(clean_years), c(numerics, factors))

clean_years2 <- clean_years %>% 
  # Keep continuous variables as numeric
  mutate_if(names(.) %in% numerics, as.numeric) %>% 
  # Change questions with classes into factors
  mutate_if(names(.) %in% factors, as.factor)

NA_prop(clean_years2)

# Take out the outcome variable missing values
clean_years3 <- clean_years2 %>% 
  filter(!is.na(DIABETE3),
         !is.na(CVDSTRK3),
         !is.na(ADDEPEV2)
         )

# Remove high NA variables (above 10% missing), make remaining NA's into a factor level, remove remaining NA's
clean_years_NA <- clean_years3 %>% 
  dplyr::select(-INCOME2, -PA_BENCHMARK, -`_RFCHOL`, -CHECKUP1) %>% 
  map_if(.p = is.factor, .f = function(x) ifelse(is.na(x), 'Missing', x)) %>% 
  as.data.frame() %>% 
  na.omit()

rm(clean_years, clean_years2, clean_years3)

NA_prop(clean_years_NA)
write_rds(clean_years_NA, 'intermediate_saves/clean_years_NA.rds')

# Feature/variable Importance must be found to see if any variables can be excluded for a shorter survey in the end
# Models with mtry that is the rounded square root of the number of columns will be assessed with this measure
mtry <- round(sqrt(ncol(clean_years_NA)),0)
survey_sample <- sample_frac(clean_years_NA, 0.25)

vi_db  <- ranger(as.factor(DIABETE3) ~ ., 
                 data = survey_sample,
                 mtry = mtry,
                 num.trees = 200,
                 splitrule = 'gini',
                 importance = 'permutation')

vi_strk <- ranger(as.factor(CVDSTRK3) ~ .,
                  data = survey_sample,
                  mtry = mtry,
                  num.trees = 200,
                  splitrule = 'gini',
                  importance = 'permutation')

vi_dep <- ranger(as.factor(ADDEPEV2) ~ .,
                 data = survey_sample,
                 mtry = mtry,
                 num.trees = 200,
                 splitrule = 'gini',
                 importance = 'permutation')

save(vi_db, vi_strk, vi_dep, file = 'intermediate_saves/vi_permutation.RData')

par(mar = c(10,5,5,1))
barplot(sort(vi_db$variable.importance ,  decreasing = T), las = 2, main = 'Diabetes Variables of Importance')
barplot(sort(vi_strk$variable.importance, decreasing = T), las = 2, main = 'Stroke Variables of Importance')
barplot(sort(vi_dep$variable.importance, decreasing = T), las = 2, main = 'Depression Variables of Importance')

clean_years_final <- transmute(clean_years_NA,
                            Diabetes = factor(DIABETE3,
                                              labels = c('Diagnosed', 'Undiagnosed'),
                                              levels = c('1', '2')),
                            Stroke = factor(CVDSTRK3,
                                            labels = c('Diagnosed', 'Undiagnosed'),
                                            levels = c('1', '2')),
                            Depression = factor(ADDEPEV2,
                                                labels = c('Diagnosed', 'Undiagnosed'),
                                                levels = c('1', '2')),
                            Medical_Cost = factor(MEDCOST,
                                             labels = c('Yes', 'No', 'Missing'), 
                                             levels = c('1', '2', 'Missing')),
                            Heart_Attack = factor(CVDINFR4, 
                                              labels = c('Yes', 'No', 'Missing'), 
                                              levels = c('1', '2', 'Missing')),
                            Heart_Disease = factor(CVDCRHD4,
                                              labels = c('Yes', 'No', 'Missing'), 
                                              levels = c('1', '2', 'Missing')),
                            COPD = factor(CHCCOPD,
                                             labels = c('Yes', 'No', 'Missing'), 
                                             levels = c('1', '2', 'Missing')),
                            Arthritis = factor(HAVARTH3,
                                              labels = c('Yes', 'No', 'Missing'), 
                                              levels = c('1', '2', 'Missing')),
                            Hypertension = factor(X_RFHYPE5,
                                               labels = c('Yes', 'No', 'Missing'), 
                                               levels = c('2', '1', 'Missing')),
                            Smoker = factor(X_SMOKER3,
                                               labels = c('Yes, every day', 'Yes, sometimes', 'Formerly (100+ cigarettes in lifetime)', 'No', 'Missing'), 
                                               levels = c('1', '2', '3', '4', 'Missing')),
                            Limited = factor(LIMITED,
                                             labels = c('Yes', 'No', 'Missing'), 
                                             levels = c('1' ,'2', 'Missing')),
                            Need_Equipment = factor(USEEQUIP,
                                             labels = c('Yes', 'No', 'Missing'), 
                                             levels = c('1' ,'2', 'Missing')),
                            Sex = factor(SEX,
                                         labels = c('Male', 'Female', 'Missing'), 
                                         levels = c('1' ,'2', 'Missing')),
                            Race = factor(RACE,
                                          labels = c('White', 'Black', 'Asian', 'American Indian or Alaskan Native', 'Hispanic', 'Other race/Multiracial', 'Missing'),
                                          levels = c('1', '2', '3', '4', '5', '6', 'Missing')),
                            Marital = factor(MARITAL,
                                             labels = c('Married', 'Divorced', 'Widowed', 'Separated', 'Never married', 'A member of an unmarried couple', 'Missing'), 
                                             levels = c('1', '2', '3', '4', '5', '6', 'Missing')),
                            Age_Group = factor(X_AGE_G,
                                               labels = c('18 to 24', '25 to 34', '35 to 44', '45 to 54', '55 to 64', '65 or older'), 
                                               levels = c('1', '2', '3', '4', '5', '6')),
                            Education = factor(EDUCA,
                                               labels = c('Never attended school or only kindergarten',	'Grades 1 through 8',	'Grades 9 through 11',	'Grade 12 or GED',	'College/technical school 1 year to 3 years',	'College 4 years or more', 'Missing'), 
                                               levels = c('1', '2', '3', '4', '5', '6', 'Missing')),
                            Employment = factor(EMPLOYMENT,
                                                labels = c('Employed for wages',	'Self-employed',	'Out of work for 1 year or more',	'Out of work for less than 1 year',	'A homemaker', 'A student',	'Retired',	'Unable to work', 'Missing'), 
                                                levels = c('1', '2', '3', '4', '5', '6', '7', '8', 'Missing')),
                            BMI = factor(X_BMI5CAT,
                                         labels = c('Less than 18.5', '18.5 to 24.9', '25 to 29.9', '30+', 'Missing'), 
                                         levels = c('1', '2', '3', '4', 'Missing')),
                            General_Health = factor(GENHLTH,
                                                    labels = c('Excellent',	'Very Good',	'Good',	'Fair',	'Poor', 'Missing'), 
                                                    levels = c('1', '2', '3', '4', '5', 'Missing')),
                            Physical_Health = PHYSHLTH,
                            Mental_Health = MENTHLTH,
                            Fruits = FRUTDA1_,
                            Fruit_Juice = FTJUDA1_,
                            Greens = GRENDAY_
                            )

write_rds(clean_years_final, 'intermediate_saves/clean_years_final.rds')

" USE IN MODELS THAT ONLY OPERATE WITH NUMERIC / DISTANCE INPUT
# Preserve the target vars  and the integers so they are not dummy coded
targets <- select(clean_years_final, DIABETE3, CVDSTRK3, ADDEPEV2, year)
nums <- select_if(clean_years_final, is.numeric)
# Dummy code input factors
dummy <- dummyVars(~ HLTHPLN1 + PERSDOC2 + MEDCOST + ASTHMA3 + CHCSCNCR + 
              CHCCOPD + HAVARTH3 + CHCKIDNY + DIABETE3 + VETERAN3 + 
              MARITAL + RENTHOM1 + SEX + LIMITED + USEEQUIP +
              X_RFBING5 + X_SMOKER3 + RACE + EMPLOYMENT,
              data = clean_years_final, fullRank = TRUE, sep = '_') %>% 
  predict(newdata = clean_years_final)

# Recombine normalized integer variables to dummy coded variables
numeric_recode <- cbind(targets, dummy, nums) %>% 
  mutate_if(is.numeric, as.integer)

# Finally!
NA_prop(numeric_recode)
save(numeric_recode, file = 'intermediate_saves/numeric_recode.RData')
"
