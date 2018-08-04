library(tidyverse)
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
factors <- c('HLTHPLN1', 'PERSDOC2', 'MEDCOST',  'CVDINFR4',  'CVDCRHD4',   
             'CVDSTRK3', 'ASTHMA3',  'CHCSCNCR', 'CHCOCNCR',   'CHCCOPD',
             'HAVARTH3', 'ADDEPEV2', 'CHCKIDNY', 'DIABETE3',   'SEX',
             'MARITAL',  'RENTHOM1', 'VETERAN3', 'EMPLOYMENT', 'LIMITED',
             'USEEQUIP', 'RACE',     '_RFHYPE5', '_RFCHOL',    '_RFBING5', 
             '_SMOKER3', 'PREGNANT', 'PA_BENCHMARK', 'year')

factors.ordered <- c('GENHLTH', 'CHECKUP1', '_AGE_G', 'EDUCA', 'INCOME2', '_BMI5CAT')

# Check to see if variable groupings are equal to number of columns
setdiff(c(numerics, factors, factors.ordered), names(clean_years)); setdiff(names(clean_years), c(numerics, factors, factors.ordered))

clean_years2 <- clean_years %>% 
  # Keep continuous variables as numeric
  mutate_if(names(.) %in% numerics, as.numeric) %>% 
  # Change questions with classes into factors
  mutate_if(names(.) %in% factors, as.factor) %>% 
  # Change questions with ordered classes into ordered factors
  mutate_if(names(.) %in% factors.ordered, as.ordered); rm(clean_years)

# Take out the outcome variable missing values
clean_years3 <- clean_years2[!is.na(clean_years2$DIABETE3),] %>% 
                .[!is.na(.$CVDSTRK3),] %>%
                .[!is.na(.$ADDEPEV2),]; rm(clean_years2)

# Make NA's into a factor level. This can be used in exploration
clean_years_NA <- clean_years3 %>% 
  map_if(.p = function(x) is.factor(x) & !is.ordered(x), .f = function(x) ifelse(is.na(x), 'Missing', x)) %>% 
  as.data.frame()
  
NA_prop(clean_years_final)

clean_years_final <- clean_years_NA %>% 
  select(-INCOME2) %>% 
  map_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x)) %>% 
  as.data.frame() %>% 
  na.omit()

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
