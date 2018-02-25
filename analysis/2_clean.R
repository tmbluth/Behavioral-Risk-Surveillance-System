library(dplyr)
library(purrr)
library(caret)
source('/home/tmbluth/Documents/GitHub/Behavioral-Risk-Surveillance-System/analysis/funcs.r')
load('intermediate_saves/all_years.RData')

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
            SCNTWRK1  = ifelse(SCNTWRK1 == 99 | SCNTWRK1 == 97, NA,
                        ifelse(SCNTWRK1 == 98, 0, SCNTWRK1)),
            DRNKMON   = ifelse(DRNKMON == 99.99, NA, DRNKMON),
            STRFREQ_  = ifelse(STRFREQ_ == 99, NA, STRFREQ_)
            ) 

# Change numeric placeholders into correct meaning (make_num_NA)
clean_years <- all_years %>% 
  select(-CHILDREN, -EMPLOYMENT, -SCNTWRK1, -DRNKMON, -STRFREQ_) %>% 
  map(make_num_NA) %>% 
  bind_cols(edge_case_vars);  rm(edge_case_vars, all_years)

# Remove highly NA columns and rows
NA.col <- clean_years %>% 
  # Get % missing in each column
  map(function(x) sum(is.na(x))/nrow(.) ) %>% 
  # If column in missing less than 33%, keep it
  map_lgl(function(x) x < 0.33)

clean_years2 <- clean_years[ , NA.col]; rm(clean_years)

# List of integer vars:
integers <- c('PHYSHLTH', 'MENTHLTH', 'CHILDREN', 'AGE_GRP', 'BMI',  'DRNKMON')
# Integers removed due to high NA proportion:
#   'FTJUDA1_', 'FRUTDA1_', 'BEANDAY_', 'GRENDAY_', 'ORNGDAY_', 'VEGEDA1_',
#   'STRFREQ_', 'SLEEP_TIME','PAINACT2', 'QLMENTL2', 'QLSTRES2', 'QLHLTH2', 'SCNTWRK1', 
#   'ADPLEASR', 'ADDOWN',   'ADSLEEP',  'ADENERGY', 'ADEAT1', 'ADFAIL', 'ADTHINK',  'ADMOVE',

# List of factor vars:
factors <- c('HLTHPLN1', 'PERSDOC2', 'MEDCOST',    'CVDINFR4', 'CVDCRHD4',   'CVDSTRK3',
             'ASTHMA3',    'CHCSCNCR', 'CHCOCNCR',   'CHCCOPD',
             'HAVARTH3', 'ADDEPEV2', 'CHCKIDNY', 'DIABETE3',   'SEX',
             'MARITAL',  'RENTHOM1', 'VETERAN3', 'EMPLOYMENT', 
             'LIMITED',  'USEEQUIP',      'RACE',
              '_RFBING5', '_SMOKER3', 'year', 'GENHLTH',  'CHECKUP1', 'EDUCA', 'INCOME2')
# Factors removed due to high NA proportion
#   'BPHIGH4', 'BLOODCHO', 'TOLDHI2', 'BLIND', 'DECIDE', 'DIFFWALK', 'DIFFDRES', 'DIFFALON',
#   'PREGNANT','PDIABTST', 'PREDIAB1''DRADVISE', 'SCNTPAID', 'MISTMNT',  'ADANXEV', 
#   'RRATWRK2', 'RRHCARE3', 'RRPHYSM2', 'RREMTSM2', 'WTCHSALT''CHOLCHK'
#   'PA_BENCHMARK', 'SCNTMONY', 'SCNTMEAL','EMTSUPRT', 'LSATIS, FY', 

# Check to see if variable groupings are equal to number of columns
setdiff(c(integers, factors), names(clean_years2)); setdiff(names(clean_years2), c(integers, factors))

clean_years3 <- clean_years2 %>% 
  # Change continuous numeric values into discrete integers
  mutate_if(names(.) %in% integers, as.integer) %>% 
  # Change questions with classes into factors
  mutate_if(names(.) %in% factors, as.factor); rm(clean_years2)

# Take out the outcome variable missing values
clean_years4 <- clean_years3[!is.na(clean_years3$CVDINFR4),] %>% .[!is.na(.$CVDCRHD4),] %>% .[!is.na(.$CVDSTRK3),] %>% .[!is.na(.$ADDEPEV2),]; rm(clean_years3)

# Make NA's into a factor level. This can be used in exploration
clean_years_NA <- map_if(.x = clean_years4, .p = is.factor, .f = addNA, ifany = TRUE) %>% as.data.frame()
NA_prop(clean_years_NA); NA_prop(clean_years4); rm(clean_years4)
save(clean_years_NA, file = 'intermediate_saves/clean_years_NA.RData')

# To prepare for modeling variables must be numeric. For distance-based algorithms make variables on same scale (0 to 1)
integers_df <- select_if(clean_years_NA, is.integer)
zero_to_one <- map_df(integers_df, function(x) (x - min(x, na.rm = TRUE))/diff(range(x, na.rm = TRUE)))

# Dummy code factors (including NA) to make them numeric. No need to include state as an input variable
factors_df <- select_if(select(clean_years_NA, - X_STATE), is.factor); rm(clean_years_NA)
dummy <- dummyVars(~ ., data = factors_df) %>% predict(factors_df)

# Recombine normalized integer variables to dummy coded variables
clean_years_final <- cbind(zero_to_one, dummy); rm(zero_to_one, dummy, factors_df, integers_df)

# Some yes/no questions are dummy coded due to the added factor level 'NA'. Since one colum is the inverse of the other column, one is removed
clean_years_final <- clean_years_final %>% 
  select(-HLTHPLN1.2, -MEDCOST.2, -CVDINFR4.2, -CVDCRHD4.2, -CVDSTRK3.2, 
         -ASTHMA3.2, -CHCSCNCR.2, -CHCOCNCR.2, -CHCCOPD.2, -HAVARTH3.2,
         -ADDEPEV2.2, -CHCKIDNY.2, -VETERAN3.2, -SEX.2, -LIMITED.2, 
                            -USEEQUIP.2, -X_RFBING5.2)

# Finally!
save(clean_years_final, file = 'intermediate_saves/clean_years_final.RData')
