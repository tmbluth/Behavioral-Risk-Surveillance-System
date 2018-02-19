library(tidyverse)
source('analysis/funcs.R')
load('intermediate_saves/all_years.RData')

# View all unique responses:
# for single digit responses 9 = Refused and 7 = Don't Know
# For double digit responses 99 = Refused, 98 or 88 = None/0, 97 or 77 = Don't Know
# For triple digit responses 999 = Refused, 888 = None/0, 777 = Don't Know
lapply(all_years, unique)

# Make RACE back into integer so that it can be cleaned by 'make_num_NA'
all_years$RACE <- as.integer(all_years$RACE)

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
  bind_cols(edge_case_vars) 

# Remove highly NA columns and rows
NA.col <- clean_years %>% 
  # Get % missing in each column
  map(function(x) sum(is.na(x))/nrow(.) ) %>% 
  # If column in missing less than 50%, keep it
  map_lgl(function(x) x < 0.5)
clean_years2 <- clean_years[ , NA.col] 
NA.row <- clean_years %>%
  # Get % missing in each respondent
  apply(MARGIN = 1, function(x) sum(is.na(x))/ncol(.) ) %>% 
  # If respondent missed less than 50% for questions that had less than 50% missing columns, keep them 
  map_lgl(function(x) x < 0.5) 
clean_years2 <- clean_years2[NA.row, ] 

# List of integer vars:
integers <- c('PHYSHLTH', 'MENTHLTH', 'CHILDREN', 'AGE_GRP',
              'BMI',      'FTJUDA1_',
              'FRUTDA1_', 'BEANDAY_', 'GRENDAY_', 'ORNGDAY_', 'VEGEDA1_',
              'STRFREQ_',  'DRNKMON')  #'SLEEP_TIME','PAINACT2', 'QLMENTL2', 'QLSTRES2', 'QLHLTH2',  'SCNTWRK1',  'ADPLEASR', 'ADDOWN',   'ADSLEEP',  'ADENERGY', 'ADEAT1', 'ADFAIL',   'ADTHINK',  'ADMOVE',
# List of factor vars:
factors <- c('HLTHPLN1', 'PERSDOC2', 'MEDCOST',  'BPHIGH4',   
             'BLOODCHO', 'TOLDHI2',  'CVDINFR4', 'CVDCRHD4',   'CVDSTRK3',
             'ASTHMA3',    'CHCSCNCR', 'CHCOCNCR',   'CHCCOPD',
             'HAVARTH3', 'ADDEPEV2', 'CHCKIDNY', 'DIABETE3',   'SEX',
             'MARITAL',  'RENTHOM1', 'VETERAN3', 'EMPLOYMENT', 
             'LIMITED',  'USEEQUIP', 'BLIND',    'DECIDE',     'DIFFWALK',
             'DIFFDRES', 'DIFFALON',     'RACE',
              '_RFBING5', '_SMOKER3', 'year') #'ASTHNOW','PREGNANT','PDIABTST', 'PREDIAB1''DRADVISE', 'SCNTPAID', 'MISTMNT',  'ADANXEV',    'RRATWRK2', 'RRHCARE3', 'RRPHYSM2', 'RREMTSM2', 'WTCHSALT'
# List of ordered factor vars:
ordered_f <- c('GENHLTH',  'CHECKUP1', 'CHOLCHK',  'EDUCA', 'INCOME2', 'PA_BENCHMARK') # 'SCNTMONY', 'SCNTMEAL','EMTSUPRT', 'LSATISFY',

# Check to see if variable groupings are equal to number of columns
setdiff(c(integers, factors, ordered_f), names(clean_years2)); setdiff(names(clean_years2), c(integers, factors, ordered_f))

clean_years_final <- clean_years2 %>% 
  # Change continuous numeric values into discrete integers
  mutate_if(names(clean_years2) %in% integers, as.integer) %>% 
  # Change questions with classes into factors
  mutate_if(names(clean_years2) %in% factors, as.factor) %>% 
  # Change ordinal questions into ordered factors
  mutate_if(names(clean_years2) %in% ordered_f, as.ordered) %>% 
  # Flip direction of some of the ordered factors
  mutate_if(names(clean_years2) %in% c('GENHLTH', 'PA_BENCHMARK'), fct_rev)

# Check to see if data set is as expected
lapply(clean_years_final, unique)
clean_years_final %>% map(function(x) paste0(round(sum(is.na(x))/nrow(.)*100, 2), '% missing'))

save(clean_years_final, file = 'intermediate_saves/clean_years_final.RData')

# Make NA's into a factor level
clean_years_NA <- map_if(.x = clean_years_final, .p = is.factor, .f = addNA, ifany = TRUE) %>% as.data.frame()
save(clean_years_NA, file = 'intermediate_saves/clean_years_NA.RData')

#--------------------------------#



