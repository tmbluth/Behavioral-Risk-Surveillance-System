library(data.table)
library(dplyr)
library(forcats)

source('analysis/funcs.r')

# List format
y11 <- fread('data/2011.csv', 
                  select = c('GENHLTH',  'PHYSHLTH', 'MENTHLTH',  'HLTHPLN1', 'PERSDOC2',
                             'MEDCOST',  'CHECKUP1', '_RFHYPE5',  '_RFCHOL', 'CVDINFR4',
                             'CVDCRHD4', 'CVDSTRK3', 'ASTHMA3',  'CHCSCNCR', 'CHCOCNCR',
                             'CHCCOPD',  'HAVARTH3', 'ADDEPEV2', 'CHCKIDNY', 'DIABETE3', 
                             '_AGE_G',   'VETERAN3', 'EMPLOY',   'MARITAL',  'CHILDREN',
                             'EDUCA',    'INCOME2',  '_BMI5CAT', 'RENTHOM1', 'SEX', 
                             'PREGNANT', 'QLACTLM2', 'FTJUDA1_', 'FRUTDA1_', 'BEANDAY_',
                             'GRENDAY_', 'ORNGDAY_', 'VEGEDA1_', '_IMPRACE', '_PA150R1',
                             'STRFREQ_', 'USEEQUIP', '_RFBING5', '_SMOKER3')) %>% 
  mutate(year = '2011', RACE = as.factor(`_IMPRACE`)) %>% 
  mutate_if(names(.) %in% c('FTJUDA1_', 'FRUTDA1_', 'BEANDAY_',  'GRENDAY_', 'ORNGDAY_', 'VEGEDA1_'), function(x) round(x/100, 1)) %>%
  rename(EMPLOYMENT = EMPLOY, LIMITED = QLACTLM2, PA_BENCHMARK = `_PA150R1`)

#-----------------------------------------------------------------------------------------------#

y13 <- fread('data/2013.csv', 
                  select = c('GENHLTH',  'PHYSHLTH', 'MENTHLTH', 'HLTHPLN1',
                             'PERSDOC2', 'MEDCOST',  'CHECKUP1', '_RFHYPE5', '_RFCHOL',
                             'CVDINFR4', 'CVDCRHD4', 'CVDSTRK3', 'ASTHMA3',  'CHCSCNCR', 
                             'CHCOCNCR', 'CHCCOPD1', 'HAVARTH3', 'ADDEPEV2', 'CHCKIDNY',
                             'DIABETE3', '_AGE_G',   'VETERAN3', 'EMPLOY1', 'MARITAL', 
                             'EDUCA',    'INCOME2',  '_BMI5CAT', 'CHILDREN', 'SEX',
                             'PREGNANT', 'QLACTLM2', 'USEEQUIP', '_SMOKER3', '_RFBING5', 
                             'FTJUDA1_', 'FRUTDA1_', 'BEANDAY_', 'GRENDAY_',  'ORNGDAY_', 
                             'VEGEDA1_', 'RENTHOM1', '_RACE', '_PA150R2', 'STRFREQ_')) %>% 
  mutate(year = '2013', 
         RACE = fct_recode(as.factor(`_RACE`),
                           '3' = '4',
                           '4' = '3',
                           '5' = '8',
                           '6' = '5',
                           '6' = '7')) %>%
  mutate_if(names(.) %in% c('FTJUDA1_', 'FRUTDA1_', 'BEANDAY_',  'GRENDAY_', 'ORNGDAY_', 'VEGEDA1_'), function(x) round(x/100, 1)) %>% 
  rename(EMPLOYMENT = EMPLOY1, LIMITED = QLACTLM2, CHCCOPD = CHCCOPD1, PA_BENCHMARK = `_PA150R2`) 

#-----------------------------------------------------------------------------------------------#

y15 <- fread('data/2015.csv',
                 select = c('GENHLTH',  'PHYSHLTH', 'MENTHLTH', 'HLTHPLN1', 'PERSDOC2',
                            'MEDCOST',  'CHECKUP1', '_RFHYPE5', '_RFCHOL',  'CVDINFR4',
                            'CVDCRHD4', 'CVDSTRK3', 'ASTHMA3',  'CHCSCNCR', 'CHCOCNCR',
                            'CHCCOPD1', 'HAVARTH3', 'ADDEPEV2', 'CHCKIDNY', 'DIABETE3',
                            '_AGE_G',   'SEX',      'MARITAL',  'EDUCA',    'RENTHOM1',
                            'VETERAN3', 'EMPLOY1',  'CHILDREN', 'INCOME2',  '_BMI5CAT',
                            'PREGNANT', 'QLACTLM2', 'USEEQUIP', '_SMOKER3', '_RFBING5',
                            'FTJUDA1_', 'FRUTDA1_', 'BEANDAY_', 'GRENDAY_', 'ORNGDAY_',
                            'VEGEDA1_',  '_RACE',   '_PA150R2', 'STRFREQ_')) %>% 
  mutate(year = '2015', 
         RACE = fct_recode(as.factor(`_RACE`),
                           '3' = '4',
                           '4' = '3',
                           '5' = '8',
                           '6' = '5',
                           '6' = '7')) %>% 
  mutate_if(names(.) %in% c('FTJUDA1_', 'FRUTDA1_', 'BEANDAY_',  'GRENDAY_', 'ORNGDAY_', 'VEGEDA1_'), function(x) round(x/100, 1)) %>% 
  rename(EMPLOYMENT = EMPLOY1, LIMITED = QLACTLM2, CHCCOPD = CHCCOPD1, PA_BENCHMARK = `_PA150R2`) 

#-----------------------------------------------------------------------------------------------#

all_years <- bind_rows(y11, y13, y15) %>% 
  mutate(STRFREQ_ = round(STRFREQ_ / 1000,1),
         `_RACE` = NULL,
         `_IMPRACE` = NULL,
         RACE = as.integer(RACE),
         DIABETE3 = ifelse(DIABETE3 %in% c(2,3,4), 2, DIABETE3),
         PREGNANT = ifelse(is.na(PREGNANT), 2, PREGNANT))

NA_prop(all_years)

save(all_years, file = 'intermediate_saves/all_years.RData')
