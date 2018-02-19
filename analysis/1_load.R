library(data.table)
library(tidyverse)
source('analysis/funcs.R')

# List format
train_11 <- fread('data/2011.csv', 
   select = c('GENHLTH',  'PHYSHLTH', 'MENTHLTH',  'HLTHPLN1',
              'PERSDOC2', 'MEDCOST',  'CHECKUP1', 'BPHIGH4',  
              'BLOODCHO', 'CHOLCHK',  'TOLDHI2',  'CVDINFR4', 'CVDCRHD4',
              'CVDSTRK3', 'ASTHMA3',  'ASTHNOW',  'CHCSCNCR', 'CHCOCNCR',
              'CHCCOPD',  'HAVARTH3', 'ADDEPEV2', 'CHCKIDNY', 'DIABETE3', 
              '_AGE_G',   'VETERAN3', 'EMPLOY',   'MARITAL',  'CHILDREN',
              'EDUCA',    'INCOME2',  '_BMI5CAT', 'RENTHOM1', 'SEX', 
              'PREGNANT', 'QLACTLM2', 'FTJUDA1_', 'FRUTDA1_',   'BEANDAY_',
              'GRENDAY_',  'ORNGDAY_',  'VEGEDA1_', 'SLEPTIME', '_IMPRACE',
              '_PA150R1', 'STRFREQ_', 'RRHCARE3', 'RRPHYSM2', 'RREMTSM2',
              'USEEQUIP', '_DRNKMO4', '_RFBING5', 'PDIABTST', '_SMOKER3',
              'PREDIAB1', 'PAINACT2', 'QLMENTL2', 'QLSTRES2', 'QLHLTH2',
              'ADPLEASR', 'ADDOWN',   'ADSLEEP',  'ADENERGY', 'ADEAT1', 
              'ADFAIL',   'ADTHINK',  'ADMOVE',   'MISTMNT',  'ADANXEV',
              'SCNTPAID', 'SCNTWRK1', 'EMTSUPRT', 'LSATISFY', 'RRATWRK2')) %>% 
  mutate(year = '2011', RACE = as.factor(`_IMPRACE`), `_IMPRACE` = NULL) %>% 
  mutate_if(names(.) %in% c('FTJUDA1_', 'FRUTDA1_', 'BEANDAY_',  'GRENDAY_', 'ORNGDAY_', 'VEGEDA1_'), function(x) x/100) %>%
  rename(SLEEP_TIME = SLEPTIME, EMPLOYMENT = EMPLOY, LIMITED = QLACTLM2, 
         AGE_GRP = `_AGE_G`, BMI = `_BMI5CAT`, PA_BENCHMARK = `_PA150R1`)

train_12 <- fread('data/2012.csv',
   select = c('GENHLTH',  'PHYSHLTH', 'MENTHLTH',  'HLTHPLN1',
              'PERSDOC2', 'MEDCOST',  'CHECKUP1', 'SLEPTIME', '_IMPRACE',
              'CVDINFR4', 'CVDCRHD4', 'CVDSTRK3', 'ASTHMA3',  'ASTHNOW',
              'CHCSCNCR', 'CHCOCNCR', 'CHCCOPD1', 'HAVARTH3', 'ADDEPEV2',
              'CHCKIDNY', 'DIABETE3', '_AGE_G',   'VETERAN3', 'EMPLOY',
              'MARITAL',  'CHILDREN', 'EDUCA',    'INCOME2',  '_BMI5CAT',
              'RENTHOM1', 'SEX',      'PREGNANT', 'QLACTLM2', 'USEEQUIP',
              '_SMOKER3', '_DRNKMO4', '_RFBING5', 'EMTSUPRT', 'LSATISFY',
              'PDIABTST', 'PREDIAB1', 'PAINACT2', 'QLMENTL2', 'QLSTRES2',
              'QLHLTH2',  'FRUITJU1', 'FRUIT1',   'FVBEANS',  'FVGREEN',
              'FVORANG',  'VEGETAB1', 'MISTMNT',  'SCNTMONY', 'SCNTMEAL', 
              'SCNTPAID', 'SCNTWRK1', 'RRATWRK2', 'RRHCARE3', 'RRPHYSM2', 'RREMTSM2')) %>% 
  mutate(year = '2012', RACE = as.factor(`_IMPRACE`), `_IMPRACE` = NULL) %>% 
  mutate_if(names(.) %in% c('FRUITJU1', 'FRUIT1', 'FVBEANS', 'FVGREEN', 'FVORANG', 'VEGETAB1'), diet_questions) %>% 
  rename(SLEEP_TIME = SLEPTIME, EMPLOYMENT = EMPLOY, LIMITED = QLACTLM2,
         CHCCOPD = CHCCOPD1, AGE_GRP= `_AGE_G`, BMI = `_BMI5CAT`, 
         FTJUDA1_ = FRUITJU1, FRUTDA1_ = FRUIT1, BEANDAY_ = FVBEANS,  GRENDAY_ = FVGREEN,
         ORNGDAY_ = FVORANG, VEGEDA1_ = VEGETAB1)


train_13 <- fread('data/2013.csv', 
   select = c('GENHLTH',  'PHYSHLTH', 'MENTHLTH',  'HLTHPLN1',
              'PERSDOC2', 'MEDCOST',  'CHECKUP1', 'BPHIGH4',
              'BLOODCHO', 'CHOLCHK',  'TOLDHI2',  'CVDINFR4', 'CVDCRHD4',
              'CVDSTRK3', 'ASTHMA3',  'ASTHNOW',  'CHCSCNCR', 'CHCOCNCR',
              'CHCCOPD1', 'HAVARTH3', 'ADDEPEV2', 'CHCKIDNY', 'DIABETE3',
              '_AGE_G',   'VETERAN3', 'EMPLOY1',  'MARITAL',  'EDUCA', 
              'INCOME2',  '_BMI5CAT', 'CHILDREN', 'SEX',      'PREGNANT',  
              'QLACTLM2', 'USEEQUIP', 'BLIND',    'DECIDE',   'DIFFWALK',
              'DIFFDRES', 'DIFFALON', '_SMOKER3',  'RREMTSM2', '_DRNKMO4', 
              '_RFBING5', 'FTJUDA1_', 'FRUTDA1_', 'BEANDAY_',  'GRENDAY_',
              'ORNGDAY_', 'VEGEDA1_', 'SLEPTIM1', 'RENTHOM1', '_RACE',
              'PDIABTST', 'PREDIAB1', 'PAINACT2', 'QLMENTL2', 'QLSTRES2', 
              'QLHLTH2',  'WTCHSALT', 'MISTMNT',  'SCNTMONY', 'SCNTMEAL',
              'SCNTPAID', 'SCNTWRK1', 'EMTSUPRT', 'LSATISFY', 'RRATWRK2', 
              'RRHCARE3', 'RRPHYSM2', 'DRADVISE', '_PA150R2', 'STRFREQ_')) %>% 
  mutate(year = '2013', 
         RACE = fct_recode(as.factor(`_RACE`),
                           '3' = '4',
                           '4' = '3',
                           '5' = '8',
                           '6' = '5',
                           '6' = '7'),
         `_RACE` = NULL) %>%
  mutate_if(names(.) %in% c('FTJUDA1_', 'FRUTDA1_', 'BEANDAY_',  'GRENDAY_', 'ORNGDAY_', 'VEGEDA1_'), function(x) x/100) %>% 
  rename(SLEEP_TIME = SLEPTIM1, EMPLOYMENT = EMPLOY1, LIMITED = QLACTLM2, 
         CHCCOPD = CHCCOPD1, AGE_GRP= `_AGE_G`, BMI = `_BMI5CAT`,
         PA_BENCHMARK = `_PA150R2`) 

train_14 <- fread('data/2014.csv',          
   select = c('GENHLTH',  'PHYSHLTH', 'MENTHLTH',  'HLTHPLN1',
              'PERSDOC2', 'MEDCOST',  'CHECKUP1',  'SLEPTIM1', '_RACE',
              'CVDINFR4', 'CVDCRHD4', 'CVDSTRK3', 'ASTHMA3',  'ASTHNOW',
              'CHCSCNCR', 'CHCOCNCR', 'CHCCOPD1', 'HAVARTH3', 'ADDEPEV2',
              'CHCKIDNY', 'DIABETE3', '_AGE_G',   'VETERAN3', 'EMPLOY1', 
              'MARITAL',  'CHILDREN', 'EDUCA',    'INCOME2',  '_BMI5CAT',
              'RENTHOM1', 'SEX',      'PREGNANT', 'QLACTLM2', 'USEEQUIP',
              'BLIND',    'DECIDE',   'DIFFWALK', 'DIFFDRES', 'DIFFALON',
              '_SMOKER3', '_DRNKMO4', '_RFBING5', 'FTJUDA1_', 'FRUTDA1_',
              'BEANDAY_',  'GRENDAY_', 'ORNGDAY_',  'VEGEDA1_', 'PDIABTST', 'PREDIAB1',
              'PAINACT2', 'QLMENTL2', 'QLSTRES2', 'QLHLTH2',  'WTCHSALT',
              'SCNTMNY1', 'SCNTMEL1', 'SCNTPAID', 'SCNTWRK1', 'RRATWRK2',
              'RRHCARE3', 'RRPHYSM2', 'RREMTSM2', 'EMTSUPRT', 'LSATISFY',
              'DRADVISE')) %>% 
  mutate(year = '2014', 
         RACE = fct_recode(as.factor(`_RACE`),
                          '3' = '4',
                          '4' = '3',
                          '5' = '8',
                          '6' = '5',
                          '6' = '7'),
         `_RACE` = NULL) %>% 
         mutate_if(names(.) %in% c('FTJUDA1_', 'FRUTDA1_', 'BEANDAY_',  'GRENDAY_', 'ORNGDAY_', 'VEGEDA1_'), function(x) x/100) %>% 
  rename(SLEEP_TIME = SLEPTIM1, EMPLOYMENT = EMPLOY1, LIMITED = QLACTLM2,
         SCNTMONY = SCNTMNY1, SCNTMEAL = SCNTMEL1, CHCCOPD = CHCCOPD1,
         AGE_GRP = `_AGE_G`, BMI = `_BMI5CAT`)  

test_15 <- fread('data/2015.csv',
  select = c('GENHLTH',  'PHYSHLTH', 'MENTHLTH',  'HLTHPLN1',
             'PERSDOC2', 'MEDCOST',  'CHECKUP1', 'BPHIGH4',  
             'BLOODCHO', 'CHOLCHK',  'TOLDHI2',  'CVDINFR4', 'CVDCRHD4',
             'CVDSTRK3', 'ASTHMA3',  'ASTHNOW',  'CHCSCNCR', 'CHCOCNCR',
             'CHCCOPD1', 'HAVARTH3', 'ADDEPEV2', 'CHCKIDNY', 'DIABETE3',
             '_AGE_G',   'SEX',      'MARITAL',  'EDUCA',    'RENTHOM1',
             'VETERAN3', 'EMPLOY1',  'CHILDREN', 'INCOME2',  '_BMI5CAT',
             'PREGNANT', 'QLACTLM2', 'USEEQUIP', 'BLIND',    'DECIDE',
             'DIFFWALK', 'DIFFDRES', 'DIFFALON', '_SMOKER3',  '_DRNKMO4',
             '_RFBING5', 'FTJUDA1_', 'FRUTDA1_',   'BEANDAY_',  'GRENDAY_',
             'ORNGDAY_',  'VEGEDA1_', 'PDIABTST', 'PREDIAB1', '_RACE',
             'PAINACT2', 'QLMENTL2', 'QLSTRES2', 'QLHLTH2',  'WTCHSALT',
             'SCNTMNY1', 'SCNTMEL1', 'SCNTPAID', 'SCNTWRK1', 'EMTSUPRT', 
             'LSATISFY', 'ADPLEASR', 'ADDOWN',   'ADSLEEP',  'ADANXEV',
             'ADENERGY', 'ADEAT1',   'ADFAIL',   'ADTHINK',  'ADMOVE',
             'MISTMNT',  'DRADVISE', '_PA150R2', 'STRFREQ_')) %>% 
  mutate(year = '2015', 
         RACE = fct_recode(as.factor(`_RACE`),
                           '3' = '4',
                           '4' = '3',
                           '5' = '8',
                           '6' = '5',
                           '6' = '7'),
         `_RACE` = NULL) %>% 
         mutate_if(names(.) %in% c('FTJUDA1_', 'FRUTDA1_', 'BEANDAY_',  'GRENDAY_', 'ORNGDAY_', 'VEGEDA1_'), function(x) x/100) %>% 
  rename(EMPLOYMENT = EMPLOY1, LIMITED = QLACTLM2, SCNTMONY = SCNTMNY1, 
         SCNTMEAL = SCNTMEL1, CHCCOPD = CHCCOPD1, AGE_GRP = `_AGE_G`,
         BMI = `_BMI5CAT`, PA_BENCHMARK = `_PA150R2`) 

all_years <- bind_rows(train_11, train_12, train_13, train_14, test_15) %>% 
  mutate(STRFREQ_ = STRFREQ_ / 1000,
         DRNKMON = `_DRNKMO4` / 100,
         `_DRNKMO4` = NULL)

save(all_years, file = 'intermediate_saves/all_years.RData')
