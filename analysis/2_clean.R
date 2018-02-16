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
                                           ifelse(SCNTWRK1 == 98, 0, SCNTWRK1)))

# Change numeric placeholders into correct meaning (make_num_NA)
clean_years <- all_years %>% 
  select(-CHILDREN, -EMPLOYMENT, -SCNTWRK1) %>% 
  map(make_num_NA) %>% 
  bind_cols(edge_case_vars)

# List of integer vars:
integers <- c('PHYSHLTH', 'MENTHLTH', 'POORHLTH', 'CHILDREN', 'AGE_GRP',
              'BMI',      'AVEDRNK2', 'DRNK3GE5', 'MAXDRNKS', "FTJUDA1_",
              "FRUTDA1_", "BEANDAY_", "GRENDAY_", "ORNGDAY_", "VEGEDA1_",
              'PAINACT2', 'QLMENTL2', 'QLSTRES2', 'QLHLTH2',  'SCNTWRK1',
              'ADPLEASR', 'ADDOWN',   'ADSLEEP',  'ADENERGY', 'ADEAT1', 
              'ADFAIL',   'ADTHINK',  'ADMOVE',   'STRFREQ_', 'SLEEP_TIME')
# List of factor vars:
factors <- c('HLTHPLN1', 'PERSDOC2', 'MEDCOST',  'BPHIGH4',    'BPMEDS',
             'BLOODCHO', 'TOLDHI2',  'CVDINFR4', 'CVDCRHD4',   'CVDSTRK3',
             'ASTHMA3',  'ASTHNOW',  'CHCSCNCR', 'CHCOCNCR',   'CHCCOPD',
             'HAVARTH3', 'ADDEPEV2', 'CHCKIDNY', 'DIABETE3',   'SEX',
             'MARITAL',  'RENTHOM1', 'VETERAN3', 'EMPLOYMENT', 'PREGNANT',
             'LIMITED',  'USEEQUIP', 'BLIND',    'DECIDE',     'DIFFWALK',
             'DIFFDRES', 'DIFFALON', 'SMOKE100', 'SMOKDAY2',   'STOPSMK2',
             'USENOW3',  'PDIABTST', 'PREDIAB1', 'WTCHSALT',   'RACE',
             'DRADVISE', 'SCNTPAID', 'MISTMNT',  'ADANXEV',    'RRATWRK2', 
             'RRHCARE3', 'RRPHYSM2', 'RREMTSM2', 'year')
# List of ordered factor vars:
ordered_f <- c('GENHLTH',  'CHECKUP1', 'CHOLCHK',  'EDUCA',    'INCOME2',
               'LASTSMK2', 'SCNTMONY', 'SCNTMEAL', 'EMTSUPRT', 'LSATISFY', 'PA_BENCHMARK')

# Check to see if variable groupings are equal to number of columns
setdiff(c(integers, factors, ordered_f), names(clean_years)); setdiff(names(clean_years), c(integers, factors, ordered_f))

clean_years2 <- clean_years %>% 
  # Change continuous numeric values into discrete integers
  mutate_if(names(clean_years) %in% integers, as.integer) %>% 
  # Change questions with classes into factors
  mutate_if(names(clean_years) %in% factors, as.factor) %>% 
  # Change ordinal questions into ordered factors
  mutate_if(names(clean_years) %in% ordered_f, as.ordered) %>% 
  # Flip direction of some of the ordered factors
  mutate_if(names(clean_years) %in% c('GENHLTH', 'LASTSMK2', 'SCNTMONY', 'SCNTMEAL', 'EMTSUPRT', 'LSATISFY', 'PA_BENCHMARK'), fct_rev)

# Check to see if data set is as expected
lapply(clean_years2, unique)
lapply(clean_years2, function(x) paste0(round(sum(is.na(x))/nrow(clean_years2)*100, 2), '% missing'))
sum(apply(clean_years2, 1, function(x) sum(is.na(x))/ncol(clean_years2)) < 0.33) # 11215 people answered 67% or more of questions

save(clean_years2, file = 'intermediate_saves/clean_years2.RData')
