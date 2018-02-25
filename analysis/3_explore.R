library(forcats)

load('intermediate_saves/clean_years_NA.RData')
#------------------------------------------------------------------------------#

prop.table(table(clean_years_NA$CVDINFR4))
prop.table(table(clean_years_NA$CVDCRHD4))
prop.table(table(clean_years_NA$CVDSTRK3))
prop.table(table(clean_years_NA$ADDEPEV2))

ggplot(clean_years_NA, aes(fct_infreq(X_STATE))) +
  geom_bar()

group_by(clean_years_NA, CVDINFR4) %>% summarise(Mean_drinks_mo = mean(DRNKMON, na.rm = T))
group_by(clean_years_NA, CVDCRHD4) %>% summarise(Mean_drinks_mo = mean(DRNKMON, na.rm = T))
group_by(clean_years_NA, CVDSTRK3) %>% summarise(Mean_drinks_mo = mean(DRNKMON, na.rm = T))
group_by(clean_years_NA, ADDEPEV2) %>% summarise(Mean_drinks_mo = mean(DRNKMON, na.rm = T))

table(is.na(clean_years_NA$DRNKMON), clean_years_NA$CVDINFR4) # chisq.test significant




