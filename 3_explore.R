library(forcats)
library(purrr)
library(ggplot2)

load('intermediate_saves/clean_years_final.RData')

# EDA is a bit less exciting visually when dealing with mainly categorical variables, but the numbers (and some graphs) can tell us a lot quickly

#--------------------------------  General Descriptive Statistics --------------------------#


# Check distributions of all variables
map(clean_years_final, table, useNA = 'ifany')

# Which states had the most respondents? (In alphabetical order from 1 to 55 [including Washington DC and some skipped numbers] and higher numbers indicating US territories)
ggplot(clean_years_final, aes(fct_infreq(X_STATE))) + geom_bar() + ggtitle('Respondent Count by State')
# Top 10:
#  1. Nebraska
#  2. Kansas
#  3. Massachusets
#  4. Minnesota
#  5. Florida
#  6. New Jersey
#  7. Washington
#  8. Colorado
#  9. California
# 10. Texas


#------------------------------------ Hypotheses ------------------------------------#

# 1. High BMI increases likeihood of having had a myocardial infarction (MI) or stroke
(mi_bmi <- table(clean_years_final$CVDINFR4, clean_years_final$BMI, useNA = 'ifany'))
prop.table(mi_bmi)
chisq.test(mi_bmi) # Significant differences between expected proportion of stoke and actual proportion

(strk_bmi <- table(clean_years_final$CVDSTRK3, clean_years_final$BMI, useNA = 'ifany'))
prop.table(strk_bmi)
chisq.test(strk_bmi) # Significant differences between expected proportion of stoke and actual proportion

#2. Having good physical health leads to lower instances of MI or stroke
ggplot(clean_years_final, aes(x = CVDINFR4, y = PHYSHLTH, color = CVDINFR4)) + 
  geom_boxplot(show.legend = FALSE) + 
  scale_x_discrete(labels = c('Has Had Heart Attack', 'Never Had Heart Attack')) +
  labs(x = '', y = 'Past 30 Days of Poor Physical Health', title = 'Heart Attack vs Days of Poor Physical Health')

ggplot(clean_years_final, aes(x = ADDEPEV2, y = MENTHLTH, color = ADDEPEV2)) + 
  geom_boxplot(show.legend = FALSE) + 
  scale_x_discrete(labels = c('Has Had Stroke', 'Never Had Stroke')) +
  labs(x = '', y = 'Past 30 Days of Poor Physical Health', title = 'Stroke vs Days of Poor Physical Health')

# 3. Number of days per month with mental struggles should be indicitive of having been depressed with mental illness
ggplot(clean_years_final, aes(x = ADDEPEV2, y = MENTHLTH, color = ADDEPEV2)) + 
  geom_boxplot(show.legend = FALSE) + 
  scale_x_discrete(labels = c('Diagnosed', 'Undiagnosed')) +
  labs(x = '', y = 'Past 30 Days of Poor Mental Health', title = 'Depression or Anxiety Diagnosis via Days of Poor Mental Health')

# 3. Being limited mentally, physically, or in some other manner may lead to depression or anxiety
ggplot(clean_years_final, aes(x = ADDEPEV2, y = LIMITED, fill = ADDEPEV2)) + 
  geom_tile(show.legend = FALSE) + 
  scale_x_discrete(labels = c('Diagnosed', 'Undiagnosed')) +
  labs(x = '', y = 'Degree of Limitation', title = 'Depression or Anxiety Diagnosis due to Limitations')
