# For possible responses check out lists below:
load('intermediate_saves/targets.RData')

# The RF models take up **6GB** of memory so be aware of your capacity
load('intermediate_saves/rf_models.RData')

#---------------------------------------------------------------------------------------------------------------------#
# Enter your results. Questions can be found in the `Question` column within the file 'Q_cat.csv' within the 'app' folder 

# Diabetes Model Inputs
sapply(db$Test_set, levels)
# Predict Diabetes Risk
predict(rf_db_3, data.frame(GENHLTH = '', X_RFHYPE5 = '', X_BMI5CAT = '', X_RFCHOL = '', EMPLOYMENT = '', 
                            CHECKUP1 = '', USEEQUIP = '', CVDINFR4 = '', RACE = '', PHYSHLTH = '', 
                            CVDCRHD4 = '', SEX = '', EDUCA = '', PERSDOC2 = '', HAVARTH3 = '',
                            INCOME2 = '', X_RFBING5 = '', PA_BENCHMARK = '', LIMITED = '', STRFREQ_ = ''), type = "prob")

#---------------------------------------------------------------------------------------------------------------------#

# Stroke Model Inputs
sapply(strk$Test_set, levels)
# Predict Stroke Risk
predict(rf_strk_3, data.frame(EMPLOYMENT = '', CVDINFR4 = '', X_AGE_G = '', GENHLTH = '', CVDCRHD4 = '', 
                              X_RFHYPE5 = '' = '', USEEQUIP = '', LIMITED = '', PHYSHLTH = '' = '', HAVARTH3 = '',
                              DIABETE3 = '', INCOME2 = '', MARITAL = '', X_RFCHOL = '', CHCCOPD = '',
                              EDUCA = '', PA_BENCHMARK = '', X_SMOKER3 = '', RENTHOM1 = '', ADDEPEV2 = ''), type = "prob")

#---------------------------------------------------------------------------------------------------------------------#

# Depression Model Inputs
sapply(dep$Test_set, levels)
# Predict Depression Risk
predict(rf_dep_3, data.frame(MENTHLTH = '', LIMITED = '', EMPLOYMENT = '', X_AGE_G = '', SEX = '',
                             GENHLTH = '', HAVARTH3 = '', X_SMOKER3 = '', MEDCOST = '', MARITAL = '',
                             PHYSHLTH = '', RACE = '', X_RFCHOL = '', USEEQUIP = '', ASTHMA3 = '',
                             RENTHOM1 = '', CHCCOPD = '', INCOME2 = '', X_BMI5CAT = '', X_RFHYPE5,
                             PA_BENCHMARK = '', PERSDOC2 = ''), type = "prob")
