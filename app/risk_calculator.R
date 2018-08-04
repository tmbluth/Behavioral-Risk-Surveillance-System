# Cannot load rf_models.RData onto GitHub due to large size
load('intermediate_saves/rf_models.RData')
load('intermediate_saves/modeling_data.RData')

library(shiny)
library(ranger)
library(lime)
library(billboarder)

# need to create custom model_type function
model_type.ranger <- function(x, ...) {
  # Function tells lime() what model type we are dealing with
  # 'classification', 'regression', 'survival', 'clustering', 'multilabel', etc
  
  return("classification")
}

model_type(rf_db_3)

# need to create custom predict_model function
predict_model.ranger <- function(x, newdata, ...) {
  # Function performs prediction and returns data frame with Response
  pred <- predict(x, newdata)
  return(as.data.frame(pred$predictions))
}

predict_model(rf_db_3, newdata = db$Test_set[1:10,])

# User Interface ---------------------

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons('CHECKUP1', 
                   'About how long has it been since you last visited a doctor for a routine checkup? A routine checkup is a general physical exam not an exam for a specific injury illness or condition.',
                   choiceNames = c('Never', 'Within the last 12 months', 'Within the past 2 years', 'Within the past 5 years', '5 or more years ago'), 
                   choiceValues = c('0', '1', '2', '3', '4'), selected = character(0)),

      radioButtons('PERSDOC2', 
                   'Do you have one person you think of as your personal doctor or health care provider?',
                   choiceNames = c('Just one', 'More than one', 'No'), 
                   choiceValues = c('1', '2', '3'), selected = character(0)),
      
      radioButtons('MEDCOST', 
                   'Was there a time in the past 12 months when you needed to see a doctor but could not because of cost?',
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('1', '2'), selected = character(0)),

      radioButtons('CVDINFR4', 
                   'Has a health professional ever told you that you had a heart attack also called a myocardial infarction?',
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('1', '2'), selected = character(0)),
      
      radioButtons('CVDCRHD4', 
                   'Has a health professional ever told you that you had angina or coronary heart disease?',  
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('1', '2'), selected = character(0)),
      
      radioButtons('CVDSTRK3', 
                   'Has a health professional ever told you that  you had a stroke?',
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('1', '2'), selected = character(0)),
      
      radioButtons('ASTHMA3', 
                   'Has a health professional ever told you that you had asthma?',
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('1', '2'), selected = character(0)),

      radioButtons('CHCCOPD', 
                   'Has a health professional ever told you that you have COPD or emphysema or chronic bronchitis?', 
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('1', '2'), selected = character(0)),
      
      radioButtons('HAVARTH3', 
                   'Has a health professional ever told you that you have some form of arthritis?',
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('1', '2'), selected = character(0)),
      
      radioButtons('CHCKIDNY',
                   'Has a health professional ever told you that you have kidney disease? Do NOT include kidney stones bladder infection or incontinence.', 
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('1', '2'), selected = character(0)),
      
      radioButtons('DIABETE3', 
                   'Has a health professional ever told you that you have diabetes?',
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('1', '2'), selected = character(0)),
      
      radioButtons('ADDEPEV2', 
                   'Has a health professional ever told you that you have depression?',
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('1', '2'), selected = character(0)),
      
      radioButtons('X_RFHYPE5', 
                   'Has a health professional ever told you that you have high blood pressure?', 
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('2', '1'), selected = character(0)), 

      radioButtons('X_RFCHOL', 
                   'Has a health professional ever told you that you have high blood cholesterol?', 
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('2', '1'), selected = character(0)),   
      
      radioButtons('X_SMOKER3', 
                   'Do you smoke?', 
                   choiceNames = c('Yes, every day', 'Yes, sometimes', 'Formerly (100+ cigarettes in lifetime)', 'No'), 
                   choiceValues = c('1', '2', '3', '4'), selected = character(0)),
      
      radioButtons('X_RFBING5', 
                   'Considering all types of alcoholic beverages, have you had 5+ drinks (if male) or 4+ drinks (if female) on one occasion in the past 30 days?', 
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('2', '1'), selected = character(0)), 
      
      radioButtons('LIMITED', 
                   'Are you limited in any way in any activities because of physical, mental, or emotional problems?', 
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('1' ,'2'), selected = character(0)), 
      
      radioButtons('USEEQUIP', 
                   'Do you now have any health problem that ever requires you to use special equipment such as a cane a wheelchair a special bed or a special telephone?',
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('1' ,'2'), selected = character(0)), 
      
      radioButtons('SEX', 
                   'Your biological sex is:',
                   choiceNames = c('Male', 'Female'), 
                   choiceValues = c('1' ,'2'), selected = character(0)), 
      
      radioButtons('RACE', 
                   'Your race is:', 
                   choiceNames = c('White', 'Black', 'Asian', 'American Indian or Alaskan Native', 'Hispanic', 'Other race/Multiracial'),
                   choiceValues = c('1', '2', '3', '4', '5', '6'), selected = character(0)), 
      
      radioButtons('MARITAL', 
                   'You are:', 
                   choiceNames = c('Married', 'Divorced', 'Widowed', 'Separated', 'Never married', 'A member of an unmarried couple'), 
                   choiceValues = c('1', '2', '3', '4', '5', '6'), selected = character(0)), 
      
      radioButtons('X_AGE_G', 
                   'Your age is:',
                   choiceNames = c('18 to 24', '25 to 34', '35 to 44', '45 to 54', '55 to 64', '65 or older'), 
                   choiceValues = c('1', '2', '3', '4', '5', '6'), selected = character(0)), 
      
      radioButtons('EDUCA', 
                   'What is the highest grade or year of school you completed?',
                   choiceNames = c('Never attended school or only kindergarten',	'Grades 1 through 8',	'Grades 9 through 11',	'Grade 12 or GED',	'College/technical school 1 year to 3 years',	'College 4 years or more'), 
                   choiceValues = c('1', '2', '3', '4', '5', '6'), selected = character(0)), 
      
      radioButtons('RENTHOM1', 
                   'Do you own or rent your home?',  
                   choiceNames = c('Own', 'Rent', 'Other arrangement'),
                   choiceValues = c('1', '2', '3'), selected = character(0)),

      radioButtons('EMPLOYMENT', 
                   'You are currently:',
                   choiceNames = c('Employed for wages',	'Self-employed',	'Out of work for 1 year or more',	'Out of work for less than 1 year',	'A homemaker', 'A student',	'Retired',	'Unable to work'), 
                   choiceValues = c('1', '2', '3', '4', '5', '6', '7', '8'), selected = character(0)), 
      
      radioButtons('INCOME2', 
                   'Your annual household income from all sources is:',
                   choiceNames = c('$0', 'Less than $10k', 'Less than $15k',	'Less than $20k',	'Less than $25k',	'Less than $35k',	'Less than $50k',	'Less than $75k',	'$75k+'), 
                   choiceValues = c('0', '1', '2', '3', '4', '5', '6', '7', '8'), selected = character(0)), 
      
      radioButtons('X_BMI5CAT', 
                   'What is your BMI? (use any online BMI calculator)',
                   choiceNames = c('Less than 18.5', '18.5 to 24.9', '25 to 29.9', '30+'), 
                   choiceValues = c('1', '2', '3', '4'), selected = character(0)),  
      
      radioButtons('GENHLTH', 
                   'Would you say that in general your health is:', 
                   choiceNames = c('Excellent',	'Very Good',	'Good',	'Fair',	'Poor'), 
                   choiceValues = c('1', '2', '3', '4', '5'), selected = character(0)), 
      
      radioButtons('PHYSHLTH', 
                   'Now thinking about your physical health which includes physical illness and injury for how many days during the past 30 days was your physical health not good?',
                   choiceNames = c('0 to 5', '6 to 10', '11 to 15', '16 to 20', '21 to 25', '26 to 30'), 
                   choiceValues = c("[0,5]", "(5,10]", "(10,15]", "(15,20]", "(20,25]", "(25,30]"), selected = character(0)), 
      
      radioButtons('MENTHLTH', 
                   'Now thinking about your mental health which includes stress depression and problems with emotions â€“ for how many days during the past 30 days was your mental health not good?', 
                   choiceNames = c('0 to 5', '6 to 10', '11 to 15', '16 to 20', '21 to 25', '26 to 30'), 
                   choiceValues = c("[0,5]", "(5,10]", "(10,15]", "(15,20]", "(20,25]", "(25,30]"), selected = character(0)), 
      
      radioButtons('STRFREQ_', 
                   'How many strength building activities do you participate in per week?',
                   choiceNames = c('0 to 1', '2', '3', '4', '5', '6', '7'), 
                   choiceValues = c("(-Inf,1]", "(1,2]",  "(2,3]",  "(3,4]",  "(4,5]", "(6,7]", "(7, Inf]"), selected = character(0)),
      
      radioButtons('PA_BENCHMARK',
                   'How much do you normally exercise per week? (1 minute of moderate exercise = 1 minute, 1 minute of vigorous exercise = 2 minutes)',
                   choiceNames = c('0 minutes', '1 to 149 minutes', '150+ minutes'), 
                   choiceValues = c('3','2','1'), selected = character(0)),
      
      actionButton('calc', 'Calculate risk scores')
      ),
    
    mainPanel(
      verbatimTextOutput('db'),
      verbatimTextOutput('strk'),
      verbatimTextOutput('dep'),
      
      billboarderOutput('LIME')
    )
    
  )
)

server <- function(input, output){  
  db_df <- reactive({ 
           data.frame(GENHLTH = input$GENHLTH,   X_RFHYPE5 = input$X_RFHYPE5,  X_BMI5CAT = input$X_BMI5CAT,       X_RFCHOL = input$X_RFCHOL, EMPLOYMENT = input$EMPLOYMENT,
                      CHECKUP1 = input$CHECKUP1, USEEQUIP = input$USEEQUIP,    CVDINFR4 = input$CVDINFR4,         RACE = input$RACE,         PHYSHLTH = input$PHYSHLTH,
                      CVDCRHD4 = input$CVDCRHD4, SEX = input$SEX,              EDUCA = input$EDUCA,               PERSDOC2 = input$PERSDOC2, HAVARTH3 = input$HAVARTH3,
                      INCOME2 = input$INCOME2,   X_RFBING5 = input$X_RFBING5,  PA_BENCHMARK = input$PA_BENCHMARK, LIMITED = input$LIMITED,   STRFREQ_ = input$STRFREQ_)
  })
  
  output$db <- renderPrint({
    if(input$calc > 0){
      
      db_pred <- predict(rf_db_3, db_df(), type = 'response')[[1]][[1]]

      paste('You have about a', as.character(round(db_pred * 100, 1)), 'percent risk of diabetes')
    }
  })
  
  output$LIME <- renderBillboarder({
    
    # Run lime() on training set
    explainer <- lime::lime(
      x = db$Train_set,
      model = rf_db_3,
      bin_continuous = FALSE
    )
    
    # Run explain() on explainer
    set.seed(42)
    explanation <- lime::explain(
      db_df(), 
      explainer = explainer, 
      n_labels = 1, 
      n_features = length(db_df()),
      kernel_width = 0.5
    )
    
    type_pal <- c('Supports', 'Contradicts')
    explanation$type <- factor(ifelse(sign(explanation$feature_weight) == 
                                        1, type_pal[1], type_pal[2]), levels = type_pal)
    description <- paste0(explanation$case, "_", explanation$label)
    desc_width <- max(nchar(description)) + 1
    description <- paste0(format(description, width = desc_width), 
                          explanation$feature_desc)
    explanation$description <- factor(description, levels = description[order(abs(explanation$feature_weight))])
    explanation$case <- factor(explanation$case, unique(explanation$case))
    
    explanation_plot_df <- explanation %>%
      mutate(risk_predictor = case_when(
        (label == 'Yes' & type == 'Supports') | (label == 'No' & type == 'Contradicts') ~ 'More likely to churn',
        (label == 'Yes' & type == 'Contradicts') | (label == 'No' & type == 'Supports') ~ 'Less likely to churn'
      )) %>%
      arrange(-abs(feature_weight)) %>% 
      head(20)
    
    billboarder() %>%
      bb_barchart(
        data = explanation_plot_df,
        mapping = bbaes(x = feature_desc, y = feature_weight, group = churn_predictor),
        rotated = TRUE,
        stacked = TRUE
      ) %>%
      bb_colors_manual('Less likely to churn' = 'rgba(63, 182, 24, 0.7)', 'More likely to churn' = 'rgba(255, 0, 57, 0.7)')
  })
  
}   
# 
#   output$strk <- renderPrint({
#     if(input$calc > 0){
#       strk_df <- data.frame(EMPLOYMENT = input$EMPLOYMENT, CVDINFR4 = input$CVDINFR4,         X_AGE_G = input$X_AGE_G,     GENHLTH = input$GENHLTH,   CVDCRHD4 = input$CVDCRHD4,
#                             X_RFHYPE5 = input$X_RFHYPE5,   USEEQUIP = input$USEEQUIP,         LIMITED = input$LIMITED,     PHYSHLTH = input$PHYSHLTH, HAVARTH3 = input$HAVARTH3,
#                             DIABETE3 = input$DIABETE3,     INCOME2 = input$INCOME2,           MARITAL = input$MARITAL,     X_RFCHOL = input$X_RFCHOL, CHCCOPD = input$CHCCOPD,
#                             EDUCA = input$EDUCA,           PA_BENCHMARK = input$PA_BENCHMARK, X_SMOKER3 = input$X_SMOKER3, RENTHOM1 = input$RENTHOM1, ADDEPEV2 = input$ADDEPEV2)
#       strk_pred <- predict(rf_strk_3, strk_df, type = 'response')[[1]][[1]]
# 
#       paste('You have about a', as.character(round(strk_pred * 100, 1)), 'percent risk of stroke')
#     }
#   })
# 
#   output$dep <- renderPrint({
#     if(input$calc > 0){
#       dep_df <- data.frame(MENTHLTH = input$MENTHLTH, LIMITED = input$LIMITED,   EMPLOYMENT = input$EMPLOYMENT, X_AGE_G = input$X_AGE_G,     SEX = input$SEX,
#                            GENHLTH = input$GENHLTH,   HAVARTH3 = input$HAVARTH3, X_SMOKER3 = input$X_SMOKER3,   MEDCOST = input$MEDCOST,     MARITAL = input$MARITAL,
#                            PHYSHLTH = input$PHYSHLTH, RACE = input$RACE,         X_RFCHOL = input$X_RFCHOL,     USEEQUIP = input$USEEQUIP,   ASTHMA3 = input$ASTHMA3,
#                            RENTHOM1 = input$RENTHOM1, CHCCOPD = input$CHCCOPD,   INCOME2 = input$INCOME2,       X_BMI5CAT = input$X_BMI5CAT, X_RFHYPE5 = input$X_RFHYPE5,
#                            PA_BENCHMARK = input$PA_BENCHMARK, PERSDOC2 = input$PERSDOC2)
#       dep_pred <- predict(rf_dep_3, dep_df, type = 'response')[[1]][[1]]
# 
#       paste('You have about a', as.character(round(dep_pred * 100, 1)), 'percent risk of depression')
#     }
#   })




shinyApp(ui = ui, server = server) 



# TEST ----------
explainer <- lime::lime(
  x = db$Train_set,
  model = rf_db_3,
  bin_continuous = FALSE
)

# Run explain() on explainer
set.seed(42)
explanation <- lime::explain(
  db$Test_set[1,], 
  explainer = explainer, 
  n_labels = 1, 
  n_features = length(db$Test_set),
  kernel_width = 0.5
)

type_pal <- c('Supports', 'Contradicts')
explanation$type <- factor(ifelse(sign(explanation$feature_weight) == 
                                    1, type_pal[1], type_pal[2]), levels = type_pal)
description <- paste0(explanation$case, "_", explanation$label)
desc_width <- max(nchar(description)) + 1
description <- paste0(format(description, width = desc_width), 
                      explanation$feature_desc)
explanation$description <- factor(description, levels = description[order(abs(explanation$feature_weight))])
explanation$case <- factor(explanation$case, unique(explanation$case))

explanation_plot_df <- explanation %>%
  mutate(risk_predictor = case_when(
    (label == 'Yes' & type == 'Supports') | (label == 'No' & type == 'Contradicts') ~ 'More likely to churn',
    (label == 'Yes' & type == 'Contradicts') | (label == 'No' & type == 'Supports') ~ 'Less likely to churn'
  )) %>%
  arrange(-abs(feature_weight)) %>% 
  head(20)

billboarder() %>%
  bb_barchart(
    data = explanation_plot_df,
    mapping = bbaes(x = feature_desc, y = feature_weight, group = churn_predictor),
    rotated = TRUE,
    stacked = TRUE
  ) %>%
  bb_colors_manual('Less likely to churn' = 'rgba(63, 182, 24, 0.7)', 'More likely to churn' = 'rgba(255, 0, 57, 0.7)')

