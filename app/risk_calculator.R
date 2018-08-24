# Cannot load rf_models.RData onto GitHub due to large size
load('intermediate_saves/rf_models.RData')
load('intermediate_saves/modeling_data.RData')

library(shiny)
library(ranger)
library(tidyverse)
library(lime)

# Create custom model_type function
model_type.ranger <- function(x, ...) {
  # Function tells lime() what model type we are dealing with
  return("classification")
}

model_type(rf_db)

# Custom predict_model function
predict_model.ranger <- function(x, newdata, ...) {
  # Function performs prediction and returns data frame with Response
  pred <- predict(x, newdata)
  return(as.data.frame(pred$predictions))
}

# Test LIME's predict function with ranger model
predict_model(rf_db, newdata = db$Test_set[1,])$Diagnosed

# User Interface ---------------------

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons('DIABETE3', 
                   'Has a health professional ever told you that you have diabetes?',
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('Diagnosed', 'Undiagonosed'),
                   selected = character(0)),
      
      radioButtons('CVDSTRK3', 
                   'Has a health professional ever told you that  you had a stroke?',
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('Diagnosed', 'Undiagonosed'), 
                   selected = character(0)),
      
      radioButtons('ADDEPEV2', 
                   'Has a health professional ever told you that you have depression?',
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('Diagnosed', 'Undiagonosed'), 
                   selected = character(0)),
      
      radioButtons('CHECKUP1', 
                   'About how long has it been since you last visited a doctor for a routine checkup? A routine checkup is a general physical exam not an exam for a specific injury illness or condition.',
                   choices = c('Never', 'Within the last 12 months', 'Within the past 2 years', 'Within the past 5 years', '5 or more years ago'), 
                   selected = character(0)),

      radioButtons('MEDCOST', 
                   'Was there a time in the past 12 months when you needed to see a doctor but could not because of cost?',
                   choices = c('Yes', 'No'), 
                   selected = character(0)),

      radioButtons('CVDINFR4', 
                   'Has a health professional ever told you that you had a heart attack also called a myocardial infarction?',
                   choices = c('Yes', 'No'), 
                   selected = character(0)),
      
      radioButtons('CVDCRHD4', 
                   'Has a health professional ever told you that you had angina or coronary heart disease?',  
                   choices = c('Yes', 'No'), 
                   selected = character(0)),

      radioButtons('CHCCOPD', 
                   'Has a health professional ever told you that you have COPD or emphysema or chronic bronchitis?', 
                   choices = c('Yes', 'No'), 
                   selected = character(0)),
      
      radioButtons('HAVARTH3', 
                   'Has a health professional ever told you that you have some form of arthritis?',
                   choices = c('Yes', 'No'), 
                   selected = character(0)),
      
      radioButtons('X_RFHYPE5', 
                   'Has a health professional ever told you that you have high blood pressure?', 
                   choices = c('Yes', 'No'), 
                   selected = character(0)), 
      
      radioButtons('X_SMOKER3', 
                   'Do you smoke?', 
                   choices = c('Yes, every day', 'Yes, sometimes', 'Formerly (100+ cigarettes in lifetime)', 'No'), 
                   selected = character(0)),   
      
      sliderInput('FTJUDA1_', 
                  'Fruit juice intake in times per day',
                  min = 0, max = 10, value = 5),
      
      sliderInput('GRENDAY_',
                  'Dark green vegetable intake in times per day',
                  min = 0, max = 10, value = 5),
      
      radioButtons('LIMITED', 
                   'Are you limited in any way in any activities because of physical, mental, or emotional problems?', 
                   choices = c('Yes', 'No'), 
                   selected = character(0)), 
      
      radioButtons('USEEQUIP', 
                   'Do you now have any health problem that ever requires you to use special equipment such as a cane a wheelchair a special bed or a special telephone?',
                   choices = c('Yes', 'No'), 
                   selected = character(0)), 
      
      radioButtons('SEX', 
                   'Your biological sex is:',
                   choices = c('Male', 'Female'), 
                   selected = character(0)), 
      
      radioButtons('RACE', 
                   'Your race is:', 
                   choices = c('White', 'Black', 'Asian', 'American Indian or Alaskan Native', 'Hispanic', 'Other race/Multiracial'),
                   selected = character(0)), 
      
      radioButtons('MARITAL', 
                   'You are:', 
                   choices = c('Married', 'Divorced', 'Widowed', 'Separated', 'Never married', 'A member of an unmarried couple'), 
                   selected = character(0)), 
      
      radioButtons('X_AGE_G', 
                   'Your age is:',
                   choices = c('18 to 24', '25 to 34', '35 to 44', '45 to 54', '55 to 64', '65 or older'), 
                   selected = character(0)), 
      
      radioButtons('EDUCA', 
                   'What is the highest grade or year of school you completed?',
                   choices = c('Never attended school or only kindergarten',	'Grades 1 through 8',	'Grades 9 through 11',	'Grade 12 or GED',	'College/technical school 1 year to 3 years',	'College 4 years or more'), 
                   selected = character(0)), 

      radioButtons('EMPLOYMENT', 
                   'You are currently:',
                   choices = c('Employed for wages',	'Self-employed',	'Out of work for 1 year or more',	'Out of work for less than 1 year',	'A homemaker', 'A student',	'Retired',	'Unable to work'), 
                   selected = character(0)), 

      radioButtons('X_BMI5CAT', 
                   'What is your BMI? (use any online BMI calculator)',
                   choices = c('Less than 18.5', '18.5 to 24.9', '25 to 29.9', '30+'), 
                   selected = character(0)),  
      
      radioButtons('GENHLTH', 
                   'Would you say that in general your health is:', 
                   choices = c('Excellent',	'Very Good',	'Good',	'Fair',	'Poor'), 
                   selected = character(0)), 
      
      sliderInput('PHYSHLTH', 
                  'Now thinking about your physical health which includes physical illness and injury; for how many days during the past 30 days was your physical health not good?',
                  min = 0, max = 30, value = 15), 
      
      sliderInput('MENTHLTH', 
                  'Now thinking about your mental health which includes stress depression and problems with emotions; for how many days during the past 30 days was your mental health not good?', 
                  min = 0, max = 30, value = 15), 
      
      
      actionButton('calc', 'Calculate risk scores')
      ),
    
    mainPanel(
      verbatimTextOutput('db')
      , plotOutput('db_LIME')
      #, verbatimTextOutput('strk')
      #, billboarderOutput('strk_LIME')
      #, verbatimTextOutput('dep')
      #, billboarderOutput('dep_LIME')
    )
    
  )
)

server <- function(input, output){   
  
  ### Diabetes ###
  output$db <- renderPrint({
    if(input$calc > 0){
      db_df <- data.frame(General_Health = input$GENHLTH,    Hypertension = input$X_RFHYPE5,   Age_Group = input$X_AGE_G,        BMI = input$X_BMI5CAT,            Employment = input$EMPLOYMENT,
                       Recent_Checkup = input$CHECKUP1,   Need_Equipment = input$USEEQUIP,  Physical_Health = input$PHYSHLTH, Heart_Attack = input$CVDINFR4,    Race = input$RACE,                 
                       Heart_Disease = input$CVDCRHD4,    Fruit_Juice = input$FTJUDA1_,     Sex = input$SEX,                  Education = input$EDUCA,          Arthritis = input$HAVARTH3,            
                       Limited = input$LIMITED)
      db_pred <- predict_model(x = rf_db, newdata = db_df, type = 'prob')$Diagnosed
      paste('You have about a', as.character(round(db_pred * 100, 0)), 'percent risk of diabetes')
    }
  })

  output$db_LIME <- renderPlot({ 
    if(input$calc > 0){
      
      db_df <- data.frame(General_Health = input$GENHLTH,    Hypertension = input$X_RFHYPE5,   Age_Group = input$X_AGE_G,        BMI = input$X_BMI5CAT,            Employment = input$EMPLOYMENT,
                          Recent_Checkup = input$CHECKUP1,   Need_Equipment = input$USEEQUIP,  Physical_Health = input$PHYSHLTH, Heart_Attack = input$CVDINFR4,    Race = input$RACE,                 
                          Heart_Disease = input$CVDCRHD4,    Fruit_Juice = input$FTJUDA1_,     Sex = input$SEX,                  Education = input$EDUCA,          Arthritis = input$HAVARTH3,            
                          Limited = input$LIMITED)
      
      explainer <- lime::lime(
        x = db$Train_set,
        model = rf_db,
        bin_continuous = FALSE
      )
      
      # Run explain() on explainer
      explanation <- lime::explain( 
        x = db_df,
        explainer = explainer, 
        n_labels = 1, 
        n_features = 15,
        kernel_width = 0.5
      ) 
      
      type_pal <- c('Contradicts', 'Supports') 
      explanation$Type <- factor(ifelse(sign(explanation$feature_weight) == 
                                               -1, type_pal[1], type_pal[2]), levels = type_pal) 
      explanation_plot_df <- explanation %>%
        mutate(Churn_Predictor = case_when(
          (label == 'Diagnosed' & type == 'Supports') | (label == 'Undiagnosed' & type == 'Contradicts') ~ 'More likely',
          (label == 'Diagnosed' & type == 'Contradicts') | (label == 'Undiagnosed' & type == 'Supports') ~ 'Less likely')
        )  
      
      ggplot(explanation_plot_df, aes(x = reorder(feature_desc, abs(feature_weight)), y = feature_weight)) +
        geom_bar(stat = "identity", aes(fill = Churn_Predictor)) + 
        scale_y_continuous('Feature Weight') +
        scale_x_discrete('') +
        ggtitle('Diabetes') +
        coord_flip()
    }
  })
  
  # ### Stroke ###
  # strk_df <- reactive({
  #            data.frame(Heart_Attack = input$CVDINFR4,  Employment = input$EMPLOYMENT,   Age_Group = input$X_AGE_G, General_Health = input$GENHLTH,   Heart_Disease = input$CVDCRHD4,
  #                       Hypertension = input$X_RFHYPE5, Need_Equipment = input$USEEQUIP, Limited = input$LIMITED,   Physical_Health = input$PHYSHLTH, Arthritis = input$HAVARTH3,
  #                       Diabetes = input$DIABETE3,      COPD = input$CHCCOPD,            Greens = input$GRENDAY_,   Marital = input$MARITAL,          Education = input$EDUCA)
  # })
  #   
  # output$strk <- renderPrint({ 
  #   if(input$calc > 0){
  #     strk_pred <- predict_model(x = rf_strk, newdata = strk_df(), type = 'prob')$Diagnosed
  #     paste('You have a', as.character(cut(strk_pred, breaks = c(0, 0.5, 0.75, 1), labels = c('lower', 'average', 'higher'))), 'risk of stroke')
  #   } 
  # }) 

  
#   ### Depression ###
#   dep_df <- reactive({
#             data.frame(Mental_Health = input$MENTHLTH, Limited = input$LIMITED,    Sex = input$SEX,                  Employment = input$EMPLOYMENT,     Age_Group = input$X_AGE_G,
#                        General_Health = input$GENHLTH, Arthritis = input$HAVARTH3, Physical_Health = input$PHYSHLTH, Smoker = input$X_SMOKER3,          Marital = input$MARITAL,
#                        Medical_Cost = input$MEDCOST,   Race = input$RACE,          Need_Equipment = input$USEEQUIP,  COPD = input$CHCCOPD,              BMI = input$X_BMI5CAT)  
#   })
#   
#   output$dep <- renderPrint({
#     if(input$calc > 0){
#       dep_pred <- predict_model(x = rf_dep, newdata = dep_df(), type = 'prob')$Diagnosed
#       paste('You have about a', as.character(round(dep_pred * 100, 0)), 'percent risk of depression')
#     }
#   })
}


shinyApp(ui = ui, server = server) 



 # TEST ----------
explainer <- lime::lime(
  x = db$Train_set,
  model = rf_db,
  bin_continuous = FALSE
)

# Run explain() on explainer
explanation <- lime::explain(
  x = db$Test_set[2,],
  explainer = explainer, 
  n_labels = 1, 
  n_features = ncol(db$Test_set[2,]),
  kernel_width = 0.5
)

type_pal <- c('Contradicts', 'Supports')
explanation$Type <- factor(ifelse(sign(explanation$feature_weight) == 
                                         -1, type_pal[1], type_pal[2]), levels = type_pal)
explanation_plot_df <- explanation %>%
  mutate(Churn_Predictor = case_when(
    (label == 'Diagnosed' & type == 'Supports') | (label == 'Undiagnosed' & type == 'Contradicts') ~ 'More likely',
    (label == 'Diagnosed' & type == 'Contradicts') | (label == 'Undiagnosed' & type == 'Supports') ~ 'Less likely')
    ) 

ggplot(explanation_plot_df, aes(x = reorder(feature_desc, abs(feature_weight)), y = feature_weight)) +
  geom_bar(stat = "identity", aes(fill = Churn_Predictor), legend = FALSE) + 
  scale_y_continuous('Feature Weight') +
  scale_x_discrete('') +
  ggtitle('') +
  coord_flip()
