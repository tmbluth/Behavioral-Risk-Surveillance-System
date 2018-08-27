# Load either normal data sets or down-sampled data sets with their respective models
load('intermediate_saves/rf_models.RData')
load('intermediate_saves/modeling_data.RData')

#load('intermediate_saves/modeling_down_data.RData')
#load('intermediate_saves/rf_models_down.RData')

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
predict_model(rf_db, newdata = db$Test[1,])$Diagnosed

# User Interface ---------------------

#options(shiny.error = browser)

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons('Diabetes', 
                   'Has a health professional ever told you that you have diabetes?',
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('Diagnosed', 'Undiagonosed'),
                   selected = character(0)),
      
      radioButtons('Stroke', 
                   'Has a health professional ever told you that  you had a stroke?',
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('Diagnosed', 'Undiagonosed'), 
                   selected = character(0)),
      
      radioButtons('Depression', 
                   'Has a health professional ever told you that you have depression?',
                   choiceNames = c('Yes', 'No'), 
                   choiceValues = c('Diagnosed', 'Undiagonosed'), 
                   selected = character(0)),

      radioButtons('Medical_Cost', 
                   'Was there a time in the past 12 months when you needed to see a doctor but could not because of cost?',
                   choices = c('Yes', 'No'), 
                   selected = character(0)),

      radioButtons('Heart_Attack', 
                   'Has a health professional ever told you that you had a heart attack also called a myocardial infarction?',
                   choices = c('Yes', 'No'), 
                   selected = character(0)),
      
      radioButtons('Heart_Disease', 
                   'Has a health professional ever told you that you had angina or coronary heart disease?',  
                   choices = c('Yes', 'No'), 
                   selected = character(0)),

      radioButtons('COPD', 
                   'Has a health professional ever told you that you have COPD or emphysema or chronic bronchitis?', 
                   choices = c('Yes', 'No'), 
                   selected = character(0)),
      
      radioButtons('Arthritis', 
                   'Has a health professional ever told you that you have some form of arthritis?',
                   choices = c('Yes', 'No'), 
                   selected = character(0)),
      
      radioButtons('Hypertension', 
                   'Has a health professional ever told you that you have high blood pressure?', 
                   choices = c('Yes', 'No'), 
                   selected = character(0)), 
      
      radioButtons('Smoker', 
                   'Do you smoke?', 
                   choices = c('Yes, every day', 'Yes, sometimes', 'Formerly (100+ cigarettes in lifetime)', 'No'), 
                   selected = character(0)),   
      
      sliderInput('Fruits', 
                  'Think of the last 30 days. On average, what was your fruit intake in times per day',
                  min = 0, max = 10, value = 5),
      
      sliderInput('Fruit_Juice', 
                  'Think of the last 30 days. On average, what was your fruit juice intake in times per day',
                  min = 0, max = 10, value = 5),
      
      sliderInput('Greens',
                  'Think of the last 30 days. On average, what was your green vegetable intake in times per day',
                  min = 0, max = 10, value = 5),
      
      radioButtons('Limited', 
                   'Are you limited in any way in any activities because of physical, mental, or emotional problems?', 
                   choices = c('Yes', 'No'), 
                   selected = character(0)), 
      
      radioButtons('Need_Equipment', 
                   'Do you now have any health problem that ever requires you to use special equipment such as a cane a wheelchair a special bed or a special telephone?',
                   choices = c('Yes', 'No'), 
                   selected = character(0)), 
      
      radioButtons('Sex', 
                   'Your biological sex is:',
                   choices = c('Male', 'Female'), 
                   selected = character(0)), 
      
      radioButtons('Race', 
                   'Your race is:', 
                   choices = c('White', 'Black', 'Asian', 'American Indian or Alaskan Native', 'Hispanic', 'Other race/Multiracial'),
                   selected = character(0)), 
      
      radioButtons('Marital', 
                   'You are:', 
                   choices = c('Married', 'Divorced', 'Widowed', 'Separated', 'Never married', 'A member of an unmarried couple'), 
                   selected = character(0)), 
      
      radioButtons('Age_Group', 
                   'Your age is:',
                   choices = c('18 to 24', '25 to 34', '35 to 44', '45 to 54', '55 to 64', '65 or older'), 
                   selected = character(0)), 
      
      radioButtons('Education', 
                   'What is the highest grade or year of school you completed?',
                   choices = c('Never attended school or only kindergarten',	'Grades 1 through 8',	'Grades 9 through 11',	'Grade 12 or GED',	'College/technical school 1 year to 3 years',	'College 4 years or more'), 
                   selected = character(0)), 

      radioButtons('Employment', 
                   'You are currently:',
                   choices = c('Employed for wages',	'Self-employed',	'Out of work for 1 year or more',	'Out of work for less than 1 year',	'A homemaker', 'A student',	'Retired',	'Unable to work'), 
                   selected = character(0)), 

      radioButtons('BMI', 
                   'What is your BMI? (use any online BMI calculator)',
                   choices = c('Less than 18.5', '18.5 to 24.9', '25 to 29.9', '30+'), 
                   selected = character(0)),  
      
      radioButtons('General_Health', 
                   'Would you say that in general your health is:', 
                   choices = c('Excellent',	'Very Good',	'Good',	'Fair',	'Poor'), 
                   selected = character(0)), 
      
      sliderInput('Physical_Health', 
                  'Now thinking about your physical health which includes physical illness and injury; for how many days during the past 30 days was your physical health not good?',
                  min = 0, max = 30, value = 15), 
      
      sliderInput('Mental_Health', 
                  'Now thinking about your mental health which includes stress depression and problems with emotions; for how many days during the past 30 days was your mental health not good?', 
                  min = 0, max = 30, value = 15), 
      
      
      actionButton('calc', 'Calculate risk scores')
      ),
    
    mainPanel(
      verbatimTextOutput('db')
      #, plotOutput('db_LIME')
      , verbatimTextOutput('strk')
      #, plotOutput('strk_LIME')
      , verbatimTextOutput('dep')
      #, plotOutput('dep_LIME')
    )
    
  )
)

server <- function(input, output){    
  
  survey <- reactive({
            data.frame(Diabetes = input$Diabetes,               Stroke = input$Stroke,                 Depression = input$Depression,   Medical_Cost = input$Medical_Cost, Heart_Attack = input$Heart_Attack,
                         Heart_Disease = input$Heart_Disease,     COPD = input$COPD,                     Arthritis = input$Arthritis,     Hypertension = input$Hypertension, Smoker = input$Smoker,         
                         Limited = input$Limited,                 Need_Equipment = input$Need_Equipment, Sex = input$Sex,                 Race = input$Race,                 Marital = input$Marital,      
                         Age_Group =input$Age_Group,              Education = input$Education,           Employment = input$Employment,   BMI = input$BMI,                   General_Health = input$General_Health,
                         Physical_Health = input$Physical_Health, Mental_Health = input$Mental_Health,   Fruits = input$Fruits,           Fruit_Juice = input$Fruit_Juice,   Greens = input$Greens)
            })
  
  ### Diabetes ###
  output$db <- renderPrint({
    if(input$calc > 0){
      db_pred <- predict_model(x = rf_db, newdata = db$Test, type = 'prob')$Diagnosed
      breaks <- unique(c(quantile(db_pred, probs = seq(0,1,1/5)),1))
      labels <- c('100%', '80 to 99%', '60 to 79%', '40 to 59%', '20 to 39%', '0 to 19%')
      my_pred <- predict_model(x = rf_db, newdata = survey(), type = 'prob')$Diagnosed 
      paste('With a risk of', as.character(round(my_pred,2)), 'you are better off than', as.character(cut(my_pred, breaks = breaks, labels = labels)), 'of adult Americans for chance of getting diabetes')
    }
  })

  # output$db_LIME <- renderPlot({
  #   if(input$calc > 0){
  #     plot_lime(Model = rf_db, Train_set = db$Train, Sample = survey())
  #   }
  # })
  
### Stroke ###

  output$strk <- renderPrint({
    if(input$calc > 0){
      strk_pred <- predict_model(x = rf_strk, newdata = strk$Test, type = 'prob')$Diagnosed
      breaks <- unique(c(quantile(strk_pred, probs = seq(0,1,1/5)),1))
      labels <- c('100%', '80 to 99%', '60 to 79%', '40 to 59%', '20 to 39%', '0 to 19%') 
      my_pred <- predict_model(x = rf_strk, newdata = survey(), type = 'prob')$Diagnosed 
      paste('With a risk of', as.character(round(my_pred,2)), 'you are better off than', as.character(cut(my_pred, breaks = breaks, labels = labels)), 'of adult Americans for chance of having a stroke')
    }
  })

  # output$strk_LIME <- renderPlot({
  #   if(input$calc > 0){
  #     plot_lime(Model = rf_strk, Train_set = strk$Train, Sample = survey())
  #   }
  # })
  
### Depression ###

  output$dep <- renderPrint({ 
    if(input$calc > 0){
      dep_pred <- predict_model(x = rf_dep, newdata = dep$Test, type = 'prob')$Diagnosed
      breaks <- unique(c(quantile(dep_pred, probs = seq(0,1,1/5)),1))
      labels <- c('100%', '80 to 99%', '60 to 79%', '40 to 59%', '20 to 39%', '0 to 19%') 
      my_pred <- predict_model(x = rf_dep, newdata = survey(), type = 'prob')$Diagnosed 
      paste('With a risk of', as.character(round(my_pred,2)), 'you are better off than', as.character(cut(my_pred, breaks = breaks, labels = labels)), 'of adult Americans for chance of having depression')
    }  
  }) 

#   output$dep_LIME <- renderPlot({
#     if(input$calc > 0){
#       plot_lime(Model = rf_dep, Train_set = dep$Train, Sample = survey())
#     }
#   })
}


shinyApp(ui = ui, server = server)  
