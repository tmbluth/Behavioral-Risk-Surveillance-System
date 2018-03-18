library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Chronic Disease Risk Calculator"),
  
  sidebarPanel(
    # This is intentionally an empty object.
    h6(textOutput("save.results")),
    h5("Created by:"),
    tags$a("Taylor Bluth", 
           href="https://www.linkedin.com/in/tmbluth"),
    h5("Github Repository:"),
    tags$a("Chronic Disease Risk Calculator", 
           href= "https://github.com/tmbluth/Behavioral-Risk-Surveillance-System"),
    # Display the page counter text.
    h5(textOutput("counter"))
  ),
  
  
  # Show a table summarizing the values entered
  mainPanel(
    # Main Action is where most everything is happenning in the
    # object (where the welcome message, survey, and results appear)
    uiOutput("MainAction"),
    # This displays the action putton Next.
    actionButton("Click.Counter", "Next")    
  )
))

