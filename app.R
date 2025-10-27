# Lanette Tyler
# NCSU ST558
# Project 2
# Shiny App
# Mobile Device Usage and User Behavior Dataset

# Load packages -----------------------------------------------------------------

library(tidyverse)
library(shiny)
library(labelled)
library(bslib)


# Load dataset -------------------------------------------------------------------

user_behavior_dataset <- read_csv("user_behavior_dataset.csv")


#Transform data --------------------------------------------------------------------

#rename variables, set appropriate data formats
phone_data <- user_behavior_dataset |>
  rename("user_id" = "User ID", "model" = "Device Model", "op_system" = "Operating System", "app_usage_time" = "App Usage Time (min/day)", "screen_on_time" = "Screen On Time (hours/day)", "batt_drain" = "Battery Drain (mAh/day)", "no_apps" = "Number of Apps Installed", "data_usage" = "Data Usage (MB/day)", "age" = "Age", "gender" = "Gender", "user_class" = "User Behavior Class") |>
  mutate(user_id = as.character(user_id), model = as.factor(model), op_system = as.factor(op_system), gender = as.factor(gender), user_class = as.factor(user_class))

#add labels to variables
var_label(phone_data) <- list(user_id = "User ID", model = "Device Model", op_system = "Operating System", app_usage_time = "App Usage Time (min/day)", screen_on_time = "Screen On Time (hours/day)", batt_drain = "Battery Drain (mAh/day)", no_apps = "Number of Apps Installed", data_usage = "Data Usage (MB/day)", age = "Age", gender = "Gender", user_class = "User Behavior Class")


# Define UI for app  -------------------------------------------------------------
ui <- fluidPage(
  
  # App title --------------------------------------------------------------------
  titlePanel("Explore Cell Phone Usage and Behavior"),
  
  # Sidebar panel for inputs -----------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      tags$h3("User Inputs"),
      
      #choose cat vars
      radioButtons(
        inputId = "cat_var",
        label = "Choose categorical variable(s):",
        choices = c("Gender" = "gender",
                    "Operating System" = "op_system",
                    "Both" = "both"),
        selected = "both"),
      
      #choose num vars
      radioButtons(
        inputId = "num_var1",
        label = "Choose numeric variable:",
        choices = c("Age" = "age",
                    "App Usage Time (min/day)" = "app_usage_time",
                    "Number of Apps Installed" = "no_apps"),
        selected = NULL
      ),
      
      #if user selects age
      conditionalPanel(
        condition = "input.num_var1 == 'age'",
        sliderInput(
          inputId = "nv1_age",
          label = "Choose age range:",
          min = 18,
          max = 59,
          value = c(18, 59),
          step = 1
        )
      ),
      
      #if user selects app_usage_time
      conditionalPanel(
        condition = "input.num_var1 == 'app_usage_time'",
        sliderInput(
          inputId = "nv1_app_usage_time",
          label = "Choose app usage time range, min/day:",
          min = 30,
          max = 598,
          value = c(30, 598),
        )
      ),
      
      #if user selects no_apps
      conditionalPanel(
        condition = "input.num_var1 == 'no_apps'",
        sliderInput(
          inputId = "nv1_no_of_apps",
          label = "Choose range for number of apps installed:",
          min = 10,
          max = 99,
          value = c(10, 99)
        )
      )
      
      ),
    
    mainPanel(
      textOutput("text1"),
      textOutput("text2")
    )
    
    )
  )



# Define server logic -----------------------------------------------------------
server <- function(input, output, session) {
  
  output$text1 <- renderText ({
    paste("You have selected categorical variable", input$cat_var,".")
  })
  
  output$text2 <- renderText ({
    paste("You have selected categorical variable", input$num_var1,".")
  })
  
  
  
}



#Create shiny app ----------------------------------------------------------------
shinyApp(ui = ui, server = server)