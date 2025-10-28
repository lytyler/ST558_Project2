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
      checkboxGroupInput(
        inputId = "cat_var",
        label = "Choose Categories:",
        choices = c("Female" = "female",
                    "Male" = "male",
                    "iOS" = "ios",
                    "Android" = "android"),
        selected = "all"),
      br(),
        
      #choose first num var
      selectizeInput(
        inputId = "num_var1",
        label = "Choose Measurement:",
        choices = c("Choose One" = "",
                    "Age" = "age",
                    "App Usage Time (min/day)" = "app_usage_time",
                    "Number of Apps Installed" = "no_apps"),
        selected = NULL
      ),
      
      #if user selects age
      conditionalPanel(
        condition = "input.num_var1 == 'age'",
        sliderInput(
          inputId = "nv1_range",
          label = "Select Age Range:",
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
          inputId = "nv1_range",
          label = "Select Range of App Usage Time (min/day):",
          min = 30,
          max = 598,
          value = c(30, 598),
        )
      ),
      
      #if user selects no_apps
      conditionalPanel(
        condition = "input.num_var1 == 'no_apps'",
        sliderInput(
          inputId = "nv1_range",
          label = "Select Range for Number of Apps Installed:",
          min = 10,
          max = 99,
          value = c(10, 99)
        )
      ),
      br(),
      
      #select second num var
      uiOutput("num_selector2"),
      
      #range slider for second num var
      uiOutput("slidey2"),
      br(),
      
      #action button to generate internal dataset
      tags$b("Generate Dataset:"),
      tags$h5("Once you have selected categories, measurements, and measurement ranges, click the button to create your dataset."),
      actionButton(
        inputId = "dataButton",
        label = "Create Specified Dataset",
#        disabled = TRUE
      )
      ),
    
    mainPanel(
      tableOutput("table")
    )
    
    )
  )



# Define server logic -----------------------------------------------------------
server <- function(input, output, session) {
  
  #choose second num var
  
  output$num_selector2 <- renderUI({
    if (input$num_var1 == "age") {
      selectizeInput(
        inputId = "num_var2",
        label = "Choose Another Measurement:",
        choices = c("Choose One" = "",
                    "App Usage Time (min/day)" = "app_usage_time",
                    "Number of Apps Installed" = "no_apps"),
        selected = NULL
      )
    } else if (input$num_var1 == "app_usage_time") {
      selectizeInput(
        inputId = "num_var2",
        label = "Choose Another Measurement:",
        choices = c("Choose One" = "",
                    "Age" = "age",
                    "Number of Apps Installed" = "no_apps"),
        selected = NULL
      )
    } else if (input$num_var1 == "no_apps") {
      selectizeInput(
        inputId = "num_var2",
        label = "Choose Another Measurement:",
        choices = c("Choose One" = "",
                    "Age" = "age",
                    "App Usage Time (min/day)" = "app_usage_time"),
        selected = NULL
      )
    }
    
  })
  
  # add slider for second variable
  output$slidey2 <- renderUI({
    req(input$num_var2)
    if (input$num_var2 == "age") {
      sliderInput(
        inputId = "nv2_range",
        label = "Select Age Range:",
        min = 18,
        max = 59,
        value = c(18, 59),
        step = 1
      )
    } else if (input$num_var2 == "app_usage_time") {
      sliderInput(
        inputId = "nv2_range",
        label = "Select Range of App Usage Time (min/day):",
        min = 30,
        max = 598,
        value = c(30, 598),
      )
    } else if (input$num_var2 == "no_apps") {
      sliderInput(
        inputId = "nv2_range",
        label = "Select Range for Number of Apps Installed:",
        min = 10,
        max = 99,
        value = c(10, 99)
      )
    }
  })
  
  # DO i need this?!?!?!
  #enable action button to create dataset once user has entered preferences
#  updateActionButton(session, enableDataButton, disabled = FALSE)
  
  observeEvent(input$dataButton,{
   
    #filter data rows based on user input -----------------------------------------------
    user_data <- phone_data #initialize user_data dataset as a copy of original dataset
    
    #cat vars
    #filter for f or m in gender if only one is chosen
    if ("female" %in% input$cat_var & !("male" %in% input$cat_var)) {
      user_data <- filter(user_data, gender == "Female")
    }
    if(!("female" %in% input$cat_var) & "male" %in% input$cat_var) {
      user_data <- filter(user_data, gender == "Male")
    }
    
    #filter for ios or android in op_system if only one is chosen
    if ("android" %in% input$cat_var & !("ios" %in% input$cat_var)) {
      user_data <- filter(user_data, op_system == "Female")
    }
    if(!("female" %in% input$cat_var) & "male" %in% input$cat_var) {
      user_data <- filter(user_data, gender == "Male")
    }
    
    #filter num vars
    #filter for age
    if(input$num_var1 == "age") {
      user_data <- filter(user_data, age >= input$nv1_range[1] & age <= input$nv1_range[2])
    }
    if(input$num_var2 == "age") {
      user_data <- filter(user_data, age >= input$nv2_range[1] & age <= input$nv2_range[2])
    }
    
    #filter for app_usage_time
    if(input$num_var1 == "app_usage_time") {
      user_data <- filter(user_data, app_usage_time >= input$nv1_range[1] & app_usage_time <= input$nv1_range[2])
    }
    if(input$num_var2 == "app_usage_time") {
      user_data <- filter(user_data, app_usage_time >= input$nv2_range[1] & app_usage_time <= input$nv2_range[2])
    }
    
    #filter for no_apps
    if(input$num_var1 == "no_apps") {
      user_data <- filter(user_data, no_apps >= input$nv1_range[1] & no_apps <= input$nv1_range[2])
    }
    if(input$num_var2 == "no_apps") {
      user_data <- filter(user_data, no_apps >= input$nv2_range[1] & no_apps <= input$nv2_range[2])
    }
    
    
    #select columns based on user input ---------------------------------------------
    #create vector of columns to select
    select_vec <- vector() # initialize vector of column name strings to select columns
    
    #select gender column if either male or female is chosen/add gender to select_vec
    if("female" %in% input$cat_var | "male" %in% input$cat_var) {
      select_vec <- append(select_vec, "gender")
    }
    
    #select op_system column if either ios or android is chosen/add op_sytem to select_vec
    if("ios" %in% input$cat_var | "android" %in% input$cat_var) {
      select_vec <- append(select_vec, "op_system")
    }
    
    #select num var columns
    if(input$num_var1 == "age" | input$num_var2 == "age") {
      select_vec <- append(select_vec, "age")
    }
    if(input$num_var1 == "app_usage_time" | input$num_var2 == "app_usage_time") {
      select_vec <- append(select_vec, "app_usage_time")
    }
    if(input$num_var1 == "no_apps" | input$num_var2 == "no_apps") {
      select_vec <- append(select_vec, "no_apps")
    }
    
    user_data <- select(user_data,starts_with(select_vec)) # select columns chosen
    
    
#  })
  
  
  output$table <- renderTable({
#    req(input$dataButton)
    user_data
  })
  }) 
}



#Create shiny app ----------------------------------------------------------------
shinyApp(ui = ui, server = server)