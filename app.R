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
      tabsetPanel(
        tabPanel("About",
                 tags$img(src = "pexels-luckysam-50614.jpg", width = 250, style = "float:right"),
                 tags$h3("Purpose"),
                 ("The purpose of this Shiny app is for the user to explore the Mobile Device Usage and User Behavior Dataset. The exploration includes both numeric and catgorical variables summarized graphically and numerically. The user can also download their selected subset of the data."),
                 br(),
                 tags$h3("Data"),
                 ("The Mobile Device Usage and User Behavior Dataset containes 700 observations related to cell phone usage and behavior. The variables included for exploration in this app are gender (male or female), device operations system (iOS or Android), age of the user (years), daily time spent using apps on the moblie device (minutes), and number of apps installed on the phone. The dataset was created using simulated user behavior."),
                 br(),
                 br(),
                 ("See data source: "),
                 tags$a("Mobile Device Usage and User Behavior Dataset", href = "https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset/data"),
                 br(),
                 tags$h3("Functionality"),
                 ("The app consists of a sidebar for user inputs to subset the data as well as a main panel with three tabs entitled About, Data Download, and Data Exploration."),
                 br(),
                 br(),
                 tags$b("User Input Sidebar: "),
                 ("User inputs for subsetting the data are located here. The user can choose to include observations for males, females, iOS operating system, and/or Android operating system. The user can also select one or two numeric variables from the choices of age, daily time spent using apps, and nymber of apps installed. For each of these numeric variables the user can further select the range of values to be included."),
                 br(),
                 br(),
                 tags$b("About Tab: "),
                 ("This tab contains descriptions of the app and the underlying dataset."),
                 br(),
                 br(),
                 tags$b("Data Download Tab: "),
                 ("info"),
                 br(),
                 br(),
                 tags$b("Data Exploration Tab: "),
                 ("info")
                 ),
        tabPanel("Data Download",dataTableOutput("table")),
        tabPanel("Data Exploration")
      
    )
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
  
  
  data_subset <- reactive({
    req(input$dataButton)
    cat_var <- isolate(input$cat_var)
    num_var1 <- isolate(input$num_var1)
    num_var2 <- isolate(input$num_var2)
    nv1_range <- isolate(input$nv1_range)
    nv2_range <- isolate(input$nv2_range)
    
    #filter data rows based on user input -----------------------------------------------
    user_data <- phone_data #initialize user_data dataset as a copy of original dataset
    
    #cat vars
    #filter for f or m in gender if only one is chosen
    if ("female" %in% cat_var & !("male" %in% cat_var)) {
      user_data <- filter(user_data, gender == "Female")
    }
    if(!("female" %in% cat_var) & "male" %in% cat_var) {
      user_data <- filter(user_data, gender == "Male")
    }
    
    #filter for ios or android in op_system if only one is chosen
    if ("android" %in% cat_var & !("ios" %in% cat_var)) {
      user_data <- filter(user_data, op_system == "Android")
    }
    if(!("android" %in% cat_var) & "ios" %in% cat_var) {
      user_data <- filter(user_data, op_system == "iOS")
    }
    
    #filter num vars
    #filter for age
    if(num_var1 == "age") {
      user_data <- filter(user_data, age >= nv1_range[1] & age <= nv1_range[2])
    }
    if(num_var2 == "age") {
      user_data <- filter(user_data, age >= nv2_range[1] & age <= nv2_range[2])
    }
    
    #filter for app_usage_time
    if(num_var1 == "app_usage_time") {
      user_data <- filter(user_data, app_usage_time >= nv1_range[1] & app_usage_time <= nv1_range[2])
    }
    if(num_var2 == "app_usage_time") {
      user_data <- filter(user_data, app_usage_time >= nv2_range[1] & app_usage_time <= nv2_range[2])
    }
    
    #filter for no_apps
    if(num_var1 == "no_apps") {
      user_data <- filter(user_data, no_apps >= nv1_range[1] & no_apps <= nv1_range[2])
    }
    if(num_var2 == "no_apps") {
      user_data <- filter(user_data, no_apps >= nv2_range[1] & no_apps <= nv2_range[2])
    }
    
    
    #select columns based on user input ---------------------------------------------
    #create vector of columns to select
    select_vec <- vector() # initialize vector of column name strings to select columns
    
    #select gender column if either male or female is chosen/add gender to select_vec
    if("female" %in% cat_var | "male" %in% cat_var) {
      select_vec <- append(select_vec, "gender")
    }
    
    #select op_system column if either ios or android is chosen/add op_sytem to select_vec
    if("ios" %in% cat_var | "android" %in% cat_var) {
      select_vec <- append(select_vec, "op_system")
    }
    
    #select num var columns
    if(num_var1 == "age" | num_var2 == "age") {
      select_vec <- append(select_vec, "age")
    }
    if(num_var1 == "app_usage_time" | num_var2 == "app_usage_time") {
      select_vec <- append(select_vec, "app_usage_time")
    }
    if(num_var1 == "no_apps" | num_var2 == "no_apps") {
      select_vec <- append(select_vec, "no_apps")
    }
    
    select(user_data,starts_with(select_vec)) # select columns chosen/final dataset
    
    
      })
    
    
    output$table <- renderDataTable({
      req(input$dataButton)
      data_subset()
    })
#  }) 
}



#Create shiny app ----------------------------------------------------------------
shinyApp(ui = ui, server = server)