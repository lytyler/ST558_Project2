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
        tabPanel("Data Exploration",
                 br(),
                 tags$b("First, specify and create dataset in sidebar."),
                 br(),
                 br(),
                 selectizeInput(
                   inputId = "summary_type",
                   label = "Choose Type of Data Summary to Display:",
                   choices = c("Choose One" = "",
                               "Categorical Variables" = "cat_var_sum",
                               "Grouped Categorical Variables" = "gr_cat_var_sum",
                               "Numeric Variables" = "num_var_sum",
                               "Grouped Numeric Variables" = "gr_num_var_sum",
                               "Numeric Variable Relationships" = "rel_num_var_sum"
                               )
                 ),
                 br(),
                 #if "categorical variables" summary is selected:
                 conditionalPanel(
                   condition = "input.summary_type == 'cat_var_sum'",
                   selectizeInput(
                     inputId = "single_cv",
                     label = "Choose Categorical Variable to Summarize:",
                     choices = c("Choose One" = "",
                                 "Gender" = "gender", 
                                 "Operating System" = "op_system")
                   ),
                   column(width = 6,
                          plotOutput("plot_single_cv")
                   ),
                   column(width = 6,
                          br(),
                          br(),
                          tableOutput("table_single_cv")
                   )
                 ),
                 #if "grouped categorical variable" is selected
                 conditionalPanel(
                   condition = "input.summary_type == 'gr_cat_var_sum'",
                   selectizeInput(
                     inputId = "gr_cv",
                     label = "Choose Categorical Variable to Summarize by Group:",
                     choices = c("Choose One" = "",
                                 "Gender, Grouped by Operating System" = "gender",
                                 "Operating System, Grouped by Gender" = "op_system")
                   ),
                   column(width = 6,
                          plotOutput("plot_gr_cv")
                   ),
                   column(width = 6,
                          br(),
                          br(),
                          tableOutput("table_gr_cv")
                   )
                 ),
                 #if "numerical variables" summary is selected:
                 conditionalPanel(
                   condition = "input.summary_type == 'num_var_sum'",
                   selectizeInput(
                    inputId = "single_nv",
                    label = "Choose numeric variable to summarize:",
                    choices = c("Choose One" = "",
                                "Age" = "age", 
                                "App Usage Time (min/day)" = "app_usage_time",
                                "Number of Apps Installed" = "no_apps"),
                   ),
                   column(width = 6,
                          plotOutput("plot_single_nv")
                   ),
                   column(width = 6,
                          br(),
                          br(),
                          tableOutput("table_single_nv")
                   )
                
                 ), # closes conditionalPanel for single numvar data summary
                 #if "grouped numerical variables" summary is selected:
                 conditionalPanel(
                   condition = "input.summary_type == 'gr_num_var_sum'",
                   selectizeInput(
                     inputId = "gr_nv_nv",
                     label = "Choose numeric variable to summarize:",
                     choices = c("Choose One" = "",
                                 "Age" = "age", 
                                 "App Usage Time (min/day)" = "app_usage_time",
                                 "Number of Apps Installed" = "no_apps"),
                   ),
                   br(),
                   #choose grouping variable
                   selectizeInput(
                     inputId = "gr_nv_gr",
                     label = "Choose grouping variable:",
                     choices = c("Choose One" = "",
                                 "Gender" = "gender", 
                                 "Operating System" = "op_system"
                                 )
                   ),
                   #add faceting checkbox for other grouping var here?
                   column(width = 6,
                         plotOutput("plot_gr_nv")
                   ),
                   column(width = 6,
                          br(),
                          br(),
                          tableOutput("table_gr_nv")
                   )
                   
                 ) # closes conditionalPanel for grouped numvar data summary
        ) #closes tabPanel for data exploration
      ) # closes tabsetPanel
    ) #closes mainPanel
  ) #closes sidebarLayout
) #closes fluidPage


# Define server logic -----------------------------------------------------------
#--------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  #choose second num var
  
  output$num_selector2 <- renderUI({
    if (input$num_var1 == "age") {
      selectizeInput(
        inputId = "num_var2",
        label = "Choose Another Measurement (Optional):",
        choices = c("Choose One" = "",
                    "App Usage Time (min/day)" = "app_usage_time",
                    "Number of Apps Installed" = "no_apps"),
        selected = NULL
      )
    } else if (input$num_var1 == "app_usage_time") {
      selectizeInput(
        inputId = "num_var2",
        label = "Choose Another Measurement (Optional):",
        choices = c("Choose One" = "",
                    "Age" = "age",
                    "Number of Apps Installed" = "no_apps"),
        selected = NULL
      )
    } else if (input$num_var1 == "no_apps") {
      selectizeInput(
        inputId = "num_var2",
        label = "Choose Another Measurement (Optional):",
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
    
    #Data Exploration Tab Outputs ------------------------------------------------
    # Categorical Variables
    #Single Categorical Variables
    #bar chart
    output$plot_single_cv <- renderPlot({
      req(input$single_cv)
      user_data <- data_subset()
      validate(
        need(input$single_cv %in% colnames(user_data),"Please choose a categorical variable that is included in the user-specified dataset, or go to the sidebar to create another dataset that includes the desired categorical variable.")
      )
      xaxis <- as.character(input$single_cv)
      ggplot(user_data, aes_string(x = xaxis, fill = xaxis)) +
        geom_bar() +
        labs(x = var_label(user_data[input$single_cv]),
             y = "Count",
             title = paste("Bar Chart:", var_label(user_data[input$single_cv]))
        ) +
        theme(legend.position = "none")
    })
    
    #one way contingency table
    output$table_single_cv <- renderTable({
      req(input$single_cv)
      user_data <- data_subset()
      validate(
        need(input$single_cv %in% colnames(user_data),"")
      )
      user_data |>
        group_by(user_data[[input$single_cv]]) |>
        summarize(count = n())
    }, colnames = FALSE)
    
    #grouped categorical variables
    #side-by-side bar chart
    output$plot_gr_cv  <- renderPlot({
      req(input$gr_cv)
      user_data <- data_subset()
      validate(
        need("gender" %in% colnames(user_data) & "op_system" %in% colnames(user_data),
             "Please go to the sidebar to specify and create a dataset with female, male or both, and iOS, Android or both for grouped summaries of categorical variables.")
      )
      xaxis <- as.character(input$gr_cv)
      if (input$gr_cv == "gender") fill_var <- as.character("op_system")
      if (input$gr_cv == "op_system") fill_var <- as.character("gender")
      ggplot(user_data,aes_string(x = xaxis, fill = fill_var)) +
        geom_bar(position = "dodge") +
        labs(x = var_label(user_data[input$gr_cv]),
             title = paste("Bar Chart:", var_label(user_data[input$gr_cv]), "Grouped by", var_label(user_data[fill_var]))
        )
    })
    
    #two way contingency table
    output$table_gr_cv <- renderTable({
      req(input$gr_cv)
      user_data <- data_subset()
      validate(
        need("gender" %in% colnames(user_data) & "op_system" %in% colnames(user_data),"")
      )
      if (input$gr_cv == "gender") gr2 <- as.character("op_system")
      if (input$gr_cv == "op_system") gr2 <- as.character("gender")
      user_data |>
        group_by(user_data[input$gr_cv], user_data[gr2]) |>
        summarize(count = n()) #|>
#        pivot_wider(names_from = user_data[gr2], values_from = count)
    }, colnames = FALSE)
    
    # Single Numeric Variables
    output$plot_single_nv <- renderPlot({
      req(input$single_nv)
      user_data <- data_subset()
      validate(
        need(input$single_nv %in% colnames(user_data),"Please choose a numeric variable that is included in the user-specified dataset, or go to the sidebar to create another dataset that includes the desired numeric variable.")
      )
      xaxis <- as.character(input$single_nv)
      ggplot(user_data, aes_string(x = xaxis)) +
        geom_density() +
        labs(x = var_label(user_data[input$single_nv]),
             y = "Density",
             title = paste("Density Plot:", var_label(user_data[input$single_nv]))
        )
    })
    
    output$table_single_nv <- renderTable({
      req(input$single_nv)
      user_data <- data_subset()
      validate(
        need(input$single_nv %in% colnames(user_data),"")
        )
      user_data |>
        summarize("Mean" = mean(user_data[[input$single_nv]]),
                  "SD" = sd(user_data[[input$single_nv]]),
                  "Median" = median(user_data[[input$single_nv]])
                  )
    })
    
    #Grouped Numeric Variables
    #grouped boxplots of numeric variables
    output$plot_gr_nv <- renderPlot({
      req(input$gr_nv_gr, input$gr_nv_nv)
      user_data <-data_subset()
      validate(
        need(input$gr_nv_nv %in% colnames(user_data) & input$gr_nv_gr %in% colnames(user_data),
             "Please choose a numeric variable and a grouping variable that are included in the user-specified dataset, or go to the sidebar to create another dataset that includes the desired variables.")
      )
      xaxis <- as.character(input$gr_nv_gr)
      yaxis <- as.character(input$gr_nv_nv)
      ggplot(user_data, aes_string(x = xaxis, y = yaxis, fill = xaxis)) +
        geom_boxplot() +
        labs(x = var_label(user_data[input$gr_nv_gr]), 
             y = var_label(user_data[input$gr_nv_nv]), 
             title = paste("Boxplot:", var_label(user_data[input$gr_nv_nv]), 
             "by", var_label(user_data[input$gr_nv_gr]))
        ) +
        theme(legend.position = "none")
    })
    
    #numeric summaries of grouped numeric variables
    output$table_gr_nv <- renderTable({
      req(input$gr_nv_gr, input$gr_nv_nv)
      user_data <- data_subset()
      validate(
        need(input$gr_nv_nv %in% colnames(user_data) & input$gr_nv_gr %in% colnames(user_data),"")
      )
      user_data |>
        group_by( !!sym(input$gr_nv_gr)) |>
        summarize("Mean" = mean(get(input$gr_nv_nv)),
                  "SD" = sd(get(input$gr_nv_nv)),
                  "Median" = median(get(input$gr_nv_nv))
        )
    })

} #final bracket of server section



#Create shiny app ----------------------------------------------------------------
#---------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)