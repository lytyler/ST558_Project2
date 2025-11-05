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
library(ggbeeswarm)


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
  titlePanel("Explore Cell Phone Usage and Behavior Dataset"),
  
  # Sidebar panel for inputs -----------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      tags$h3("User Inputs"),
      
      #choose cat vars
      checkboxGroupInput(
        inputId = "cat_var",
        label = "Choose Categories to Include:",
        choices = c("Female" = "female",
                    "Male" = "male",
                    "iOS" = "ios",
                    "Android" = "android"),
        selected = "all"),
      br(),
      
      
      #select first num var
      uiOutput("num_selector1"),
      
      #range slider for first num var
      uiOutput("slidey1"),
      br(),
      
      #select second num var
      uiOutput("num_selector2"),
      
      #range slider for second num var
      uiOutput("slidey2"),
      br(),
      
      #action button to generate internal dataset
      tags$b("Generate/Update Dataset:"),
      tags$h5("Once you have selected categories, measurements, and measurement ranges, click the button to create or update your dataset."),
      actionButton(
        inputId = "dataButton",
        label = "Create/Update Specified Dataset",
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("About",
                 tags$img(src = "pexels-luckysam-50614.jpg", width = 250, style = "float:right"),
                 tags$h3("Purpose"),
                 ("The purpose of this Shiny app is for the user to explore the Mobile Device Usage and User Behavior Dataset. The exploration includes both numeric and catgorical variables summarized graphically and numerically, with some options for user-subsetting of some variables. The user can also download their selected subset of the data."),
                 br(),
                 tags$h3("Data"),
                 ("The Mobile Device Usage and User Behavior Dataset containes 700 observations related to cell phone usage and behavior. The variables include:"),
                 br(),
                 br(),
                 tags$b("Device Model: "),
                 ("Model of the user's smartphone"),
                 br(),
                 br(),
                 tags$b("Operating System: "),
                 ("The OS of the device (iOS or Android)"),
                 br(),
                 br(),
                 tags$b("App Usage Time: "),
                 ("Daily time spent on mobile applications, measured in minutes"),
                 br(),
                 br(),
                 tags$b("Screen On Time: "),
                 ("Average hours per day the screen is active"),
                 br(),
                 br(),
                 tags$b("Battery Drain: "),
                 ("Daily battery consumption in mAh"),
                 br(),
                 br(),
                 tags$b("Number of Apps Installed: "),
                 ("Total apps available on the device"),
                 br(),
                 br(),
                 tags$b("Data Usage: "),
                 ("Daily mobile data consumption in megabytes"),
                 br(),
                 br(),
                 tags$b("Age: "),
                 ("Age of the user"),
                 br(),
                 br(),
                 tags$b("Gender: "),
                 ("Gender of the user (Male or Female)"),
                 br(),
                 br(),
                 tags$b("User Behavior Class: "),
                 ("Classification of user behavior based on usage patterns (1 to 5)"),
                 br(),
                 br(),
                 ("The dataset was created using simulated user behavior. See data source: "),
                 tags$a("Mobile Device Usage and User Behavior Dataset", href = "https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset/data"),
                 br(),
                 tags$h3("Functionality"),
                 ("The app consists of a sidebar for user inputs to subset the data as well as a main panel with three tabs entitled About, Data Download, and Data Exploration."),
                 br(),
                 br(),
                 tags$b("User Input Sidebar: "),
                 ("User inputs for subsetting the data are located here. The user can choose to include observations for males, females, iOS operating system, and/or Android operating system. The user can also select one or two numeric variables from the choices of age, daily time spent using apps, and number of apps installed. For each of these numeric variables the user can further select the range of values to be included. If the range values on the slider(s) are left at the default setting(s), no filtering occurs for these numeric variables. After making these selections, the user generates the subsetted dataset by pressing the Create/Update Dataset Button."),
                 br(),
                 br(),
                 tags$b("About Tab: "),
                 ("This tab contains descriptions of the app and the underlying dataset."),
                 br(),
                 br(),
                 tags$b("Data Download Tab: "),
                 ("After the dataset is created or updated using the button at the bottom of the sidebar, a table of the dataset is visible here. The dataset can be downloaded as a .csv file by pressing the Download Data Button under the table."),
                 br(),
                 br(),
                 tags$b("Data Exploration Tab: "),
                 ("After creating/updating the dataset in the sidebar, the user can explore the dataset in this tab. The user must choose the type of data summary to display (categorical variables, grouped categorical variables, numeric variables, grouped numeric variables, relationships between numeric variables, or fun with beeswarm plots). For each data summary type, the user will be prompted to choose relevant variables, and must then click the Create/Update Plot button to see the summary."),
                 br(),
                 br(),
                 br(),
                 br(),
                 br()
        ),
        tabPanel("Data Download",
                 dataTableOutput("table"),
                 br(),
                 br(),
                 ("After dataset is created/updated in the sidebar and data table is visible above, click Download Data button to download .csv file of dataset."),
                 br(),
                 br(),
                 downloadButton("download","Download Data"),
                 br(),
                 br(),
                 br(),
                 br(),
                 br()
        ),
        tabPanel("Data Exploration",
                 br(),
                 tags$b("First, subset and create dataset in sidebar."),
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
                               "Numeric Variable Relationships" = "rel_num_var_sum",
                               "Fun with Beeswarms!" = "beeswarm"
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
                                 "Device Model" = "model",
                                 "Operating System" = "op_system",
                                 "Gender" = "gender",
                                 "User Behavior Class" = "user_class")
                   ),
                   br(),
                   actionButton(
                     inputId = "summary1Button",
                     label = "Create/Update Plot"
                   ),
                   br(),
                   br(),
                   column(width = 6,
                          plotOutput("plot_single_cv")
                   ),
                   column(width = 6,
                          br(),
                          br(),
                          tableOutput("table_single_cv")
                   )
                 ),
                 #if "grouped categorical variables" is selected
                 conditionalPanel(
                   condition = "input.summary_type == 'gr_cat_var_sum'",
                   selectizeInput(
                     inputId = "gr_cv1",
                     label = "Choose Categorical Variable to Summarize:",
                     choices = c("Choose One" = "",
                                 "Device Model" = "model",
                                 "Operating System" = "op_system",
                                 "Gender" = "gender",
                                 "User Behavior Class" = "user_class")
                   ),
                   br(),
                   uiOutput("gr_cv2_selector"),
                   br(),
                   actionButton(
                     inputId = "summary2Button",
                     label = "Create/Update Plot",
                   ),
                   br(),
                   br(),
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
                                 "Screen On Time (hours/day)" = "screen_on_time",
                                 "Number of Apps Installed" = "no_apps",
                                 "Battery Drain (mAh/day)" = "batt_drain",
                                 "Data Usage (MB/day)" = "data_usage"),
                   ),
                   br(),
                   actionButton(
                     inputId = "summary3Button",
                     label = "Create/Update Plot",
                   ),
                   br(),
                   br(),
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
                                 "Screen On Time (hours/day)" = "screen_on_time",
                                 "Number of Apps Installed" = "no_apps",
                                 "Battery Drain (mAh/day)" = "batt_drain",
                                 "Data Usage (MB/day)" = "data_usage")
                   ),
                   br(),
                   #choose grouping variable
                   selectizeInput(
                     inputId = "gr_nv_gr",
                     label = "Choose grouping variable:",
                     choices = c("Choose One" = "",
                                 "Device Model" = "model",
                                 "Operating System" = "op_system",
                                 "Gender" = "gender",
                                 "User Behavior Class" = "user_class"
                     )
                   ),
                   br(),
                   actionButton(
                     inputId = "summary4Button",
                     label = "Create/Update Plot",
                   ),
                   br(),
                   br(),
                   column(width = 6,
                          plotOutput("plot_gr_nv")
                   ),
                   column(width = 6,
                          br(),
                          br(),
                          tableOutput("table_gr_nv")
                   )
                   
                 ), # closes conditionalPanel for grouped numvar data summary
                 # if numeric variable relationship summary is selected
                 conditionalPanel(
                   condition = "input.summary_type == 'rel_num_var_sum'",
                   # column(
                   #   width = 5,
                     selectizeInput(
                       inputId = "nv_nv_nv1",
                       label = "Choose first numeric variable to summarize:",
                       choices = c("Choose One" = "",
                                   "Age" = "age", 
                                   "App Usage Time (min/day)" = "app_usage_time",
                                   "Screen On Time (hours/day)" = "screen_on_time",
                                   "Number of Apps Installed" = "no_apps",
                                   "Battery Drain (mAh/day)" = "batt_drain",
                                   "Data Usage (MB/day)" = "data_usage")
                     ),
                     br(),
                     #select second numvar for scatter plot and correlation
                     uiOutput("scPlot_nv2_selector"),
                     br(),
                     #select catvar for grouping
                     selectizeInput(
                       inputId = "scPlot_gr_cv",
                       label = "Choose Categorical Variable for Grouping:",
                       choices = c("Choose One" = "",
                                   "Device Model" = "model",
                                   "Operating System" = "op_system",
                                   "Gender" = "gender",
                                   "User Behavior Class" = "user_class"
                       )
                     ),
                     br(),
                     #select catvar for faceting
                     uiOutput("scPlot_cv_faceting_selector"),
#                   ),
                   br(),
                   actionButton(
                     inputId = "summary5Button",
                     label = "Create/Update Plot",
                   ),
                   br(),
                   br(),
                   column(width = 6,
                          plotOutput("plot_nv_nv")
                   ),
                   column(width = 6,
                          br(),
                          br(),
                          tableOutput("table_nv_nv")
                   )
                 ), #closes conditional panel for numeric variable rel summary
                 #if "fun with beeswarms" summary is selected:
                 conditionalPanel(
                   condition = "input.summary_type == 'beeswarm'",
                   selectizeInput(
                     inputId = "beeswarm_nv",
                     label = "Choose numeric variable to summarize:",
                     choices = c("Choose One" = "",
                                 "Age" = "age", 
                                 "App Usage Time (min/day)" = "app_usage_time",
                                 "Screen On Time (hours/day)" = "screen_on_time",
                                 "Number of Apps Installed" = "no_apps",
                                 "Battery Drain (mAh/day)" = "batt_drain",
                                 "Data Usage (MB/day)" = "data_usage")
                   ),
                   br(),
                   #choose grouping variable
                   selectizeInput(
                     inputId = "beeswarm_cv1",
                     label = "Choose grouping variable:",
                     choices = c("Choose One" = "",
                                 "Device Model" = "model",
                                 "Operating System" = "op_system",
                                 "Gender" = "gender",
                                 "User Behavior Class" = "user_class"
                     )
                   ),
                   #choose second grouping variable color)
                   uiOutput("beeswarm_cv2_selector"),
                   br(),
                   actionButton(
                     inputId = "summary6Button",
                     label = "Create/Update Plot"
                   ),
                   br(),
                   br(),
                   column(width = 6,
                          plotOutput("plot_beeswarm")
                   ),
                   column(width = 6,
                          br(),
                          br(),
                          tableOutput("table_beeswarm")
                   )
                   
                 ), # closes conditionalPanel for beeswarm data summary
        ) #closes tabPanel for data exploration
      ) # closes tabsetPanel
    ) #closes mainPanel
  ) #closes sidebarLayout
) #closes fluidPage


# Define server logic -----------------------------------------------------------
#--------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  #Reactive UI functionality for sidebar selections------------------------------
  #choose first num var in sidebar
  output$num_selector1 <-renderUI({
    selectizeInput(
      inputId = "num_var1",
      label = "Choose Measurement to Filter:",
      choices = c(#"Choose One" = "",
                  "Age" = "age",
                  "App Usage Time (min/day)" = "app_usage_time",
                  "Number of Apps Installed" = "no_apps")
    )
  })
  
  # add slider for first num var
  output$slidey1 <- renderUI({
    req(input$num_var1)
    if (input$num_var1 == "age") {
      sliderInput(
        inputId = "nv1_range",
        label = "Limit Age Range if Desired:",
        min = 18,
        max = 59,
        value = c(18, 59)
      )
    } else if (input$num_var1 == "app_usage_time") {
      sliderInput(
        inputId = "nv1_range",
        label = "Limit Range of App Usage Time (min/day) if Desired:",
        min = 30,
        max = 598,
        value = c(30, 598),
      )
    } else if (input$num_var1 == "no_apps") {
      sliderInput(
        inputId = "nv1_range",
        label = "Limit Range for Number of Apps Installed if Desired:",
        min = 10,
        max = 99,
        value = c(10, 99)
      )
    }
  })
  
  #choose second num var in sidebar
  output$num_selector2 <- renderUI({
    req(input$num_var1)
    if (input$num_var1 == "age") {
      selectizeInput(
        inputId = "num_var2",
        label = "Choose Another Measurement to Filter (Optional):",
        choices = c("Choose One" = "",
                    "App Usage Time (min/day)" = "app_usage_time",
                    "Number of Apps Installed" = "no_apps"),
        selected = NULL
      )
    } else if (input$num_var1 == "app_usage_time") {
      selectizeInput(
        inputId = "num_var2",
        label = "Choose Another Measurement to Filter (Optional):",
        choices = c("Choose One" = "",
                    "Age" = "age",
                    "Number of Apps Installed" = "no_apps"),
        selected = NULL
      )
    } else if (input$num_var1 == "no_apps") {
      selectizeInput(
        inputId = "num_var2",
        label = "Choose Another Measurement to Filter (Optional):",
        choices = c("Choose One" = "",
                    "Age" = "age",
                    "App Usage Time (min/day)" = "app_usage_time"),
        selected = NULL
      )
    }
    
  })
  
  # add slider for second num var
  output$slidey2 <- renderUI({
    req(input$num_var2)
    if (input$num_var2 == "age") {
      sliderInput(
        inputId = "nv2_range",
        label = "Limit Age Range if Desired:",
        min = 18,
        max = 59,
        value = c(18, 59)
      )
    } else if (input$num_var2 == "app_usage_time") {
      sliderInput(
        inputId = "nv2_range",
        label = "Limit Range of App Usage Time (min/day) if Desired:",
        min = 30,
        max = 598,
        value = c(30, 598),
      )
    } else if (input$num_var2 == "no_apps") {
      sliderInput(
        inputId = "nv2_range",
        label = "Limit Range for Number of Apps Installed if Desired:",
        min = 10,
        max = 99,
        value = c(10, 99)
      )
    }
  })
  
  
  
  #create dataset
  data_subset <- reactive({
    req(input$dataButton)
    cat_var <- isolate(input$cat_var)
    num_var1 <- isolate(input$num_var1)
    num_var2 <- isolate(input$num_var2)
    nv1_range <- isolate(input$nv1_range)
    nv2_range <- isolate(input$nv2_range)
    
    #filter data rows based on user input 
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
    if (!("android" %in% cat_var) & "ios" %in% cat_var) {
      user_data <- filter(user_data, op_system == "iOS")
    }
    
    #filter num vars
    #filter num_var1
    if (num_var1 == "age") {
      user_data <- filter(user_data, age >= nv1_range[1] & age <= nv1_range[2])
    } else if (num_var1 == "app_usage_time") {
      user_data <- filter(user_data, app_usage_time >= nv1_range[1] & app_usage_time <= nv1_range[2])
    } else if (num_var1 == "no_apps") {
      user_data <- filter(user_data, no_apps >= nv1_range[1] & no_apps <= nv1_range[2])
    }
    
    #filter num_var2
    if (num_var2 == "age") {
      user_data <- filter(user_data, age >= nv2_range[1] & age <= nv2_range[2])
    } else if (num_var2 == "app_usage_time") {
      user_data <- filter(user_data, app_usage_time >= nv2_range[1] & app_usage_time <= nv2_range[2])
    } else if (num_var2 == "no_apps") {
      user_data <- filter(user_data, no_apps >= nv2_range[1] & no_apps <= nv2_range[2])
    }
    
    user_data <- subset(user_data, select =-user_id) #drop id column
    user_data #output dataset
  })
  
  #Outputs for Data Download Tab------------------------------------------------
  output$table <- renderDataTable({
    req(input$dataButton)
    validate(
      need(input$num_var1 == "age" | input$num_var1 == "app_usage_time" | input$num_var1 == "no_apps", "In the sidebar, a selection for the first measurement to filter is required.")
    )
    data_subset()
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("phoneData-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_subset(),file)
    }
  )
  
  #Data Exploration Tab Outputs ------------------------------------------------
  # Categorical Variables
  #Single Categorical Variables-------------------------------------------------
  #bar chart
  output$plot_single_cv <- renderPlot({
    req(input$summary1Button) # input$summary1Button
    single_cv <- isolate(input$single_cv)
    user_data <- data_subset()
    xaxis <- as.character(single_cv)
    ggplot(user_data, aes_string(x = xaxis, fill = xaxis)) +
      geom_bar() +
      labs(x = var_label(user_data[single_cv]),
           y = "Count",
           title = paste("Bar Chart:", var_label(user_data[single_cv]))
      ) +
      theme(legend.position = "none")
  })
  
  #one way contingency table
  output$table_single_cv <- renderTable({
    req(input$summary1Button)
    single_cv <- isolate(input$single_cv)
    user_data <- data_subset()
    user_data |>
      group_by(user_data[[single_cv]]) |>
      summarize(count = n())
  }, colnames = FALSE)
  
  #grouped categorical variables-----------------------------------------------
  
  #select second categorical variable for grouping
  output$gr_cv2_selector <- renderUI({
    req(input$gr_cv1)
    if (input$gr_cv1 == "model") {
      selectizeInput(
        inputId = "gr_cv2",
        label = "Choose Second Categorical Variable for Grouping:",
        choices = c("Choose One" = "",
                    "Operating System" = "op_system",
                    "Gender" = "gender",
                    "User Behavior Class" = "user_class"),
        selected = NULL
      )
    } else if (input$gr_cv1 == "op_system") {
      selectizeInput(
        inputId = "gr_cv2",
        label = "Choose Second Categorical Variable for Grouping:",
        choices = c("Choose One" = "",
                    "Device Model" = "model",
                    "Gender" = "gender",
                    "User Behavior Class" = "user_class"),
        selected = NULL
      )
    } else if (input$gr_cv1 == "gender") {
      selectizeInput(
        inputId = "gr_cv2",
        label = "Choose Second Categorical Variable for Grouping:",
        choices = c("Choose One" = "",
                    "Device Model" = "model",
                    "Operating System" = "op_system",
                    "User Behavior Class" = "user_class"),
        selected = NULL
      )
    } else if (input$gr_cv1 == "user_class") {
      selectizeInput(
        inputId = "gr_cv2",
        label = "Choose Second Categorical Variable for Grouping:",
        choices = c("Choose One" = "",
                    "Device Model" = "model",
                    "Operating System" = "op_system",
                    "Gender" = "gender"),
        selected = NULL
      )
    }
  })
  
  #side-by-side bar chart
  output$plot_gr_cv  <- renderPlot({
    gr_cv1 <- isolate(input$gr_cv1)
    gr_cv2 <- isolate(input$gr_cv2)
    req(input$summary2Button, gr_cv1, gr_cv2)
    user_data <- data_subset()
    xaxis <- as.character(gr_cv1)
    fill_var <- as.character(gr_cv2)
    ggplot(user_data, aes_string(x = xaxis, fill = fill_var)) +
      geom_bar(position = "dodge") +
      labs(x = var_label(user_data[gr_cv1]),
           title = paste("Bar Chart:", var_label(user_data[gr_cv1]), "Grouped by", var_label(user_data[gr_cv2]))
      )
  })
  
  #two way contingency table
  output$table_gr_cv <- renderTable({
    gr_cv1 <- isolate(input$gr_cv1)
    gr_cv2 <- isolate(input$gr_cv2)
    req(input$summary2Button, gr_cv1, gr_cv2)
    user_data <- data_subset()
    user_data |>
      group_by(user_data[gr_cv1], user_data[gr_cv2]) |>
      summarize(count = n()) 
  }, colnames = FALSE)
  
  # Single Numeric Variables---------------------------------------------------
  output$plot_single_nv <- renderPlot({
    single_nv <- isolate(input$single_nv)
    req(input$summary3Button,single_nv)
    user_data <- data_subset()
    xaxis <- as.character(single_nv)
    ggplot(user_data, aes_string(x = xaxis)) +
      geom_density() +
      labs(x = var_label(user_data[single_nv]),
           y = "Density",
           title = paste("Density Plot:", var_label(user_data[single_nv]))
      )
  })
  
  output$table_single_nv <- renderTable({
    single_nv <- isolate(input$single_nv)
    req(input$summary3Button,single_nv)
    user_data <- data_subset()
    user_data |>
      summarize("Mean" = mean(user_data[[single_nv]]),
                "SD" = sd(user_data[[single_nv]]),
                "Median" = median(user_data[[single_nv]])
      )
  })
  
  #Grouped Numeric Variables---------------------------------------------------
  #grouped boxplots of numeric variables
  output$plot_gr_nv <- renderPlot({
    gr_nv_nv <- isolate(input$gr_nv_nv)
    gr_nv_gr <- isolate(input$gr_nv_gr)
    req(input$summary4Button,gr_nv_gr, gr_nv_nv)
    user_data <-data_subset()
    xaxis <- as.character(gr_nv_gr)
    yaxis <- as.character(gr_nv_nv)
    ggplot(user_data, aes_string(x = xaxis, y = yaxis, fill = xaxis)) +
      geom_boxplot() +
      labs(x = var_label(user_data[gr_nv_gr]), 
           y = var_label(user_data[gr_nv_nv]), 
           title = paste("Boxplot:", var_label(user_data[gr_nv_nv]), 
                         "by", var_label(user_data[gr_nv_gr]))
      ) +
      theme(legend.position = "none")
  })
  
  #numeric summaries of grouped numeric variables
  output$table_gr_nv <- renderTable({
    gr_nv_nv <- isolate(input$gr_nv_nv)
    gr_nv_gr <- isolate(input$gr_nv_gr)
    req(input$summary4Button,gr_nv_gr, gr_nv_nv)
    user_data <- data_subset()
    user_data |>
      group_by( !!sym(gr_nv_gr)) |>
      summarize("Mean" = mean(get(gr_nv_nv)),
                "SD" = sd(get(gr_nv_nv)),
                "Median" = median(get(gr_nv_nv))
      )
  })
  
  #Relate two numeric variables/Numeric Variable Relationships-----------------
  #with grouping and faceting
  
  #Select second numvar for plot and summary
  output$scPlot_nv2_selector <- renderUI({
    req(input$nv_nv_nv1)
    if (input$nv_nv_nv1 == "age") {
      selectizeInput(
        inputId = "nv_nv_nv2",
        label = "Choose Second Numeric Variable to Summarize:",
        choices = c("Choose One" = "",
                    "App Usage Time (min/day)" = "app_usage_time",
                    "Screen On Time (hours/day)" = "screen_on_time",
                    "Number of Apps Installed" = "no_apps",
                    "Battery Drain (mAh/day)" = "batt_drain",
                    "Data Usage (MB/day)" = "data_usage"),
        selected = NULL
      )
    } else if (input$nv_nv_nv1 == "app_usage_time") {
      selectizeInput(
        inputId = "nv_nv_nv2",
        label = "Choose Second Numeric Variable to Summarize:",
        choices = c("Choose One" = "",
                    "Age" = "age", 
                    "Screen On Time (hours/day)" = "screen_on_time",
                    "Number of Apps Installed" = "no_apps",
                    "Battery Drain (mAh/day)" = "batt_drain",
                    "Data Usage (MB/day)" = "data_usage"),
        selected = NULL
      )
    } else if (input$nv_nv_nv1 == "screen_on_time") {
      selectizeInput(
        inputId = "nv_nv_nv2",
        label = "Choose Second Numeric Variable to Summarize:",
        choices = c("Choose One" = "",
                    "Age" = "age", 
                    "App Usage Time (min/day)" = "app_usage_time",
                    "Number of Apps Installed" = "no_apps",
                    "Battery Drain (mAh/day)" = "batt_drain",
                    "Data Usage (MB/day)" = "data_usage"),
        selected = NULL
      )
    } else if (input$nv_nv_nv1 == "no_apps") {
      selectizeInput(
        inputId = "nv_nv_nv2",
        label = "Choose Second Numeric Variable to Summarize:",
        choices = c("Choose One" = "",
                    "Age" = "age", 
                    "App Usage Time (min/day)" = "app_usage_time",
                    "Screen On Time (hours/day)" = "screen_on_time",
                    "Battery Drain (mAh/day)" = "batt_drain",
                    "Data Usage (MB/day)" = "data_usage"),
        selected = NULL
      )
    } else if (input$nv_nv_nv1 == "batt_drain") {
      selectizeInput(
        inputId = "nv_nv_nv2",
        label = "Choose Second Numeric Variable to Summarize:",
        choices = c("Choose One" = "",
                    "Age" = "age", 
                    "App Usage Time (min/day)" = "app_usage_time",
                    "Screen On Time (hours/day)" = "screen_on_time",
                    "Number of Apps Installed" = "no_apps",
                    "Data Usage (MB/day)" = "data_usage"),
        selected = NULL
      )
    } else if (input$nv_nv_nv1 == "data_usage") {
      selectizeInput(
        inputId = "nv_nv_nv2",
        label = "Choose Second Numeric Variable to Summarize:",
        choices = c("Choose One" = "",
                    "Age" = "age", 
                    "App Usage Time (min/day)" = "app_usage_time",
                    "Screen On Time (hours/day)" = "screen_on_time",
                    "Number of Apps Installed" = "no_apps",
                    "Battery Drain (mAh/day)" = "batt_drain"),
        selected = NULL
      )
    } 
  })
  
  #Select catvar for faceting (second catvar for this plotting/summary instance)
  output$scPlot_cv_faceting_selector <- renderUI({
    req(input$scPlot_gr_cv)
    if (input$scPlot_gr_cv == "model") {
      selectizeInput(
        inputId = "scPlot_cv_facet",
        label = "Choose Categorical Variable for Faceting:",
        choices = c("Choose One" = "",
                    "Operating System" = "op_system",
                    "Gender" = "gender",
                    "User Behavior Class" = "user_class"),
        selected = NULL
      )
    } else if (input$scPlot_gr_cv == "op_system") {
      selectizeInput(
        inputId = "scPlot_cv_facet",
        label = "Choose Categorical Variable for Faceting:",
        choices = c("Choose One" = "",
                    "Device Model" = "model",
                    "Gender" = "gender",
                    "User Behavior Class" = "user_class"),
        selected = NULL
      )
    } else if (input$scPlot_gr_cv == "gender") {
      selectizeInput(
        inputId = "scPlot_cv_facet",
        label = "Choose Categorical Variable for Faceting:",
        choices = c("Choose One" = "",
                    "Device Model" = "model",
                    "Operating System" = "op_system",
                    "User Behavior Class" = "user_class"),
        selected = NULL
      )
    } else if (input$scPlot_gr_cv == "user_class") {
      selectizeInput(
        inputId = "scPlot_cv_facet",
        label = "Choose Categorical Variable for Faceting:",
        choices = c("Choose One" = "",
                    "Device Model" = "model",
                    "Operating System" = "op_system",
                    "Gender" = "gender",),
        selected = NULL
      )
    }
  })
  
  #Plot
  #graphical summaries of numeric variables
  output$plot_nv_nv <- renderPlot({
    nv_nv_nv1 <- isolate(input$nv_nv_nv1)
    nv_nv_nv2 <- isolate(input$nv_nv_nv2)
    scPlot_gr_cv <- isolate(input$scPlot_gr_cv)
    scPlot_cv_facet <- isolate(input$scPlot_cv_facet)
    req(input$summary5Button, nv_nv_nv1, nv_nv_nv2,scPlot_gr_cv, scPlot_cv_facet)
    user_data <- data_subset()
    ggplot(user_data, aes_string(x = nv_nv_nv1, 
                                 y = nv_nv_nv2,
                                 color = scPlot_gr_cv)) +
      geom_point() +
      geom_smooth(method = lm) +
      labs(x = var_label(user_data[nv_nv_nv1]), 
           y = var_label(user_data[nv_nv_nv2]),
           title = paste("Scatter Plot:", var_label(user_data[nv_nv_nv2]), 
                         "by", var_label(user_data[nv_nv_nv1]), "faceted by", var_label(user_data[scPlot_cv_facet]))) +
      facet_wrap(~get(scPlot_cv_facet))
  })
  
  # #Table
  # #numeric summaries of grouped numeric variables
  # #need to update still copied and pasted from elsewhere
  output$table_nv_nv <- renderTable({
    nv_nv_nv1 <- isolate(input$nv_nv_nv1)
    nv_nv_nv2 <- isolate(input$nv_nv_nv2)
    scPlot_gr_cv <- isolate(input$scPlot_gr_cv)
    scPlot_cv_facet <- isolate(input$scPlot_cv_facet)
    req(input$summary5Button, nv_nv_nv1, nv_nv_nv2,scPlot_gr_cv, scPlot_cv_facet)
    user_data <- data_subset()
    user_data |>
      group_by(!!sym(scPlot_gr_cv),!!sym(scPlot_cv_facet)) |>
      summarize("Correlation" = cor(get(nv_nv_nv1), get(nv_nv_nv2)))
  })
  
  #Beeswarm Tab----------------------------------------------------------------
  #Select second cat var for beeswarm plot and summary
  output$beeswarm_cv2_selector <- renderUI({
    req(input$beeswarm_cv1)
    if (input$beeswarm_cv1 == "model") {
      selectizeInput(
        inputId = "beeswarm_cv2",
        label = "Choose Second Categorical Variable for Color Grouping:",
        choices = c("Choose One" = "",
                    "Operating System" = "op_system",
                    "Gender" = "gender",
                    "User Behavior Class" = "user_class"),
        selected = NULL
      )
    } else if (input$beeswarm_cv1 == "op_system") {
      selectizeInput(
        inputId = "beeswarm_cv2",
        label = "Choose Second Categorical Variable for Color Grouping:",
        choices = c("Choose One" = "",
                    "Device Model" = "model",
                    "Gender" = "gender",
                    "User Behavior Class" = "user_class"),
        selected = NULL
      )
    } else if (input$beeswarm_cv1 == "gender") {
      selectizeInput(
        inputId = "beeswarm_cv2",
        label = "Choose Second Categorical Variable for Color Grouping:",
        choices = c("Choose One" = "",
                    "Device Model" = "model",
                    "Operating System" = "op_system",
                    "User Behavior Class" = "user_class"),
        selected = NULL
      )
    } else if (input$beeswarm_cv1 == "user_class") {
      selectizeInput(
        inputId = "beeswarm_cv2",
        label = "Choose Second Categorical Variable for Color Grouping:",
        choices = c("Choose One" = "",
                    "Device Model" = "model",
                    "Operating System" = "op_system",
                    "Gender" = "gender"),
        selected = NULL
      )
    }
  })
  
  #Beeswarm Plot
  output$plot_beeswarm <- renderPlot({
    beeswarm_nv <- isolate(input$beeswarm_nv)
    beeswarm_cv1 <- isolate(input$beeswarm_cv1)
    beeswarm_cv2 <- isolate(input$beeswarm_cv2)
    req(input$summary6Button,beeswarm_nv, beeswarm_cv1, beeswarm_cv2)
    user_data <-data_subset()
    xaxis <- as.character(beeswarm_cv1)
    yaxis <- as.character(beeswarm_nv)
    col_cv <- as.character(beeswarm_cv2)
    ggplot(user_data, aes_string(x = xaxis, y = yaxis, color = col_cv)) +
      geom_beeswarm() +
      labs(x = var_label(user_data[beeswarm_cv1]), 
           y = var_label(user_data[beeswarm_nv]), 
           title = paste("Beeswarm Plot:", var_label(user_data[beeswarm_nv]),                            "by", var_label(user_data[beeswarm_cv1]))
      ) +
      scale_color_discrete(var_label(user_data[beeswarm_cv2]))
  })
  
  #Beeswarm Data Summary Table
  
  output$table_beeswarm <- renderTable({
    beeswarm_nv <- isolate(input$beeswarm_nv)
    beeswarm_cv1 <- isolate(input$beeswarm_cv1)
    beeswarm_cv2 <- isolate(input$beeswarm_cv2)
    req(input$summary6Button,beeswarm_nv, beeswarm_cv1, beeswarm_cv2)
    user_data <- data_subset()
    user_data |>
      group_by( !!sym(beeswarm_cv1), !!sym(beeswarm_cv2)) |>
      summarize("Mean" = mean(get(beeswarm_nv)),
                "SD" = sd(get(beeswarm_nv)),
                "Median" = median(get(beeswarm_nv))
      )
  })
  
} #final bracket of server section



#Create shiny app ----------------------------------------------------------------
#---------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)