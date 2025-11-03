# Lanette Tyler
# NCSU ST558
# Project 2
# Static Analysis

# Read in packages --------------------------------------------------------------
library(tidyverse)
library(labelled)
library(ggbeeswarm)

# Read data ---------------------------------------------------------------------
user_behavior_dataset <- read_csv("user_behavior_dataset.csv")

#Transform data -----------------------------------------------------------------
#--------------------------------------------------------------------------------
phone_data <- user_behavior_dataset |>
  rename("user_id" = "User ID", "model" = "Device Model", "op_system" = "Operating System", "app_usage_time" = "App Usage Time (min/day)", "screen_on_time" = "Screen On Time (hours/day)", "batt_drain" = "Battery Drain (mAh/day)", "no_apps" = "Number of Apps Installed", "data_usage" = "Data Usage (MB/day)", "age" = "Age", "gender" = "Gender", "user_class" = "User Behavior Class") |>
  mutate(user_id = as.character(user_id), model = as.factor(model), op_system = as.factor(op_system), gender = as.factor(gender), user_class = as.factor(user_class))

var_label(phone_data) <- list(user_id = "User ID", model = "Device Model", op_system = "Operating System", app_usage_time = "App Usage Time (min/day)", screen_on_time = "Screen On Time (hours/day)", batt_drain = "Battery Drain (mAh/day)", no_apps = "Number of Apps Installed", data_usage = "Data Usage (MB/day)", age = "Age", gender = "Gender", user_class = "User Behavior Class")

# EDA ---------------------------------------------------------------------------
#--------------------------------------------------------------------------------

#Categorical variables------------------------------------------------------------
#Numeric Summaries
#One Way Contingency Tables
table(phone_data$model) # leave this one off?
table(phone_data$op_system)
table(phone_data$gender)
table(phone_data$user_class)

#alternative method for One Way Tables
phone_data |>
  group_by(model) |>
  summarize(count = n())

phone_data |>
  group_by(op_system) |>
  summarize(count = n())

phone_data |>
  group_by(gender) |>
  summarize(count = n())

phone_data |>
  group_by(user_class) |>
  summarize(count = n())

#Categorical Variables
#Graphical Summaries 
#Bar Chart
ggplot(phone_data, aes(x = gender, fill = gender)) +
  geom_bar() +
  labs(x = "Gender", y = "Count") +
  scale_fill_discrete("Gender")

#Grouped Categorical Variables---------------------------------------------------
#Numeric Summaries
# Two Way Contingency Tables
table(phone_data$model, phone_data$gender)
table(phone_data$op_system, phone_data$gender)
table(phone_data$user_class, phone_data$gender)
table(phone_data$model, phone_data$user_class)
table(phone_data$op_system, phone_data$user_class)

#Alternative Method for Two Way Tables
phone_data |>
  group_by(op_system, gender) |>
  summarize(count = n()) |>
  pivot_wider(names_from = gender, values_from = count)

#Grouped Categorical Variables
#Graphical Summaries
#Side-by-side bar charts
# User Behavior by Gender
ggplot(phone_data,aes(x = user_class, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(x = var_label(phone_data$user_class),
       title = "Bar Chart of User Behavior Class by Gender") +
  scale_fill_discrete(var_label(phone_data$gender))

# User Behavior by Operating System
ggplot(phone_data,aes(x = user_class, fill = op_system)) +
  geom_bar(position = "dodge") +
  labs(x = var_label(phone_data$user_class),
       title = "Bar Chart of User Behavior Class by Operating System") +
  scale_fill_discrete(var_label(phone_data$op_system))

#Grouped Categorical Variables, faceted
#Faceted Side-by-side bar chart 
# Operating System by Gender, faceted by user class
ggplot(phone_data,aes(x = op_system, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(x = var_label(phone_data$op_system),
       title = "Bar Chart of Operating System by Gender Faceted by User Behavior Class") +
  scale_fill_discrete(var_label(phone_data$gender)) +
  facet_wrap(~user_class)

#Numeric Variables ---------------------------------------------------------
#Numeric Summaries
phone_data |> 
  summarize("mean_app_usage_time" = mean(app_usage_time), "sd_app_usage_time" = sd(app_usage_time), "median_app_usage_time" = median(app_usage_time))

phone_data |> 
  summarize("mean_no_apps" = mean(no_apps), "sd_no_apps" = sd(no_apps), "median_no_apps" = median(no_apps))

phone_data |> 
  summarize("mean_age" = mean(age), "sd_age" = sd(age), "median_age" = median(age))

#Numeric Variables
#Graphic Summary
ggplot(phone_data, aes(x = age)) +
  geom_density() +
  labs(x = var_label(phone_data$age),
       y = "Density",
       title = paste("Density Plot:", var_label(phone_data$age)))

#Grouped Numeric Variables-------------------------------------------------------
#Numeric Summaries
#model
phone_data |>
  group_by(model) |>
  summarize("mean_app_usage_time" = mean(app_usage_time), "sd_app_usage_time" = sd(app_usage_time), "median_app_usage_time" = median(app_usage_time))

#op_system
phone_data |>
  group_by(op_system) |>
summarize("mean_app_usage_time" = mean(app_usage_time), "sd_app_usage_time" = sd(app_usage_time), "median_app_usage_time" = median(app_usage_time))

#gender
phone_data |>
  group_by(gender) |>
  summarize("mean_age" = mean(age), "sd_age" = sd(age), "median_age" = median(age))

phone_data |>
  group_by(gender) |>
  summarize("mean_app_usage_time" = mean(app_usage_time), "sd_app_usage_time" = sd(app_usage_time), "median_app_usage_time" = median(app_usage_time))

#user_class
phone_data |>
  group_by(user_class) |>
  summarize("mean_app_usage_time" = mean(app_usage_time), "sd_app_usage_time" = sd(app_usage_time), "median_app_usage_time" = median(app_usage_time))

#Grouped Numeric Variables
#Graphical Summaries
# Smoothed Density Plots
ggplot(phone_data, aes(x = app_usage_time)) +
  geom_density(alpha = 0.5, aes(fill = gender)) +
  facet_wrap(~op_system) + 
  labs( x = var_label(phone_data$app_usage_time),
        title = var_label(phone_data$app_usage_time))

ggplot(phone_data, aes(x = no_apps)) +
  geom_density(alpha = 0.5, aes(fill = gender)) +
  facet_wrap(~op_system) + 
  labs( x = var_label(phone_data$no_apps),
        title = var_label(phone_data$no_apps))

ggplot(phone_data, aes(x = age)) +
  geom_density(alpha = 0.5, aes(fill = gender)) +
  facet_wrap(~model) + 
  labs( x = var_label(phone_data$age),
        title = var_label(phone_data$age))

#Grouped Numeric Variables
#Graphical Summaries
#Grouped Box Plots
#age by gender
ggplot(phone_data, aes(x = gender, y = age, fill = gender)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Age", title = "Boxplot: Age by Gender") +
  theme(legend.position = "none")

#age by op_system
ggplot(phone_data, aes(x = op_system, y = age, fill = op_system)) +
  geom_boxplot() +
  labs(x = "Operating System", y = "Age", title = "Boxplot: Age by Operating System") +
  theme(legend.position = "none")

#app_usage_time by gender
ggplot(phone_data, aes(x = gender, y = app_usage_time, fill = gender)) +
  geom_boxplot() +
  labs(x = "Gender", y = "App Usage Time (min/day)", title = "Boxplot: App Usage Time by Gender") +
  theme(legend.position = "none")

#app_usage_time by op_system
ggplot(phone_data, aes(x = op_system, y = app_usage_time, fill = op_system)) +
  geom_boxplot() +
  labs(x = "Operating System", y = "App Usage Time (min/day)", title = "Boxplot: App Usage Time by Operating System") +
  theme(legend.position = "none")

#no_apps by gender
ggplot(phone_data, aes(x = gender, y = no_apps, fill = gender)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Number of Apps Installed", title = "Boxplot: Number of Apps Installed by Gender") +
  theme(legend.position = "none")

#no_apps by op_system
ggplot(phone_data, aes(x = op_system, y = no_apps, fill = op_system)) +
  geom_boxplot() +
  labs(x = "Operating System", y = "Number of Apps Installed", title = "Boxplot: Number of Apps Installed by Operating System") +
  theme(legend.position = "none")

#Faceted Grouped Boxplot
ggplot(phone_data, aes(x = op_system, y = no_apps, fill = op_system)) +
  geom_boxplot() +
  labs(x = "Operating System", y = "Number of Apps Installed", title = "Boxplot: Number of Apps Installed by Operating System") +
  theme(legend.position = "none") +
  facet_wrap(~gender)

#Grouped Numeric Variables
#Graphical Summaries
#Bee Swarm Plots
ggplot(phone_data, aes(x = op_system, y = age, color = op_system)) +
  geom_beeswarm() +
  labs(x = var_label(phone_data$op_system),
       y = var_label(phone_data$age),
       title = paste("Bee Swarm Plot:", var_label(phone_data$age))) +
  theme(legend.position = "none")

ggplot(phone_data, aes(x = op_system, y = no_apps, color = op_system)) +
  geom_beeswarm() +
  labs(x = var_label(phone_data$op_system),
       y = var_label(phone_data$no_apps),
       title = paste("Bee Swarm Plot:", var_label(phone_data$op_system))) +
  theme(legend.position = "none")

#Relationship Between Numeric Variables -----------------------------------------
#Numeric Summaries
#Correlation
#age, app_usage_time
phone_data |>
  summarize(correlation = cor(age, app_usage_time))

#age, app_usage_time grouped by gender and op_system
phone_data |>
  group_by(gender, op_system) |>
  summarize(correlation = cor(age, app_usage_time))

#app_usage_time, no_apps
phone_data |>
  summarize(correlation = cor(app_usage_time, no_apps))
cor(phone_data$app_usage_time, phone_data$no_apps)

# app_usage_time, no_apps, grouped by
phone_data |>
  group_by(gender, op_system) |>
  summarize(correlation = cor(app_usage_time, no_apps)) |>
  pivot_wider(names_from = op_system, values_from = correlation)

#age, no_apps
cor(phone_data$age, phone_data$no_apps)

#age, no_apps, grouped by
phone_data |>
  group_by(gender, op_system) |>
  summarize(correlation = cor(age, no_apps))

#Relationship Between Numeric Variables
#Graphical Summaries
# Scatter Plots
ggplot(phone_data, aes(x = age, y = app_usage_time, color = op_system)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = var_label(phone_data$age),
       y = var_label(phone_data$app_usage_time),
       title = "Scatter Plot") +
  scale_color_discrete(var_label(phone_data$op_system))

ggplot(phone_data, aes(x = app_usage_time, y = no_apps, color = gender)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = var_label(phone_data$app_usage_time),
       y = var_label(phone_data$no_apps),
       title = "Scatter Plot") +
  scale_color_discrete(var_label(phone_data$gender))

ggplot(phone_data, aes(x = age, y = no_apps, color = op_system)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(x = var_label(phone_data$age),
       y = var_label(phone_data$no_apps),
       title = "Scatter Plot") +
  scale_color_discrete(var_label(phone_data$op_system)) +
  facet_wrap(~gender)
