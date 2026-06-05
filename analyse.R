library(tidyverse)
library(stringr)
library(questionr)
library(janitor)

data <- read_csv("degrees-that-pay-back.csv")

data <- data %>% 
  mutate(
    #'Starting Median Salary' = str_sub(`Starting Median Salary`, 2) %>% parse_number(),
    'Starting Median Salary' = parse_number(`Starting Median Salary`),
    'Mid-Career Median Salary' = parse_number(`Mid-Career Median Salary`),
    'Mid-Career 10th Percentile Salary' = parse_number(`Mid-Career 10th Percentile Salary`),
    'Mid-Career 25th Percentile Salary' = parse_number(`Mid-Career 25th Percentile Salary`),
    'Mid-Career 75th Percentile Salary' = parse_number(`Mid-Career 75th Percentile Salary`),
    'Mid-Career 90th Percentile Salary' = parse_number(`Mid-Career 90th Percentile Salary`),
  ) 

career <- data %>% 
  pivot_longer(
    cols = c("Mid-Career 10th Percentile Salary", "Mid-Career 25th Percentile Salary", "Mid-Career 75th Percentile Salary", "Mid-Career 90th Percentile Salary"),
    names_to = "Mid-Career Percentile Salary",
    values_to = "value"
  )

career$`Undergraduate Major` <- as.factor(career$`Undergraduate Major`)
career$`Mid-Career Percentile Salary` <- as.factor(career$`Mid-Career Percentile Salary`)

career$`Mid-Career Percentile Salary` <- career$`Mid-Career Percentile Salary` |>
  fct_recode(
    "10th Percentile" = "Mid-Career 10th Percentile Salary",
    "25th Percentile" = "Mid-Career 25th Percentile Salary",
    "75th Percentile" = "Mid-Career 75th Percentile Salary",
    "90th Percentile" = "Mid-Career 90th Percentile Salary"
  )

career <- career %>% 
        clean_names()

write_csv2(career, "career.csv")

  
  
  
  
  
  