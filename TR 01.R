my_data <- read.csv("heart.csv")
head (my_data)
tail (my_data)
str (my_data) # Check the structure of the data
install.packages ("tidyverse")
library ("tidyverse")
starwars
airquality
summary (my_data)
View (my_data)
data_starwars <- read.csv("starwars.csv")
# Check for missing values
my_data [1,3]
my_data [ ,3]
my_data [1:5,3]
my_data$STEN   
my_data$TIME
install.packages ("tidyverse")
library ("tidyverse")
install.packages ("dplyr")
library ("dplyr")
install.packages ("ggplot2")
library ("ggplot2")
install.packages ("ggpubr")
library ("ggpubr")
my_data %>% 
  select (Id, STEN, TIME, OUTCOME) %>% 
  filter (TIME > 100 & STEN >43) %>% 
  arrange (STEN)
data()
View (starwars)
str (starwars)
head (starwars)
summary (starwars)
# Check for missing values
starwars [1,3]
starwars [ ,3]
starwars %>% 
  select (gender, mass, height, species) %>% 
  filter (species == "Human") %>% 
  na.omit() %>% 
  mutate (height = height / 100) %>% 
  mutate (BMI = mass / height^2) %>%
  group_by(gender) %>%
  summarise (Average_BMI = mean (BMI))
# Check for missing values
install.packages ("naniar")
library ("naniar")  
install.packages ("gtExtras")
library ("gtExtras")
install.packages("rlang")
library("rlang")
install.packages("rlang")
library(gt)
library(gt)
library(gtExtras)
data_starwars <- read.csv("starwars")
install.packages("dplyr")  # Only run this once
library(dplyr)
library(dplyr)
starwars
airquality
head (airquality)


View (airquality)
 miss_var_summary (airquality) %>% 
  gt() %>%
   gtExtra() %>%
  gtExtras::gt_theme_538() %>% 
  tab_header (title = "Missing Values in Air Quality Data") %>%  
  tab_spanner (label = "Missing Values", columns = everything()) %>%
my_data %>% 
   select (STEN, TIME, hair_color, AGE) %>%
   head(5) %>%
   gt_theme_guardian() %>%
   tab_header (title = "my_data") %>%
   
#Table
   miss_var_summary (airquality) %>%
   gtExtras::gt_theme_quardian() %>%   
   tab_header (title = "Missing Values in airquality Data") %>%   
   tab_spanner (label = "Missing Values", columns = everything()) %>%
   