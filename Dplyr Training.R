#Install.packages("tidyverse", "nycflights13")
install.packages("tidyverse")
library(tidyverse)
library(tidyverse)
library(tidyverse)
R.version.string
packageVersion("purrr")
packageVersion("tidyr")

install.packages("nycflights13")
library(nycflights13)
# run the pipeline
jan_feb_flights <- flights %>% 
  filter(month %in% c(1, 2))
# view the first few rows of the result
head(jan_feb_flights)
# view the structure of the result
str(jan_feb_flights)
view(jan_feb_flights)
# get summary statistics of the result
summary(jan_feb_flights)
#count flights per month
count_per_month <- flights %>%
  group_by(month) %>%
  summarise(num_flights = n())  
print(count_per_month)
# visualize the number of flights per month
ggplot(count_per_month, aes(x = month, y = num_flights)) +
  geom_bar(stat = "identity") 
flights_per_month <- flights %>% 
  group_by(month) %>% 
  summarise(n_flights = n()) %>% 
  arrange(desc(n_flights))
print(flights_per_month)  
jan_feb_flights <- flights %>% 
  filter(month %in% c(1, 2)) %>% 
  count(month, name = "n_flights") %>%
  arrange(n_flights) %>%
  select(month, n_flights)
print(jan_feb_flights)
jan_feb_mar_apr_flights <- flights %>% 
  filter(month %in% c(1, 2, 3, 4)) %>%
count(month, name = "n_flights") %>% 
  ggplot(aes(x = month, y = n_flights)) +
  geom_col(fill = "steelblue") +
  labs(title = "Flights per Month", x = "Month", y = "Number of Flights")
print(jan_feb_mar_apr_flights)
head(jan_feb_mar_apr_flights)

jan_feb_mar_apr_may_jun_flights <- flights %>% 
  filter(month %in% c(1, 2, 3, 4,5 ,6)) %>%
  count(month, name = "n_flights") %>%
  ggplot(aes(x = reorder(month, n_flights), y = n_flights)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Flights per Month",
    x = "Month",
    y = "Number of Flights")
print(jan_feb_mar_apr_may_jun_flights)
head(jan_feb_mar_apr_may_jun_flights)
#CALCULATE aVERAGE ARRIVAL DELAY PER DAY
daily_IAH_delay <- flights %>% 
  filter(dest == "IAH") %>%        # keep only flights going to IAH
  group_by(year, month, day) %>%   # group by year, month, and day
  summarise(avg_delay = mean(arr_delay, na.rm = TRUE), .groups = "drop")# calculate average arrival delay

print(daily_IAH_delay)
head(daily_IAH_delay)

jan_feb_mar_apr_may_jun_flights <- flights %>% 
  filter(month %in% c(1:6)) %>%
  count(month, name = "n_flights") %>%
  mutate(month = factor(
    month,
    levels = 1:6,
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun"))) %>% 
  ggplot(aes(x = reorder(month, n_flights), y = n_flights)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Flights per Month",
    x = "Month",
    y = "Number of Flights")
print(jan_feb_mar_apr_may_jun_flights)
?who2
library(who2)
data("who2")
head(who2)              
print(who2)
who2_clean <- who2 %>% 
  pivot_longer(
    cols = !(country:year),
    names_to = c("diagnosis", "gender", "age"),
    names_sep = "_",
    values_to = "count",
    values_drop_na = TRUE) %>% 
  mutate(gender = case_match(gender, "m" ~ "Male", "f" ~ "Female", .default = "gender")) %>%  # convert gender to full words
  mutate(age = case_match(age, "014" ~ "0-14",                           # making age range readable
                               "1524" ~ "15-24",
                               "2534"~ "25-34",
                               "3544"~ "35-44",
                               "4554" ~ "45-54",
                               "5564" ~ "55-64",
                               "65" ~ "65+", .default = "age"))
head(who2_clean)

who2_str_replace_all <- who2 %>% 
  pivot_longer(
    cols = !(country:year),
    names_to = c("diagnosis", "gender", "age"),
    names_sep = "_",
    values_to = "count",
    values_drop_na = TRUE) %>%     
  mutate(age = str_replace_all(age,
                           c("1524" = "15-24",
                          "2534" = "25-34",
                          "3544" = "35-44")))
head(who2_str_replace_all)
# Str_ replace function ---------------------------------------------------
who2_str_replace_regex <- who2 %>% 
  pivot_longer(
    cols = !(country:year),
    names_to = c("diagnosis", "gender", "age"),
    names_sep = "_",
    values_to = "count",
    values_drop_na = TRUE) %>% 
mutate(age = str_replace(age, "(\\d{2})(\\d{2})", "\\1-\\2")) %>% 
mutate(age = case_match(age, "65" ~ "65+", "014" ~ "0-14", .default = age))

head(who2_str_replace_regex)       
# Filter data for tuberculosis diagnosis and summarize counts
tuberculosis_data <- who2_str_replace_regex %>% 
  filter(diagnosis %in% c("rel", "sn", "sp")) %>% 
  group_by(country, year) %>% 
  summarise(total_count = sum(count), .groups = "drop")
head(tuberculosis_data)
# order age groups
who2_ordered_age <- who2_str_replace_regex %>% 
  mutate(age = factor(age, 
                      levels = c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+"), 
                      ordered = TRUE))
levels(who2_ordered_age$age)  # check the levels of the ordered age factor
head(who2_ordered_age)
print(who2_ordered_age)
# Summarize counts by age group
age_group_summary <- who2_ordered_age %>%
  group_by(age) %>%
  summarise(total_count = sum(count), .groups = "drop")
print(age_group_summary)
# Visualize total counts by age group
ggplot(age_group_summary, aes(x = age, y = total_count)) +
  geom_col(fill = "lightgreen") +
  labs(title = "Total Counts by Age Group", x = "Age Group", y = "Total Count")

# Tidyr -------------------------------------------------------------------
library(tidyr)
# Billboard dataset
data("billboard")
head(billboard)
glimpse(billboard)
# Pivot longer to convert wide format to long format
billboard_long <- billboard %>%
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    values_to = "rank",
    values_drop_na = TRUE
  ) %>% 
  mutate(week = parse_number(week)) # convert week values from character to numeric
head(billboard_long)
print(billboard_long)
#plotting rank vs week
billboard_long %>% 
  ggplot(aes(x = week, y = rank, group = track)) +
  geom_line(alpha = 0.3) +
  scale_y_reverse() +  # reverse y-axis so that rank 1 is at the top
  labs(title = "Billboard Track Rankings Over Weeks", x = "Week", y = "Rank")
  
not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !i  s.na(arr_delay))
daily_means <- not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(mean = mean(dep_delay)) 
print(daily_means, n = 15)
daily_means %>%
  ggplot(aes(x = day, y = mean)) +
  geom_line() +
  facet_wrap(~ month) +
  labs(title = "Average Daily Departure Delays by Month", x = "Day of Month", y = "Average Departure Delay (minutes)")  
