library(tidyverse)
install.packages("nycflights13")
library(nycflights13)
library(ggplot2)
# if_else Uses three arguments the first is the condition, the second is TURE when the condition is true
# the third is false when the condition is false
x <-  c(-3:3, NA)
if_else(x > 0, "+ve", "-ve")

if_else(x < 0, -x, x)
if_else(is.na(x), 0, x)
# nested if_else
if_else(x > 0, "+ve",
        if_else(x < 0, "-ve", "zero"))
# example
x1 <-  c(NA, 1, 2, NA)
y1 <-  c(3, NA, 4, 6)
if_else(is.na(x1), y1, x1)

# case_when function, it takes pairs that look like condition ~ output. 
# condition must be logical vector; when it is TRUE the output will be used.
x <-  c(-3:3, NA)
case_when(x == 0 ~ "0", x < 0 ~ "-ve", x > 0 ~ "+ve", is.na(x) ~ "missing")
case_when(x < 0 ~ "-ve", x > 0 ~ "+ve", TRUE ~ "???")
case_when(x > 0 ~  "+ve", x > 2 ~ "big")
# example with flights data
flights_status <- flights |>
  mutate(
    status = case_when(
      is.na(arr_delay) ~ "cancelled",
      arr_delay < -30 ~ "very early",
      arr_delay < -15 ~ "early",
      arr_delay <= 15 ~ "on time",
      arr_delay < 60 ~ "late",
      TRUE ~ "very late"),
    .keep = "used")
# keep only valid delay ( no cancelled flights)
flights_status <- flights |>
  mutate(
    status = case_when(
      is.na(arr_delay) ~ "cancelled",
      arr_delay < -30 ~ "very early",
      arr_delay < -15 ~ "early",
      arr_delay <= 15 ~ "on time",
      arr_delay < 60 ~ "late",
      TRUE ~ "very late"
    )
  ) |>
  select(arr_delay, status) |>
  filter(!is.na(arr_delay))
View(flights_status)
# Use boxplot to show delay by status, excluding cancelled flights
# this also shows the distribution, spread and outliers
ggplot(flights_status, aes(x = status, y = arr_delay)) +
  geom_boxplot() +
  labs(
    title = "Arrival Delays by Flight Status",
    x = "Flight Status",
    y = "Arrival Delay (minutes)"
  )
theme_minimal()


# On time flights cluster around 0–15 minutes
# Very late flights have large spread, many outliers with large spread this indicate operational risk
# Early and very early arrivals are tightly grouped this show predictable performance

# Count number of flights per status --------------------------------------

flights_status_count <- flights |>
  mutate(
    status = case_when(
      is.na(arr_delay) ~ "cancelled",
      arr_delay < -30 ~ "very early",
      arr_delay < -15 ~ "early",
      arr_delay <= 15 ~ "on time",
      arr_delay < 60 ~ "late",
      TRUE ~ "very late"
    )
  ) |>
  count(status)
print(flights_status_count)

# Printed Result ----------------------------------------------------------

#A tibble: 6 × 2
# status          n
# <chr>       <int>
#  1 cancelled    9430
# 2 early       70416
# 3 late        49313
# 4 on time    159216
# 5 very early  20084
# 6 very late   28317

# Bar chart to show the numbers of flights per status ---------------------
# How many flights fell into each operational outcome
# First reorder status by factor levels
flights_status_count <- flights_status_count |>
  mutate(status = factor(status, levels = c( "very early", "early", "on time", "late", "very late","cancelled")))

# Create bar chart
ggplot(flights_status_count, aes(x = status, y = n, fill = status)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of Flights by Status",
    x = "Flight Status",
    y = "Number of Flights"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
# most flights arrive early or on time
# cancelled flights are significant proportion of total flights


# summary view - Mean delay per status ------------------------------------

flights_status_summary <- flights_status |> 
  mutate(status = factor(status, levels = c( "very early", "early", "on time", "late", "very late"))) |>
  group_by(status) |> 
  summarise(mean_delay = mean(arr_delay)) |>  
  ggplot(aes(x = status, y = mean_delay, fill = status)) +
  geom_col() +
  labs(
    title = "Average Arrival Delay by Status",
    x = "Flight Status",
    y = "Average Delay (minutes)"
  )+
  theme_minimal()
print(flights_status_summary)
# very late flights have high average delay
# very early flights have high average negative delay (arrive much earlier than scheduled)
# on time flights have average delay close to zero


# histogram plot ------------------------------------------------
# histogram to show distribution of arrival delays by status
flights_status|> 
  mutate(status = factor(status, levels = c( "very early", "early", "on time", "late", "very late"))) |> 
ggplot(aes(x = arr_delay, fill = status)) +
  geom_histogram(binwidth = 30, position = "identity", alpha = 0.6) +
  labs(
    title = "Histogram of Arrival Delays by Flight Status",
    x = "Arrival Delay (minutes)",
    y = "Count"
  ) +
  theme_minimal()
# very late flights have wide range of delays
# on time flights cluster around 0-15 minutes
# very early flights cluster around -30 to -15 minutes and the highest in number followed by early flights


# Density Plot ------------------------------------------------------------
# density plot to show distribution of arrival delays by status
flights_status |> 
  mutate(status = factor(status, levels = c( "very early", "early", "on time", "late", "very late"))) |> 
  ggplot(aes(x = arr_delay, color = status)) +
  geom_density(size = 1) +
  labs(
    title = "Density Plot of Arrival Delays by Flight Status",
    x = "Arrival Delay (minutes)",
    y = "Density"
  ) +
  theme_minimal()
# very late flights have wide range of delays
# on time flights cluster around 0-15 minutes

# END ---------------------------------------------------------------------



