library(tidyverse)
library("ggplot2")
library("maps")
library("mapproj")

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Value 1: What was the average prison population per state in 1975?
average_pop_1975 <- df %>%
  select(year, state, total_jail_pop) %>%
  drop_na() %>%
  filter(year == 1975) %>%
  group_by(state) %>%
  summarize(average_population = round(mean(total_jail_pop)))

 # Value 2: What was the average prison population per state in 2015?
average_pop_2015 <- df %>%
  select(year, state, total_jail_pop) %>%
  drop_na() %>%
  filter(year == 2015) %>%
  group_by(state) %>%
  summarize(average_population = round(mean(total_jail_pop)))

# Value 3: What is the difference in average prison population per state from 1975 to 2015?
joined <- left_join(average_pop_1975, average_pop_2015, by = "state")
avg_pop_diff <- joined %>%
  rename("avg_1975" = "average_population.x") %>%
  rename("avg_2015" = "average_population.y") %>%
  mutate(avg_difference = (avg_2015 - avg_1975))

# Value 4: What states had the max and min difference in average prison population from 1975 to 2015?
max_avg_difference <- avg_pop_diff %>%
  filter(avg_difference == max(avg_difference)) %>%
  pull(state)
min_avg_difference <- avg_pop_diff %>%
  filter(avg_difference == min(avg_difference)) %>%
  pull(state)

# Value 5: What is the national average difference in prison population from 1975 to 2015 across all states?
national_average_difference <- avg_pop_diff %>%
  summarize(mean(avg_difference))
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  test <- df %>%
    select(year, total_jail_pop) %>%
    drop_na() %>%
    group_by(year) %>%
    summarize(total_jail_pop = sum(total_jail_pop))
return(test)   
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function() {
  b <- ggplot(data = get_year_jail_pop()) +
    geom_col(
      mapping = aes(x = year, y = total_jail_pop)) +
    scale_y_continuous(
      breaks = c(0, 200000, 400000, 600000, 800000),
      labels = c("0", "200,000", "400,000", "600,000", "800,000")) +
    labs(
      x = "Year",
      y = "Total Jail Population",
      title = "Increase of Jail Population in U.S. (1970-2018)",
      caption = "Figure 1: US Jail Pop.") +
    scale_x_continuous()
  return(b)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas

get_state_df <- function(variable) {
  state_df <- df %>%
    select(year, state, total_jail_pop) %>%
    drop_na() %>%
    filter(state == variable) %>%
    group_by(year) %>%
    summarize(pop = round(sum(total_jail_pop))) %>%
    mutate(state = variable)
  return(state_df)
}
ex <- c("CA", "WA", "OR")
get_jail_pop_by_states <- function(states) {
  state1 <- as.data.frame(lapply(states[1], get_state_df))
  state2 <- as.data.frame(lapply(states[2], get_state_df))
  state3 <- as.data.frame(lapply(states[3], get_state_df))
  if (length(states) == 3) {
    state_info_df <- rbind(state1, state2, state3)
  }
  else if (length(states) == 4) {
    state4 <- as.data.frame(lapply(states[4], get_state_df))
    state_info_df <- rbind(state1, state2, state3, state4)
  }
  else if (length(states) == 5) {
    state4 <- as.data.frame(lapply(states[5], get_state_df))
    state_info_df <- rbind(state1, state2, state3, state4)
  }
  return(state_info_df)
}

plot_jail_pop_by_states <- function(states) {
  state_graph <- ggplot(data = get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(
      x = year,
      y = pop,
      linetype = state,
      color = state)) +
    labs(
      title = "Increase of Jail Population in U.S. by State (1970-2018)",
      x = "Year",
      y = "Total Jail Population",
      caption = "Figure 2: Jail Pop. by State")
  return(state_graph)
}
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
# Question: How have male versus female jail populations changed from 1970 to 2018?

get_gender_pop <- function() {
  df1 <- df %>%
    select(year, female_jail_pop, male_jail_pop) %>%
    drop_na() %>%
    group_by(year) %>%
    summarize_if(is.numeric, sum) %>%
    summarize_if(is.numeric, round) %>%
    rename("female" = "female_jail_pop", "male" = "male_jail_pop")
  gender_pop_df <- gather(
    df1,
    key = "gender",
    value = "jail_pop", 
    -year
  )
  return(gender_pop_df)
}

plot_gender_pop <- function() {
  gender_pop_graph <- ggplot(data = get_gender_pop()) +
    geom_col(mapping = aes(
      x = year,
      y = jail_pop,
      fill = gender),
      position = "dodge") +
    scale_y_continuous(
      breaks = c(0, 200000, 400000, 600000),
      labels = c("0", "200,000", "400,000", "600,000")) +
    labs(
      title = "Male versus Female Jail Populations (1970 - 2018)", 
      x = "Year",
      y = "Jail Population",
      caption = "Figure 3: Gender Differences in Jail Pop."
    )
  return(gender_pop_graph)
}
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
# Question: In 2018, where in the United States was the Black population in jails highest?

get_black_pop_percentage <- function(){
  # Calculates average ratio of black individuals in jail per state
  black_jail_percentages <- df %>%
    select(year, state, total_jail_pop, black_jail_pop) %>%
    filter(year == 2018) %>%
    mutate(percentage = (black_jail_pop / total_jail_pop) * 100) %>%
    filter(percentage < 100) %>%
    group_by(state) %>%
    summarize(percentages = round(mean(percentage))) %>%
    rename("abb" = "state")
  # Rewrites the way states are named
  names <- df %>%
    summarize(state = state.name) %>%
    mutate(state = tolower(state)) %>%
    mutate(abb = state.abb)
  # Removes states which don't appear in the percentages df
  new_names <- as.data.frame(names[-c(7, 8, 11, 39), ])
  # Joins the two dataframes
  new_df <- left_join(new_names, black_jail_percentages) %>%
    select(state, percentages)
  # Joins the new dataframe with the state data, which contains coordinates
  state_data <- map_data("state") %>%
    rename("state" = "region") %>%
    left_join(new_df, by="state")
  return(state_data)
}

get_map <- function() {
  map <- ggplot(get_black_pop_percentage()) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = percentages),
      color = "White", 
      size = .1) +
    coord_map() +
    scale_fill_continuous(low = "Light Gray", high = "Black") + 
    labs(
      x = "Longitude",
      y = "Latitude",
      title = "Percentage of Black Individuals in Jails (2018)",
      caption = "Figure 4: Map of Black Jail Pop.",
      fill = "%")
  return(map)
}
#----------------------------------------------------------------------------#

## Load data frame ---- 
