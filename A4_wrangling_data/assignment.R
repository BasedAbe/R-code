library(tidyr)

#################
#### PART 1 #####
#################

#1. Loaded data into R and saved it into a variable. Then View() data. 

projects_df <- read.csv("data/ks-projects-201801.csv", stringsAsFactors = FALSE)
View(projects_df)

#2. Printing out the number of records/observations & features
#   in the data frame using nrow() and colnames() function.

print(nrow(projects_df))
print(colnames(projects_df))

#3. Getting summary data for data frame using summary()
#   function. With and without stringsAsFactors.

without_strings_as_factors <- summary(projects_df, stringAsFactors = FALSE)
with_strings_as_factors <- summary(projects_df, stringAsFactors = TRUE)

print(without_strings_as_factors)
print(with_strings_as_factors)

#4.What are 2 facts that you notice in this summary data 
#  that seem interesting, and what questions do they suggest 
#  for further exploration?

#      One fact that seemed intresting in this summary data would be the discrepancies with in 
#      usd.pledged mean and the actual usd.pledged real mean. Another fact that was intresting was
#      the change in the number of backers depending on which quarter it was. So there can further 
#      exploration to determine why there is a difference in mean pledges and if it has anything to 
#      do with the quarter based of the backer averages.

#################
#### PART 2 ##### 
#################

#1. Loading the dplyr package to access its functions

library("dplyr")

#2. Kickstarter projects that received no money pledged.
#   Variable is then printed.

num_nothing_pledged <- projects_df %>% 
  filter(pledged == 0) %>% 
  count() %>% 
  pull(n)

print(num_nothing_pledged)

#3. Percentage of projects were successful.
#   Variable is then printed

percent_successful <- projects_df %>% 
  filter(state == "successful") %>% 
  count() / nrow(projects_df)        

print(percent_successful)

#4. Number of projects with 2018 deadline.Variable is then 
#   printed.

num_project_2018 <- projects_df %>% 
  filter(startsWith(deadline, "2018")) %>%  
  count() %>% 
  pull(n)

print(num_project_2018)

#5. Name and category of the project that had the highest amount pledged 
#   Variable is then printed.

most_pledged <- projects_df %>% 
  filter(pledged == max(pledged)) %>% 
  select(name, category, pledged)

print(most_pledged)

#6. Names and categories of the projects that had the highest goal.
#   Variable is then printed.
  
highest_goal <- projects_df %>% 
  filter(goal == max(goal)) %>% 
  select(name, category, goal)

print(highest_goal)

#7. Name of the project that failed with the highest amount pledged, and how much 
#   was pledged to that project.Variable is then printed.

biggest_failure <- projects_df %>%
  filter(state == "failed") %>% 
  filter(pledged == max(pledged)) %>% 
  select(name, pledged)
  
print(biggest_failure)

#8. Project has the that has largest amount pledged over their goal, and how much over. 
#   Variable is then printed.

largest_margin <- projects_df %>%
  mutate(margin = pledged - goal) %>% 
  filter(margin == max(margin)) %>% 
  select(name, margin)

print(largest_margin)

#9. The average (mean) and total amount of money pledged to projects that were not 
#   successful. Variable is then printed.

not_successfull <- projects_df %>% 
  filter(state == "failed") %>% 
  summarise(average = mean(pledged),total = sum(pledged)) 
  
print(not_successfull)

#################
#### PART 3 #####
#################

#1.Projects posted in each category, and ranked in descending order.
#  Variable is then printed.

category_counts <- projects_df %>% 
  group_by(category) %>% 
  summarise(count = n()) %>% 
  arrange(-count)
  
print(category_counts)

#2.Category of project had the highest average (mean) number of backers.
#  Varriable is then printed.

highest_avg_backers <- projects_df %>%
  group_by(category) %>% 
  summarise(averages = mean(backers)) %>% 
  filter(averages == max(averages)) %>% 
  pull(category)
  
print(highest_avg_backers) 

#3. Top 3 most popular categories that ended in 2018.  
#   Variable is then printed.

top_3_categories <- projects_df %>% 
  filter(startsWith(deadline, "2018")) %>%         # Filtering for deadline that starts with 2018
  group_by(category) %>% 
  summarise(total_backers = sum(backers)) %>%      # Summing the total backers then having it 
  arrange(-total_backers) %>%                      # arranged in descending order and using the 
  head(3)                                          # head() function to pull the top 3

print(top_3_categories)

#4.Activity-in terms of backers and money pledged-did Kickstarter for each year in the 
#  data set.Variable is then printed.
  
popularity_by_year <- projects_df %>% 
 mutate(date = as.Date(launched, "%Y-%m-%d")) %>% 
 mutate(year = substr(date, start = 1, stop = 4 )) %>% 
 group_by(year) %>% 
 summarise(total_backers = sum(backers), total_pledged = sum(pledged))

print(popularity_by_year)

#5.Based on the values in your popularity_by_year data frame, what do you observe about the popularity of the 
#  Kickstarter platform over time?
  
# Based on the values from the popularity_by_year frame I observed that the popularity of Kickstarters
# has really took a rise from 2010 to 2013 and then began to stabilize and grow slowly from 2014 to 
# 2018.

#6.The most popular day to launch a Kickstarter campaign.
#  Variable is then printed.

most_popular_launch_day <- projects_df %>% 
  mutate(date = weekdays(as.Date(launched, "%Y-%m-%d"))) %>% 
  group_by(date) %>% 
  summarise(launched_days = n()) %>% 
  filter(launched_days == max(launched_days)) %>% 
  pull(date)
 
print(most_popular_launch_day)

#7.The least successful day of the week to launch a campaign.
#  Variable is then printed.
  
least_successful_launch_day <- projects_df %>% 
  mutate(date = weekdays(as.Date(launched, "%Y-%m-%d"))) %>%            # Mutate launched column to become as.Date
  group_by(date) %>% 
  summarise(failures = sum(state != "successful"), total_projects = n()) %>% 
  mutate(failure_rate = failures/total_projects) %>% 
  filter(failure_rate == max(failure_rate)) %>%                         # Filtering for the max failure rate and
  pull(date)                                                            # then pulling that data out of the data frame
  
print(least_successful_launch_day)
  
#8.The most successful day of the week to launch a campaign.
#  The date you should launch. Variable is then printed.
  
most_successful_launch_day <- projects_df %>% 
  mutate(date = weekdays(as.Date(launched, "%Y-%m-%d"))) %>%             
  group_by(date) %>%                                                    
  summarise(failures = sum(state != "failed"), total_projects = n()) %>%   # Changed string in sum function to 
  mutate(failure_rate = failures/total_projects) %>%                       # find the most successful launch date        
  filter(failure_rate == max(failure_rate)) %>%                            # instead of the least successful
  pull(date)                                                            

print(most_successful_launch_day)

#9. The average pledge for each category for each deadline year 2013 and later. 

avg_year_category_df <- projects_df %>%                                       # was unable to figure out how to include
  mutate(date = as.Date(launched, "%Y-%m-%d")) %>%                            # the category column. I understand that 
  mutate(year = substr(date, start = 1, stop = 4)) %>%                        # I am supposed to groupby category also
  group_by(year) %>%                                                          # but i couldnt figure it out in time.
  summarise(average_pledged_per_backer = sum(pledged)/sum(backers)) %>% 
  filter(year >= 2013)

print(avg_year_category_df)

#################
#### PART 4 #####
#################

#1. Loaded data into R and saved it into a variable. Then View() data. 

cex_data <- read.csv("data/cex_multiyear.csv", stringsAsFactors = FALSE)
View(cex_data)

cex_data <- cex_data %>% 
  mutate("Item" = ï..Item)                                               # Had to manually change Item column name because
cex_data <- cex_data[2:8]                                                # Item was not properly spelled in the cvs file.
                                                                         # Item was showing as i..item before. Got help from
                                                                         # info tutors for this part because of the issue.
#2. Reshaping CEX data frame to long data frame and then back into
#   a wide data frame.

cex_long_df <- cex_data %>%                            # Using gather() function to make data 
  gather(key = "year",value = "category",-Item) %>%    # long
  mutate(year = substr(year, start = 2, stop = 5))

print(cex_long_df)

cex_wide_df <- cex_long_df %>%                         # Using spread() function to make data
  spread(key = Item, value = category)                 # wide

print(cex_wide_df)

#3. CEX data merged with the avg_year_category_df by using left_join(function)

all_spending <- left_join(cex_wide_df, avg_year_category_df, by = "year")

print(all_spending)

#4.Ask and answer a question of my own choosing! 
# What year had the highest average number of Alcoholic Beverages?  

avg_alcoholic_drinks <- all_spending %>% 
  group_by(year) %>% 
  summarise(average = mean(Alcoholic_beverages)) %>% 
  filter(average == max(average)) %>% 
  pull(year)
