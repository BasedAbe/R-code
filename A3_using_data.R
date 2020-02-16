#################
#### PART 1 ##### TA Data
#################

#1. Created variable of vectors with Info TAs 

tas <- c("Phuong Vu", "Yunwei Liang","Yubing Tian","Gavin Sreesangkom", 
         "Coco Li", "George Zhang","Saurav Sawansukha", "YuYu Madigan")

#2. Created variable that is a vector representing
#   grades in a math course using rnorm() function.

math_grades <- rnorm(8, mean = 3.6, sd = 0.4)

#3. Created variable that is a vector representing
#   grades in a spanish course using rnorm() function.

spanish_grades <- rnorm(8, mean = 3.8, sd = 0.25)

#4. Created variable that is a data frame with the three
#   previous vectors

ta_grades <- data.frame(tas, math_grades, spanish_grades, stringsAsFactors = FALSE)

#5. Created variable that contains string with colname_string
#   pasted into it.Variable is then printed.

names_of_col <- colnames(ta_grades)
colnames_string <- paste(names_of_col, collapse = ", ")
table_description <- paste("The grade data frame has 8 rows and 3 cols:", colnames_string)
print(table_description)

#6. Renaming column names to be singular and not plural
#   using the colnames() function.

colnames(ta_grades) <- c("ta", "math_grade", "spanish_grade")

#7.  Added a column data frame that represents whether the 
#    TA got a higher grade in math than in Spanish. 

ta_grades$better_at_math <-  ta_grades$math_grade > ta_grades$spanish_grade 

#8. Printed out row with my TA's grades using bracket 
#   notation and a filter

print(ta_grades[1, 1:3])

#9. Created a variable  that counts how many TAs are better at math. 
#   Variable is then printed.

num_better_at_math <- nrow(ta_grades[ta_grades$better_at_math == TRUE, ])
print(num_better_at_math)

#10. Used bracket notation to select elements from the data 
#    table above 4.0 and reassigned them a value of 4.0 instead.

ta_grades[ta_grades$math_grade > 4.0,  "math_grade"] <- 4.0
ta_grades[ta_grades$spanish_grade > 4.0, "spanish_grade" ] <- 4.0
View(ta_grades)
  
#11. Writing ta_grades data frame to a new .csv file inside assignment's repo's data/ 
#    directory with the filename grade_data.csv. 

write.csv(ta_grades, "data/grade_data.csv", row.names = FALSE)

#################
#### PART 2 ##### The Titanic
#################

#1. Using data() function to load Titanic data set
#   and using View() function to view the data set.

data("Titanic")
View(Titanic)

#2. Confirming that data set is in table format and 
#   not data frame format.

is.data.frame(Titanic)

#3. Creating data frame version of Titanic variable 
#   using the as.data.frame() function.

titanic_df <- as.data.frame(Titanic, stringsAsFactors = FALSE)

#4. Created variable that is a dataframe containing
#   the children from titanc_df by filtering.

children <- titanic_df[titanic_df$Age == "Child", ]

#5. Created variable whose value is the total number 
#   of children on the Titanic.Variable is then printed.

num_children <- sum(children$Freq)
print(num_children)

#6. Created a variable most_losses that is the row 
#   from the titanic_df data frame which represents the 
#   category of passenger with the largest number losses.
#   Variable is then printed.

max_casualties <- titanic_df[titanic_df$Survived == "No", "Freq"]
most_losses <- max(max_casualties)
print(most_losses)

#7. Defined a function that takes in a ticket class as an 
#   argument and returns a string describing the survival 
#   rates of the men and women.

calc_survival_rate <- function(ticket_class){
  
  class <- titanic_df[titanic_df$Class == ticket_class, ]   # Filtering the ticket classes and storing it in a variable.
  population <- sum(class$Freq)                             
  
  total_survivaldf <- class[class$Survived == "Yes", ]        # Filtering the class for survivals and storing it in a variable       
  total_survival <- sum(total_survivaldf$Freq)                # Calculate sum of the frequencys of who survived.
  
  male_total_df <- (class[class$Sex == "Male" & class$Age == "Adult", ]) # Filtering the class for the Male Adults in data frame
  total_male <- sum(male_total_df$Freq)                                  # Calculate sum of the frequencys of male adults in the class.
  male_survivaldf <- (male_total_df[male_total_df$Survived == "Yes", ])  # Filtering the male_total_df for male adults who survive.
  total_male_survivors <- sum(male_survivaldf$Freq)                      # Calculate sum of the frequencys of male adults who survived.
  
  survivors_without_males <- total_survival - total_male_survivors 
  women_and_children <- population - total_male
  
  women_and_children_survival_rate <- round(survivors_without_males / women_and_children * 100) # Calculating the percentages of the
  men_survival_rate <- round(total_male_survivors / total_male * 100)                           # survival rates and storing them into variables
  
  (paste("Of", ticket_class, "class,", men_survival_rate,"% of men survived and",  # Returning the string with the rates of survival
         women_and_children_survival_rate,"% of women and children survived."))    # inserted into the string.
  
}

#8. Calling function above and passing through each of the ticket classes then
#   printing the result.

print(calc_survival_rate("1st"))
print(calc_survival_rate("2nd"))
print(calc_survival_rate("3rd"))

#9. 
  # 1.What notable differences do you observe in the survival rates across classes(e.g., 1st vs 3rd)

  #     A notable diffrence between the three classes would be the fact that the survival rates dropped
  #     with the lowering classes with the only exception being the males in the 3rd class who survived at 
  #     a slighlty higher rate than their 2nd class counterparts. Overall the main difference is that 
  #     being in 1st class drastically increases your chances of survival.
  
  # 2.What notable differences do you observe in the survival rates between the men and the women and 
  #  children in each group?

   #    The most obvious difference is the huge gap between the women and children who survived 
   #    between 1st and 3rd class. From nearly 100% survival rate in the 1st class to less than
   #    45% in the 3rd class is a very steep drop. The drop from 2nd class to 3rd class is 
   #    signifcant as well considering the it dropped more than 50%.

#################
#### PART 3 ##### Life Expectancy
#################

#1. Original Source of Data.
#   http://gapm.io/ilex
#
#   Period 1800-1970, main source: v7, by Mattias Lindgren
#   Period 1970-2016, main source: IHME
#   Period 2017-2099, main source: UN

#2. Created variable that contains contents of a csv file.

life_exp_df <- read.csv("data/life_expectancy_years.csv", stringsAsFactors = FALSE)

#3. Defined a function that expects a single column of a dataframe 
#   and return the mathmatical mean of that column.

get_col_mean <- function(column){
  avg_column <- mean(column, na.rm = TRUE)
  avg_column
}

#4. Passing in the 2018 coulumn into the get_col_mean function and
#   then printing the result.

print(get_col_mean(life_exp_df$X2018))

#5. Created variable that is a list of the average life expectancies for
#   each year in the data set.                                            
                                                                         
world_averages <- lapply(life_exp_df[-1], get_col_mean)
  
#6. Created variable that is difference between the world average life expectnacy
#   in 1918 and in 2018.

world_change_18 <- get_col_mean(life_exp_df$X2018) - get_col_mean(life_exp_df$X1918)

#7. Added column to dataframe that contains change in life expectancy
#   from 1996_to_2016.

life_exp_df$recent_change <- life_exp_df$X2016 - life_exp_df$X1996

#8. Created a variable that has the number of countries whose life expectnacy
#   did not improve by 1 year or more between 1996 and 2016.Variable is 
#   then printed.

num_small_gain <- life_exp_df[life_exp_df$recent_change < 1, ]
print(num_small_gain)
View(life_exp_df)

#9. Created a variable with the name of the country with the largest gain in life 
#   expectancy between 1996-2016.

most_improved <- life_exp_df[life_exp_df$recent_change == max(life_exp_df$recent_change, na.rm = TRUE), "country"]

#10.Defined a function that takes a country and a data frame as arguments.
#   returns that country's change in life expextancy from 1968-2018.

get_country_change <- function(country_name, data_frame){
   cntry <- data_frame[data_frame$country == country_name, ]
   cntry$X2018 - cntry$X1968   
}

#11.Using function get_country_change with Haiti and life_exp_df passed through.

get_country_change("Haiti", life_exp_df)

#12.Defined a function that takes in two arguments and retruns a new
#   data frame representing a table of those two countries.

compare_countries <- function(data_frame, country_name_1, country_name_2){ # Made function that takes three arguments
  country_row1 <- data_frame[data_frame$country == country_name_1,]
  country_row2 <- data_frame[data_frame$country == country_name_2,]# Filtered and stored the specifc country rows in
                                                                   # variables
  country <- c(country_row1$country, country_row2$country)
  X2018 <- c(country_row1$X2018, country_row2$X2018)       # Extracted then stored country, X2018, recent_change into variables
  recent_change <- c(country_row1$recent_change, country_row2$recent_change)
  
  compare <- data.frame(country, X2018, recent_change, stringsAsFactors = FALSE) # Used variables above to make a new
  compare                                                                        # dataframe.Then returned dataframe.
}

#13. Using compare_countries() function with United States and Cuba.
#    Variable is then printed.

us_vs_cuba <- compare_countries(life_exp_df, "United States", "Cuba")
print(us_vs_cuba)
