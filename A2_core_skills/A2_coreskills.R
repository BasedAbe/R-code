#################
#### PART 1 ##### INTRO 
#################

# Creating variables for my age and name.

my_age <- 25
my_name <- "Abel"

# Defining a function with two arguments name and age.Then the
# function returns a character string with the arugmment inputs.

make_introduction <- function(name, age){
  intro <- paste("Hello, my name is", name, "and I'm", age, "years old")
  intro
}

# Creating variable my_intro and assigning it the result of 
# make_introduction with variables my_age & my_name passed 
# through it. Variable is then printed.

my_intro <- make_introduction(my_name, my_age)
print(my_intro)

# Creating variable casual_intro which replaces "Hello, my
# name is" in my_intro into  "Hey, I'm" by using sub().
# Variable is then printed.

casual_intro <- sub("Hello, my name is", "Hey, I'm", my_intro)
print(casual_intro)

# Creating variable capital_intro which capitalizes each word
# in my_intro by using str_to_title(). Variable is then printed.

capital_intro <- str_to_title(my_intro)
print(capital_intro)

# Creating variable intro_e_count which stores the number of
# lower case 'e's that appear in my_intro. Variable is then printed.

intro_e_count <- str_count(my_intro, "e")
print(intro_e_count)

#################
#### PART 2 ##### BOOKS 
#################

# Variable books assgined a vector of 6 books 
# that I like. Variable is then printed.

books <- c("As a man thinketh", "Power of the Habit", "They Came Before Colombus", 
           "Data and Goliath", "The Autobiography of Malcolm X", "In Defense of Food" )
print(books)

# Created variable top_three_books with the first three books
# of the books variable using bracket notation to select the 
# elements. Variable is then printed.

top_three_books <- books[1:3]
print(top_three_books)

# Created variable book_reviews that takes the names from the
# books vector and pastes "is a great read!" at the end.
# Variable is then printed.

book_reviews <- paste(books, "is a great read!")
print(book_reviews)

# Defining function remove_book which expects two arugments
# and returns a new vector but with the title at the specific
# index removed.

remove_book <- function(books_titles, index_num){
  books_titles[-index_num] 
}

# Creating variable books_without_five which holds
# the book vector with th 5th book removed.Variable 
# is then printed.

books_without_five <- remove_book(books, 5)
print(books_without_five)

# Creating variable long_titles which contains the elements
# in books vector longer than 15 characters.Variable then printed.

long_titles <- books[nchar(books) > 15]
print(long_titles)

#################
#### PART 3 ##### SQUARES 
#################

# Created variable numbers containing vector with numbers 1-201.

numbers <- c(1:201)

# Created variable squared_numbers contains numbers squared.

squared_numbers <- (numbers * numbers)

# Created variable squared_mean contains squared_numbers
# and finds the average. Variable is then printed.

squared_mean <- mean(squared_numbers)
print(squared_mean)

# Created variable squared_median contains squared_numbers
# and finds the median. Variable is then printed.

squared_median <- median(squared_numbers)
print(squared_median)

# Created variable perfect_squares which is a vector
# of only the values in numbers that are perfect squares.
# Variable is then printed.

sqrt_of_nums <- sqrt(numbers)
perfect_squares <- numbers[sqrt_of_nums == round(sqrt_of_nums)]
print(perfect_squares)

#################
#### PART 4 ##### 
#################

# Created variable spring_break which represents the
# first day of spring break as a Date-type value.

first_day <- "2020-03-21"
spring_break <- as.Date(first_day)
class(spring_break)

# Created variable today that represent the current   
# date as Date-type value using Sys.Date().

today <- Sys.Date()

# Created variable day_to_break which represents the
# amount of time from the current date till the start
# of spring break.Variable is then printed.

days_to_break <- (spring_break - today)
print(days_to_break)

# Created function change_year which takes two
# arguments date_type & year then returns the new
# date_type value with the year changed into whatever
# was placed into the arugment and returns it as a 
# as.Date value.

change_year <- function(date_type, year){
  date_as_string <- as.character(date_type)
  month_and_day <- substr(date_as_string, 5, 10)
  as.Date(paste(year,month_and_day, sep =""))
}

# Testing change_year function by passing in spring_break
# and assigning the new value into spring_break_2025.Variable
# is then printed.

spring_break_2025 <- change_year(spring_break, 2025)
class(spring_break_2025)
print(spring_break_2025)

# Define function date_has_passed which determines wheter the date 
# has passed by subtracting arugment date from the current date.

date_has_passed <- function(date_type_two){
  Sys.Date()-date_type_two > 0
}

# Testing date_has_passed function by calling this
# assignments deadline. Variable is then printed.

deadline <- "2020-01-23"
a2_deadline <- as.Date(deadline)
print(date_has_passed(a2_deadline))

# Created variable birthdays which contains list
# of birth dates of my close family as Date-values.
# Variable birthday is then printed.

mom <- "1970-03-22"
moms_bday <- as.Date(mom)

dad <- "1968-9-19"
dads_bday <- as.Date(dad)

sister <- "2009-04-15"
sisters_bday <- as.Date(sister)

me <- "1994-11-30"
my_bday <- as.Date(me)

birthdays <- list(
  ruth = moms_bday,
  kas = dads_bday,
  nina = sisters_bday,
  abe = my_bday
)

print(birthdays)

# Defining function get_next_birthday which takes in 
# a Date-type value and returns a Date-type value with
# that person's next birthday.

get_next_birthday <- function(birthdate){
  this_year <- change_year(birthdate, 2020)
  if (date_has_passed(birthdate)) {
    next_birthday <- change_year(birthdate, 2021)
    next_birthday
  }
  this_year
}

# Testing get_next_birthday by calling it and passing the
# values in birthdays using bracket notation or dollar-sign
# notation to access values in birthdays list

get_next_birthday(birthdays[[3]])

# Creating variable next_birthdays which contains list of 
# next birthdays.Variable is then printed.

next_birthdays <- lapply(birthdays, get_next_birthday)
print(next_birthdays)