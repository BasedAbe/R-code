# Assignment 5
# Accessing the Data API
library("httr")
library("jsonlite")
library("dplyr")
source("apikey.R")

base_uri <- "https://api.themoviedb.org/3"
key_param <- list("api_key" = tmdb_key)

# 1. Trending Actors

get_trending_table <- function(){
  endpoint <- "/trending/person/week"
  uri <- paste0(base_uri, endpoint)
  response <- GET(uri, query = key_param)
  trending_df <- fromJSON(content(response, "text"))
  
  trending_df$results %>% 
    filter(known_for_department == "Acting") %>%    # Filter for only the Acting roles
    mutate(photo = paste0("![Photo of Actor](https://image.tmdb.org/t/p/h100", profile_path, ")")) %>%  # Mutating column to have the photo with the profile path 
    select(name, photo) %>%   # Selecting the name and photo colunms 
    head(5) # Only the top 5 of the dataframe
}

########################################################################################################

# 2. Specific Actor Data

get_actor_data <- function(actor_name){
  endpoint <- "/search/person"
  uri <- paste0(base_uri, endpoint)
  params_list <- list("api_key" = tmdb_key, "query" = actor_name) # Passing in the arugment as the query
  response <- GET(uri, query = params_list)
  specific_actor_results <- fromJSON(content(response, "text"))
  
  results <- specific_actor_results$result 
  actor_id_num <- results$id
  actor_id <- actor_id_num[1]
  known_for <- results$known_for
  titles_actor_is_known_for <- known_for[[1]] %>% pull(title)
  
  get_person_endpoint <- paste0("/person/", actor_id)
  get_person_uri <- paste0(base_uri, get_person_endpoint)
  get_person_response <- GET(get_person_uri, query = params_list)
  get_person_results <- fromJSON(content(get_person_response, "text"))
  
  actor_name <- get_person_results[[5]] # Extracting the name, bio and imdb id
  actor_bio <- get_person_results[[8]]  # from the the get_person_results data
  actor_imdb <- get_person_results[[13]]# frame and saving them in variables
 
  actor_summary <- list(    # Placing the values from the variables above into a list
    name = actor_name,      # which can return on line 59.
    id = actor_id, 
    imdb_id = actor_imdb, 
    biography = actor_bio, 
    titles = titles_actor_is_known_for
    )
  return(actor_summary)
}

#########################################################################################################

#3. Will Smith is known for being in front of the camera but how many TV credits does 
#   Will Smith have as strictly being a television series producer and what was the average
#   rating of the shows where he received a production credit?

analyze_actor <- function(){
  endpoint <- "/person/2888/tv_credits"
  uri <- paste0(base_uri, endpoint)
  response <- GET(uri, query = key_param)
  results <- fromJSON(content(response, "text"))
  production_crew <- results$crew
  
  credits_data <- production_crew %>% 
    group_by(department) %>% 
    filter(department == "Production") %>% 
    select(department, episode_count, vote_average)
    
  
  tv_credits <- production_crew %>% 
    group_by(department) %>% 
    filter(department == "Production") %>%  # Filtering for only the credits that were Production
    summarise(total_productions = sum(episode_count),# Sum the total epsiodes produced and then 
              average_rating = mean(vote_average)) # getting the averges votes for those productions
                                                                                    

  analysis_results <- list(
    productions = tv_credits$total_productions, # Putting the values from the tv_credits data frame
    average = tv_credits$average_rating,         # into a list which is then returned in the line below
    raw = credits_data 
  )
  
  return(analysis_results)

}