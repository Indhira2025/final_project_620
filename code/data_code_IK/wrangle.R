library(httr2)
library(dplyr)
library(tidyverse)
library(janitor)
library(jsonlite)
library(purrr)

census_key <- "947c5387e37d34b28cd8f032b07f10501d8ede76"
url1 <- "https://api.census.gov/data/2021/pep/population"

request <- request(url1) %>% 
  req_url_query(
    get = "POP_2020,POP_2021,NAME",
    `for` = "state:*",
    key = census_key
  )

response <- req_perform(request)

population <- response %>% 
  resp_body_json(simplifyVector = TRUE) 

population <- population %>%  
  row_to_names(1) %>% 
  as_tibble() %>%  # Convert to tibble
  select(-state) %>% # Remove state column
  rename(state_name = NAME) %>%  # Rename NAME column to state_name
  pivot_longer(cols = starts_with("POP_"), names_to = "year", values_to = "population") %>% 
  mutate(
    year = str_remove(year, "POP_"),
    year = as.numeric(year),
    population = as.numeric(population),
    state = case_when(
      state_name == "District of Columbia" ~ "DC",
      state_name == "Puerto Rico" ~ "PR",
      .default = state.abb[match(state_name, state.name)]
    )
  )

url2 <- "https://github.com/datasciencelabs/2024/raw/refs/heads/main/data/regions.json"
regions <- fromJSON(url2)

regions <- regions %>%
  mutate(
    region_name = ifelse(region_name == "New York and New Jersey, Puerto Rico, Virgin Islands", 
                         "NY, NJ & Territories", region_name),
    region = as.factor(as.character(region))
  ) %>% 
  unnest(states) %>% 
  rename(state_name = states)

population <- population %>% 
  left_join(regions, by = "state_name")
head(population)

