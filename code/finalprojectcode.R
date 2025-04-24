library(httr2)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library (janitor))
library(ggplot2)
library(scales)
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(tidyverse))


##setwd("C:/Users/indhi/Documents/final_project_620")
getwd()
##Retreving Data from cdc Website for covid cases, deaths, hospitalizations and ##vaccines

get_cdc_data_v <- function(endpoint) {
  api_url <- paste0("https://data.cdc.gov/resource/", endpoint, ".json")
  
  data <- request(api_url) |> 
    req_url_query("$limit" = 10000000) |> 
    req_perform() |> 
    resp_body_json(simplifyVector = TRUE)
  
  return(data)
}
#cases_raw <- get_cdc_data("pwn4-m3yp")
#hosp_raw <- get_cdc_data("39z2-9zu6")
deaths_raw_v <- get_cdc_data_v("r8kw-7aab")
#vax_raw <- get_cdc_data("rh2h-3yt2")
#head(deaths_raw_v)



# Working on the covid deaths data 2020-2025
#View(deaths_raw)
#head(deaths_raw_v)
state_abbreviations_v <- c(state.abb, "DC", "PR")
state_names_v <- c(state.name, "District of Columbia", "Puerto Rico")
# Convert and summarize deaths data
deaths_clean_v <- deaths_raw_v %>%
  filter(state != "United States")%>%
  mutate(
    start_date = as_date(start_date),
    covid_19_deaths = as.numeric(covid_19_deaths),
    mmwr_year = epiyear(start_date),
    mmwr_week = epiweek(start_date),
  )%>%
  select(start_date, state, mmwr_year, mmwr_week, covid_19_deaths) %>%
  filter(start_date > as.Date("2020-01-01")) %>%
  arrange(state, mmwr_year)  
#print(head(deaths_clean_v))



suppressPackageStartupMessages(library(zoo))  

# Summarize deaths per day (national total)
us_deaths_daily_v <- deaths_clean_v %>%
  group_by(start_date) %>%
  summarise(total_deaths = sum(covid_19_deaths, na.rm = TRUE), .groups = "drop") %>%
  arrange(start_date) %>%
  mutate(rolling_avg_7day = zoo::rollmean(total_deaths, k = 7, fill = NA, align = "right"))

# Plot
plot1_v <- ggplot(us_deaths_daily_v, aes(x = start_date)) +
  geom_col(aes(y = total_deaths), fill = "red", alpha = 0.8) +
  geom_line(aes(y = rolling_avg_7day), color = "darkred", size = 1) +
  labs(
    title = "Daily COVID-19 Deaths in the US",
    subtitle = "With 10-Day Rolling Average",
    x = "Date",
    y = "Number of Deaths"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_date(date_breaks = "6 months",date_labels = "%b %Y")+
  scale_y_continuous(limits = c(0, 150000), labels = scales::label_comma())
#plot1_v

#ggsave("docs/new/Covid deaths in US (2020-2025).pdf", plot= plot1, width = 12, height = 8)
#ggsave("docs/new/Covid deaths in US (2020-2025).png", plot = plot1, width = 12, height = 8, dpi = 300)


## Divide the pandemic period, January 2020 to April 2025 into waves

variant_waves_v <- tibble::tibble(
  start = as_date(c("2020-03-01", "2021-01-01", "2021-07-01", "2021-12-01", "2022-04-01", "2023-06-15")),
  end   = as_date(c("2021-02-28", "2021-06-30", "2021-10-30", "2022-03-31", "2023-06-14", "2024-03-31")),
  variant = c("1st Wave: Original", 
              "2nd Wave: Alpha", 
              "3rd Wave: Delta", 
              "4th Wave: Omicron BA.1", 
              "5th Wave: Omicron Other",
              "6th Wave: XBB")
)

# Plot2: Covid deaths with variant overlay by state
plot2_v <- ggplot(us_deaths_daily_v, aes(x = start_date, y = total_deaths)) +
  geom_rect(
    data = variant_waves_v,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = variant),
    inherit.aes = FALSE,
    alpha = 0.2
  ) +
  geom_line(aes(y = rolling_avg_7day), color = "darkred", size = 1) +
  scale_x_date(date_labels = "%b/%Y", date_breaks = "6 months") +
  labs(
    title = "US Weekly COVID-19 Deaths with Variant Waves (2020–2025)",
    x = "Month/Year",
    y = "Total Deaths",
    fill = "Variant Waves"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(limits = c(0, 150000), labels = scales::label_comma())
#plot2_v
#ggsave("docs/Covid deaths with variant overlay by state.pdf", plot= plot2, width = 12, height = 8)
#ggsave("docs/Covid deaths with variant overlay by state.png", plot = plot2, width = 12, height = 8, dpi = 300)

library(stringr)
library(ggplot2)
suppressPackageStartupMessages(library(viridis))

##A heatmap showing intensity of deaths by week and state.

deaths_filtered_v <- deaths_clean_v %>%
  filter(!state %in% c("United States", "New York City"))
deaths_quarterly_v <- deaths_filtered_v %>%
  mutate(quarter = paste0(year(start_date), " Q", quarter(start_date))) %>%
  group_by(state, quarter) %>%
  summarise(total_deaths = sum(covid_19_deaths, na.rm = TRUE), .groups = "drop")
deaths_quarterly_v$quarter <- factor(deaths_quarterly_v$quarter, levels = unique(deaths_quarterly_v$quarter))

#plot4: Heatmap showing intensity of deaths by state
plot4_v <- ggplot(deaths_quarterly_v, aes(x = quarter, y = state, fill = total_deaths)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "magma", name = "Deaths", direction = -1) +
  labs(
    title = "COVID-19 Deaths by State (Quarterly)",
    x = "Quarter",
    y = "State"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 6)
  )
#plot4_v  
#ggsave("docs/new/COVID-19 Deaths by State (Quarterly).pdf", plot= plot4, width = 12, height = 8)
#2ggsave("docs/new/COVID-19 Deaths by State (Quarterly).png", plot = plot4, width = 12, height = 8, dpi = 300)

## Reteriving the population of US states form Census website

suppressPackageStartupMessages(library(jsonlite))
library(purrr)

census_key_v <- "a498a5717131540bcea2c82b3e8fd47a367cef98"
url_v <- "https://api.census.gov/data/2021/pep/population"
request_v <- request(paste0(url_v,
                            paste0("?get=POP_2020,POP_2021,NAME&for=state:*&key=",
                                   census_key_v)))
#print(request_v)
response_v <- request_v |> req_perform() 
#response_v

#content_type_v <- resp_content_type(response_v)
#cat("The content type of the response is:", content_type_v, "\n")
population_v <- response_v |>resp_body_string()|>jsonlite::fromJSON()
#head(population_v)

population_v <- population_v %>%
  row_to_names(row_number = 1)%>%
  as_tibble()
#head(population_v)

pop_latest_v<- read.csv("data/population_estimate_census.csv")
#head(pop_latest)
pop_latest_new2_v <- pop_latest_v %>%
  mutate(
    POP_2022 = as.character(gsub(",", "", X2022)),
    POP_2023 = as.character(gsub(",", "", X2023)),
    POP_2024 = as.character(gsub(",", "", X2024)),
    state_name = str_remove(state_name, "^\\.")
  ) %>%
  rename(NAME= "state_name") %>% 
  select(NAME, POP_2022, POP_2023, POP_2024) %>% 
  as.data.frame()
#pop_latest_new2_v 

population_new_v <- population_v %>% 
  left_join(pop_latest_new2_v, by = "NAME")
#head(population_new_v)

cleaned_population_v <- population_new_v %>% #changing this
  select(-state) %>%
  rename(state_name = NAME)%>%
  pivot_longer(cols = starts_with("POP_"),  
               names_to = "Year",
               values_to = "Population") %>%
  mutate(Year = gsub("POP_", "", Year),  
         Year = as.integer(Year), 
         Population = as.numeric(Population))  

state_abbreviations_v <- c(state.abb, "DC", "PR")
state_names_v <- c(state.name, "District of Columbia", "Puerto Rico")

cleaned_population_v <- cleaned_population_v %>%
  mutate(
    state = case_when(
      state_name %in% state_names_v ~ state_abbreviations_v[match(state_name, state_names_v)]
    )
  )
#head(cleaned_population_v)
#unique(cleaned_population_v$Year)
#colnames(cleaned_population_v)

#adding Region name data
url_iv <- "https://github.com/datasciencelabs/2024/raw/refs/heads/main/data/regions.json"
# regions <- use fromJSON to read as a data.frame
regions_data_v <- fromJSON(url_iv)
#str(regions_data)
#head(regions_data)
#View(regions_data)
#unique(regions_data_v$region_name)
# Convert the JSON data to a data frame and process it
regions_v <- regions_data_v %>%
  # Unnest the states from the nested list
  unnest(cols = states) %>%
  # Rename the columns
  rename(state_name = states) %>%
  # Rename the region with a long name to something shorter
  mutate(region_name = ifelse(region_name == "New York and New Jersey, Puerto Rico, Virgin Islands", "NYNJPV", region_name)) %>%
  # Ensure the region_name column is a factor
  mutate(region_name = factor(region_name))

# Print the first few rows of the regions data frame
#head(regions_v)
#view(regions)
#class(regions$region_name)

cleaned_population_v <- cleaned_population_v %>%
  left_join(regions_v, by = "state_name")
#head(cleaned_population_v)

all_dates_v <- data.frame(date = seq(make_date(2020, 1, 25),
                                     make_date(2024, 12, 31), 
                                     by = "week")) |>
  mutate(date = ceiling_date(date, unit = "week", week_start = 7) - days(1)) |>
  mutate(mmwr_year = epiyear(date), mmwr_week = epiweek(date)) 


dates_and_pop_v <- cross_join(all_dates_v, data.frame(state = unique(cleaned_population_v$state))) |> left_join(cleaned_population_v, by = c("state", "mmwr_year" = "Year"))
#head(dates_and_pop_v)

#unique(dates_and_pop_v$mmwr_year)
#View(dates_and_pop)


dates_and_pop1_v <-dates_and_pop_v %>% 
  mutate(mmwr_year= as.character(mmwr_year)) 

#head(dates_and_pop1_v)
#colnames(deaths_clean_v)
#unique(deaths_clean_v$mmwr_year)

us_deaths_weekly_v <- deaths_clean_v %>%
  rename(state_name= "state")%>%
  group_by(start_date,state_name, mmwr_week, mmwr_year) %>%
  summarise(total_deaths = sum(covid_19_deaths, na.rm = TRUE), .groups = "drop") %>% 
  arrange(state_name, mmwr_week)
#unique(us_deaths_weekly_v$mmwr_year)

us_deaths_weeklyregion_v <-us_deaths_weekly_v %>% 
  left_join(regions_v, by = "state_name")
#head(us_deaths_weeklyregion_v)  

#View(us_deaths_weeklyregion)
# merging the population data
datdeaths_v<-us_deaths_weeklyregion_v %>%
  left_join(dates_and_pop_v, by = c("state_name", "mmwr_year", "mmwr_week","region_name"))

#View(datdeaths1)

#datdeaths <- dates_and_pop %>%
#left_join(us_deaths_weekly, by = c("state_name", "mmwr_year", "mmwr_week"))  
#left_join(pop_latest_new1,by = c("state_name", "mmwr_year","mmwr_week"))
#head(datdeaths_v)
#View(datdeaths_v)

#colnames(datdeaths_v)
#unique(datdeaths_v$mmwr_year)


##  For each period, compute the death rates by state. Describe which states did 
## better or worse during the different periods.


#  Merge variant period info to deaths+population dataset
datdeaths_waves_v <- datdeaths_v %>%
  mutate(wave = case_when(
    start_date >= as_date("2020-03-01") & start_date <= as_date("2021-02-28") ~ "1st Wave: Original",
    start_date >= as_date("2021-01-01") & start_date <= as_date("2021-06-30") ~ "2nd Wave: Alpha",
    start_date >= as_date("2021-07-01") & start_date <= as_date("2021-10-30") ~ "3rd Wave: Delta",
    start_date >= as_date("2021-12-01") & start_date <= as_date("2022-03-31") ~ "4th Wave: Omicron BA.1",
    start_date >= as_date("2022-04-01") & start_date <= as_date("2023-06-14") ~ "5th Wave: Omicron-BA",
    start_date >= as_date("2023-06-15") ~ "6th Wave:Omicron other",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(wave))


#head(datdeaths_waves_v)




#Compute average death rates by region and wave
region_wave_death_rates_v <- datdeaths_waves_v %>%
  mutate(population = as.numeric(unlist(Population))) %>%
  group_by(region_name) %>%
  mutate(population = ifelse(is.nan(Population), NA, Population)) %>%
  fill(Population, .direction = "down")
region_data_v<- region_wave_death_rates_v %>%   
  group_by(region_name,wave) %>%
  summarise(
    total_deaths = sum(total_deaths, na.rm = TRUE),
    total_population = mean(Population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(death_rate_per_100k = (total_deaths / total_population) * 100000)
#View(region_data)

library(ggplot2)



# Treand analysis of Death Rates Across COVID-19 Waves by Region

# Treat wave as an ordered factor (for display)
region_data_v <- region_data_v %>%
  mutate(wave = factor(wave, levels = unique(wave), ordered = TRUE))

plot_trend_v<- ggplot(region_data_v, aes(x = wave, y = death_rate_per_100k, group = region_name, color = region_name)) +
  geom_point(size = 3) +
  geom_line(aes(group = region_name)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Linear Trend of Death Rates Across COVID-19 Waves by Region",
    x = "Wave",
    y = "Death Rate per 100,000",
    color = "Region"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot_trend_v

#ggsave("docs/new/Linear Trend of Death Rates Across COVID-19 Waves by Region.pdf", plot= plot_trend, width = 12, height = 8)
#ggsave("docs/new/Linear Trend of Death Rates Across COVID-19 Waves by Region.png", plot = plot_trend, width = 12, height = 8, dpi = 300)



#death rates (per 100,000) by state and wave
state_wave_death_rates_v <- datdeaths_waves_v %>%
  mutate(population = as.numeric(unlist(Population))) %>%
  group_by(state_name) %>%
  mutate(population = ifelse(is.nan(Population), NA, Population)) %>%
  fill(Population, .direction = "down")
state_data_v <-state_wave_death_rates_v %>% 
  group_by(state_name,wave) %>% 
  summarise(
    total_deaths = sum(total_deaths, na.rm = TRUE),
    avg_population = mean(Population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(death_rate_per_100k = (total_deaths / avg_population) * 100000)
#View(state_data)

# Table 2: top states by death rate per wave
topstate_by_wave_v <- state_data_v %>%
  group_by(wave) %>%
  slice_max(death_rate_per_100k, n = 1) %>%
  arrange(desc(death_rate_per_100k)) %>% 
  ungroup() 
summary_table1_v <- topstate_by_wave_v %>%
  select(State = state_name, Wave = wave,Population= avg_population, `Death Rate` = death_rate_per_100k) %>%
  arrange(Wave, desc(`Death Rate`))  
summary_table1_v <- topstate_by_wave_v %>%
  select(State = state_name, Wave = wave, `Death Rate` = death_rate_per_100k) %>%
  mutate(`Death Rate` = round(`Death Rate`, 1)) %>%
  arrange(Wave, desc(`Death Rate`))
kable(summary_table1_v, caption = "Table 1: Top States by COVID-19 Death Rate  per 100,000 population")

top5_by_wave_v <- state_data_v %>%
  group_by(wave) %>%
  slice_max(death_rate_per_100k, n = 5) %>%
  arrange(desc(death_rate_per_100k)) %>% 
  ungroup() 
# Prepare your summary table BEFORE using kable
summary_table_v <- top5_by_wave_v %>%
  select(State = state_name, Wave = wave, `Death Rate` = death_rate_per_100k) %>%
  arrange(Wave, desc(`Death Rate`))  # optional for nicer ordering
#kable(summary_table_v, caption = "Top 5 States by COVID-19 Death Rate per Variant Wave")
summary_table_v <- top5_by_wave_v %>%
  select(State = state_name, Wave = wave, `Death Rate` = death_rate_per_100k) %>%
  mutate(`Death Rate` = round(`Death Rate`, 1)) %>%
  arrange(Wave, desc(`Death Rate`))

# Plot6: Top 5 States by COVID-19 Death Rate for Each Variant Wave facet by wave
plot6_v <- ggplot(top5_by_wave_v, aes(x = fct_reorder(state_name, death_rate_per_100k), y = death_rate_per_100k, fill = wave)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ wave, scales = "free_y") +
  labs(
    title = "Top 5 States by COVID-19 Death Rate for Each Variant Wave",
    x = "State",
    y = "Death Rate (per 100,000 people)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )
#plot6_v
#ggsave("docs/new/Top 5 States by COVID-19 Death Rate for Each Variant Wave.pdf", plot= plot6, width = 12, height = 8)
#2ggsave("docs/new/Top 5 States by COVID-19 Death Rate for Each Variant Wave.png", plot = plot6, width = 12, height = 8, dpi = 300)



top_state_per_year_v <- datdeaths_v %>%
  filter(mmwr_year != 2025) %>% 
  group_by(mmwr_year, state_name) %>%
  summarise(
    total_deaths = sum(total_deaths, na.rm = TRUE),
    total_population = mean(Population, na.rm = TRUE),
    death_rate_per_100k = (total_deaths / total_population) * 100000,
    .groups = "drop"
  ) %>%
  group_by(mmwr_year) %>%
  slice_max(death_rate_per_100k, n = 1) %>%  # top 1 state per year
  ungroup() %>%
  arrange(mmwr_year) %>%
  rename(
    Year = mmwr_year,
    State = state_name,
    Covid_Deaths = total_deaths,
    Population = total_population,
    Death_rate_per_100k = death_rate_per_100k
  ) %>%
  kable(caption = "Table 2: Summary - States with highest death rate (by year)")

top_state_per_year_v


top5_statesname_v <- datdeaths_v %>%
  filter(mmwr_year != 2025) %>% 
  group_by(mmwr_year, state_name) %>%
  summarise(
    total_deaths = sum(total_deaths, na.rm = TRUE),
    total_population = mean(Population, na.rm = TRUE),
    death_rate_per_100k = (total_deaths / total_population) * 100000,
    .groups = "drop"
  ) %>%
  group_by(mmwr_year) %>%
  slice_max(death_rate_per_100k, n = 1) %>%  # top 1 state per year
  ungroup() %>%
  slice_head(n = 5) %>%
  pull(state_name)
#top5_statesname_v

top5_data_v <- datdeaths_v %>%
  filter(state_name %in% top5_statesname_v) 


library(ggplot2)
top_state_per_year_data_v <- datdeaths_v %>%
  filter(mmwr_year != 2025) %>% 
  group_by(mmwr_year, state_name) %>%
  summarise(
    total_deaths = sum(total_deaths, na.rm = TRUE),
    total_population = mean(Population, na.rm = TRUE),
    death_rate_per_100k = (total_deaths / total_population) * 100000,
    .groups = "drop"
  ) %>%
  group_by(mmwr_year) %>%
  slice_max(death_rate_per_100k, n = 1) %>%  # top 1 state per year
  ungroup() %>% 
  arrange(mmwr_year) %>%
  rename(
    Year = mmwr_year,
    State = state_name,
    Covid_Deaths = total_deaths,
    Population = total_population,
    Death_rate_per_100k = death_rate_per_100k
  ) 
  
#top_state_per_year_data_v

plot8_v <- ggplot(top_state_per_year_data_v, aes(x = factor(Year), y = Death_rate_per_100k, fill = State)) +
  geom_col(width = 0.6) +
  labs(
    title = "Top State by COVID-19 Death Rate per 100,000 (by Year)",
    x = "Year",
    y = "Death Rate per 100,000",
    fill = "State"
  ) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "bottom")
#plot8_v
#ggsave("docs/new/Top State by COVID-19 Death Rate per 100,000 (by Year).pdf", plot= plot8, width = 12, height = 8)
#ggsave("docs/new/Top State by COVID-19 Death Rate per 100,000 (by Year).png", plot = plot8, width = 12, height = 8, dpi = 300)


# Plot9: Facet plot Covid deaths for top 10 states  
facet_plot_v<- ggplot(top5_data_v, aes(x = start_date, y = total_deaths)) +
  geom_line(color = "steelblue", size = 1) +
  geom_smooth(se = FALSE, color = "darkred", method = "loess", span = 0.3) +
  facet_wrap(~ state_name,scales = "fixed") +
  labs(
    title = "COVID-19 Deaths Over Time – Faceted by States",
    x = "Date",
    y = "Deaths"
  ) +
  scale_x_date(date_breaks = "9 months", date_labels = "%b\n%Y") +
  theme_minimal(base_size = 15) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
#facet_plot_v
#ggsave("docs/new/facet_top5_statesnew.png", plot = facet_plot, width = 12, height = 8, dpi = 300)
#ggsave("docs/new/facet_top5_statesnew.pdf", plot = facet_plot, width = 12, height = 8)


library(ggplot2)
library(patchwork)

suppressPackageStartupMessages(library(gridExtra))

myplot_v <- suppressMessages(suppressWarnings(
  grid.arrange(plot2_v, plot4_v, plot_trend_v, plot6_v, ncol = 2)
))
myplot_v

ggsave("docs/myplot_v.pdf", plot = myplot_v, width = 6, height = 4)








### Medha's Code 

library(httr2)
library(dplyr)
library(lubridate)
library(janitor)
library(tidyverse)
library(jsonlite)
library(knitr)
library(MMWRweek)



get_cdc_data_msr <- function(endpoint) {
  api_url <- paste0("https://data.cdc.gov/resource/", endpoint, ".json")
  data <- request(api_url) |> 
    req_url_query("$limit" = 10000000) |> 
    req_perform() |> 
    resp_body_json(simplifyVector = TRUE)
  return(data)
}

cases_raw_msr <- get_cdc_data_msr("pwn4-m3yp")
hosp_raw_msr <- get_cdc_data_msr("39z2-9zu6")
deaths_raw_msr <- get_cdc_data_msr("r8kw-7aab")
vax_raw_msr <- get_cdc_data_msr("rh2h-3yt2")

deaths_clean_msr <- deaths_raw_msr %>%
  mutate(
    start_date = as_date(start_date),
    mmwr_year = epiyear(start_date),
    mmwr_week = epiweek(start_date),
    covid_19_deaths = as.numeric(covid_19_deaths)
  ) %>%
  mutate(state = dplyr::recode(state, !!!setNames(state.abb, state.name))) %>%
  dplyr::select(start_date, state, mmwr_year, mmwr_week, covid_19_deaths)

cases_clean_msr <- cases_raw_msr %>%
  mutate(
    start_date = as_date(start_date),
    mmwr_year = epiyear(start_date),
    mmwr_week = epiweek(start_date)
  ) %>%
  dplyr::select(state, mmwr_year, mmwr_week, new_cases)

hosp_clean_msr = hosp_raw_msr |> 
  dplyr::select(jurisdiction, collection_date, hosp = new_covid_19_hospital) |>
  mutate(
    collection_date = as_date(collection_date),
    mmwr_week = epiweek(collection_date), 
    mmwr_year = epiyear(collection_date)
  ) |>
  mutate(hosp = as.integer(hosp)) |>
  rename(state = jurisdiction) |>
  group_by(state, mmwr_week, mmwr_year) |>
  summarise(total_hosp = sum(hosp, na.rm = TRUE))

vax_clean_msr <- vax_raw_msr %>%
  filter(date_type == "Admin") %>%
  mutate(
    date = as_date(date),
    mmwr_year = epiyear(date),
    mmwr_week = epiweek(date),
    series_complete = as.numeric(series_complete_cumulative),
    booster = as.numeric(booster_cumulative)
  ) %>%
  group_by(date, location, mmwr_year, mmwr_week) %>%
  summarise(
    series_complete = max(series_complete, na.rm = TRUE),
    booster = max(booster, na.rm = TRUE)
  ) %>%
  rename(state = location) %>%
  dplyr::select(date, state, mmwr_year, mmwr_week, series_complete, booster)


census_key <- "a498a5717131540bcea2c82b3e8fd47a367cef98"
url <- "https://api.census.gov/data/2021/pep/population"
request <- request(paste0(url, 
                          paste0("?get=POP_2020,POP_2021,NAME&for=state:*&key=",
                                 census_key)))
response <- request |> req_perform()
population <- response |> resp_body_string() |> jsonlite::fromJSON()

population_msr <- population %>%
  row_to_names(row_number = 1) %>%
  as_tibble()

clean_msr_population <- population_msr %>%
  dplyr::select(-state) %>%
  rename(state_name = NAME) %>%
  pivot_longer(cols = starts_with("POP_"),  
               names_to = "Year",
               values_to = "Population") %>%
  mutate(
    Year = gsub("POP_", "", Year),  
    Year = as.integer(Year), 
    Population = as.numeric(Population)
  )

state_abbreviations <- c(state.abb, "DC", "PR")
state_names <- c(state.name, "District of Columbia", "Puerto Rico")

clean_msr_population <- clean_msr_population %>%
  mutate(
    state = case_when(
      state_name %in% state_names ~ state_abbreviations[match(state_name, state_names)]
    )
  )

url <- "https://github.com/datasciencelabs/2024/raw/refs/heads/main/data/regions.json"
regions_data_msr <- fromJSON(url)
regions_msr <- regions_data_msr %>%
  unnest(cols = states) %>%
  rename(state_name = states) %>%
  mutate(
    region_name = ifelse(region_name == "New York and New Jersey, Puerto Rico, Virgin Islands", "NYNJPV", region_name),
    region_name = factor(region_name)
  )

clean_msr_population <- clean_msr_population %>%
  left_join(regions_msr, by = "state_name")


all_dates <- data.frame(date = seq(make_date(2020, 1, 1), make_date(2025, 4, 14), by = "week")) %>%
  mutate(
    date = ceiling_date(date, unit = "week", week_start = 7) - days(1),
    mmwr_year = epiyear(date), 
    mmwr_week = epiweek(date)
  )

master_data_msr <- tidyr::expand_grid(
  date = unique(all_dates$date),
  state = unique(clean_msr_population$state)
) %>%
  mutate(
    mmwr_year = epiyear(date),
    mmwr_week = epiweek(date)
  )

master_data_msr <- master_data_msr %>%
  left_join(
    clean_msr_population %>% dplyr::select(state, state_name, Year, Population, region_name),
    by = c("state", "mmwr_year" = "Year")
  )

master_data_msr <- master_data_msr %>%
  mutate(wave = case_when(
    date >= as_date("2020-03-01") & date <= as_date("2021-02-28") ~ "1st Wave: Original",
    date >= as_date("2021-01-01") & date <= as_date("2021-06-30") ~ "2nd Wave: Alpha",
    date >= as_date("2021-07-01") & date <= as_date("2021-11-30") ~ "3rd Wave: Delta",
    date >= as_date("2021-12-01") & date <= as_date("2022-03-31") ~ "4th Wave: Omicron BA.1",
    date >= as_date("2022-04-01") & date <= as_date("2023-06-14") ~ "5th Wave: Omicron BA.2–BA.5–XBB",
    date >= as_date("2023-06-15") & date <= as_date("2024-03-31") ~ "6th Wave: XBB",
    TRUE ~ NA_character_
  ))%>%
  filter(!is.na(wave))

master_data_msr <- master_data_msr %>%
  left_join(deaths_clean_msr %>% rename(date = start_date), 
            by = c("date", "state", "mmwr_year", "mmwr_week")) %>%
  left_join(cases_clean_msr, 
            by = c("state", "mmwr_year", "mmwr_week")) %>%
  left_join(hosp_clean_msr, 
            by = c("state", "mmwr_year", "mmwr_week")) %>%
  left_join(vax_clean_msr, 
            by = c("date", "state", "mmwr_year", "mmwr_week"))


head(master_data_msr)
dim(master_data_msr)

assign_wave_msr <- function(date) {
  case_when(
    date >= as_date("2020-03-01") & date <= as_date("2021-02-28") ~ "1st Wave: Original",
    date >= as_date("2021-01-01") & date <= as_date("2021-06-30") ~ "2nd Wave: Alpha",
    date >= as_date("2021-07-01") & date <= as_date("2021-11-30") ~ "3rd Wave: Delta",
    date >= as_date("2021-12-01") & date <= as_date("2022-03-31") ~ "4th Wave: Omicron BA.1",
    date >= as_date("2022-04-01") & date <= as_date("2023-06-14") ~ "5th Wave: Omicron BA.2–BA.5–XBB",
    date >= as_date("2023-06-15") & date <= as_date("2024-03-31") ~ "6th Wave: XBB",
    TRUE ~ NA_character_
  )
}

deaths_clean_msr <- deaths_clean_msr %>%
  mutate(wave = assign_wave_msr(start_date)) %>%
  filter(!is.na(wave))

cases_clean_msr <- cases_clean_msr %>%
  mutate(
    date = MMWRweek::MMWRweek2Date(MMWRyear = mmwr_year, MMWRweek = mmwr_week),
    wave = assign_wave_msr(date)
  ) %>%
  filter(!is.na(wave))

hosp_clean_msr <- hosp_clean_msr %>%
  mutate(date = MMWRweek::MMWRweek2Date(MMWRyear = mmwr_year, MMWRweek = mmwr_week),
         wave = assign_wave_msr(date)) %>%
  filter(!is.na(wave))

# National totals by wave
deaths_by_wave <- deaths_clean_msr %>%
  group_by(wave) %>%
  summarise(deaths = sum(covid_19_deaths, na.rm = TRUE))

cases_by_wave <- cases_clean_msr %>%
  group_by(wave) %>%
  summarise(cases = sum(as.numeric(new_cases), na.rm = TRUE))

hosp_by_wave <- hosp_clean_msr %>%
  group_by(wave) %>%
  summarise(hospitalizations = sum(total_hosp, na.rm = TRUE))


# Join all together
wave_summary <- deaths_by_wave %>%
  left_join(cases_by_wave, by = "wave") %>%
  left_join(hosp_by_wave, by = "wave") %>%
  mutate(
    CFR = deaths / cases,
    deaths_per_1000_cases = deaths / cases * 1000,
    hosp_per_1000_cases = hospitalizations / cases * 1000
  )

print(wave_summary)

# CFR plot
cfr_plot_msr <- ggplot(wave_summary, aes(x = wave, y = CFR)) +
  geom_col(fill = "hotpink") +
  labs(title = "Case Fatality Rate (CFR) by COVID-19 Variant Wave",
       x = "Variant Wave", y = "CFR (Deaths / Cases)") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 6))


# Hospitalization per 1,000 cases plot
hosp_plot_msr <- ggplot(wave_summary, aes(x = wave, y = hosp_per_1000_cases)) +
  geom_col(fill = "orange") +
  labs(title = "Hospitalizations per 1,000 Cases by Variant Wave",
       x = "Variant Wave", y = "Hospitalizations per 1,000 Cases") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 6))

# For each wave, get the max cumulative series_complete (fully vaccinated)
library(data.table)
setDT(vax_clean_msr)[, wave := fcase(
  date %between% as.IDate(c("2020-03-01", "2021-02-28")), "1st Wave: Original",
  date %between% as.IDate(c("2021-01-01", "2021-06-30")), "2nd Wave: Alpha",
  date %between% as.IDate(c("2021-07-01", "2021-11-30")), "3rd Wave: Delta",
  date %between% as.IDate(c("2021-12-01", "2022-03-31")), "4th Wave: Omicron BA.1",
  date %between% as.IDate(c("2022-04-01", "2023-06-14")), "5th Wave: Omicron BA.2–BA.5–XBB",
  date %between% as.IDate(c("2023-06-15", "2024-03-31")), "6th Wave: XBB",
  default = NA_character_
)]

vax_by_wave <- vax_clean_msr %>% 
  filter(!is.na(wave)) %>%
  group_by(wave) %>%
  summarise(fully_vax = max(series_complete, na.rm = TRUE),
            booster = max(booster, na.rm = TRUE))

wave_summary <- wave_summary %>%
  left_join(vax_by_wave, by = "wave")

# Vax per 1,000 cases plot
vax_plot_msr <- ggplot(wave_summary, aes(x = wave, y = fully_vax)) +
  geom_col(fill = "lavender") +
  labs(title = "Completed Vaccination Series by Variant Wave",
       x = "Variant Wave", y = "Vaccination series complete") +
  theme_classic() + 
  theme(axis.text.x = element_text(size = 6)) 

## Aggregate Data by Region
regional_data <- master_data_msr %>%
  group_by(region_name, mmwr_year, mmwr_week, wave) %>%
  summarise(
    cases = sum(as.numeric(new_cases), na.rm = TRUE),
    deaths = sum(covid_19_deaths, na.rm = TRUE),
    hosp = sum(total_hosp, na.rm = TRUE),
    population = sum(Population, na.rm = TRUE), .groups = "drop"
  ) %>%
  mutate(
    cases_per_100k = (cases/population)*100000,
    cfr = deaths/cases,
    hosp_per_100k = (hosp/population)*100000
  )
# Convert MMWR weeks to actual dates for plotting
regional_data <- regional_data %>%
  mutate(date = MMWRweek::MMWRweek2Date(MMWRyear = mmwr_year, MMWRweek = mmwr_week))

# Define wave periods separately (should match master_data's wave assignments)
wave_dates <- data.frame(
  wave = c("1st Wave: Original", "2nd Wave: Alpha", "3rd Wave: Delta",
           "4th Wave: Omicron BA.1", "5th Wave: Omicron BA.2–BA.5–XBB",
           "6th Wave: XBB"),
  start = as.Date(c("2020-03-01", "2021-01-01", "2021-07-01",
                    "2021-12-01", "2022-04-01", "2023-06-15")),
  end = as.Date(c("2021-02-28", "2021-06-30", "2021-11-30",
                  "2022-03-31", "2023-06-14", "2024-03-31"))
)

# Create the plot
regional_data |>  filter(!is.na(region_name)) |>
  ggplot() +
  geom_rect(
    data = wave_dates,  # Use separate wave data
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = wave),
    alpha = 0.2
  ) +
  geom_line(aes(x = date, y = cases_per_100k, color = "Cases per 100k"), linewidth = 0.5) +
  geom_line(aes(x = date, y = hosp_per_100k, color = "Hospitalizations per 100k"), linewidth = 0.5) +
  scale_fill_brewer(palette = "Pastel1", name = "Variant Wave") +
  scale_color_manual(values = c("Cases per 100k" = "blue", "Hospitalizations per 100k" = "red")) +
  facet_wrap(~region_name, scales = "free_y", ncol = 5) +
  labs(
    title = "COVID-19 Trends by US Region with Variant Wave Periods",
    x = "Date",
    y = "Rate per 100,000 Population",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(ncol = 1)) +
  guides(fill = guide_legend(ncol = 1))

library(rcompanion)
Test_Hosp <- wave_summary |> mutate(nonHosp = cases-hospitalizations) |> dplyr::select(hospitalizations, nonHosp)
print(Test_Hosp)

Test_Deaths <- wave_summary |> mutate(nonDeaths = cases-deaths) |> dplyr::select(deaths, nonDeaths)
print(Test_Deaths)

wave_summary <- wave_summary %>%
  mutate(
    hosp_rate = hospitalizations / cases * 1000,
    death_rate = deaths / cases * 1000
  )

# Reference wave (1st wave)
ref_hosp <- wave_summary$hospitalizations[1]
ref_cases <- wave_summary$cases[1]
ref_rate <- wave_summary$hosp_rate[1]

# Calculate RR and CIs for each wave compared to the reference
Hosp_Rate_Test <- wave_summary %>%
  mutate(
    RR_vs_ref = hosp_rate / ref_rate,
    log_RR = log(RR_vs_ref),
    SE_log_RR = sqrt( (1/hospitalizations) + (1/ref_hosp) - (1/cases) - (1/ref_cases) ),
    RR_lower_CI = exp(log_RR - 1.96 * SE_log_RR),
    RR_upper_CI = exp(log_RR + 1.96 * SE_log_RR)
  ) %>%
  dplyr::select(wave, hosp_rate, RR_vs_ref, RR_lower_CI, RR_upper_CI)

print(Hosp_Rate_Test)

library(dplyr)

# Create sequential comparisons
Hosp_Rate_Sequential_Test <- wave_summary %>%
  mutate(
    next_wave = lead(wave),
    next_hosp = lead(hospitalizations),
    next_cases = lead(cases),
    next_rate = lead(hosp_rate)
  ) %>%
  filter(!is.na(next_wave)) %>%  # Remove last row (no next wave)
  mutate(
    RR = next_rate / hosp_rate,
    log_RR = log(RR),
    SE_log_RR = sqrt( (1/next_hosp) + (1/hospitalizations) - (1/next_cases) - (1/cases) ),
    RR_lower_CI = exp(log_RR - 1.96 * SE_log_RR),
    RR_upper_CI = exp(log_RR + 1.96 * SE_log_RR)
  ) %>%
  select(
    Wave_Comparison = wave,
    Next_Wave = next_wave,
    Rate1_per_1000 = hosp_rate,
    Rate2_per_1000 = next_rate,
    Rate_Ratio = RR,
    Lower_CI = RR_lower_CI,
    Upper_CI = RR_upper_CI
  )

print(Hosp_Rate_Sequential_Test)

# Create sequential comparisons
Death_Rate_Sequential_Test <- wave_summary %>%
  mutate(
    next_wave = lead(wave),
    next_death = lead(deaths),
    next_cases = lead(cases),
    next_rate = lead(death_rate)
  ) %>%
  filter(!is.na(next_wave)) %>%  # Remove last row (no next wave)
  mutate(
    RR = next_rate / death_rate,
    log_RR = log(RR),
    SE_log_RR = sqrt( (1/next_death) + (1/deaths) - (1/next_cases) - (1/cases) ),
    RR_lower_CI = exp(log_RR - 1.96 * SE_log_RR),
    RR_upper_CI = exp(log_RR + 1.96 * SE_log_RR)
  ) %>%
  select(
    Wave_Comparison = wave,
    Next_Wave = next_wave,
    Rate1_per_1000 = hosp_rate,
    Rate2_per_1000 = next_rate,
    Rate_Ratio = RR,
    Lower_CI = RR_lower_CI,
    Upper_CI = RR_upper_CI
  )

print(Death_Rate_Sequential_Test)



# Isha Code here: 

