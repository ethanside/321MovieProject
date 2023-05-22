library(tidyverse)
data <- read.csv("netflix_titles.csv")
gdp_data <- read.csv("CountryIncomeBrakcets.csv")
movies <- data %>% filter(type == "Movie")
high_income_movies <- subset(gdp_data, IncomeGroup == "High income")
low_income_movies <- subset(gdp_data, IncomeGroup == "Low income")
upper_middle_income_movies <- subset(gdp_data, IncomeGroup == "Upper middle income")
lower_middle_income_movies <- subset(gdp_data, IncomeGroup == "Lower middle income")

high_income_genre_counts <- table()

# In 2020 after lockdown approx March 15 2020 were there more 
#comedy movies added to netflix than other genres?
Covid_data <- movies %>% 
              filter(date_added #past march 15th and before 2021) %>%
              filter(listed_in == comedy)
              nrows
              
# are there differences in countries producing movies of certain genre
# e.g(does india produce more dramas)
              
              
gdp_data$country = gdp_data$TableName
merged_data <- data %>% left_join(gdp_data)         
merged_data <- merged_data %>% rowwise() %>% mutate(Drama = ifelse(("Dramas" %in% listed_in) | ("TV Dramas" %in% listed_in), 1, 0))
test <- merged_data %>% mutate(Drama1 = grepl("Dramas", listed_in))
table(test$Drama1)
