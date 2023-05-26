library(tidyverse)
library(plotly)
data <- read.csv("netflix_titles.csv")
gdp_data <- read.csv("CountryIncomeBrakcets.csv")




# Merging Income Data and netflix movie data. Creating data for the main 3 entertainment genres      
gdp_data$country = gdp_data$TableName
merged_data <- data %>% left_join(gdp_data)  
merged_data <- merged_data %>% rowwise() %>% mutate(Drama = ifelse(("Dramas" %in% listed_in) | ("TV Dramas" %in% listed_in), 1, 0))
merged_data <- merged_data %>% rowwise() %>% mutate(Comedy = ifelse(("Comedies" %in% listed_in) | ("TV Comedies" %in% listed_in), 1, 0))
merged_data <- merged_data %>% rowwise() %>% mutate(Action = ifelse(("Action & Adventure" %in% listed_in) | ("TV Action & Adventure" %in% listed_in), 1, 0))
merged_data <- merged_data %>% filter(!is.na(IncomeGroup) & IncomeGroup != "")
drama_data_set <- merged_data %>% mutate(Drama1 = grepl("Dramas", listed_in))
comedy_data_set <- merged_data %>% mutate(Comedy1 = grepl("Comedies", listed_in))
action_data_set <- merged_data %>% mutate(Action1 = grepl("Action & Adventure", listed_in))




# Do countries that are not high income have a different movie genre proportion than lower income countries
# Research Design: We wanted to see if the 
drama_plot <- ggplot(drama_data_set, mapping = aes(x = IncomeGroup, fill = Drama1)) + 
  geom_bar() +
  theme_classic() + 
  labs(title = "Proportion of Drama Movies and TV shows by Income Group", x = "Income Group", y = "Total Number of Movies Produced", fill = "Genre = Drama")
ggplotly(drama_plot)

comedy_plot <- ggplot(comedy_data_set, mapping = aes(x = IncomeGroup, fill = Comedy1)) + 
  geom_bar() + 
  theme_classic() + 
  labs(title = "Proportion of Comedy Movies and TV shows by Income Group",x = "Income Group", y = "Total Number of Movies Produced", fill = "Genre = Comedy")
ggplotly(comedy_plot)

action_plot <- ggplot(action_data_set, mapping = aes(x = IncomeGroup, fill = Action1)) + 
  geom_bar() + 
  theme_classic() + 
  labs(title = "Proportion of Action Movies and TV shows by Income Group", x = "Income Group", y = "Total Number of Movies Produced", fill = "Genre = Action")
ggplotly(action_plot)

# Are Countries that have lower income levels more likely to have a higher proportion of Dramas?     
# Research Design: We decided to look at four of the higher producing entertainment countries in 
# the world, two being high income and two being lower middle income. The two income groups that produced 
# the most movies. We then wanted to test their proportion of Drama movies and Tv shows produced to see if lower middle
# income countries were more inclined to produce Dramas as it may relate to their population more. We then did the same
# with comedies to see if higher income countries had a similar trend in the other direction.

# Data filtering for the four largest producers of film
US_data_set <- drama_data_set %>% filter(TableName == "United States") 
Nigeria_data_set <- drama_data_set %>% filter(TableName == "Nigeria") 
India_data_set <- drama_data_set %>% filter(TableName == "India")
UK_data_set <- drama_data_set %>% filter(TableName == "United Kingdom") 
US_drama_proportion <- table(US_data_set$Drama1)
Nigeria_drama_proportion <- table(Nigeria_data_set$Drama1)
India_drama_proportion <- table(India_data_set$Drama1)
UK_drama_proportion <- table(UK_data_set$Drama1)

#US Drama proportions
(US_drama_proportion[2] / (US_drama_proportion[1] + US_drama_proportion[2]))

#Nigeria Drama proportions
(Nigeria_drama_proportion[2] / (Nigeria_drama_proportion[1] + Nigeria_drama_proportion[2]))

# India Drama Proportions
(India_drama_proportion[2] / (India_drama_proportion[1] + India_drama_proportion[2]))

# UK Drama Proportions
(UK_drama_proportion[2] / (UK_drama_proportion[1] + UK_drama_proportion[2]))



# Data Analysis for comedy proportions
US_data_set1 <- comedy_data_set %>% filter(TableName == "United States") 
Nigeria_data_set1 <- comedy_data_set %>% filter(TableName == "Nigeria") 
India_data_set1 <- comedy_data_set %>% filter(TableName == "India")
UK_data_set1 <- comedy_data_set %>% filter(TableName == "United Kingdom") 
US_comedy_proportion <- table(US_data_set1$Comedy1)
Nigeria_comedy_proportion <- table(Nigeria_data_set1$Comedy1)
India_comedy_proportion <- table(India_data_set1$Comedy1)
UK_comedy_proportion <- table(UK_data_set1$Comedy1)

#US Comedy proportions
(US_comedy_proportion[2] / (US_comedy_proportion[1] + US_comedy_proportion[2]))

#Nigeria Comedy proportions
(Nigeria_comedy_proportion[2] / (Nigeria_comedy_proportion[1] + Nigeria_comedy_proportion[2]))

# India Comedy Proportions
(India_comedy_proportion[2] / (India_comedy_proportion[1] + India_comedy_proportion[2]))

# UK Comedy Proportions
(UK_comedy_proportion[2] / (UK_comedy_proportion[1] + UK_comedy_proportion[2]))
