library(tidyverse)
library(ggplot2)
library(dplyr)

#The Research design for this portion of our analysis is breaking down how ratings change in
#one country as GDP fluctuates in order to to analyze without the different impacts studying countries
#different countries can have. 

#In order to do this we created a merged data file that was merged with
#both country and year/ year of release as the common factor.

#Then we created a new column labeled GDPcategory which was labeld 1 2 3 or 4 based on which
#quartie the country's GDP was in that year

#Then we made 4 different graphs showing the percent break down of rating dependent on for the years
#in which the US was either in quartile 1 2 3 or 4#

#We will be looking at the differences in theses graphs to view change#

setwd("C:/Users/emmar/OneDrive/Desktop/Data and Society")
data <- read_csv("netflix_titles.csv") %>% 
  mutate(year = release_year) %>% 
  filter(country %in% c("United States","India","United Kingdom"))
# gdp_data <- read_csv("CountryIncomeBrackets.csv")
# movies <- data %>% filter(type == "Movie") 
gdpchange <- read_csv("gdpchange.csv") %>%
  filter(country %in% c("United States","India","United Kingdom"))  %>% 
  pivot_longer(GDP_1960:GDP_2021, names_to = "year", values_to = "gdp") %>% 
  mutate(year = as.numeric(str_sub(year, 5L, -1L))) %>% 
  select(-`2022`)

  
mergeddata <- data %>% 
  left_join(gdpchange) 
# head(mergeddata)  
# 
# names(mergeddata)

usa <- mergeddata %>% filter(country == "United States")
summary(usa$gdp)

usa <- usa %>% 
  mutate(GDPcategory = case_when(gdp <1.667 ~ 1,
                                  between(gdp, 1.667, 2.288)~ 2,
                                 between(gdp, 2.288, 2.945)~ 3,
                                 TRUE ~ 4 ))

lowestquartileUSA <- usa %>% 
  filter(GDPcategory == 1)

rating_counts <- lowestquartileUSA %>% 
  count(rating) %>% 
  mutate(percentage = n / sum(n)*100)
print(rating_counts)

 
  rating_counts %>% 
    rename(Percentage = percentage) %>% 
    mutate(pct_label = paste0(round(Percentage, 1), "%")) %>% 
  mutate(rating = factor(rating,
                       levels = c("G","TV-G","TV-Y", 
                                  "TV-Y7","PG","TV-PG", 
                                     "PG-13","TV-14", "R","TV-MA", "NR","UR"))) %>% 
  ggplot(aes(x = Percentage, y= rating, 
             fill = rating, label = pct_label)) +
  geom_col() +
    geom_text(hjust = -0.75) +
    scale_x_continuous(limits = c(0, 40))
 
  # now for second quartile#
  
  
  secondquartileUSA <- usa %>% 
    filter(GDPcategory == 2)
  
  rating_counts2 <- secondquartileUSA %>% 
    count(rating) %>% 
    mutate(percentage = n / sum(n)*100)
  print(rating_counts2)
  
  
  rating_counts2 %>% 
    rename(Percentage = percentage) %>% 
    mutate(pct_label = paste0(round(Percentage, 1), "%")) %>% 
    mutate(rating = factor(rating,
                           levels = c("G","TV-G","TV-Y", 
                                      "TV-Y7","PG","TV-PG", 
                                      "PG-13","TV-14", "R","TV-MA", "NR","UR"))) %>% 
    ggplot(aes(x = Percentage, y= rating, 
               fill = rating, label = pct_label)) +
    geom_col() +
    geom_text(hjust = -0.75) +
    scale_x_continuous(limits = c(0, 40))
  
  #third quartile#
  
  thirdquartileUSA <- usa %>% 
    filter(GDPcategory == 3)
  
  rating_counts3 <- thirdquartileUSA %>% 
    count(rating) %>% 
    mutate(percentage = n / sum(n)*100)
  print(rating_counts3)
  
  
  rating_counts3 %>% 
    rename(Percentage = percentage) %>% 
    mutate(pct_label = paste0(round(Percentage, 1), "%")) %>% 
    mutate(rating = factor(rating,
                           levels = c("G","TV-G","TV-Y", 
                                      "TV-Y7","PG","TV-PG", 
                                      "PG-13","TV-14", "R","TV-MA", "NR","UR"))) %>% 
    ggplot(aes(x = Percentage, y= rating, 
               fill = rating, label = pct_label)) +
    geom_col() +
    geom_text(hjust = -0.75) +
    scale_x_continuous(limits = c(0, 40))
  
  
#4th quartile#
  
  fourthquartileUSA <- usa %>% 
    filter(GDPcategory == 4)
  
  rating_counts4 <- fourthquartileUSA %>% 
    count(rating) %>% 
    mutate(percentage = n / sum(n)*100)
  print(rating_counts4)
  
  
  rating_counts4 %>% 
    rename(Percentage = percentage) %>% 
    mutate(pct_label = paste0(round(Percentage, 1), "%")) %>% 
    mutate(rating = factor(rating,
                           levels = c("G","TV-G","TV-Y", 
                                      "TV-Y7","PG","TV-PG", 
                                      "PG-13","TV-14", "R","TV-MA", "NR","UR"))) %>% 
    ggplot(aes(x = Percentage, y= rating, 
               fill = rating, label = pct_label)) +
    geom_col() +
    geom_text(hjust = -0.75) +
    scale_x_continuous(limits = c(0, 40))
  

# legend_labels <- paste0(ratingcounts$rating, " (", ratingcounts$percentage, "%)")
# plot1 + guides(fill = guide_legend(label = legend_labels))


