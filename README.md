# COVID-19 Analysis - Death Rate
#### Kevin Tran

## Prerequisites
```{r setup}
rm(list = ls()) #removes all variables stored previously in Environment (good habit to include)

library(tidyverse) #Run tidyverse, if you do not have: install.packages("tidyverse")
library(lubridate) #to modify date-time entries, if you do not have it: install.packages("lubridate")
library(scales) #allows to modify scientific notation for values

#read the data set into RStudio and stored into object
covid19_data <- read.csv("C:/Users/Kevin/Desktop/COVID19/us_states_covid19_daily.csv")

data <- as_tibble(covid19_data) #changes data set into a tibble (used in conjunction with tidyverse functions)

data <- data %>%
  mutate(date = ymd(date)) #fixed formatting with dates to YYYY-MM-DD
```

## Abstract

This analysis deals with exploring death trends in the United States, specifically having Florida as the focal point. The exploration in Florida begins with analyzing positive cases/trends, visualizing the positive and negative cases, and inspecting the total number of deaths. On the basis of population comparisons, Florida will be compared to California and Texas due to having the most similar total number of positive COVID-19 cases. The two main factors that will be investigated between the states are the average percent increase of deaths per day and the average deaths per day. The data represented in this analysis can be found on <https://covidtracking.com/> and will cover the time frame of 2020-03-04 to 2020-08-07.

**The questions asked during the analysis were:**

* How does Florida compare to other states in the U.S, in terms of, positive COVID-19 cases, total numbers of deaths, death rate, etc?

* Which states currently (2020-08-07) have the most positive cases?

* Since positive cases and deaths are positively correlated, which state has the highest death rate? Can we hypothesize that the state with the most total deaths has the highest death rate?

* Based on linear models, can we predict which states will have the most deaths in the long run?

**The data analysis will be broken down by the following sections:**

* Florida Data

* Data for All States

* Narrow Focus - Florida, California, and Texas

* Florida vs California

* Florida vs Texas

* Florida vs California vs Texas