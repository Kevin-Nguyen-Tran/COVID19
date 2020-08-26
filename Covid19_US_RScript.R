rm(list = ls()) #removes all variables stored previously in Environment (good habit to include)

library(tidyverse) #Run tidyverse, if you do not have: install.packages("tidyverse")
library(lubridate) #to modify date-time entries, if you do not have: install.packages("lubridate")
library(scales) #allows to modify scientific notation for values

#read the data set into RStudio and stored into object
covid19_data <- read.csv("C:/Users/Kevin/Desktop/COVID19/us_states_covid19_daily.csv")

data <- as_tibble(covid19_data) #changes data set into a tibble (used in conjunction with tidyverse functions)

data <- data %>%
  mutate(date = ymd(date)) #fixed formatting with dates to YYYY-MM-DD

#Data is now in the correct format. Now we can filter as necessary to target data sets of interest for exploration. -----------------------------------------------
# Below is the data on Florida, we will look at positive cases and trends, proportion of cases that are either positive or negative, visualize the difference
# in positive and negative cases in Florida, and death trends in Florida.

#desired data for only Florida
FL_data <- data %>%
  filter(state == "FL") %>%
  select(c(date:hospitalizedCumulative, recovered, death))

#look at positive cases and trends
positive_cases <- ggplot(data = FL_data, mapping = aes(x = date, y = positive)) +
  geom_point(position = "jitter", alpha = 1/5) +
  scale_y_continuous(labels = comma) + #overrides scientific notation for y values
  geom_smooth() + #adds a line of best fit to express the relationship between the points
  labs(x = "Date", 
       y = "Number of Positive Cases",
       title = "Number of Positive Cases by Date in Florida",
       subtitle = "March 2020 - August 2020",
       caption = "Source: https://covidtracking.com/") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) #used to center the title and subtitle

#by month, what is the proportion of positive cases vs total cases & negative cases vs total cases in the state of Florida
FL_data_by_month <- FL_data %>%
  group_by(month = month(date)) %>%
  filter(date == max(date)) %>%
  summarise(total_positive_cases = positive,
            total_negative_cases = negative,
            proportion_of_positive = total_positive_cases/(total_positive_cases + total_negative_cases),
            proportion_of_negative = total_negative_cases/(total_positive_cases + total_negative_cases)) %>%
  mutate(month_name = month.name[month]) %>%
  select(month_name, everything())

#create line chart to show positive vs negative cases per month in Florida
colors <- c("positive_cases" = "red", "negative_cases" = "green") #added to make custom/manual legend for line chart

linechart_proportion <- ggplot(data = FL_data_by_month) +
  geom_line(mapping = aes(x = month, y = total_positive_cases, color = "positive_cases"), size = 1) +
  geom_line(mapping = aes(x = month, y = total_negative_cases, color = "negative_cases"), size = 1) +
  scale_y_continuous(labels = comma) +
  labs(x = "Month", 
       y = "Number of Cases",
       title = "Positive vs Negative Cases by Month in Florida",
       subtitle = "March 2020 - August 2020",
       caption = "Source: https://covidtracking.com/",
       color = "Legend") +
  scale_color_manual(values = colors) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "right")

# look at deaths and trends in FL. We will use this data later on to compare with a different population.
deaths <- ggplot(data = FL_data, mapping = aes(x = date, y = death)) +
  geom_point(position = "jitter", alpha = 1/5) +
  scale_y_continuous(labels = comma) + #overrides scientific notation for y values
  geom_smooth() + #adds a line of best fit to express the relationship between the points
  labs(x = "Date", 
       y = "Number of Deaths",
       title = "Number of Deaths by Date in Florida",
       subtitle = "March 2020 - August 2020",
       caption = "Source: https://covidtracking.com/") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#------------------------------------------------------------------------------------------------------------------------

# We will also look at total number of cases (positive and negative) of all states
total_cases_all <- data %>%
  filter(date == "2020-08-07") %>%
  select(date : negative) %>%
  mutate(total_testings = positive + negative) %>% 
  arrange(desc(positive))

# Look at total number of deaths per state (from inception to 2020-08-07) Will give us an idea of populations to compare.
death_cases <- data %>%
  filter(date == "2020-08-07") %>%
  arrange(desc(death)) %>%
  select(date : state, death)

# We will also look at total number of cases (positive and negative) within Florida, California, and Texas.
states_of_interest <- c("FL", "CA", "TX")

total_cases <- data %>%
  filter(date == "2020-08-07", state %in% states_of_interest) %>%
  select(date : negative) %>%
  mutate(total_cases = positive + negative) %>%
  arrange(total_cases)
# As shown in the above data set: total_cases_all, California has more positive cases than Florida, and Florida has more positive cases than Texas. 

# CA, FL, and TX are the top 3 states with the most positive cases in the U.S. We will compare Florida to these states to analyze two factors: death and recovery.
FL_TX_CA_data <- data %>%
  filter(state == "FL" | state == "TX" | state == "CA") %>%
  arrange(state, date) %>%
  select(date : state, death)

ggplot(data = FL_TX_CA_data, mapping = aes(x = date, y = death, color = state)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Date", 
       y = "Deaths",
       title = "Deaths Over Time in California, Florida, and Texas",
       subtitle = "March 2020 - August 2020",
       caption = "Source: https://covidtracking.com/") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Add recovery plot similar to above graph [HERE] ***

#----------------------------------------------------------------------------------------------------------------------------------------------------------------

# We will first start by comparing the average difference in deaths between Florida and California.
# Conducting a Independent 2-sample t-test and generating CI to see if Florida and California's average deaths differ 
# as the states with the highest amounts of cases as of August 2020.
# This t-test will tell us how significant the average difference of deaths are between Florida and California
# Null Hypothesis (Ho): The average difference of death for Florida and California are the same (do not differ and equal to zero)
# Alternative Hypothesis (Ha): The average difference of death for Florida and California are different (not equal to zero)

#To test the hypothesis, we will use the t.test(). However, we will need the data set to show data for only FL and CA
FL_CA_data <- data %>%
  filter(state == "FL" | state == "CA") %>%
  arrange(state, date) %>%
  select(date : state, death)

#Visualize the variance between the two states with a box plot! As we can see, CA has more variability than FL over the same time frame
ggplot(data = FL_CA_data) +
  geom_boxplot(aes(x = state, y = death)) +
  coord_flip() +
  labs(x = "Deaths", 
       y = "State",
       title = "Deaths in Florida vs California",
       subtitle = "March 2020 - August 2020",
       caption = "Source: https://covidtracking.com/") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#Now we can run the t.test keeping in mind, our null hypothesis is equal to zero, alternative is a two.sided, 95% confidence interval and variance is not equal and they are independent populations
t.test(FL_CA_data$death ~ FL_CA_data$state, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
#p-value is close to zero, therefore we can reject our null hypothesis that there is no difference in average deaths
#with 95% confidence, we can say that the difference within means fall between 767.51 to 1929.88
#since zero is not in the confidence interval as well, we can reject the null hypothesis with 95% confidence.
# Now that we confirmed our mean difference is statistically significant, we can compare the death rates between Florida and California.

#Calculating the average growth rate per day for each respective state 
death_CA <- FL_CA_data %>%
  filter(state == "CA") %>%
  mutate(diff_day = date - lag(date),
         diff_death = death - lag(death),
         rate_percent = diff_death/lag(death) * 100)

#now we can calculate the average rate of growth for deaths in California:
average_death_rate_CA <- mean(death_CA$rate_percent, na.rm = TRUE)
#Average rate of death is 5.81% per day in CA.
average_deaths_per_day_CA <- mean(death_CA$diff_death, na.rm = TRUE)
# Average deaths per day in California is 67.61

#Now we can repeat for FL:
FL_TX_data <- data %>%
  filter(state == "FL" | state == "TX") %>%
  arrange(state, date) %>%
  select(date : state, death)

death_FL <- FL_TX_data %>%
  filter(state == "FL") %>%
  mutate(diff_day = date - lag(date),
         diff_death = death - lag(death),
         rate_percent = diff_death/lag(death) * 100)

#now we can calculate the average rate of growth for deaths in Florida:
average_death_rate_FL <- mean(death_FL$rate_percent, na.rm = TRUE)
#Average rate of death is 6.13% per day in FL.
average_deaths_per_day_FL <- mean(death_FL$diff_death, na.rm = TRUE)
# Average deaths per day in Florida is 54.02

#--------------------------------------------------------------------------------------------------------------------------------

# We will conduct the same analysis as above, but comparing the average difference in deaths between Florida and Texas.
# Keep in mind, that Florida has more positive cases than Texas as shown above in the data set total_cases.
# Null Hypothesis (Ho): The average difference of death for Florida and Texas are the same (do not differ and equal to zero)
# Alternative Hypothesis (Ha): The The average difference of death for Florida and Texas are different (not equal to zero)
# To test the hypothesis, we will use the t.test(). However, we will need the data set to show data for only FL and TX

FL_TX_data <- data %>%
  filter(state == "FL" | state == "TX") %>%
  arrange(state, date) %>%
  select(date : state, death)

#Visualize the variance between the two states with a box plot! As we can see, FL has more variability then TX over the same time frame
ggplot(data = FL_TX_data) +
  geom_boxplot(aes(x = state, y = death)) +
  coord_flip() +
  labs(x = "Deaths", 
       y = "State",
       title = "Deaths in Florida vs Texas",
       subtitle = "March 2020 - August 2020",
       caption = "Source: https://covidtracking.com/") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

#Now we can run the t.test keeping in mind, our null hypothesis is equal to zero, alternative is two.sided, 95% conf int and variance is not equal and they are independent
t.test(FL_TX_data$death ~ FL_TX_data$state, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
# P-value is less than alpha = 5%, therefore we can reject our null hypothesis: there is no difference in death rates between the 2 states
# With 95% confidence, we can say that the difference within means fall between 98.05 to 1014.56
# Since zero is not in the confidence interval as well, we can reject the null hypothesis with 95% confidence.

# Analyzing the box plot with the variance in number of deaths for Texas, it shows that there was a huge spike of deaths
# by the outliers shown between 6,000 to 8,000 deaths. Based on that, could we hypothesize that the rate of death
# is greater in Texas than in Florida?

#Ho: The rate of death in Texas is the same as that of Florida
#Ha: The rate of death in Texas in greater than that of Florida

# Calculating the average growth rate per day for each respective state 
death_TX <- FL_TX_data %>%
  filter(state == "TX") %>%
  mutate(diff_day = date - lag(date),
         diff_death = death - lag(death),
         rate_percent = diff_death/lag(death) * 100)
  
# Now we can calculate the average rate of growth in Texas:
average_death_rate_TX <- mean(death_TX$rate_percent, na.rm = TRUE)
# Average rate of death is 7.14% per day in TX.
average_deaths_per_day_TX <- mean(death_TX$diff_death, na.rm = TRUE)
# Average deaths per day in Texas is 56.61

# Note, from the previous hypothesis test between FL and CA. Florida's average death rate and average deaths per day are as shown below:
average_death_rate_FL <- mean(death_FL$rate_percent, na.rm = TRUE)
#Average rate of death is 6.13% per day in FL.
average_deaths_per_day_FL <- mean(death_FL$diff_death, na.rm = TRUE)
# Average deaths per day in Florida is 54.02

tribble(
  ~state, ~avg_percent_inc_death, ~avg_deaths_per_day,
  "CA", average_death_rate_CA, average_deaths_per_day_CA,
  "FL", average_death_rate_FL, average_deaths_per_day_FL,
  "TX", average_death_rate_TX, average_deaths_per_day_TX
)
# It is interesting to see that despite have the most cases, California has the lowest average percent increase in deaths out of the three states.
# It is also interesting to see that Texas, with the lowest amount of positive cases, has the highest average percent increase in deaths.


# Average rate of death over a 5 month period between March 2020 and August 2020, TX showed the greatest rate of death
# To visualize, we will fit a non-linear regression for the relationship between Y = death rate, X1 = date, and X2 = state (TX, FL and CA)
# This is will show the rate of death over 5 months, between TX, FL, and CA.
death_FL_TX_CA <- rbind(death_FL, death_TX, death_CA) # Combining the three data frames together to visualize death rates 

ggplot(data = death_FL_TX_CA, mapping = aes(x = date, y = diff_death, color = state)) +
  geom_point() +
  geom_smooth(se = FALSE) + #added line of best fit to show death rates per state
  labs(x = "Date", 
       y = "% Increase of Deaths per Day",
       title = "Rapid Growth of Texas Death Rate",
       subtitle = "March 2020 - August 2020",
       caption = "Source: https://covidtracking.com/") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# Based on percent increase of deaths per day, we can conclude visually and mathematically that Texas has the highest at 7.14% increase per day, Florida with 6.13% increase per day, and California with the lowest at 5.81% increase per day
# We will now look at the recovery among the three states!

#-------------------------------------------------------------------------------------------------------------------------------












