#1) Loading packages, reading in data set, and reformatting.----------------------------------------------------------------------------------------------------------

rm(list = ls()) #removes all variables stored previously in Environment (good habit to include)

library(tidyverse) #Run tidyverse, if you do not have: install.packages("tidyverse")
library(lubridate) #to modify date-time entries, if you do not have it: install.packages("lubridate")
library(scales) #allows to modify scientific notation for values

#read the data set into RStudio and stored into object
covid19_data <- read.csv("C:/Users/Kevin/Desktop/COVID19/us_states_covid19_daily.csv")

data <- as_tibble(covid19_data) #changes data set into a tibble (used in conjunction with tidyverse functions)

data <- data %>%
  mutate(date = ymd(date)) #fixed formatting with dates to YYYY-MM-DD

#2) Data is now in the correct format. We can now filter as necessary to target data sets of interest for exploration. ----------------------------------------------------------------
# Below is the data on Florida, we will look at positive cases and trends, proportion of cases that are either positive or negative, visualize the difference in positive and negative cases in Florida, and death trends in Florida.

# Desired data for only Florida
FL_data <- data %>%
  filter(state == "FL") %>%
  select(c(date:hospitalizedCumulative, recovered, death))

# Look at positive cases and trends
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

# By month, what is the proportion of positive cases vs total cases & negative cases vs total cases in the state of Florida
FL_data_by_month <- FL_data %>%
  group_by(month = month(date)) %>%
  filter(date == max(date)) %>%
  summarise(total_positive_cases = positive,
            total_negative_cases = negative,
            proportion_of_positive = total_positive_cases/(total_positive_cases + total_negative_cases),
            proportion_of_negative = total_negative_cases/(total_positive_cases + total_negative_cases)) %>%
  mutate(month_name = month.name[month]) %>%
  select(month_name, everything())

# Create line chart to show positive vs negative cases per month in Florida
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

# Look at deaths and trends in FL. We will use this data later on to compare with different populations.
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

#3) --Data below will show total cases (positive and negative) and total deaths for all states----------------------------------------------------------------------
#--We will then narrow our focus to Florida, California, and Texas to show total cases (positive and negative) and total deaths for each respective state

# Look at total number of cases (positive and negative) of all states. Will give us an idea of populations to compare.
total_cases_all <- data %>%
  filter(date == "2020-08-07") %>%
  select(date : negative) %>%
  mutate(total_testings = positive + negative) %>% 
  arrange(desc(positive))

# Look at total number of deaths per state (from inception to 2020-08-07) 
death_cases <- data %>%
  filter(date == "2020-08-07") %>%
  arrange(desc(death)) %>%
  select(date : state, death)

# We will focus on our states of interest and look at the total number of cases (positive and negative) within Florida, California, and Texas.
states_of_interest <- c("FL", "CA", "TX")

total_cases <- data %>%
  filter(date == "2020-08-07", state %in% states_of_interest) %>%
  select(date : negative) %>%
  mutate(total_cases = positive + negative) %>%
  arrange(total_cases)
# As shown in the above data set: total_cases_all, California has more positive cases than Florida, and Florida has more positive cases than Texas. 
# California, Florida, and Texas are the top 3 states with the most positive cases in the U.S. We will compare Florida to these states to analyze average % increase of deaths and average deaths per day.

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
# As shown in the above graph, CA (10,011 deaths) has the most deaths, followed by TX (8,096 deaths), and then FL (8,051 deaths). - As of 2020-08-07

#4) ----------------------------------------------------------------------------------------------------------------------------------------------------------------

# We will first start by comparing the average difference in deaths between Florida and California.
# Conducting an Independent 2-sample t-test and generating a confidence interval to see if Florida and California's average deaths differ as the states with the highest amounts of cases as of 2020-08-07
# This t-test will tell us how significant the average difference of deaths are between Florida and California
# Null Hypothesis (Ho): The average difference of death for Florida and California are the same (do not differ and equal to zero)
# Alternative Hypothesis (Ha): The average difference of death for Florida and California are different (not equal to zero)

#To test the hypothesis, we will utilize the two sample t-test. However, we will need the data set to show data for only Florida and California
FL_CA_data <- data %>%
  filter(state == "FL" | state == "CA") %>%
  arrange(state, date) %>%
  select(date : state, death)

#Visualize the variance between the two states with a box plot. As we can see, CA has more variability than FL over the same time frame
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

#Now we can run the t-test keeping in mind, our null hypothesis is equal to zero, alternative is two.sided, 95% confidence interval, variance is not equal and they are independent populations.
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

# We will utilize the above averages in a ANOVA test to compare statistical significance at the end of the analysis.

#5) --------------------------------------------------------------------------------------------------------------------------------

# We will conduct the same analysis as above, but comparing the average difference in deaths between Florida and Texas.
# Keep in mind, that Florida has more positive cases than Texas as shown above in the data set total_cases.
# Null Hypothesis (Ho): The average difference of death for Florida and Texas are the same (do not differ and equal to zero)
# Alternative Hypothesis (Ha): The The average difference of death for Florida and Texas are different (not equal to zero)
# To test the hypothesis, we will use the two sample t-test again. However, we will need the data set to show data for only FL and TX

FL_TX_data <- data %>%
  filter(state == "FL" | state == "TX") %>%
  arrange(state, date) %>%
  select(date : state, death)

#Visualize the variance between the two states with a box plot. As we can see, Florida has more variability then Texas over the same time frame
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

# Analyzing the box plot with the variance in number of deaths for Texas, it shows that there was a huge spike of deaths by the outliers shown between 6,000 to 8,000 deaths. Based on that, could we hypothesize that the rate of death is greater in Texas than in Florida?

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

# We will utilize the above averages in a ANOVA test to compare statistical significance in the next section!

#6) --------------------------------------------------------------------------------------------------------------------------------------------------
# Comparing the data for California, Florida, and Texas in regards to average percent increase in deaths per day and average deaths per day.
# We will visualize via table and visualize the percent increase of death via scatter plot. Lastly, we will compare all 3 states in regards to the above two factors to test statistical significance!

tribble(
  ~state, ~positive_cases, ~avg_percent_inc_death, ~avg_deaths_per_day,
  "CA", total_cases_all$positive[1], average_death_rate_CA, average_deaths_per_day_CA,
  "FL", total_cases_all$positive[2], average_death_rate_FL, average_deaths_per_day_FL,
  "TX", total_cases_all$positive[3], average_death_rate_TX, average_deaths_per_day_TX
)
# It is interesting to see that despite having the most cases, California has the lowest average percent increase in deaths out of the three states.
# It is also interesting to see that Texas, with the lowest amount of positive cases, has the highest average percent increase in deaths.

# To visualize, we will fit a non-linear regression for the relationship between Y = death rate, X1 = date, and X2 = state (TX, FL and CA)
# This will show the rate of death over 5 months, between TX, FL, and CA.
death_FL_TX_CA <- rbind(death_FL, death_TX, death_CA) # Combining the three data frames together to visualize death rates 

ggplot(data = death_FL_TX_CA, mapping = aes(x = date, y = diff_death, color = state)) +
  geom_point() +
  geom_smooth(se = FALSE) + #added line of best fit to show death rates per state
  labs(x = "Date", 
       y = "Deaths per Day",
       title = "Rapid Growth of Texas Death Rate",
       subtitle = "March 2020 - August 2020",
       caption = "Source: https://covidtracking.com/") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
# Based on percent increase of deaths per day, we can conclude visually and mathematically that Texas has the highest at 7.14% increase per day, Florida with 6.13% increase per day, and California with the lowest at 5.81% increase per day
# Is this statistically significant?

# We can run an ANOVA (Analysis of Variance) test to see if the average % increase of deaths per day and average deaths per day are equal within the 3 states.
# If they are not equal we can run further tests to see which one(s) are different.
# Our Null Hypothesis (Ho) is: that the average percent increase of deaths per day between the 3 states are not significantly different.
# Our Alternative Hypothesis (Ha) is: that at least one of the states will have a difference in average percent increase of deaths per day. (the ANOVA test will not tell us which one differs)
ANOVA1 <- aov(death_FL_TX_CA$rate_percent ~ death_FL_TX_CA$state)
summary(ANOVA1) # To run a more informative summary
# Our p-value is 57.9%. Therefore, we fail to reject our null hypothesis: that the average percent increase of deaths per day between the 3 states are not significantly different.
plot(TukeyHSD(ANOVA1)) # To visualize, the means of death rate per day do not differ from each other.

# Our Null Hypothesis (Ho) is: that the average deaths per day between the 3 states are not significantly different.
# Our Alternative Hypothesis (Ha) is: that at least one of the states will have a difference in average deaths per day. (the ANOVA test will not tell us which one differs)
ANOVA2 <- aov(death_FL_TX_CA$diff_death ~ death_FL_TX_CA$state)
summary(ANOVA2) # To run a more informative summary
# Our p-value is 14.6%. Therefore, we fail to reject our null hypothesis: that the average deaths per day between the 3 states are not significantly different.
plot(TukeyHSD(ANOVA2)) # To visualize, the means of deaths per day do not differ from each other.

#-------------------------------------------------------------------------------------------------------------------------------
# We will look at recovery data for FL, CA, and TX.

# Start with filtering the data set to show the recovery from positive cases in FL, CA, and TX.
recovery_data <- data %>%
  filter(state == "FL" | state == "CA" | state == "TX") %>%
  arrange(state, date) %>%
  select(date : state, recovered)

#Unfortunately, recovery data is not available within this data set for Florida and California.
#Therefore, recovery is outside the scope of this data set and further research is needed.





