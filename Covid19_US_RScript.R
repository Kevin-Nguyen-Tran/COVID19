rm(list = ls()) #removes all variables stored previously in Environment (good habit to include)

library(tidyverse) #Run tidyverse, if you do not have: install.packages("tidyverse")
library(lubridate) #to modify date-time entries
library(scales) #allows to modify scientific notation for values

#read the data set into RStudio and stored into object called covid19_data
covid19_data <- read.csv("C:/Users/Kevin/Desktop/COVID19/us_states_covid19_daily.csv")

data <- as_tibble(covid19_data) #changes data set into a tibble (used in conjunction with tidyverse functions)

data <- data %>%
  mutate(date = ymd(date)) #fixed formatting with dates to YYYY-MM-DD

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

# look at deaths and trends
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

# Conducting a Independent 2-sample t-test and CI to see if Florida and California's death rate differ 
# as the states with the highest amounts of cases as of August 2020.
# This t-test will tell us how significant the difference of death rates are between Florida and California
# Null Hypothesis (Ho): The death rate of Florida and California are the same (do not differ)
# Alternative Hypothesis (Ha): The death rate of Florida and California are different (not equal to zero)

#To test the hypothesis, we will use the t.test(). However, we will need the data set to show data for only FL and CA
FL_CA_data <- data %>%
  filter(state == "FL" | state == "CA") %>%
  arrange(state, date) %>%
  select(date : state, death)

#Visualize the variance between the two states with a box plot! As we can see, CA has more variability then FL over the same time frame
ggplot(data = FL_CA_data) +
  geom_boxplot(aes(x = state, y = death))

#Now we can run the t.test keeping in mind, our null hypothesis is equal to zero, alternative is two.sided, 95% conf int and variance is not equal and they are independent
t.test(FL_CA_data$death ~ FL_CA_data$state, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
#p-value is close to zero, therefore we can reject our null hypothesis that there is no difference in death rates
#with 95% confidence, we can say that the mean within the sample or its extremes fall between 767.51 to 1929.88
#since zero is not in the confidence interval as well, we can reject the null hypothesis with 95% confidence.


