---
title: "COVID-19 Analysis - Death Rate"
author: "Kevin Tran"
date: "8/27/2020"
output:
  md_document:
    variant: markdown_github
---

```{r setup, include=FALSE}
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

## Florida Data
Below is the data on Florida and we will look at the following:

* Positive Cases/Trends

* Total Positive Cases vs Total Negative Cases

* Death Trends

```{r, include = FALSE}
# Desired data for only Florida
FL_data <- data %>%
  filter(state == "FL") %>%
  select(c(date:hospitalizedCumulative, recovered, death))
```

**Positive Cases/Trends in Florida:**
```{r,message=FALSE}
ggplot(data = FL_data, mapping = aes(x = date, y = positive)) +
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
```

```{r, include=FALSE, message=FALSE}
FL_data_by_month <- FL_data %>%
  group_by(month = month(date)) %>%
  filter(date == max(date)) %>%
  summarise(total_positive_cases = positive,
            total_negative_cases = negative,
            proportion_of_positive = total_positive_cases/(total_positive_cases + total_negative_cases),
            proportion_of_negative = total_negative_cases/(total_positive_cases + total_negative_cases)) %>%
  mutate(month_name = month.name[month]) %>%
  select(month_name, everything())
```

**Line Chart to Show Total Positive vs Total Negative Cases in Florida:**
```{r}
colors <- c("positive_cases" = "red", "negative_cases" = "green") #added to make custom/manual legend for line chart

ggplot(data = FL_data_by_month) +
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
```

**Death Trends in Florida:**
```{r, message=FALSE, warning=FALSE}
ggplot(data = FL_data, mapping = aes(x = date, y = death)) +
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
```

## Data for All States

**The data below will show total cases (positive and negative) and total deaths for all states:**

*(The data will be restricted to the top 10 states for visibility purposes)*
```{r}
total_cases_all <- data %>%
  filter(date == "2020-08-07") %>%
  select(date : negative) %>%
  mutate(total_testings = positive + negative) %>% 
  arrange(desc(positive))

knitr::kable(total_cases_all[1:10, ], caption = "Total Number of Positive and Negative Cases for All States (Top 10)")

# Look at total number of deaths per state (from inception to 2020-08-07) 
death_cases <- data %>%
  filter(date == "2020-08-07") %>%
  arrange(desc(death)) %>%
  select(date : state, death)

knitr::kable(death_cases[1:10, ], caption = "Total Number of Deaths for All States (Top 10)")
```

## Narrow Focus

**We will narrow our focus to Florida, California, and Texas to show total cases (positive and negative) and total deaths for each respective state:**
```{r}
states_of_interest <- c("FL", "CA", "TX")

total_cases <- data %>%
  filter(date == "2020-08-07", state %in% states_of_interest) %>%
  select(date : negative) %>%
  mutate(total_cases = positive + negative) %>%
  arrange(desc(positive))

knitr::kable(total_cases, caption = "Total Positive Cases in California, Florida, and Texas")
```

As shown in the above data set: California has more positive cases than Florida and Florida has more positive cases than Texas. 

California, Florida, and Texas are the top 3 states with the most positive cases in the U.S. 

Because California and Texas have the most similar amount of total positive cases among all states, we will compare Florida to these states to analyze the average percent increase of deaths and average deaths per day to see if they differ statistically.

**Below we will see California (10,011 deaths) accounts for the most deaths, followed by Texas (8,096 deaths), and then by Florida (8,051 deaths). - As of 2020-08-07:**
```{r, message=FALSE, warning=FALSE}
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
```

## Florida vs California
We will first start by comparing the average difference in deaths between Florida and California. To do so, we will conduct an Independent 2-sample t-test and generate a confidence interval to see if Florida and California's average deaths differ as the states that account for the most positive cases in the U.S.

This t-test will tell us how significant the average difference of death is between Florida and California

* Null Hypothesis (Ho): The average difference of death for Florida and California are the same (do not differ and equal to zero)

* Alternative Hypothesis (Ha): The average difference of death for Florida and California are different (not equal to zero)

To test the hypothesis, we will need the data set to show data for only Florida and California

```{r, results='hide'}
FL_CA_data <- data %>%
  filter(state == "FL" | state == "CA") %>%
  arrange(state, date) %>%
  select(date : state, death)
```

**We will then visualize the variance between the two states with a box plot. As we can see, California has more variability than Florida over the same time frame as seen in the width of the interquartile range:**

```{r, warning=FALSE}
ggplot(data = FL_CA_data) +
  geom_boxplot(aes(x = state, y = death)) +
  coord_flip() +
  labs(x = "State", 
       y = "Deaths",
       title = "Deaths in Florida vs California",
       subtitle = "March 2020 - August 2020",
       caption = "Source: https://covidtracking.com/") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
```

Now we can run the t-test keeping in mind, our null hypothesis is equal to zero, the alternative is two.sided, the confidence interval is set to 95%, the variance is not equal (as shown in the box plot above) and they are independent populations.

```{r}
t.test(FL_CA_data$death ~ FL_CA_data$state, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
```
The output tells us that the p-value is close to zero, therefore we can reject our null hypothesis that there is no difference in average deaths. The difference within means falls between 767.51 to 1929.88 and since zero is not within the confidence interval as well, we can reject the null hypothesis with 95% confidence.

Now that we confirmed our mean difference is statistically significant, we can compare the death rates between Florida and California.

**We will tidy our data to only show death statistics in California:** 

```{r,results='hide'}
death_CA <- FL_CA_data %>%
  filter(state == "CA") %>%
  mutate(diff_day = date - lag(date),
         diff_death = death - lag(death),
         rate_percent = diff_death/lag(death) * 100)

```

*Now we can calculate the average rate of growth for deaths and average deaths per day in California:*
```{r}
average_death_rate_CA <- mean(death_CA$rate_percent, na.rm = TRUE)
```
The average increase of death is 5.81% per day in California.
```{r}
average_deaths_per_day_CA <- mean(death_CA$diff_death, na.rm = TRUE)
```
The average deaths per day in California is 67.61

**Now we will repeat the same process for Florida:**
```{r, include=FALSE}
FL_TX_data <- data %>%
  filter(state == "FL" | state == "TX") %>%
  arrange(state, date) %>%
  select(date : state, death)
```
```{r, results='hide'}
death_FL <- FL_TX_data %>%
  filter(state == "FL") %>%
  mutate(diff_day = date - lag(date),
         diff_death = death - lag(death),
         rate_percent = diff_death/lag(death) * 100)
```

*Calculate the average rate of growth for deaths and average deaths per day in Florida:*
```{r}
average_death_rate_FL <- mean(death_FL$rate_percent, na.rm = TRUE)
```
The average increase of death is 6.13% per day in Florida.
```{r}
average_deaths_per_day_FL <- mean(death_FL$diff_death, na.rm = TRUE)
```
The average deaths per day in Florida is 54.02

*We will utilize the above averages in an ANOVA (Analysis of Variance) test to compare statistical significance at the end of the analysis.*

## Florida vs Texas
Again, we will first start by comparing the average difference in deaths between Florida and Texas. We will conduct another Independent 2-sample t-test and generate a confidence interval to see if Florida and Texas' average deaths differ.

This t-test will tell us how significant the average difference of death is between Florida and Texas.

* Null Hypothesis (Ho): The average difference of death for Florida and Texas are the same (do not differ and equal to zero)

* Alternative Hypothesis (Ha): The average difference of death for Florida and Texas are different (not equal to zero)

To test the hypothesis, we will need the data set to show data for only Florida and Texas
```{r, results='hide'}
FL_TX_data <- data %>%
  filter(state == "FL" | state == "TX") %>%
  arrange(state, date) %>%
  select(date : state, death)
```

**We will then visualize the variance between the two states with a box plot. As we can see, Florida has more variability then Texas over the same time frame:**

```{r, warning=FALSE}
ggplot(data = FL_TX_data) +
  geom_boxplot(aes(x = state, y = death)) +
  coord_flip() +
  labs(x = "State", 
       y = "Deaths",
       title = "Deaths in Florida vs Texas",
       subtitle = "March 2020 - August 2020",
       caption = "Source: https://covidtracking.com/") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
```

Now we can run the t-test keeping in mind, our null hypothesis is equal to zero, alternative is two.sided, confidence interval is set to 95%, variance is not equal (as shown in the box plot above) and they are independent populations.
```{r}
t.test(FL_TX_data$death ~ FL_TX_data$state, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
```
The output tells us the p-value is less than alpha = 5%, therefore we can reject our null hypothesis.
With 95% confidence, we can say that the difference within means fall between 98.05 to 1014.56
Since zero is not in the confidence interval as well, we can reject the null hypothesis with 95% confidence.

After analyzing the box plot that shows the variance between Texas and Florida, it shows that there was a spike in deaths by the outliers. Which is shown between 6,000 to 8,000 deaths. Based on that, could we hypothesize that the rate of death is greater in Texas than in Florida?

* Ho: The rate of death in Texas is the same as that of Florida
* Ha: The rate of death in Texas is greater than that of Florida

**We will tidy our data to only show death statistics in Texas:** 

```{r, results='hide'}
death_TX <- FL_TX_data %>%
  filter(state == "TX") %>%
  mutate(diff_day = date - lag(date),
         diff_death = death - lag(death),
         rate_percent = diff_death/lag(death) * 100)
```

*Calculate the average rate of growth for deaths and average deaths per day in Texas:*
```{r}
average_death_rate_TX <- mean(death_TX$rate_percent, na.rm = TRUE)
```
Average rate of death is 7.14% per day in TX.
```{r}
average_deaths_per_day_TX <- mean(death_TX$diff_death, na.rm = TRUE)
```
Average deaths per day in Texas is 56.61

**Note, from the previous hypothesis test between FL and CA. Florida's average death rate and average deaths per day are as shown below:**
```{r}
average_death_rate_FL <- mean(death_FL$rate_percent, na.rm = TRUE)
```
The average increase of death is 6.13% per day in Florida.
```{r}
average_deaths_per_day_FL <- mean(death_FL$diff_death, na.rm = TRUE)
```
The average deaths per day in Florida is 54.02

*We will utilize the above averages in an ANOVA (Analysis of Variance) test to compare statistical significance, between all three states, in the next section!*

## Florida vs California vs Texas
We will compare the data for Florida, California, and Texas in regards to the average percent increase in deaths per day and average deaths per day.

We will organize all relevant data on a table and visualize the percent increase of death on a scatter plot with a regression line. 

Lastly, we will compare all three states regarding the above two factors (average percent increase in deaths per day and average deaths per day) to test statistical significance with an ANOVA (Analysis of Variance) test!

**Below is a table that shows the relevant statistics for all three states:**

*(positive cases, average percent increase in deaths per day, and average deaths per day)*

```{r}
three_state_statistics <- tribble(
  ~state, ~positive_cases, ~avg_percent_inc_death, ~avg_deaths_per_day,
  "CA", total_cases_all$positive[1], average_death_rate_CA, average_deaths_per_day_CA,
  "FL", total_cases_all$positive[2], average_death_rate_FL, average_deaths_per_day_FL,
  "TX", total_cases_all$positive[3], average_death_rate_TX, average_deaths_per_day_TX
)
knitr::kable(three_state_statistics, caption = "Relevant State Statistics")
```

* It is interesting to see that despite having the most cases, California has the lowest average percent increase in deaths out of the three states.

* It is also interesting to see that Texas, with the lowest amount of positive cases, has the highest average percent increase in deaths.

**We will fit a non-linear regression line for the relationship between Y = death rate, X1 = date, and X2 = state (TX, FL, and CA)
This will show the rate of death over 5 months, between Florida, California, and Texas.**

```{r, warning=FALSE, message=FALSE}
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
```

Based on the percent increase of deaths per day, we can conclude visually and mathematically that Texas has the highest at 7.14% increase per day, Florida with a 6.13% increase per day, and California with the lowest at 5.81% increase per day.

*Is this statistically significant?*

#### **We can run an ANOVA (Analysis of Variance) test to see if the average % increase of deaths per day are equal (not statistically significant) within the three states.**

If they are not equal we can run further tests to see which one(s) are different.

* Our Null Hypothesis (Ho) is: The average percent increase of deaths per day between the three states is not significantly different.

* Our Alternative Hypothesis (Ha) is: At least one of the states will have a difference in average percent increase of deaths per day. (the ANOVA test will not tell us which one differs)

```{r}
ANOVA1 <- aov(death_FL_TX_CA$rate_percent ~ death_FL_TX_CA$state)
summary(ANOVA1) # To run a more informative summary
```

Our p-value is 57.9%. Therefore, we fail to reject our null hypothesis. This means that the average percent increase of deaths per day between the three states is not significantly different.

```{r}
plot(TukeyHSD(ANOVA1)) # To visualize, the means of death rate per day do not differ from each other.
```

This visual shows that the means of death rate per day do not statistically differ from each other.

#### **Next, we can run an ANOVA test to see if the average deaths per day are equal (not statistically significant) within the three states.**

* Our Null Hypothesis (Ho) is: The average deaths per day between the 3 states are not significantly different.

* Our Alternative Hypothesis (Ha) is: At least one of the states will have a difference in average deaths per day. (the ANOVA test will not tell us which one differs)

```{r}
ANOVA2 <- aov(death_FL_TX_CA$diff_death ~ death_FL_TX_CA$state)
summary(ANOVA2) # To run a more informative summary
```

Our p-value is 14.6%. Therefore, we fail to reject our null hypothesis. This means that the average deaths per day between the 3 states are not significantly different.

```{r}
plot(TukeyHSD(ANOVA2)) # To visualize, the means of deaths per day do not differ from each other.
```

This visual shows that the average deaths per day do not statistically differ from each other.

## Conclusion

The death rate and total deaths within a state are imperative to understanding the conditions and responsiveness to the current global pandemic, COVID-19. The increase of death rate as shown in Florida, California, and Texas continues to contribute to the grimness of reality we live in today. As shown in this analysis there are certain states, such as Texas, that are showing a rapid growth in deaths that exceed other states with similar or substantially more positive cases.

As shown in the statistical analysis, the mean differences between death rates are not significant among the independent populations. Therefore, further analysis is needed to remedy this growth issue that is shared between the populations. 

**Opportunities for further analysis could be focusing on the above states and researching the following:** 

* The effectiveness of the state lockdowns

* The consequences of premature state reopenings

* The protocol adherence of the state residents

* The process of recording COVID related deaths

* The recovery rate of positive COVID-19 cases













