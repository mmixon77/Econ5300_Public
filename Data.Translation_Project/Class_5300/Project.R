library(tidyverse)
library(fixest)
library(rio)
library(vtable)
library(stringr)
library(lubridate)

#display all files that contain 'trends_up_to_' in file name
Google_Trends <- list.files(pattern = 'trends_up_to_', full.names=TRUE)


#use import_list() to read in that vector of filenames and bind all results together
Google_Trends <- import_list(Google_Trends, rbind = TRUE, fill = TRUE)


#aggregating the monthorweek data in Google_Trends
Google_Trends$monthorweek <- str_sub(Google_Trends$monthorweek, 1, 10)

Google_Trends$monthorweek <- ymd(Google_Trends$monthorweek)

Google_Trends$month <- floor_date(Google_Trends$monthorweek, unit = "month")

#aggregating to standardize the index variable by schname and keyword--now a one-unit change in the standardized index can be understood and interpreted as a one-standard-deviation change in the search interest
Google_Trends <- Google_Trends %>%
  group_by(schname, keyword) %>%
  mutate(
    standarized_index = (index - mean(index)) / sd(index)
  )

#reading in the Scorecard data
ScoreCard_data <- Most_Recent_Cohorts_Scorecard_Elements_ <- read_csv("Most+Recent+Cohorts+(Scorecard+Elements).csv")

#reading in the id_name_link data
id_name_link <- read_csv("id_name_link.csv")

#to count how many times each school name is in id_name_link and filter to get rid of any school names that show up more than once
school_names <- id_name_link %>%
  group_by(schname) %>%
  mutate(n = n()) %>%
  filter(n == 1)

#rename unitid 
ScoreCard_data <- ScoreCard_data %>% rename(unitid = UNITID)  

#merge schname and unitid data together, linking them to ScoreCard_data
merged <- Google_Trends %>%
  inner_join(school_names, by = "schname") %>%
  inner_join(ScoreCard_data, by = "unitid")

#limit the data just to colleges that predominantly grant bachelor's degrees
filtered <- merged %>%
  filter(PREDDEG == 3)

#rename variable to show median earnings
filtered <- filtered %>%
  rename(median_earn = 'md_earn_wne_p10-REPORTED-EARNINGS')

#making median_earn vector into numeric values
filtered$'median_earn' <- as.numeric(filtered$'median_earn')

#creating a variable to establish how high income versus low income is measured
high_low <- quantile(filtered$median_earn, 0.75, na.rm = TRUE)

#creating a dummy variable to determine median earnings are high or low, based on the threshold
filtered <- filtered %>%
  mutate(income = ifelse(median_earn > high_low, 0, 1))

#creating a variable for before and after ScoreCard
#due to the ScoreCard being released at the start of Sept. 2015, the before and after time is set for first day of Sept.
filtered <- filtered %>%
  mutate(After_ScoreCard = ymd(month) >= ymd('2015-09-01'))

#creating a variable for cost of college
filtered <- filtered %>%
  rename(cost = 'NPT4_PUB-AVERAGE-ANNUAL-COST')

#making cost vector into numeric value
filtered$'cost' <- as.numeric(filtered$'cost')

#regress standardized_index on median_earn, along with After_ScoreCard and cost
#the standardized index is the Outcome because it's the variation of clicks of the colleges(typo in standardized_index variable) 

model1 <- feols(standarized_index ~ median_earn + After_ScoreCard + cost, data = filtered)
summary(model1)

#regress standardized_index on median_earn along with After_ScoreCar
model2 <- feols(standarized_index ~ median_earn, data = filtered)
summary(model2)

#plotting model1

#plotting model2


#rename filtered to bach_degree_sch
#potentially get rid of hetero
#because the scorecard provided more info about the cost, in-state and out of state tuition and the acceptance rate, it 
#p-value is verry high so we would reject the null at 0
