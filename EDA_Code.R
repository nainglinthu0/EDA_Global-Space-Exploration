library(dplyr)
library(ggplot2)

data <- read.csv("C:/Users/Administrator/Yever Dropbox/Naing Lin Thu/Personal_NAING/Parami_Naing/Data Communications and Ethics/Midterm/Global_Space_Exploration_Dataset.csv")
View(data)
head(data)
summary(data)


#We will check the missing values.
colSums(is.na(data))
#We do not have missing values.

data <- data %>% distinct()

data
#We are ready to start the analysis.
---------------------------------------------------------------------------------------------------
  
  #I have one big question. How have countries contributed to space exploration? We will look at it in terms of budget, mission types, success rates, environmental impact and technology use.
  #All 10 analysis questions will be used to answer that one big question.
  
  #Which country has conducted the most space missions?
  #Which country has invested the highest total budget in space missions?
  #What is the trend of total space mission budgets over the years?
  #Which mission type (Manned or Unmanned) is more common, and how does it vary across countries?
  #What are the success rates of space missions, and which countries achieve the highest success?
  #How do satellite types vary across countries?
  #Which launch sites are the most frequently used?
  #What are the trends in the use of advanced technologies (Reusable Rocket, AI Navigation, Solar Propulsion) over the years?
  #What is the environmental impact of space missions across different countries?
  #Which missions have the longest duration, and what factors contribute to them? 
  
  ------------------------------------------------------------------------------------------------------------
  #Which country has conducted the most space missions?
  missions_per_country <- data %>%
  count(Country, name = "Number_of_Missions") %>%
  arrange(desc(Number_of_Missions))

head(missions_per_country)

#We can see that China and UK have conducted the most space missions (322), followed by Israel, France, UAE and USA.

--------------------------------------------------------------------------------------------------------------------
  #Which country has invested the highest total budget in space missions?
  
  total_budget_country <- data %>%
  group_by(Country) %>%
  summarise(Total_Budget = sum(Budget..in.Billion...)) %>%
  arrange(desc(Total_Budget))

head(total_budget_country)

#Not surprisingly, China and UK had the most budget invested for space mission.
#But, it's interesting to see France was third country that spend most for the space mission even though the number of mission was less than Israel.
#USA and India was the 4th and 5th country that spend most for space mission.
#This acknowledges that even though the number of mission is relatively low compared to the other top countries, they allocated significant amount of budgets.

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #What is the trend of total space mission budgets over the years?
  
  total_budget_year <- data %>%
  group_by(Year) %>%
  summarise(Total_Budget = sum(Budget..in.Billion...))

total_budget_year

# Load ggplot2 for visualization
library(ggplot2)

# Visualize total budget per year using a line chart
ggplot(total_budget_year, aes(x = Year, y = Total_Budget)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    title = "Total Space Mission Budget Over the Years",
    x = "Year",
    y = "Total Budget (in Billion $)"
  ) +
  theme_minimal()


#We can say that the total budget trends are up and down and it may be due to many reasons.
#2000-2005: 2,688?3,123 billion dollars annually.
#2006-2010: 2,521?3,312 billion dollars annually.
#2011-2015: 2,730?3,589 billion dollars annually.
#2016-2020: 2,634?3,317 billion dollars annually.
#2021-2025: 2,312?3,291 billion dollars annually.

-------------------------------------------------------------------------------------------------------------
  #Which mission type (Manned or Unmanned) is more common, and how does it vary across countries?
  #Manned = human involved, unmanned = no human involved
  missions_type_country <- data %>%
  group_by(Country, Mission.Type) %>%
  summarise(Number_of_Missions = n()) %>%
  arrange(desc(Number_of_Missions))

missions_type_country

#We can say that Israel, Uk and USA have more manned missions.
#China, UAE and Japan have more unmanned missions.
#Typically, manned missions are good for complex problems and 
#unmanned missions can be useful to explore potentially hazardous environments.

---------------------------------------------------------------------------------------------------------------------------
  #What are the success rates of space missions, and which countries achieve the highest success?
  
  success_rate_country <- data %>%
  group_by(Country) %>%
  summarise(Average_Success_Rate = mean(Success.Rate....)) %>%
  arrange(desc(Average_Success_Rate))

head(success_rate_country)

#We can say that on average, Germany is the highest average success rate of 76.2%. It means that it has consistent success rate compared to others.
#India, France and Russia is followed after Germany.

#I am interested in the success rate based on the mission type.
# Calculate the average success rate for manned and unmanned missions
success_rate_mission_type <- data %>%
  group_by(Mission.Type) %>%
  summarise(Average_Success_Rate = mean(Success.Rate....))

# View the result
print(success_rate_mission_type)

#Manned mission type has more average success rate but it's not a big gap.
--------------------------------------------------------------------------------------------------
  #How do satellite types vary across countries? 
  
  satellite_types_country <- data %>%
  group_by(Country, Satellite.Type) %>%
  summarise(Number_of_Missions = n()) %>%
  arrange(desc(Number_of_Missions))

satellite_types_country
#Satellite types are used for different purposes. Each country has their own priorities.
#Russia and USA focus more on research satellites while France prioritizes spy satellites.
#UK has multiple satellite types such as weather, research and spy.

--------------------------------------------------------------------------------------------------------------
  #Which launch sites are the most frequently used?
  launch_sites <- data %>%
  count(Launch.Site, name = "Number_of_Missions") %>%
  arrange(desc(Number_of_Missions))

head(launch_sites)

#West Michael has hosted the most missions, 5.
#Then, it is followed by Ashleymouth, Michaelview, New James, New Michael, New Steven (4).
#We can say that many launch sites were used and the missions weren't happening in just one location.
#This may be to avoid the delays of the missions.

------------------------------------------------------------------------------------------------------------------------------
  #What are the trends in the use of advanced technologies over the years?
  tech_trends <- data %>%
  group_by(Year, Technology.Used) %>%
  summarise(Number_of_Missions = n()) %>%
  arrange(Year, desc(Number_of_Missions))

tech_trends

#Visual
ggplot(tech_trends, aes(x = Number_of_Missions, y = Technology.Used, fill = as.factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Technology Used by Year",
    x = "Number of Missions",
    y = "Technology",
    fill = "Year"
  ) +
  theme_minimal()

# We will count the number of missions for each technology used
tech_count <- data %>%
  count(Technology.Used, name = "Number_of_Missions") %>%
  arrange(desc(Number_of_Missions))
head(tech_count)

# Visual
ggplot(tech_count, aes(x = reorder(Technology.Used, -Number_of_Missions), y = Number_of_Missions, fill = Technology.Used)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Most Relevant Technology Used in Space Missions",
    x = "Technology",
    y = "Number of Missions"
  ) +
  theme_minimal()
#Tradition Rocket is the most used technology, 650 followed by AI Navigation.
--------------------------------------------------------------------------------------------------------------------------------------------
  #What is the environmental impact of space missions across different countries?
  environmental_impact <- data %>%
  group_by(Country, Environmental.Impact) %>%
  summarise(Number_of_Missions = n()) %>%
  arrange(desc(Number_of_Missions))

environmental_impact

# Count the number of missions for each environmental impact level by country
impact_count <- data %>%
  count(Country, Environmental.Impact) %>%
  arrange(desc(n))
impact_count

# Visual
library(ggplot2)
ggplot(impact_count, aes(x = reorder(Country, -n), y = n, fill = Environmental.Impact)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Environmental Impact of Space Missions by Country",
    x = "Country",
    y = "Number of Missions",
    fill = "Environmental Impact"
  ) +
  theme_minimal()

#UK and UAE have most missions with medium and low environmental impact.
#China and Israel have mix of all environmental impacts.
#Overall, most countries have medium or low environmental impact.

-----------------------------------------------------------------------------------------------------------------------
  #Which missions have the longest duration, and what factors contribute to them?
  
  longest_missions <- data %>%
  arrange(desc(Duration..in.Days.)) %>%
  select(Country, Mission.Name, Duration..in.Days., Mission.Type, Environmental.Impact, Success.Rate....)

head(longest_missions)

#The longest mission is 365 days.
#It's both unmanned and manned mission types.
#Unmanned missions typically last longer. It may be due to the lack of necesscity for humans.
#Manned missions tend to have a greater environmental impact.

-----------------------------------------------------------------------------------------------------------------------------------
  #Insight:
  
  #1. China and the UK lead in space missions, suggesting they have strong programs.
  #2. Despite fewer missions, France, the USA, and India invest heavily in space exploration.
  #3. Manned missions are often carried out by the USA, UK, and Israel, while unmanned missions dominate in countries like China, UAE, and Japan.
  #4. Germany has more successful missions, and there is a growing trend of using advanced technologies like AI and reusable rockets even though the traditional rocket usage is most relevant.
  #5. Missions have different purposes. So, they use different satellites. Most missions aim for low to medium environmental impact.
  
  #Recommendation:
  
  #1. Small countries with limited budget should collaborate so that they can share costs and expertise.
  #2. Countries should focus on reusable rocket for greener rocket that help reduce environmental impact.
  
#Note: I have used only count(), arrange(), group_by(), summarise() since using only them is enough for the analysis.
#However, we can also use filter() to filter out whatever numbers we want for interactive purpose.

  
#Ei Phyu Sin Win
#Does the success rate has anything to do with the technology used?
  success_rate <- data %>%
  group_by(Mission.Type, Technology.Used) %>%
  summarise(
    Average_Success_Rate = mean(Success.Rate...., na.rm = TRUE),
    Total_Budget = sum(Budget..in.Billion..., na.rm = TRUE),
    MissionCount = n()
  ) %>%
  arrange(desc(Average_Success_Rate))

print(success_rate)

#Reusable rocket (Unmanned) has the highest success rate of 76.6%. We can also say that the mission type affects the success rates as some technologies do differently for each type.
#The total budget is vary for different technologies but the success rate does not correlate with higher budgets.
#To answer your question, yes! The success rate appears to be influenced by the techology used but the mission type also plays a role.
  

#Ko Bhome Khant
#Is there a difference in terms of space budget usage between manned mission and unmanned mission?
budget_by_mission_type <- data %>%
  group_by(Mission.Type) %>%
  summarise(
    Total_Budget = sum(Budget..in.Billion..., na.rm = TRUE),
    Average_Budget = mean(Budget..in.Billion..., na.rm = TRUE),
    Number_of_Missions = n()
  ) %>%
  arrange(desc(Total_Budget))

print(budget_by_mission_type)

#Manned missions have slightly higher total budget and the data indicates that the cost of conducting mission is almost the same amount regardless of manned or unmanned mission type.
#To answer your question, yes! There is a difference in budget usage but it is not significant.

  
summary_df <- data %>%
  filter(Mission.Type == "Unmanned") %>%
  group_by(Country) %>%
  summarise(
    Avg_Budget = mean(Budget..in.Billion..., na.rm = TRUE),
    Tech_Count = n_distinct(Technology.Used),
    Unmanned_Share = n() / nrow(filter(data, Country == unique(Country)))
  )
summary_df
#Tech count is the same for all countries. It means that countries use the same number of technologies in unmanned missions.
#Budget varies but it's not significant. To answer your question, No! The countries that prioritize unmanned missions don't necessarily allocate larger budgets based on technological capabilities.
  
  
  