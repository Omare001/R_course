# Name: Vintage (Omare Collins Mokaya)
# Date: 24/07/2026
# Assignment: Tutorial 2 Skill Check - COVID-19 dplyr exercises

# Q1 --------------------------------------------------------------
# Data placed in data/ folder inside the RStudio Project.
# (Assumes project structure: project_root/data/covid19_data.csv)

# Q2: Load dplyr ----------------------------------------------------
library(dplyr)
getwd()
# Q3: Read in the dataset -------------------------------------------
covid19 <- read.csv("C:\\Users\\Omare Collins\\Downloads\\covid19_data.csv")
# Q4: Dimensions ------------------------------------------------------
dim(covid19)
#  The dataset has 2214 rows and 8 columns

# Q5: Structure --------------------------------------------------------
str(covid19)
#  The 'continent' variable is a character (chr) data type

# Q6: Summary -----------------------------------------------------------
summary(covid19)
#The highest number of cases recorded in a single row is 4,335,894

# Q7: Smallest population in 2019 ----------------------------------------
covid19 %>%
  arrange(population_2019) %>%
  head(1)
#Holy_See (Vatican City) had the smallest population in 2019 (815 people)

# Q8: Filter for Ireland --------------------------------------------------
ireland_covid_data <- covid19 %>%
  filter(countries_and_territories == "Ireland")
head(ireland_covid_data)

# Q9: Filter for East Africa countries -------------------------------------
east_africa_covid_data <- covid19 %>%
  filter(countries_and_territories %in% c("Kenya", "Uganda", "Tanzania",
                                          "Rwanda", "Burundi"))
head(east_africa_covid_data)

# Q10: Create mortality_rate variable ---------------------------------------
mortality_covid19 <- covid19 %>%
  mutate(mortality_rate = (deaths / population_2019) * 1000)

# Q11: Sort by mortality_rate (descending) -----------------------------------
mortality_covid19 %>%
  arrange(desc(mortality_rate)) %>%
  head(5)
#San_Marino had the highest mortality rate, recorded in March 2020
# (mortality_rate ~ 0.726 per 1,000 people)

# Q12: Select specific columns -------------------------------------------------
mortality_covid19 %>%
  select(year_month, countries_and_territories, mortality_rate)

# Q13: Count observations per country --------------------------------------------
monthly_data_count <- covid19 %>%
  group_by(countries_and_territories) %>%
  count()

# Q14: Smallest number of observations -----------------------------------------
monthly_data_count %>%
  arrange(n)
#Vanuatu had the smallest number of observations (2 rows)

# Q15: Mean deaths in Ireland -------------------------------------------------------
ireland_covid_data %>%
  summarise(mean_deaths = mean(deaths))
#The mean monthly number of deaths in Ireland is approximately 177.4

# Q16: Total cases by continent -----------------------------------------------------
covid19 %>%
  group_by(continent) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(desc(total_cases))
#America had the highest number of reported cases (~30.9 million),
# followed by Europe (~21.4 million)

# Q17: Mean mortality rate by continent ----------------------------------------------
covid19 %>%
  mutate(mortality_rate = (deaths / population_2019) * 1000) %>%
  group_by(continent) %>%
  summarise(mean_mortality_rate = mean(mortality_rate)) %>%
  arrange(desc(mean_mortality_rate))
#Europe had the highest mean mortality rate (~0.046 per 1,000)

# Q18: Highest-case month for Europe and America --------------------------------------
covid19 %>%
  filter(continent %in% c("Europe", "America")) %>%
  group_by(continent, year_month) %>%
  summarise(total_cases = sum(cases), .groups = "drop") %>%
  arrange(desc(total_cases))
#Europe's highest case month was 2020-11 (~7.85 million cases)
# America's highest case month was 2020-11 (~6.28 million cases)

# Q19: Percentage case fatality for Africa & Europe ----------------------------------
covid19 %>%
  filter(continent %in% c("Africa", "Europe")) %>%
  group_by(countries_and_territories) %>%
  summarise(total_cases = sum(cases),
            total_deaths = sum(deaths)) %>%
  mutate(percentage_case_fatality = (total_deaths / total_cases) * 100) %>%
  arrange(desc(percentage_case_fatality))
#Isle_of_Man had the highest percentage case fatality (~6.76%),
# followed by Sudan (~6.30%) and Chad (~5.76%)





