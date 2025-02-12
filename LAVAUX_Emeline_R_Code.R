############################### R Project ############################### 

## Clean everything 

rm(list = ls())

## Packages needed

library(readr)
library(readxl) 
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(xtable)
library(stargazer)
library(knitr)


############################### Preparation of the Data ###############################


#### Preparation of Data on Health Care Expenditure :

Life_expectancy <- read_excel("C:/Users/emeli/MASTER 1/R/PROJECT/DATA/Life_expectancy.xlsx", 
                              sheet = "Sheet 1")

# Cleaning the dataset
str(Life_expectancy)

# Removing irrelevant columns from the dataset
Life_expectancy <- Life_expectancy %>%
  select(-3, -5, -7, -9, -5, -11, -13, -15, -17, -19, -20, -21) 

# Removing observations that do not correspond to specific countries using regular expressions
Life_expectancy <- Life_expectancy %>%
  filter(!grepl("Euro", TIME))

Life_expectancy <- Life_expectancy %>%
  filter(TIME != "GEO (Labels)")

# Removing observations with missing values using a loop over all variables 
remove_colon <- function(df) {
  for (var in names(df)) {
    # Apply the filter only to character columns
    if (is.character(df[[var]])) {
      df <- df %>% filter(get(var) != ":")
    }
  }
  return(df)
}

Life_expectancy <- remove_colon(Life_expectancy)

# Creating a table of life expectancy for every country from 2014 to 2022
Life_expectancy <- pivot_longer(Life_expectancy, cols = "2014":"2022", 
                                names_to = "year",
                                values_to = "Life_expectancy", 
                                values_drop_na = TRUE)

# Renaming columns
Life_expectancy <- dplyr::rename(Life_expectancy, Country = TIME)
Life_expectancy <- dplyr::rename(Life_expectancy, Year = year)

# Converting all stored data to numeric values 
Life_expectancy$`Life_expectancy` <- as.numeric(Life_expectancy$`Life_expectancy`)
Life_expectancy$Year <- as.numeric(Life_expectancy$Year)

#### Preparation of Data on Net Greenhouse Gas Emissions :

Net_greenhouse_gas_emissions <- read_excel("C:/Users/emeli/MASTER 1/R/PROJECT/DATA/NGGE.xlsx")

# Cleaning the dataset
colnames(Net_greenhouse_gas_emissions)

# Removing unnecessary columns
Net_greenhouse_gas_emissions <- Net_greenhouse_gas_emissions %>% 
  select(-c(1, 2, 3, 4, 5, 6, 10))

# Focusing only on years between 2013 and 2022 
Net_greenhouse_gas_emissions <- Net_greenhouse_gas_emissions[!grepl("^19", Net_greenhouse_gas_emissions$TIME_PERIOD), ]

Net_greenhouse_gas_emissions$TIME_PERIOD <- as.numeric(Net_greenhouse_gas_emissions$TIME_PERIOD)
Net_greenhouse_gas_emissions <- Net_greenhouse_gas_emissions[!(Net_greenhouse_gas_emissions$TIME_PERIOD >= 2000 & Net_greenhouse_gas_emissions$TIME_PERIOD <= 2013), ]

# Renaming every country 
country_names <- c(
  "AT" = "Austria",
  "BE" = "Belgium",
  "BG" = "Bulgaria",
  "CH" = "Switzerland",
  "CY" = "Cyprus",
  "CZ" = "Czechia",
  "DE" = "Germany",
  "DK" = "Denmark",
  "EE" = "Estonia",
  "EL" = "Greece",
  "ES" = "Spain",
  "FI" = "Finland", 
  "FR" = "France",
  "HR" = "Croatia",
  "HU" = "Hungary",
  "IE" = "Ireland",
  "IS" = "Iceland",
  "IT" = "Italy",
  "LT" = "Lithuania",
  "LU" = "Luxembourg",
  "LV" = "Latvia",
  "MT" = "Malta",
  "NL" = "Netherlands",
  "NO" = "Norway",
  "PL" = "Poland",
  "PT" = "Portugal",
  "RO" = "Romania",
  "SE" = "Sweden",
  "SI" = "Slovenia",
  "SK" = "Slovakia"
)

Net_greenhouse_gas_emissions$geo <- country_names[Net_greenhouse_gas_emissions$geo]

# Removing missing values
Net_greenhouse_gas_emissions <- drop_na(Net_greenhouse_gas_emissions)

str(Net_greenhouse_gas_emissions)

# Renaming columns 
Net_greenhouse_gas_emissions <- dplyr::rename(Net_greenhouse_gas_emissions, Country = geo)
Net_greenhouse_gas_emissions <- dplyr::rename(Net_greenhouse_gas_emissions, Year = TIME_PERIOD)
Net_greenhouse_gas_emissions <- dplyr::rename(Net_greenhouse_gas_emissions, Net_greenhouse_gas_emissions = OBS_VALUE)

# Converting all stored data to numeric values 
Net_greenhouse_gas_emissions$Year <- as.numeric(Net_greenhouse_gas_emissions$Year)
Net_greenhouse_gas_emissions$`Net_greenhouse_gas_emissions` <- as.numeric(Net_greenhouse_gas_emissions$`Net_greenhouse_gas_emissions`)

## Reshaping the life expectancy dataset because it contains more countries than the other one
countries_to_remove <- c("Serbia", "Germany including former GDR", "Albania", "Liechtenstein")
Life_expectancy <- Life_expectancy[!(Life_expectancy$Country %in% countries_to_remove), ]
head(Life_expectancy)

#####  Combine Data on net greenhouse gas emissions and life expectancy :

# Merging the two datasets
Data_share <- inner_join(Life_expectancy, Net_greenhouse_gas_emissions, by = c("Year" = "Year", "Country" = "Country"))
head(Data_share)

# Verifying the merge to check if there are any missing values from Life Expectancy
Data_share <- Data_share %>%
  mutate(match_status = ifelse(is.na(Life_expectancy), "Not Matched", "Matched")) 
print("Merged data with match status:")
print(Data_share)

# Everything is fine, so we can now remove this column and continue with our study 
Data_share <- Data_share %>% select(-match_status)

# 7) How many observations do you have in your final dataset ?
n_observations <- nrow(Data_share) 
print(paste("Number of observations in the final dataset:", n_observations))

write.csv(Data_share, file = "C:/Users/emeli/MASTER 1/R/PROJECT/DATA/Data_final.csv")


###############################  Descriptive statistics  ############################### 


rm(list = ls())

Data <- read_csv("C:/Users/emeli/MASTER 1/R/PROJECT/DATA/Data_final.csv" , show_col_types = FALSE)

# 8) What is the distribution of your variable X ? 

ggplot(Data, aes(x = Life_expectancy)) +
  geom_density(fill = "lightblue", alpha = 0.3) +  # Adjust alpha for balance
  labs(title = "Density Plot of Life Expectancy", 
       x = "Life Expectancy (Years)", 
       y = "Density") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("C:/Users/emeli/MASTER 1/R/PROJECT/GRAPHS/density_plot_life_expectancy.png", width = 8, height = 6)

# 9) What is the distribution of your variable Y ? 

ggplot(Data, aes(x = Net_greenhouse_gas_emissions)) +
  geom_density(fill = "lightpink", alpha = 0.3) +  # Softer green for environmental context with adjusted alpha
  labs(title = "Density Plot of Net Greenhouse Gas Emissions", 
       x = "Net Greenhouse Gas Emissions (Metric Tons)", 
       y = "Density") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("C:/Users/emeli/MASTER 1/R/PROJECT/GRAPHS/density_plot_greenhouse_emissions.png", width = 8, height = 6)

# 10) Summary statistics for X and Y

# Summary statistics for life expectancy with rounding :
Life_expectancy_stats <- Data %>%
  summarise(
    Variable = "Life Expectancy",
    Mean = round(mean(Life_expectancy, na.rm = TRUE), 2),
    SD = round(sd(Life_expectancy, na.rm = TRUE), 2),
    Min = round(min(Life_expectancy, na.rm = TRUE), 2),
    Max = round(max(Life_expectancy, na.rm = TRUE), 2)
  )

# Summary statistics for greenhouse gas emissions with rounding :
Net_greenhouse_gas_emissions_stats <- Data %>%
  summarise(
    Variable = "Share of greenhouse gas emissions",
    Mean = round(mean(Net_greenhouse_gas_emissions, na.rm = TRUE), 2),
    SD = round(sd(Net_greenhouse_gas_emissions, na.rm = TRUE), 2),
    Min = round(min(Net_greenhouse_gas_emissions, na.rm = TRUE), 2),
    Max = round(max(Net_greenhouse_gas_emissions, na.rm = TRUE), 2)
  )

# Combinaition of the two summary tables :
Summary_statistics <- bind_rows(Net_greenhouse_gas_emissions_stats, Life_expectancy_stats)

colnames(Summary_statistics) <- c("Variable", "Mean", "Standard Deviation", "Minimum", "Maximum")

write.csv(Summary_statistics, file = "C:/Users/emeli/MASTER 1/R/PROJECT/STATS/Summary_statistics.csv", row.names = FALSE)

knitr::kable(Summary_statistics, format = "latex", booktabs = TRUE)

# 11) Summary statistics per country : 

Country_summary <- Data %>%
  group_by(Country) %>%
  summarise(
    Mean_X = mean(Life_expectancy, na.rm = TRUE),
    SD_X = sd(Life_expectancy, na.rm = TRUE),
    Mean_Y = mean(Net_greenhouse_gas_emissions, na.rm = TRUE),
    SD_Y = sd(Net_greenhouse_gas_emissions, na.rm = TRUE)
  )
write.csv(Country_summary, file = "C:/Users/emeli/MASTER 1/R/PROJECT/STATS/Country_summary.csv", row.names = FALSE)

knitr::kable(Country_summary, format = "latex", booktabs = TRUE)

# 12) Average evolution for X and Y over the years :

# Average Life Expectancy by Year
Avg_life_expectancy <- Data %>%
  group_by(Year) %>%
  summarise(Mean_X = mean(Life_expectancy, na.rm = TRUE))

ggplot(Avg_life_expectancy, aes(x = Year, y = Mean_X)) +
  geom_line(color = "lightblue") +
  labs(title = "Average Life Expectancy \nover the years", 
       x = "Year", 
       y = "Average Life Expectancy (years)") +
  theme_minimal()

ggsave("C:/Users/emeli/MASTER 1/R/PROJECT/GRAPHS/Average_life_expectancy_over_years.png")

# Average Net Greenhouse Gas Emissions by Year
Avg_greenhouse_emissions <- Data %>%
  group_by(Year) %>%
  summarise(Mean_Y = mean(Net_greenhouse_gas_emissions, na.rm = TRUE))

ggplot(Avg_greenhouse_emissions, aes(x = Year, y = Mean_Y)) +
  geom_line(color = "lightpink") +
  labs(title = "Average Net Greenhouse Gas Emissions \nover the years", 
       x = "Year", 
       y = "Average Net Greenhouse Gas Emissions \n(metric tons per capita)") +
  theme_minimal()

ggsave("C:/Users/emeli/MASTER 1/R/PROJECT/GRAPHS/Average_greenhouse_emissions_over_years.png")

# 13) Select two countries and plot the average evolution for X and Y -> I will compare Norway and Poland

selected_countries <- c("Norway", "Poland")  

# Plot for Life Expectancy :
ggplot(Data %>% filter(Country %in% selected_countries), aes(x = Year, y = Life_expectancy, color = Country)) +
  geom_line(size = 1.2) +  # You can adjust line size if needed
  labs(title = "Life Expectancy over the Years \nfor Norway and Poland", 
       x = "Year", 
       y = "Life Expectancy (Years)") +
  scale_color_manual(values = c("Norway" = "darkgoldenrod1", "Poland" = "mistyrose")) + 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank()) 

ggsave("C:/Users/emeli/MASTER 1/R/PROJECT/GRAPHS/Life_expectancy_selected_countries.png", width = 8, height = 6)

# Plot for Greenhouse Gas Emissions :
ggplot(Data %>% filter(Country %in% selected_countries), 
        aes(x = Year, y = Net_greenhouse_gas_emissions, color = Country)) +
  geom_line(size = 1.2) +  # Adjust the line thickness if needed
  labs(title = "Net Greenhouse Gas Emissions Over the Years \nfor Norway and Poland", 
       x = "Year", 
       y = "Net Greenhouse Gas Emissions \n(metric tons per capita)") +
  scale_color_manual(values = c("Norway" = "darkgoldenrod1", "Poland" = "mistyrose")) +  
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_blank())  

ggsave("C:/Users/emeli/MASTER 1/R/PROJECT/GRAPHS/Greenhouse_emissions_selected_countries.png", width = 8, height = 6)

# 14) Compute the change in X and Y between the first and last year for each country
Change_summary <- Data %>%
  group_by(Country) %>%
  summarise(
    Change_X = Life_expectancy[Year == max(Year)] - Life_expectancy[Year == min(Year)],
    Change_Y = Net_greenhouse_gas_emissions[Year == max(Year)] - Net_greenhouse_gas_emissions[Year == min(Year)]
  )

# Rank countries by Change_X
Change_summary <- Change_summary %>%
  arrange(desc(Change_X))

# Plot for Change in Life Expectancy
ggplot(Change_summary, aes(x = reorder(Country, Change_X), y = Change_X)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Change in Life Expectancy by Country", 
       x = "Country", 
       y = "Change in Life Expectancy (years)") +
  theme_minimal()

# Rank countries by Change_Y
Change_summary <- Change_summary %>%
  arrange(desc(Change_Y))

# Plot for Change in Greenhouse Gas Emissions
ggplot(Change_summary, aes(x = reorder(Country, Change_Y), y = Change_Y)) +
  geom_bar(stat = "identity", fill = "lightpink") +
  coord_flip() +
  labs(title = "Change in Net Greenhouse \nvs. Gas Emissions by Country", 
       x = "Country", 
       y = "Change in Net Greenhouse Gas Emissions (metric tons per capita)") +
  theme_minimal()

# Combination of the two plots into a single graph
p1 <- ggplot(Change_summary, aes(x = reorder(Country, Change_X), y = Change_X)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Change in Life Expectancy", 
       x = "Country", 
       y = "Change in Life Expectancy \n(years)") +
  theme_minimal()

p2 <- ggplot(Change_summary, aes(x = reorder(Country, Change_Y), y = Change_Y)) +
  geom_bar(stat = "identity", fill = "lightpink") +
  coord_flip() +
  labs(title = "Change in Net GES", 
       x = "Country", 
       y = "Change in \nNet GES \n(metric tons per capita)") +
  theme_minimal()

grid.arrange(p1, p2, ncol = 2)
combined_plot <- grid.arrange(p1, p2, ncol = 2)

ggsave("C:/Users/emeli/MASTER 1/R/PROJECT/GRAPHS/Change_life_expectancy_and_emissions.png", 
       plot = combined_plot, width = 8, height = 6)


############################### Relationship between X and Y ############################### 


# 15) Scatterplot for the relationship between Y and X /
ggplot(Data, aes(x = Net_greenhouse_gas_emissions, y = Life_expectancy)) +
  geom_point(color = "lightpink", alpha = 0.6) +
  geom_smooth(method = "lm", color = "hotpink3", se = FALSE) +
  labs(title = "Scatterplot of Life Expectancy vs. Greenhouse \nGas Emissions", x = "Net Greenhouse \nGas Emissions", y = "Life Expectancy") +
  theme_minimal()

ggsave("C:/Users/emeli/MASTER 1/R/PROJECT/GRAPHS/Scatterplot_Life_Expectancy_X.png", width = 8, heigh = 6)

# 16) Scatterplot with quadratic fit
ggplot(Data, aes(x = Net_greenhouse_gas_emissions, y = Life_expectancy)) +
  geom_point(color = "lightpink", alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "hotpink3", se = FALSE) +
  labs(title = "Scatterplot with Quadratic Fit", x = "Net Greenhouse Gas Emissions", y = "Life Expectancy") +
  theme_minimal()

ggsave("C:/Users/emeli/MASTER 1/R/PROJECT/GRAPHS/Scatterplot_Quadratic_Fit.png", width = 8, heigh = 6)

# 17) Compute within-country changes and scatterplot :

# Calculate yearly changes in greenhouse gas emissions and life expectancy
Data <- Data %>%
  group_by(Country) %>%
  mutate(
    Yearly_change_greenhouse_gas_emissions = Net_greenhouse_gas_emissions - lag(Net_greenhouse_gas_emissions),
    yearly_change_life_expectancy = Life_expectancy - lag(Life_expectancy)
  )

Data_clean <- Data %>%
  filter(!is.na(Yearly_change_greenhouse_gas_emissions) & 
           !is.na(yearly_change_life_expectancy))

# Create scatterplot
ggplot(Data_clean, aes(x = Yearly_change_greenhouse_gas_emissions, y = yearly_change_life_expectancy)) +
  geom_point(color = "lightpink", alpha = 0.6) +
  geom_smooth(method = "lm", color = "hotpink3", se = FALSE) +
  labs(title = "Scatterplot of Yearly Change in Life Expectancy \nvs. Yearly Change in Greenhouse Gas Emissions", 
       x = "Change in Greenhouse Gas Emissions", 
       y = "Change in Life Expectancy") +
  theme_minimal()

ggsave("C:/Users/emeli/MASTER 1/R/PROJECT/GRAPHS/Scatterplot_Yearly_Changes.png", width = 8, heigh = 6)

# 18) Model 1: OLS regression of Net Greenhouse Gas Emissions (Y) on Life Expectancy (X)
model_1 <- lm(Net_greenhouse_gas_emissions ~ Life_expectancy, data = Data)

# 19) Model 2:Regress Y on X controlling for country fixed effects
model_2 <- lm(Net_greenhouse_gas_emissions ~ Life_expectancy + factor(Country), data = Data)

# 20) Model 3: Regress Y on X controlling for year and country fixed effects
model_3 <- lm(Net_greenhouse_gas_emissions ~ Life_expectancy + factor(Country) + factor(Year), data = Data)

stargazer(model_1, model_2, model_3, 
           type = "latex", 
           out = "C:/Users/emeli/MASTER 1/R/PROJECT/STATS/combined_results.tex", 
           title = "Regression of Net Greenhouse Gas Emissions on Life Expectancy", 
           column.labels = c("Model 1", "Model 2", "Model 3"), 
           covariate.labels = c("Life Expectancy"), 
           dep.var.labels = c("Net Greenhouse Gas Emissions"), 
           add.lines = list(c("Country Fixed Effects", "-", "Yes", "Yes"),
                            c("Year Fixed Effects", "-", "-", "Yes")),
           star.cutoffs = c(0.05, 0.01, 0.001)) 

# Extracting R-squared values from models

summary(model_1)$r.squared
summary(model_2)$r.squared
summary(model_3)$r.squared


