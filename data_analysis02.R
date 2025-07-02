# data_analysis.R

# load packages
source("setup.R")

# Create summary statistics for specific columns in two data frames. The summary function generates: minimum, 1st quartile (25th percentile), median (50th percentile), mean, 3rd quartile (75th percentile), maximum.
Tab_CO <- summary(EBEW_CO$`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`)
Tab_TWU <- summary(EBEW_TWU$`TOTAL WATER USE (kgal)`)


# Create box plot for Carbon Dioxide Emissions in 2024
G_co2_BP <- ggplot(EBEW_CO, aes(y=`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`))+
  geom_boxplot(fill="orange")+
  scale_y_log10(labels = label_comma())+
  labs(title="Boxplot of CO2 Emissions in 2024",
       y = "Metric Tons CO2e")+
  theme_minimal()
# Create box plot for Total Water Use in 2024
G_TWU_BP <- ggplot(EBEW_TWU, aes(y=`TOTAL WATER USE (kgal)`))+
  geom_boxplot(fill="skyblue")+
  scale_y_log10(labels = label_comma()) +
  labs(title="Boxplot of CO2 Emissions in 2024",
       y = "Water Use kgal")+
  theme_minimal()
# Create bar graph for Total Water Use by Zip Codes
G_TWU_ZipCode <- ggplot(TabZipTWU, aes(x = reorder(`POSTAL CODE`, -total_TWU), y = total_TWU)) +
  geom_col(fill = "skyblue") +
  labs(title = "Top 3 Postal Codes by Total Water Use in 2024",
       x = "Postal Code",
       y = "Total Water Use (kgal)") +
  theme_minimal()
# Create bar graph for CO2 Emissions by Zip Codes
G_CO_ZipCode <-ggplot(TabZipCO,aes(x = reorder(`POSTAL CODE`, -total_CO), y = total_CO)) +
  geom_col(fill = "orange") +
  labs(title = "Top 3 Postal Codes by Total CO Use in 2024",
       x = "Postal Code",
       y = "Total CO  Use ") +
  theme_minimal()
G_CO_YrBuilt <- ggplot(EBEW_FILT, aes(x=`YEAR BUILT`,y=`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`))+
  geom_point(alpha = 0.4, color="orange")+ #alpha fcn reduces over plotting. each point becomes 40% more opaque
  labs(
    title = "Relation between Building Age and CO2 Emissions in 2024",
    x = "Year Built",
    y = "CO2 Emissions (Metric Tons)")+
  theme_minimal()
# graph for year built vs water use: 
G_TWU_YrBuilt <- ggplot(EBEW_FILT, aes(x=`YEAR BUILT`,y=`TOTAL WATER USE (kgal)`))+
  geom_point(alpha = 0.5,color="skyblue")+
  labs(
    title = "Relation between Year Built and Total Water Use in 2024",
    x = "Year Built",
    y = "Total Water Use (kgal)") +
  theme_minimal()


# 
G_CO_YrBuilt_BP <-ggplot(EBEW_FILT,aes(x=yr_built,y=`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`))+
  geom_boxplot(fill="orange")+
  labs(title="CO2 Emissions by Year Built",
       x = "Decade Built",
       y = "CO2 Emissions")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
G_TWU_YrBuilt_BP <- ggplot(EBEW_FILT,aes(x=yr_built,y=`TOTAL WATER USE (kgal)`))+
  geom_boxplot(fill="skyblue")+
  labs(title="Total Water Use by Year Built",
       x = "Decade Built",
       y = "Total Water Use (kgal)")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




G_C02_AGE <- EBEW_long %>% 
  mutate(age_group = cut(Building_Age,
                         breaks = c(0, 25, 50, 75, 100, Inf),
                         labels = c("0–25 yrs", "26–50 yrs", "51–75 yrs", "76–100 yrs", "100+ yrs"),
                         right = FALSE)) %>%
  group_by(`PROGRAM YEAR`, age_group) %>% 
  summarise(mean_CO2 = mean(`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`, na.rm = TRUE)) %>%
  ggplot(aes(x = `PROGRAM YEAR`, y = mean_CO2, color = age_group)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Mean CO2 Emissions by Building Age Group Over Time",
       x = "Program Year", y = "Mean CO₂ Emissions (Metric Tons CO₂e)",
       color = "Building Age") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
# a note: program collection started at 2016, so that's why the graph begins at 2016.
# replicate for Total Water Use
G_TWU_age <- EBEW_long %>% 
  mutate(age_group = cut(Building_Age,
                         breaks = c(0, 25, 50, 75, 100, Inf),
                         labels = c("0–25 yrs", "26–50 yrs", "51–75 yrs", "76–100 yrs", "100+ yrs"),
                         right = FALSE)) %>%
  group_by(`PROGRAM YEAR`, age_group) %>% 
  summarise(mean_TWU = mean(`TOTAL WATER USE (kgal)`, na.rm = TRUE)) %>%
  ggplot(aes(x = `PROGRAM YEAR`, y = mean_TWU, color = age_group)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Mean Total Water Use By Building Age Group Over Time",
       x = "Program Year", y = "Mean CO₂ Emissions (Metric Tons CO₂e)",
       color = "Building Age") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()




# graph for property types vs CO2 emitted
G_CO_PropType <- ggplot(top5_CO_polluters, aes(x =reorder(`PROPERTY TYPE`,-totalCO2),
                                               y = totalCO2)) +
  geom_col(fill = "orange") +
  labs(title = "Top 5 Non-Compliant Property Types by CO₂ Emission in 2024",
       x = "Property Type",
       y = "Total CO2 Emissions (Metric Tons)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# graph for property types vs water use
G_TWU_PropType <- ggplot(top3_TWU_polluters, aes(x =reorder(`PROPERTY TYPE`,-totalTWU),y = totalTWU)) +
  geom_col(fill = "skyblue") +
  labs(title = "Top 3 Non-Compliant Property Types by CO₂ Emissionsin 2024",
       x = "Property Type",
       y = "TOTAL WATER USE (kgal)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#  normalizes the count of non-compliant buildings by the total buildings that year
G_CompliancyYrs <- ggplot(compliance_trend, aes(x = `PROGRAM YEAR`, y = non_comp_rate)) +
  geom_line(size = 1.2, color = "firebrick") +
  geom_point(color = "firebrick") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Proportion of Non-Compliant Buildings Over Time",
    x = "Program Year",
    y = "Non-Compliance Rate"
  ) +
  theme_minimal() 
# raw counts instead (not percentages)
G_CompliancyYrs2 <- ggplot(compliance_trend, aes(x = `PROGRAM YEAR`, y = non_comp_count)) +
  geom_line(size = 1.2, color = "pink")+
  geom_point(color = "pink")+
  labs(
    title = "Non-Compliant Buildings Over Time",
    x = "Program Year",
    y = "Non-Compliance Raw Numbers"
  ) +
  theme_minimal()



# graph for top 3 addresses: water use
G_TWU_Top3_AD <- ggplot(top3_TWU_AD, aes(x = reorder(`BUILDING ADDRESS`, -`TOTAL WATER USE (kgal)`), 
                                         y = `TOTAL WATER USE (kgal)`)) +
  geom_col(fill = "skyblue") +
  labs(title = "Top 3 Buildings by Water Use in 2024",
       x = "Building Address",
       y = "Total Water Use (kgal)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# graph for top 3 addresses:CO2 emissions
G_CO_Top3_AD <- ggplot(top3_CO2_AD, aes(x = reorder(`BUILDING ADDRESS`, -`CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`),
                                        y = `CARBON DIOXIDE EMISSIONS (Metric Ton CO2e)`)) +
  geom_col(fill = "orange") +
  labs(title = "Top 3 Buildings by CO2 Emissions in 2024",
       x = "Building Adresses",
       y = "Carbon Dioxide Emissions") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 